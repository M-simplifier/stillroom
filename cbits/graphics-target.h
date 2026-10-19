// Included after Nature and GL declarations in both native renderers.
// Failed replacements never destroy the previously valid target.
int sr_graphics_target_allowed(int w, int h, int color);
static void freeTarget(RenderTexture2D r) {
    int parent = 0;
    glGetIntegerv(0x8CA6, &parent);
    if (r.texture.id) rlUnloadTexture(r.texture.id);
    // rlUnloadFramebuffer owns its attached depth texture.
    if (r.id) rlUnloadFramebuffer(r.id);
    rlEnableFramebuffer((unsigned int)parent == r.id ? 0 : (unsigned int)parent);
}
static RenderTexture2D target(Nature *s, int width, int height, int color) {
    RenderTexture2D r = {0};
    if (!sr_graphics_target_allowed(width, height, color)) {
        snprintf(s->errorText, sizeof(s->errorText), "Diagnostic allocation limit");
        return r;
    }
    int parent = 0;
    glGetIntegerv(0x8CA6, &parent);
    r.id = rlLoadFramebuffer();
    r.texture.width = width; r.texture.height = height;
    rlEnableFramebuffer(r.id);
    if (color) {
        r.texture.id = rlLoadTexture(NULL, width, height, PIXELFORMAT_UNCOMPRESSED_R16G16B16A16, 1);
        r.texture.mipmaps = 1;
        r.texture.format = PIXELFORMAT_UNCOMPRESSED_R16G16B16A16;
        rlFramebufferAttach(r.id, r.texture.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_TEXTURE2D, 0);
    }
    r.depth.id = rlLoadTextureDepth(width, height, false);
    r.depth.width = width; r.depth.height = height;
    r.depth.format = 19; r.depth.mipmaps = 1;
    rlFramebufferAttach(r.id, r.depth.id, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_TEXTURE2D, 0);
    int valid = r.id && r.depth.id && (!color || r.texture.id) && rlFramebufferComplete(r.id);
    rlEnableFramebuffer((unsigned int)parent);
    if (!valid) {
        freeTarget(r);
        snprintf(s->errorText, sizeof(s->errorText), "Framebuffer unavailable %dx%d", width, height);
        return (RenderTexture2D){0};
    }
    if (color) {
        SetTextureFilter(r.texture, TEXTURE_FILTER_BILINEAR);
        SetTextureWrap(r.texture, TEXTURE_WRAP_CLAMP);
    }
    SetTextureWrap(r.depth, TEXTURE_WRAP_CLAMP);
    return r;
}
static int resizeTargets(Nature *s, int w, int h, float reflectionScale) {
    RenderTexture2D hdr = target(s, w, h, 1);
    if (!hdr.id) return 0;
    RenderTexture2D reflection = target(s, (int)ceilf(w * reflectionScale), (int)ceilf(h * reflectionScale), 1);
    if (!reflection.id) { freeTarget(hdr); return 0; }
    freeTarget(s->hdr); freeTarget(s->reflection);
    s->hdr = hdr; s->reflection = reflection;
    s->currentWidth = w; s->currentHeight = h; s->cachedTick = -1;
    return 1;
}
// Allocation failures belong to representation selection, not the world/shader
// lifetime. Keep old targets until replacements succeed; remember a lower
// ceiling so a saved Ultra preference cannot retry a failed allocation forever.
static void allocationRecovered(Nature *s) {
    s->errorText[0] = 0;
    while (glGetError()) {} // discard errors from the failed allocation only
}
static int selectQuality(Nature *s, int *quality, int *ceiling, int desired) {
    if (!s->ready || s->errorText[0] || desired < 0 || desired > 3) return 0;
    int candidate = desired < *ceiling ? desired : *ceiling;
    for (;;) {
        if (candidate == *quality) return 1;
        int size = sr_shadow_size(candidate);
        RenderTexture2D shadow = target(s, size, size, 0);
        if (shadow.id) {
            freeTarget(s->shadowMap); s->shadowMap = shadow;
            *quality = candidate; s->currentWidth = 0; s->cachedTick = -1;
            s->blockedWidth = s->blockedHeight = 0;
            return 1;
        }
        if (candidate == 0) {
            if (s->shadowMap.id) {
                allocationRecovered(s); *ceiling = *quality = 0;
                s->currentWidth = 0; s->cachedTick = -1;
                return 1;
            }
            s->ready = 0; return 0;
        }
        allocationRecovered(s);
        *ceiling = --candidate;
    }
}
static int budgetTargets(Nature *s, int *quality, int *ceiling, float width, float height, int reference) {
    for (;;) {
        float ratio = reference ? 1.0f : sr_raster_ratio(*quality, width, height);
        int w = (int)fmaxf(1, width * ratio), h = (int)fmaxf(1, height * ratio);
        if (w == s->blockedWidth && h == s->blockedHeight && s->hdr.id) return 1;
        if (w == s->currentWidth && h == s->currentHeight && s->hdr.id) return 1;
        if (resizeTargets(s, w, h, reference ? 1.0f : sr_reflection_scale(*quality))) return 1;
        if (*quality == 0 || reference) {
            if (s->hdr.id && s->reflection.id && !reference) {
                allocationRecovered(s);
                s->currentWidth = s->hdr.texture.width;
                s->currentHeight = s->hdr.texture.height;
                s->blockedWidth = w; s->blockedHeight = h;
                return 1;
            }
            s->ready = 0; return 0;
        }
        allocationRecovered(s);
        *ceiling = *quality - 1;
        if (!selectQuality(s, quality, ceiling, *ceiling)) return 0;
    }
}
