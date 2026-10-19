// Shared representation budgets; no simulation/terrain parameters live here.
#ifndef STILLROOM_GRAPHICS_H
#define STILLROOM_GRAPHICS_H
static int sr_shadow_size(int q) { return q == 3 ? 4096 : q == 2 ? 3072 : q == 1 ? 2048 : 1024; }
static float sr_reflection_scale(int q) { return q == 3 ? 1.0f : q == 2 ? .75f : .5f; }
static float sr_raster_ratio(int q, float w, float h) {
    float mw = q == 3 ? 3840 : q == 2 ? 2560 : q == 1 ? 1600 : 1120;
    float mh = q == 3 ? 2160 : q == 2 ? 1440 : q == 1 ? 900 : 630;
    return fminf(q == 3 ? 1.5f : 1.0f, fminf(mw / w, mh / h));
}
#endif
