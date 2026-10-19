// Nonblocking GL 3.3 timer queries. Window-thread ownership; no glFinish, no
// allocation per frame, no polling loop and no hardware-name heuristics.
#include "include/raylib.h"
#include "include/rlgl.h"
#include <stdint.h>
#include <stdlib.h>
__declspec(dllimport) void __stdcall glGetIntegerv(unsigned int, int *);
__declspec(dllimport) unsigned int __stdcall glGetError(void);
typedef void (__stdcall *Gen)(int, unsigned int *);
typedef void (__stdcall *Delete)(int, const unsigned int *);
typedef void (__stdcall *Begin)(unsigned int, unsigned int);
typedef void (__stdcall *End)(unsigned int);
typedef void (__stdcall *Available)(unsigned int, unsigned int, int *);
typedef void (__stdcall *Result)(unsigned int, unsigned int, uint64_t *);
typedef struct {
    unsigned int ids[4];
    int pending[4], active, cursor;
    Gen gen; Delete del; Begin begin; End end; Available available; Result result;
} Timer;
// Only the isolated graphics diagnostic sets these. Normal runs leave both 0.
static int checkColorLimit, checkShadowLimit;
void sr_graphics_allocation_limits_for_check(int color, int shadow) {
    checkColorLimit = color; checkShadowLimit = shadow;
}
int sr_graphics_target_allowed(int w, int h, int color) {
    int limit = color ? checkColorLimit : checkShadowLimit;
    return limit <= 0 || (w <= limit && h <= limit);
}
int sr_graphics_surface(int w, int h, RenderTexture2D *out) {
    *out = (RenderTexture2D){0};
    if (!sr_graphics_target_allowed(w, h, 1)) return 0;
    int parent = 0;
    glGetIntegerv(0x8CA6, &parent);
    RenderTexture2D next = LoadRenderTexture(w, h);
    rlEnableFramebuffer(next.id);
    int valid = next.id && next.texture.id && next.depth.id && rlFramebufferComplete(next.id);
    if (!valid) {
        if (next.id) UnloadRenderTexture(next);
        while (glGetError()) {}
    } else {
        SetTextureFilter(next.texture, TEXTURE_FILTER_BILINEAR);
        *out = next;
    }
    rlEnableFramebuffer((unsigned int)parent);
    return valid;
}
void *sr_graphics_open(void) {
    Timer *t = calloc(1, sizeof(Timer));
    if (!t) return NULL;
    t->gen = (Gen)rlGetProcAddress("glGenQueries");
    t->del = (Delete)rlGetProcAddress("glDeleteQueries");
    t->begin = (Begin)rlGetProcAddress("glBeginQuery");
    t->end = (End)rlGetProcAddress("glEndQuery");
    t->available = (Available)rlGetProcAddress("glGetQueryObjectiv");
    t->result = (Result)rlGetProcAddress("glGetQueryObjectui64v");
    if (!t->gen || !t->del || !t->begin || !t->end || !t->available || !t->result) {
        free(t); return NULL;
    }
    t->gen(4, t->ids);
    t->active = -1;
    return t;
}
void sr_graphics_close(Timer *t) {
    if (!t) return;
    if (t->active >= 0) t->end(0x88BF);
    t->del(4, t->ids);
    free(t);
}
// Return at most one completed sample, in submission order. -1 means no data.
double sr_graphics_sample(Timer *t) {
    if (!t) return -1;
    for (int n = 0; n < 4; n++) {
        int i = (t->cursor + n) % 4;
        if (!t->pending[i]) continue;
        int done = 0;
        t->available(t->ids[i], 0x8867, &done);
        if (!done) return -1;
        uint64_t ns = 0;
        t->result(t->ids[i], 0x8866, &ns);
        t->pending[i] = 0;
        return (double)ns / 1000000.0;
    }
    return -1;
}
void sr_graphics_begin(Timer *t) {
    if (!t || t->active >= 0 || t->pending[t->cursor]) return;
    rlDrawRenderBatchActive();
    t->begin(0x88BF, t->ids[t->cursor]);
    t->active = t->cursor;
}
void sr_graphics_end(Timer *t) {
    if (!t || t->active < 0) return;
    rlDrawRenderBatchActive();
    t->end(0x88BF);
    t->pending[t->active] = 1;
    t->cursor = (t->active + 1) % 4;
    t->active = -1;
}
