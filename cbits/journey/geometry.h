// Deterministic landscapes. Shape, material detail and movement are generated.
// One native context owns all resources; callers bracket it within a raylib
// window. GPU objects are owned by journeyNature_load/journeyNature_free; draw
// marshals only scalars.
#include "../include/raylib.h"
#include "world.h"
#define RAYMATH_STATIC_INLINE
#include "../include/raymath.h"
#include "../include/rlgl.h"
#include <math.h>
#include "../graphics.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
__declspec(dllimport) void __stdcall glFinish(void);
__declspec(dllimport)
const unsigned char *__stdcall glGetString(unsigned int name);
__declspec(dllimport) unsigned int __stdcall glGetError(void);

typedef struct {
  Vector3 p, n;
  Vector2 uv, extra;
  Color c;
} Vertex;
typedef struct {
  Vertex *v;
  int count, capacity, failed;
} Builder;
typedef struct {
  Vector4 origin, params;
} Bone;
typedef struct {
  Mesh meshes[4], water;
  Texture2D bones, height;
  int boneCount, treeCount, grassCount;
} Geometry;
// Borrowed for a frame, after target selection. Allocation/free remain owned
// by Nature; both producer and consumer use this image-and-projection pair.
typedef struct {
  RenderTexture2D image;
  Matrix clipFromRender;
} ProjectedField;
typedef struct Nature {
  Builder builders[4];
  Mesh meshes[4], waterMesh;
  Shader landShader, waterShader, skyShader, postShader;
  Material material;
  RenderTexture2D shadowMap, reflection, hdr;
  Texture2D boneTexture, heightTexture;
  Bone bones[16000];
  int boneCount, treeCount, grassCount, currentWidth, currentHeight, ready,
      scene, reference;
  unsigned int draws;
  int blockedWidth, blockedHeight;
  long cachedTick;
  uint32_t seed;
  char errorText[256], infoText[1024];
  ProjectedField shadowField, reflectionField;
  Vector3 sun, referenceEye, renderOrigin;
  Camera3D camera;
  const char *common;
  Geometry cached[3];
  double generationMs[3];
} Nature;
__declspec(dllimport) void __stdcall glGetIntegerv(unsigned int name,
                                                   int *values);

static float randf(Nature *s) {
  s->seed ^= s->seed << 13;
  s->seed ^= s->seed >> 17;
  s->seed ^= s->seed << 5;
  return (s->seed >> 8) * (1.0f / 16777216.0f);
}
static float range(Nature *s, float a, float b) {
  return a + (b - a) * randf(s);
}
static float fractf(float x) { return x - floorf(x); }
static float hash2(float x, float z) {
  return fractf(sinf(x * 127.1f + z * 311.7f) * 43758.5453f);
}
static float noise(float x, float z) {
  float ix = floorf(x), iz = floorf(z), fx = x - ix, fz = z - iz;
  fx = fx * fx * (3 - 2 * fx);
  fz = fz * fz * (3 - 2 * fz);
  return Lerp(Lerp(hash2(ix, iz), hash2(ix + 1, iz), fx),
              Lerp(hash2(ix, iz + 1), hash2(ix + 1, iz + 1), fx), fz);
}
static float terrain(Nature *s, float x, float z) {
  (void)s;
  return (float)ground(x, z);
}
static Vector3 surface(Nature *s, float x, float z) {
  return (Vector3){x, terrain(s, x, z), z};
}
static Vector3 terrainNormal(Nature *s, float x, float z) {
  float d = .12f;
  return Vector3Normalize(
      (Vector3){terrain(s, x - d, z) - terrain(s, x + d, z), 2 * d,
                terrain(s, x, z - d) - terrain(s, x, z + d)});
}
static Color tint(float r, float g, float b, float a) {
  return (Color){(unsigned char)(Clamp(r, 0, 1) * 255),
                 (unsigned char)(Clamp(g, 0, 1) * 255),
                 (unsigned char)(Clamp(b, 0, 1) * 255),
                 (unsigned char)(Clamp(a, 0, 1) * 255)};
}
static void add(Builder *b, Vertex v) {
  if (b->failed)
    return;
  if (b->count >= b->capacity) {
    if (b->capacity >= 8388608) {
      b->failed = 1;
      return;
    }
    int capacity = b->capacity ? b->capacity * 2 : 16384;
    Vertex *next = realloc(b->v, (size_t)capacity * sizeof(Vertex));
    if (!next) {
      b->failed = 1;
      return;
    }
    b->v = next;
    b->capacity = capacity;
  }
  b->v[b->count++] = v;
}
static void tri(Builder *b, Vertex a, Vertex c, Vertex d) {
  add(b, a);
  add(b, c);
  add(b, d);
}
static Vertex vert(Vector3 p, Vector3 n, Vector2 uv, int bone, float phase,
                   Color c) {
  return (Vertex){p, n, uv, {(float)bone, phase}, c};
}
static int newBone(Nature *s, Vector3 origin, int parent, float stiffness,
                   float phase) {
  if (s->boneCount >= 16000) {
    snprintf(s->errorText, sizeof(s->errorText), "bone capacity exceeded");
    return -1;
  }
  int id = s->boneCount++;
  s->bones[id] = (Bone){{origin.x, origin.y, origin.z, (float)(parent + 1)},
                        {stiffness, phase, 0, 0}};
  return id;
}
static void tube(Nature *s, Vector3 a, Vector3 b, float ra, float rb, int bone,
                 Color color, int sides) {
  Vector3 axis = Vector3Normalize(Vector3Subtract(b, a));
  Vector3 u = Vector3Normalize(Vector3CrossProduct(
      axis, fabsf(axis.y) < .9f ? (Vector3){0, 1, 0} : (Vector3){1, 0, 0}));
  Vector3 v = Vector3CrossProduct(axis, u);
  for (int j = 0; j < sides; j++) {
    float p = j * 2 * PI / sides, q = (j + 1) * 2 * PI / sides;
    Vector3 n0 = Vector3Add(Vector3Scale(u, cosf(p)), Vector3Scale(v, sinf(p)));
    Vector3 n1 = Vector3Add(Vector3Scale(u, cosf(q)), Vector3Scale(v, sinf(q)));
    Vertex a0 = vert(Vector3Add(a, Vector3Scale(n0, ra)), n0,
                     (Vector2){(float)j / sides, a.y}, bone, 0, color);
    Vertex a1 = vert(Vector3Add(a, Vector3Scale(n1, ra)), n1,
                     (Vector2){(float)(j + 1) / sides, a.y}, bone, 0, color);
    Vertex b0 = vert(Vector3Add(b, Vector3Scale(n0, rb)), n0,
                     (Vector2){(float)j / sides, b.y}, bone, 0, color);
    Vertex b1 = vert(Vector3Add(b, Vector3Scale(n1, rb)), n1,
                     (Vector2){(float)(j + 1) / sides, b.y}, bone, 0, color);
    tri(&s->builders[1], a0, b0, a1);
    tri(&s->builders[1], a1, b0, b1);
  }
}
static void leaf(Nature *s, Vector3 base, Vector3 dir, float length, int bone,
                 Color color) {
  dir = Vector3Normalize(dir);
  Vector3 side = Vector3Normalize(Vector3CrossProduct(
      dir, (Vector3){range(s, -.5, .5), 1, range(s, -.5, .5)}));
  Vector3 n = Vector3Normalize(Vector3CrossProduct(side, dir));
  Vector3 mid = Vector3Add(base, Vector3Scale(dir, length * .48f));
  Vector3 fold = Vector3Add(mid, Vector3Scale(n, length * .055f));
  float phase = randf(s) * 2 * PI;
  Vector2 profile[6] = {{0, 0}, {.28f, -.30f}, {.70f, -.27f},
                        {1, 0}, {.70f, .27f},  {.28f, .30f}};
  Vertex rim[6], center = vert(fold, n, (Vector2){.48f, 0}, bone, phase, color);
  for (int i = 0; i < 6; i++) {
    Vector3 p =
        Vector3Add(base, Vector3Add(Vector3Scale(dir, profile[i].x * length),
                                    Vector3Scale(side, profile[i].y * length)));
    rim[i] = vert(p, n, (Vector2){profile[i].x, profile[i].y / .30f}, bone,
                  phase, color);
  }
  for (int i = 0; i < 6; i++)
    tri(&s->builders[2], rim[i], rim[(i + 1) % 6], center);
}
static void branch(Nature *s, Vector3 base, Vector3 dir, float len,
                   float radius, int depth, int parent, float hue,
                   float leafSize, int lod) {
  dir = Vector3Normalize(dir);
  int bone =
      newBone(s, base, parent, depth == 0 ? .018f : .012f, range(s, 0, 6.28));
  Vector3 bend = Vector3Add(Vector3Scale(dir, len),
                            (Vector3){range(s, -.12, .12) * len, .12f * len,
                                      range(s, -.12, .12) * len});
  Vector3 end = Vector3Add(base, bend), mid = Vector3Lerp(base, end, .52f);
  mid.y -= len * .07f;
  Color bark = tint(.43f + hue * .1f, .41f + hue * .08f, .32f + hue * .05f, 1);
  tube(s, base, mid, radius, radius * .67f, bone, bark, 5);
  tube(s, mid, end, radius * .67f, radius * .24f, bone, bark, 5);
  if (depth > 0) {
    int count = lod ? 2 : 3;
    for (int i = 0; i < count; i++) {
      float t = .44f + .5f * (float)i / (count - 1), a = range(s, -PI, PI);
      Vector3 d = Vector3Normalize(Vector3Add(
          Vector3Scale(dir, .55f),
          (Vector3){cosf(a) * .72f, range(s, .05, .6), sinf(a) * .72f}));
      branch(s, Vector3Lerp(base, end, t), d, len * range(s, .48f, .68f),
             radius * .43f, depth - 1, bone, hue, leafSize, lod);
    }
  } else {
    int count = lod ? 64 : 80;
    for (int j = 0; j < count; j++) {
      float t = range(s, .10f, 1.05f), a = j * 2.39996f;
      Vector3 p =
          Vector3Add(Vector3Lerp(base, end, t),
                     (Vector3){range(s, -.30f, .30f), range(s, -.25f, .25f),
                               range(s, -.30f, .30f)});
      Vector3 d = Vector3Normalize(
          Vector3Add(Vector3Scale(dir, .28f),
                     (Vector3){cosf(a), range(s, -.18f, .65f), sinf(a)}));
      Color c = tint(
          range(s, .075f, .15f) + hue * .42f, range(s, .18f, .29f) + hue * .09f,
          range(s, .045f, .10f) * (1 - .55f * hue), range(s, .5f, 1.0f));
      leaf(s, p, d, leafSize * range(s, .7f, 1.35f), bone, c);
    }
  }
}
static void tree(Nature *s, float x, float z, float h, int lod) {
  Vector3 root = surface(s, x, z);
  if (root.y < .12f)
    return;
  s->treeCount++;
  Habitat ecology = habitat(x, z);
  float hue = (float)(ecology.w[BIRCH] *
                      (.65 + .35 * wnoise(x * .009, z * .009))),
        lean = range(s, -.10, .10), phase = range(s, 0, 6.28);
  int trunk = newBone(s, root, -1, .0045f, phase);
  Vector3 prior = root;
  for (int i = 1; i <= 12; i++) {
    float t = i / 12.0f;
    Vector3 next =
        Vector3Add(root, (Vector3){lean * h * t * t, h * t,
                                   sinf(t * 3 + phase) * h * .025f * t});
    float r = h * .023f * powf(1 - (i - 1) / 13.0f, 1.15f);
    tube(s, prior, next, r, h * .023f * powf(1 - i / 13.0f, 1.15f), trunk,
         tint(.57, .55, .46, 1), 7);
    if (i >= 4 && i <= 11 && (!lod || i % 2 == 0)) {
      float a = i * 2.39996f + phase;
      Vector3 dir = {cosf(a), range(s, .38, .75), sinf(a)};
      float len = h * (.39f - .14f * t) * range(s, .85, 1.25);
      branch(s, prior, dir, len, r * .59f, 2, trunk, hue, lod ? .36f : .30f,
             lod);
    }
    prior = next;
  }
  branch(s, Vector3Add(root, (Vector3){lean * h * .8f, h * .84f, 0}),
         (Vector3){.05, 1, .1}, h * .22f, h * .025f * .18f, 2, trunk, hue, .28f,
         lod);
}
static void needleMass(Nature *s, Vector3 center, float radius, float vertical,
                       int bone, Color color) {
  // A volume of pointed sprays: no closed blob or distant sub-pixel needles.
  for (int i = 0; i < 20; i++) {
    float y = 1 - 2 * (i + .5f) / 20, a = i * 2.39996f, ring = sqrtf(1 - y * y);
    Vector3 outward = {cosf(a) * ring, y, sinf(a) * ring};
    Vector3 base = Vector3Add(center, (Vector3){outward.x * radius * .52f,
                                                outward.y * vertical * .52f,
                                                outward.z * radius * .52f});
    Vector3 side = Vector3Normalize(
        Vector3CrossProduct(outward, (Vector3){.05f, 1, .13f}));
    Vector3 n = Vector3Normalize(Vector3CrossProduct(side, outward));
    for (int j = -2; j <= 2; j++) {
      Vector3 dir =
          Vector3Normalize(Vector3Add(outward, Vector3Scale(side, j * .3f)));
      Vector3 tip = Vector3Add(base, (Vector3){dir.x * radius * .64f,
                                               dir.y * vertical * .80f,
                                               dir.z * radius * .64f});
      float width = radius * .065f;
      Vertex left = vert(Vector3Subtract(base, Vector3Scale(side, width)), n,
                         (Vector2){.25f, -1}, bone, a, color);
      Vertex right = vert(Vector3Add(base, Vector3Scale(side, width)), n,
                          (Vector2){.25f, 1}, bone, a, color);
      tri(&s->builders[2], left, right,
          vert(tip, n, (Vector2){1, 0}, bone, a, color));
    }
  }
}
static void conifer(Nature *s, float x, float z, float h) {
  Vector3 root = surface(s, x, z);
  if (root.y < .14f)
    return;
  s->treeCount++;
  int trunk = newBone(s, root, -1, .003f, range(s, 0, 6.28f));
  Vector3 top = Vector3Add(root, (Vector3){h * .025f, h, 0});
  tube(s, root, top, h * .025f, .025f, trunk, tint(.19, .15, .10, 1), 7);
  float phase = range(s, 0, 6.28f);
  int distant = 1,
      levels = 11; // World-space shape is independent of the camera.
  for (int level = 0; level < levels; level++) {
    float f = .28f + level * (.67f / (levels - 1)), y = h * f;
    int count = 4;
    for (int j = 0; j < count; j++) {
      float a = phase + j * 2 * PI / count + level * 1.31f;
      Vector3 base = Vector3Add(
          root, (Vector3){h * .025f * f, y + range(s, -.024f, .024f) * h, 0});
      float length = h * (1 - f) * range(s, .28f, .42f);
      Vector3 tip = Vector3Add(base, (Vector3){cosf(a) * length,
                                               -length * range(s, .15f, .35f),
                                               sinf(a) * length});
      int bone = newBone(s, base, trunk, .009f, phase + level * .7f + j);
      tube(s, base, tip, h * .010f * (1 - f), .008f, bone,
           tint(.23, .18, .10, 1), 4);
      for (int m = 0; m < (distant ? 2 : 3); m++) {
        float along = .37f + m * (distant ? .35f : .24f);
        Vector3 center = Vector3Lerp(base, tip, along);
        center.y += length * .10f;
        float radius = length * (1 - along) * .48f + .13f;
        needleMass(s, center, radius, radius * range(s, .80f, 1.65f), bone,
                   tint(.045, .115, .073, .68));
      }
      if (distant)
        continue;
      int samples = distant ? 6 : 8, sprays = distant ? 3 : 6;
      for (int k = 0; k < samples; k++) {
        float t = .16f + k * (.76f / (samples - 1));
        Vector3 p = Vector3Lerp(base, tip, t);
        for (int side = -1; side <= 1; side += 2) {
          Vector3 d = {cosf(a + side * .90f), .15f, sinf(a + side * .90f)};
          float reach = length * (1 - t) * .48f + .12f;
          Vector3 end = Vector3Add(p, Vector3Scale(d, reach));
          for (int n = 0; n < sprays; n++) {
            Vector3 at = Vector3Lerp(p, end, (float)n / sprays);
            at.y += range(s, -.20f, .24f) * length;
            Vector3 needle = {cosf(a + side * (.5f + n * .28f)),
                              range(s, -.2, .55),
                              sinf(a + side * (.5f + n * .28f))};
            leaf(s, at, needle, range(s, .24f, .42f) * (distant ? 1.5f : 1),
                 bone,
                 tint(range(s, .035, .075), range(s, .10, .18),
                      range(s, .075, .12), .75f));
          }
        }
      }
    }
  }
}
static void rock(Nature *s, float x, float z, float radius) {
  Vector3 root = surface(s, x, z);
  root.y -= radius * .22f;
  for (int j = 0; j < 5; j++)
    for (int i = 0; i < 9; i++) {
      Vertex v[4];
      for (int k = 0; k < 4; k++) {
        float u = (i + (k % 2)) * 2 * PI / 9, vv = (j + (k / 2)) * PI / 5;
        Vector3 n = {cosf(u) * sinf(vv), cosf(vv), sinf(u) * sinf(vv)};
        float r =
            radius * (.85f + .25f * noise(n.x * 3 + root.x, n.z * 3 + root.z));
        Vector3 p = Vector3Add(
            root, (Vector3){n.x * r, n.y * r * .62f, n.z * r * .85f});
        v[k] = vert(p, n, (Vector2){1, 0}, -1, 0, WHITE);
      }
      tri(&s->builders[0], v[0], v[2], v[1]);
      tri(&s->builders[0], v[1], v[2], v[3]);
    }
}
static void blade(Nature *s, float x, float z, float y, float h, float width,
                  Color c) {
  Vector3 root = {x, y, z};
  if (root.y < .10f)
    return;
  float a = range(s, 0, 2 * PI), curve = range(s, -.22f, .22f);
  Vector3 side = {cosf(a), 0, sinf(a)}, n = {-sinf(a), .18f, cosf(a)};
  Vertex left[4], right[4];
  for (int i = 0; i < 4; i++) {
    float t = i / 3.0f, w = width * (1 - t) * (.7f + .3f * sinf(t * PI));
    Vector3 p = Vector3Add(
        root, (Vector3){curve * t * t, h * t, -curve * .28f * t * t});
    left[i] = (Vertex){
        Vector3Subtract(p, Vector3Scale(side, w)), n, {t, root.y}, {x, z}, c};
    right[i] = (Vertex){
        Vector3Add(p, Vector3Scale(side, w)), n, {t, root.y}, {x, z}, c};
  }
  for (int i = 0; i < 3; i++) {
    tri(&s->builders[3], left[i], right[i], left[i + 1]);
    if (i < 2)
      tri(&s->builders[3], right[i], right[i + 1], left[i + 1]);
  }
  s->grassCount++;
}
static Mesh upload(Nature *s, Builder *b) {
  Mesh m = {0};
  m.vertexCount = b->count;
  m.triangleCount = b->count / 3;
  if (b->failed || !b->count) {
    snprintf(s->errorText, sizeof(s->errorText),
             "Landscape geometry allocation failed");
    return (Mesh){0};
  }
  m.vertices = MemAlloc(m.vertexCount * 3 * sizeof(float));
  m.normals = MemAlloc(m.vertexCount * 3 * sizeof(float));
  m.texcoords = MemAlloc(m.vertexCount * 2 * sizeof(float));
  m.texcoords2 = MemAlloc(m.vertexCount * 2 * sizeof(float));
  m.colors = MemAlloc(m.vertexCount * 4);
  if (!m.vertices || !m.normals || !m.texcoords || !m.texcoords2 || !m.colors) {
    MemFree(m.vertices);
    MemFree(m.normals);
    MemFree(m.texcoords);
    MemFree(m.texcoords2);
    MemFree(m.colors);
    snprintf(s->errorText, sizeof(s->errorText),
             "Landscape upload allocation failed");
    return (Mesh){0};
  }
  for (int i = 0; i < m.vertexCount; i++) {
    memcpy(m.vertices + 3 * i, &b->v[i].p, 3 * sizeof(float));
    memcpy(m.normals + 3 * i, &b->v[i].n, 3 * sizeof(float));
    memcpy(m.texcoords + 2 * i, &b->v[i].uv, 2 * sizeof(float));
    memcpy(m.texcoords2 + 2 * i, &b->v[i].extra, 2 * sizeof(float));
    memcpy(m.colors + 4 * i, &b->v[i].c, 4);
  }
  UploadMesh(&m, false);
  MemFree(m.vertices);
  MemFree(m.normals);
  MemFree(m.texcoords);
  MemFree(m.texcoords2);
  MemFree(m.colors);
  m.vertices = m.normals = m.texcoords = m.texcoords2 = NULL;
  m.colors = NULL;
  free(b->v);
  b->v = NULL;
  b->count = b->capacity = 0;
  return m;
}
static char *shaderText(Nature *s, const char *body) {
  if (!body || !s->common)
    return NULL;
  size_t len = strlen(body) + strlen(s->common) + 64;
  char *text = malloc(len);
  if (text)
    snprintf(text, len, "#version 330 core\n%s\n%s", s->common, body);
  return text;
}
static Shader program(Nature *s, const char *vs, const char *fs,
                      const char *required) {
  char *v = vs ? shaderText(s, vs) : NULL, *f = shaderText(s, fs);
  Shader result = {0};
  if (!f || (vs && !v)) {
    snprintf(s->errorText, sizeof(s->errorText), "Shader source unavailable");
    goto done;
  }
  result = LoadShaderFromMemory(v, f);
  if (result.id == rlGetShaderIdDefault() || !IsShaderValid(result) ||
      GetShaderLocation(result, required) < 0) {
    if (result.id && result.id != rlGetShaderIdDefault())
      UnloadShader(result);
    result = (Shader){0};
    snprintf(s->errorText, sizeof(s->errorText),
             "Landscape shader unavailable: %s", required);
  }
done:
  free(v);
  free(f);
  return result;
}
static void f1(Shader s, const char *n, float f) {
  int l = GetShaderLocation(s, n);
  if (l >= 0)
    SetShaderValue(s, l, &f, SHADER_UNIFORM_FLOAT);
}
static void i1(Shader s, const char *n, int i) {
  int l = GetShaderLocation(s, n);
  if (l >= 0)
    SetShaderValue(s, l, &i, SHADER_UNIFORM_INT);
}
static void v3(Shader s, const char *n, Vector3 v) {
  int l = GetShaderLocation(s, n);
  if (l >= 0)
    SetShaderValue(s, l, &v, SHADER_UNIFORM_VEC3);
}
static void bindTexture(Shader s, const char *name, Texture texture, int slot) {
  rlEnableShader(s.id);
  rlActiveTextureSlot(slot);
  rlEnableTexture(texture.id);
  int loc = GetShaderLocation(s, name);
  rlSetUniform(loc, &slot, SHADER_UNIFORM_INT, 1);
  rlActiveTextureSlot(0);
}
#include "../graphics-target.h"
static void clearGeometry(Nature *s) {
  for (int i = 0; i < 4; i++) {
    if (s->meshes[i].vaoId)
      UnloadMesh(s->meshes[i]);
    s->meshes[i] = (Mesh){0};
    free(s->builders[i].v);
    s->builders[i] = (Builder){0};
  }
  if (s->waterMesh.vaoId)
    UnloadMesh(s->waterMesh);
  s->waterMesh = (Mesh){0};
  if (s->boneTexture.id)
    UnloadTexture(s->boneTexture);
  if (s->heightTexture.id)
    UnloadTexture(s->heightTexture);
  s->boneTexture = s->heightTexture = (Texture){0};
  s->boneCount = s->treeCount = s->grassCount = 0;
}
static Geometry takeGeometry(Nature *s) {
  Geometry g = {0};
  memcpy(g.meshes, s->meshes, sizeof(g.meshes));
  g.water = s->waterMesh;
  g.bones = s->boneTexture;
  g.height = s->heightTexture;
  g.boneCount = s->boneCount;
  g.treeCount = s->treeCount;
  g.grassCount = s->grassCount;
  memset(s->meshes, 0, sizeof(s->meshes));
  s->waterMesh = (Mesh){0};
  s->boneTexture = s->heightTexture = (Texture){0};
  s->boneCount = s->treeCount = s->grassCount = 0;
  return g;
}
static void putGeometry(Nature *s, Geometry g) {
  memcpy(s->meshes, g.meshes, sizeof(s->meshes));
  s->waterMesh = g.water;
  s->boneTexture = g.bones;
  s->heightTexture = g.height;
  s->boneCount = g.boneCount;
  s->treeCount = g.treeCount;
  s->grassCount = g.grassCount;
}
static void journeyNature_free(Nature *s) {
  if (!s)
    return;
  clearGeometry(s);
  for (int i = 0; i < 3; i++) {
    putGeometry(s, s->cached[i]);
    clearGeometry(s);
  }
  Shader all[] = {s->landShader, s->waterShader, s->skyShader, s->postShader};
  for (int i = 0; i < 4; i++)
    if (all[i].id)
      UnloadShader(all[i]);
  freeTarget(s->shadowMap);
  freeTarget(s->reflection);
  freeTarget(s->hdr);
  if (s->material.maps)
    MemFree(s->material.maps);
  free(s);
}
static Nature *journeyNature_load(const char *const *sources) {
  Nature *s = calloc(1, sizeof(Nature));
  if (!s)
    return NULL;
  s->scene = -1;
  s->cachedTick = -1;
  s->common = sources[0];
  s->landShader = program(s, sources[1], sources[2], "uTime");
  s->waterShader = program(s, sources[3], sources[4], "uReflection");
  s->skyShader = program(s, NULL, sources[5], "uForward");
  s->postShader = program(s, NULL, sources[6], "texture0");
  s->common = NULL;
  if (!s->landShader.id || !s->waterShader.id || !s->skyShader.id ||
      !s->postShader.id)
    return s;
  s->material = LoadMaterialDefault();
  if (!s->material.maps)
    snprintf(s->errorText, sizeof(s->errorText),
             "Landscape material allocation failed");
  s->ready = !s->errorText[0];
  int quality = -1, ceiling = 3;
  selectQuality(s, &quality, &ceiling, 1);
  return s;
}
static int journeyNature_ready(Nature *s) { return s && s->ready; }
static const char *journeyNature_error(Nature *s) {
  return s ? s->errorText : "Landscape allocation failed";
}
static const char *journeyNature_info(Nature *s) {
  if (!s)
    return "{}";
  int triangles = 0;
  for (int i = 0; i < 4; i++)
    triangles += s->meshes[i].triangleCount;
  snprintf(
      s->infoText, sizeof(s->infoText),
      "{\"scene\":%d,\"trees\":%d,\"blades\":%d,\"triangles\":%d,\"draws\":%u,"
      "\"width\":%d,"
      "\"height\":%d,\"generationMs\":[%.3f,%.3f,%.3f],\"renderer\":\"%s\"}",
      s->scene, s->treeCount, s->grassCount,
      triangles + s->waterMesh.triangleCount, s->draws, s->currentWidth,
      s->currentHeight, s->generationMs[0], s->generationMs[1],
      s->generationMs[2], glGetString(0x1F01));
  return s->infoText;
}
static void journeyNature_sync(void) { glFinish(); }
// Diagnostic comparison only: identical geometry, camera, clock and shaders,
// with unrestricted raster size, full-size reflection and a 2048 shadow map.
static void journeyNature_reference(Nature *s, int enabled) {
  if (!s || !s->ready || s->reference == (enabled != 0))
    return;
  s->reference = enabled != 0;
  s->cachedTick = -1;
  s->currentWidth = 0;
  freeTarget(s->shadowMap);
  int size = s->reference ? 2048 : 1536;
  s->shadowMap = target(s, size, size, 0);
}
static void sky(Nature *s, Matrix view, Matrix projection, int w, int h) {
  i1(s->skyShader, "uLandscape", s->scene);
  // Consume the planned basis. Adding a tiny forward vector to a Float eye
  // and subtracting it again can erase the direction near a vertical view.
  Vector3 r = {view.m0, view.m4, view.m8},
          u = {view.m1, view.m5, view.m9},
          f = {-view.m2, -view.m6, -view.m10};
  Vector2 res = {(float)w, (float)h};
  SetShaderValue(s->skyShader, GetShaderLocation(s->skyShader, "uResolution"),
                 &res, SHADER_UNIFORM_VEC2);
  v3(s->skyShader, "uForward", f);
  v3(s->skyShader, "uRight", r);
  v3(s->skyShader, "uUp", u);
  v3(s->skyShader, "uSun", s->sun);
  f1(s->skyShader, "uTanFov", 1.0f / projection.m5);
  BeginShaderMode(s->skyShader);
  DrawRectangle(0, 0, w, h, WHITE);
  EndShaderMode();
}
static void world(Nature *s, float t, int mask, int shadow, int clip,
                  Camera3D cam) {
  i1(s->landShader, "uLandscape", s->scene);
  s->material.shader = s->landShader;
  i1(s->landShader, "uShadowPass", shadow);
  i1(s->landShader, "uClip", clip);
  v3(s->landShader, "uEye", cam.position);
  v3(s->landShader, "uSun", s->sun);
  SetShaderValueMatrix(
      s->landShader, GetShaderLocation(s->landShader, "uLightVP"), s->shadowField.clipFromRender);
  bindTexture(s->landShader, "uBones", s->boneTexture, 8);
  bindTexture(s->landShader, "uShadow", s->shadowField.image.depth, 9);
  rlDisableBackfaceCulling();
  for (int i = 0; i < 4; i++) {
    if (i == 3 && (shadow || !(mask & 4)))
      continue;
    i1(s->landShader, "uKind", i);
    f1(s->landShader, "uTime", ((i == 3 ? (mask & 1) : (mask & 2)) ? t : 0));
    if (s->meshes[i].vaoId)
      DrawMesh(s->meshes[i], s->material, MatrixIdentity());
  }
  rlEnableBackfaceCulling();
}
