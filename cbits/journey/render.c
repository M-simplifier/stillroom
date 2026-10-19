// Bounded streamed journey renderer. Haskell owns this context on its window
// thread.
#include "geometry.h"
#include "ecology.h"
#include "railway.h"

#define SIDE 7
#define CAPACITY (SIDE * SIDE)
#define TILE 32
#define FAR_SIDE 9
#define FAR_CAPACITY (FAR_SIDE * FAR_SIDE)
#define FAR_TILE 128
double sr_journey_geometry_height(uint32_t seed, double x, double z) {
  selectWorld(seed);
  return (double)ground((float)x, (float)z);
}
typedef struct {
  int valid, x, z;
  Geometry g;
  uint64_t signature;
} Chunk;
typedef struct {
  Nature *n;
  uint32_t seed;
  Mesh railcar;
  double carS, carSpeed;
  int aboard;
  int quality; // Preview only: 0 released, 1 detailed, 2 supersampled.
  Chunk chunks[CAPACITY], farChunks[FAR_CAPACITY];
  unsigned int generated, evicted, frames, farGenerated, farEvicted;
  double maxGenerationMs, lastGenerationMs;
  int centerX, centerZ, nearMissing;
  char info[2048];
} WalkWorld;

static int makeChunk(WalkWorld *w, Chunk *c, int cx, int cz, int far) {
  Nature *s = w->n;
  double began = GetTime();
  int span = far ? FAR_TILE : TILE, cells = far ? 16 : 32, stride = cells + 1;
  float stepSize = (float)span / cells;
  float ox = cx * span, oz = cz * span;
  s->seed = whash(cx, cz) | 1u;
  s->scene = 0;
  Vertex grid[33 * 33];
  float heights[33 * 33];
  float apron[35 * 35];
  int apronStride = cells + 3;
  // One authoritative height evaluation per lattice point. The one-cell apron
  // keeps normals identical at chunk seams. Grass roots and LOD morph targets
  // interpolate these actual terrain triangles instead of recomputing noise.
  for (int j = -1; j <= cells + 1; j++)
    for (int i = -1; i <= cells + 1; i++)
      apron[(j + 1) * apronStride + i + 1] =
          (float)ground(ox + i * stepSize, oz + j * stepSize);
  for (int j = 0; j <= cells; j++)
    for (int i = 0; i <= cells; i++)
      heights[j * stride + i] = apron[(j + 1) * apronStride + i + 1];
  float low = 1e9f;
  for (int j = 0; j <= cells; j++)
    for (int i = 0; i <= cells; i++) {
      float x = ox + i * stepSize, z = oz + j * stepSize;
      int center = (j + 1) * apronStride + i + 1;
      Vector3 normal = Vector3Normalize(
          (Vector3){apron[center - 1] - apron[center + 1], 2 * stepSize,
                    apron[center - apronStride] - apron[center + apronStride]});
      grid[j * stride + i] =
          vert((Vector3){x, heights[j * stride + i], z}, normal,
               (Vector2){0, 0}, -1, 0, terrainTint(x, z));
      grid[j * stride + i].extra.x =
          far ? grid[j * stride + i].p.y : patchHeight(heights, i, j, 8);
      heights[j * stride + i] = grid[j * stride + i].p.y;
      low = fminf(low, heights[j * stride + i]);
    }
  for (int j = 0; j < cells; j++)
    for (int i = 0; i < cells; i++) {
      Vertex a = grid[j * stride + i], b = grid[j * stride + i + 1],
             d = grid[(j + 1) * stride + i], e = grid[(j + 1) * stride + i + 1];
      tri(&s->builders[0], a, d, b);
      tri(&s->builders[0], b, d, e);
    }
  for (int j = cz * (span / 8); j < (cz + 1) * (span / 8); j++)
    for (int i = cx * (span / 8); i < (cx + 1) * (span / 8); i++) {
      double x, z, h;
      if (treePlace(i, j, &x, &z, &h)) {
        s->seed = whash(i, j) | 1u;
        int species = treeSpecies(x, z);
        if (far)
          distantTree(s, (float)x, (float)z, (float)h, species);
        else if (species == PINE)
          conifer(s, (float)x, (float)z, (float)h);
        else
          tree(s, (float)x, (float)z, (float)h, 1);
      }
    }
  s->seed = whash(cx + 119, cz - 271) | 1u;
  if (!far) {
    vegetation(s, ox, oz, heights);
    for (int j = (int)floor(oz / 12); j <= (int)floor((oz + span) / 12); j++)
      for (int i = (int)floor(ox / 12); i <= (int)floor((ox + span) / 12);
           i++) {
        double x, z, r;
        if (boulderPlace(i, j, &x, &z, &r) && x >= ox && x < ox + span &&
            z >= oz && z < oz + span)
          rock(s, (float)x, (float)z, (float)r);
      }
  }
  railChunk(s, cx, cz, span, far);
  _Static_assert(sizeof(Vertex) == 44,
                 "Vertex fingerprint requires fully initialized layout");
  uint64_t signature = 1469598103934665603ULL;
  const unsigned char *boneBytes = (const unsigned char *)s->bones;
  for (size_t k = 0; k < (size_t)s->boneCount * sizeof(Bone); k++) {
    signature ^= boneBytes[k];
    signature *= 1099511628211ULL;
  }
  const unsigned char *heightBytes = (const unsigned char *)heights;
  for (size_t k = 0; k < (size_t)stride * stride * sizeof(float); k++) {
    signature ^= heightBytes[k];
    signature *= 1099511628211ULL;
  }
  for (int i = 0; i < 4; i++) {
    const unsigned char *bytes = (const unsigned char *)s->builders[i].v;
    size_t count = (size_t)s->builders[i].count * sizeof(Vertex);
    for (size_t k = 0; k < count; k++) {
      signature ^= bytes[k];
      signature *= 1099511628211ULL;
    }
    if (s->builders[i].count)
      s->meshes[i] = upload(s, &s->builders[i]);
  }
  if (s->errorText[0])
    return 0;
  int bh = (s->boneCount * 2 + 255) / 256;
  if (bh < 1)
    bh = 1;
  float *bones = calloc((size_t)256 * bh * 4, sizeof(float));
  if (!bones) {
    snprintf(s->errorText, sizeof(s->errorText), "Bone allocation failed");
    return 0;
  }
  memcpy(bones, s->bones, s->boneCount * sizeof(Bone));
  s->boneTexture = LoadTextureFromImage(
      (Image){bones, 256, bh, 1, PIXELFORMAT_UNCOMPRESSED_R32G32B32A32});
  free(bones);
  SetTextureFilter(s->boneTexture, TEXTURE_FILTER_POINT);
  if (low < 0) {
    s->waterMesh = GenMeshPlane(span, span, 16, 16);
    if (!s->waterMesh.vertices) {
      snprintf(s->errorText, sizeof(s->errorText), "Water allocation failed");
      return 0;
    }
    for (int i = 0; i < s->waterMesh.vertexCount; i++) {
      s->waterMesh.vertices[i * 3] += ox + span * .5f;
      s->waterMesh.vertices[i * 3 + 2] += oz + span * .5f;
    }
    UpdateMeshBuffer(s->waterMesh, 0, s->waterMesh.vertices,
                     s->waterMesh.vertexCount * 3 * sizeof(float), 0);
    s->heightTexture = LoadTextureFromImage(
        (Image){heights, stride, stride, 1, PIXELFORMAT_UNCOMPRESSED_R32});
    SetTextureFilter(s->heightTexture, TEXTURE_FILTER_BILINEAR);
    SetTextureWrap(s->heightTexture, TEXTURE_WRAP_CLAMP);
  }
  c->x = cx;
  c->z = cz;
  c->valid = 1;
  c->g = takeGeometry(s);
  c->signature = signature;
  if (far)
    w->farGenerated++;
  else
    w->generated++;
  w->lastGenerationMs = (GetTime() - began) * 1000;
  w->maxGenerationMs = fmax(w->maxGenerationMs, w->lastGenerationMs);
  return 1;
}
static int findChunk(WalkWorld *w, int x, int z) {
  for (int i = 0; i < CAPACITY; i++)
    if (w->chunks[i].valid && w->chunks[i].x == x && w->chunks[i].z == z)
      return i;
  return -1;
}
unsigned long long sr_journey_signature(WalkWorld *w, int x, int z) {
  int slot = findChunk(w, x, z);
  return slot < 0 ? 0 : w->chunks[slot].signature;
}
unsigned long long sr_journey_far_signature(WalkWorld *w, int x, int z) {
  for (int i = 0; i < FAR_CAPACITY; i++)
    if (w->farChunks[i].valid && w->farChunks[i].x == x &&
        w->farChunks[i].z == z)
      return w->farChunks[i].signature;
  return 0;
}
static int streamOne(WalkWorld *w, double x, double z) {
  int cx = (int)floor(x / TILE), cz = (int)floor(z / TILE);
  w->centerX = cx;
  w->centerZ = cz;
  int bx = 0, bz = 0, best = 999999, missing = 0;
  for (int j = -3; j <= 3; j++)
    for (int i = -3; i <= 3; i++)
      if (findChunk(w, cx + i, cz + j) < 0) {
        int d = i * i + j * j;
        missing++;
        if (d < best) {
          best = d;
          bx = cx + i;
          bz = cz + j;
        }
      }
  if (!missing)
    return 0;
  int slot = -1;
  for (int i = 0; i < CAPACITY; i++)
    if (!w->chunks[i].valid) {
      slot = i;
      break;
    }
  if (slot < 0)
    for (int i = 0; i < CAPACITY; i++)
      if (abs(w->chunks[i].x - cx) > 3 || abs(w->chunks[i].z - cz) > 3) {
        slot = i;
        break;
      }
  if (slot < 0) {
    snprintf(w->n->errorText, sizeof(w->n->errorText),
             "No bounded stream slot");
    return -1;
  }
  Chunk *c = &w->chunks[slot];
  if (c->valid) {
    putGeometry(w->n, c->g);
    clearGeometry(w->n);
    c->valid = 0;
    w->evicted++;
  }
  if (!makeChunk(w, c, bx, bz, 0))
    return -1;
  return 1;
}
static int streamFar(WalkWorld *w, double x, double z) {
  int cx = (int)floor(x / FAR_TILE), cz = (int)floor(z / FAR_TILE), bx = 0,
      bz = 0, best = 99999;
  for (int j = -4; j <= 4; j++)
    for (int i = -4; i <= 4; i++) {
      int exists = 0;
      for (int k = 0; k < FAR_CAPACITY; k++)
        if (w->farChunks[k].valid && w->farChunks[k].x == cx + i &&
            w->farChunks[k].z == cz + j) {
          exists = 1;
          break;
        }
      if (!exists && i * i + j * j < best) {
        best = i * i + j * j;
        bx = cx + i;
        bz = cz + j;
      }
    }
  if (best == 99999)
    return 0;
  int slot = -1;
  for (int k = 0; k < FAR_CAPACITY; k++)
    if (!w->farChunks[k].valid) {
      slot = k;
      break;
    }
  if (slot < 0)
    for (int k = 0; k < FAR_CAPACITY; k++)
      if (abs(w->farChunks[k].x - cx) > 4 || abs(w->farChunks[k].z - cz) > 4) {
        slot = k;
        break;
      }
  if (slot < 0) {
    snprintf(w->n->errorText, sizeof(w->n->errorText),
             "No bounded horizon slot");
    return -1;
  }
  Chunk *c = &w->farChunks[slot];
  if (c->valid) {
    putGeometry(w->n, c->g);
    clearGeometry(w->n);
    c->valid = 0;
    w->farEvicted++;
  }
  return makeChunk(w, c, bx, bz, 1) ? 1 : -1;
}
WalkWorld *sr_journey_open(uint32_t seed, const char *const *sources) {
  selectWorld(seed);
  WalkWorld *w = calloc(1, sizeof(WalkWorld));
  if (!w)
    return NULL;
  w->seed = seed;
  w->n = journeyNature_load(sources);
  if (w->n) {
    w->railcar = createRailcar(w->n);
    w->n->scene = 0;
    w->n->sun = Vector3Normalize((Vector3){-.55f, .57f, .38f});
  }
  return w;
}
void sr_journey_close(WalkWorld *w) {
  if (!w)
    return;
  if (w->railcar.vaoId)
    UnloadMesh(w->railcar);
  if (w->n) {
    clearGeometry(w->n);
    for (int i = 0; i < CAPACITY; i++)
      if (w->chunks[i].valid) {
        putGeometry(w->n, w->chunks[i].g);
        clearGeometry(w->n);
      }
    for (int i = 0; i < FAR_CAPACITY; i++)
      if (w->farChunks[i].valid) {
        putGeometry(w->n, w->farChunks[i].g);
        clearGeometry(w->n);
      }
    journeyNature_free(w->n);
  }
  free(w);
}
int sr_journey_ready(WalkWorld *w) {
  return w && w->n && w->n->ready && !w->n->errorText[0];
}
int sr_journey_quality(WalkWorld *w, int quality) {
  if (!sr_journey_ready(w) || quality < 0 || quality > 2)
    return 0;
  if (w->quality == quality)
    return 1;
  Nature *s = w->n;
  RenderTexture2D shadow = target(s, quality == 2 ? 4096 : quality == 1 ? 3072 : 2048,
                                 quality == 2 ? 4096 : quality == 1 ? 3072 : 2048, 0);
  if (!shadow.id) return 0;
  freeTarget(s->shadowMap);
  s->shadowMap = shadow;
  w->quality = quality;
  s->currentWidth = 0;
  return sr_journey_ready(w);
}
const char *sr_journey_error(WalkWorld *w) {
  return w && w->n ? w->n->errorText : "World allocation failed";
}
// One near and one far chunk per frame. Return the number still missing.
// Music is pumped by the caller between these bounded generation steps.
int sr_journey_prepare(WalkWorld *w, double x, double z) {
  if (!sr_journey_ready(w))
    return -1;
  selectWorld(w->seed);
  if (streamOne(w, x, z) < 0 || streamFar(w, x, z) < 0)
    return -1;
  int missing = 0;
  for (int i = 0; i < CAPACITY; i++)
    missing += !w->chunks[i].valid;
  for (int i = 0; i < FAR_CAPACITY; i++)
    missing += !w->farChunks[i].valid;
  return missing;
}
static void allLand(WalkWorld *w, float t, int shadow, int clip, Camera3D cam) {
  Nature *s = w->n;
  if (!shadow) {
    i1(s->landShader, "uFarPass", 1);
    for (int i = 0; i < FAR_CAPACITY; i++) {
      Chunk *c = &w->farChunks[i];
      if (!c->valid)
        continue;
      float dx = fmaxf(0, fabsf(cam.position.x - (c->x + .5f) * FAR_TILE) -
                              FAR_TILE * .5f);
      float dz = fmaxf(0, fabsf(cam.position.z - (c->z + .5f) * FAR_TILE) -
                              FAR_TILE * .5f);
      if (dx * dx + dz * dz > 560 * 560)
        continue;
      putGeometry(s, c->g);
      world(s, t, 3, 0, clip, cam);
      takeGeometry(s);
    }
  }
  i1(s->landShader, "uFarPass", 0);
  for (int i = 0; i < CAPACITY; i++) {
    Chunk *c = &w->chunks[i];
    if (!c->valid)
      continue;
    Vector3 reference = shadow ? s->camera.position : cam.position;
    Vector3 p = {(c->x + .5f) * TILE, reference.y, (c->z + .5f) * TILE};
    float dist = Vector3Distance(p, reference);
    if (dist > (shadow ? 112 : 132))
      continue;
    putGeometry(s, c->g);
    float dx = fmaxf(0, fabsf(reference.x - p.x) - TILE * .5f);
    float dz = fmaxf(0, fabsf(reference.z - p.z) - TILE * .5f);
    // Grass is fully collapsed at 46 m. Skip those entire distant meshes,
    // allowing extra margin for blade width and wind displacement.
    world(s, t, dx * dx + dz * dz < 50 * 50 ? 7 : 3, shadow, clip, cam);
    takeGeometry(s);
  }
}
void sr_car_car(WalkWorld *w, double progress, double speed, int aboard) {
  w->carS = progress;
  w->carSpeed = speed;
  w->aboard = aboard;
}
static void drawRailcar(WalkWorld *w, int shadow, int clip, Camera3D cam) {
  Nature *s = w->n;
  double z = -w->carS, x = railX(z), y = railY(z);
  if (Vector3Distance(cam.position, (Vector3){(float)x, (float)y, (float)z}) >
      540)
    return;
  double angle = -atan2(-railSlopeX(z), 1.0);
  // Level passenger camera; the body follows the gentle track grade.
  double grade = atan2(railY(z - .1) - railY(z + .1),
                       .2 * sqrt(1 + railSlopeX(z) * railSlopeX(z)));
  Matrix model = MatrixMultiply(
      MatrixMultiply(MatrixRotateX((float)grade), MatrixRotateY((float)angle)),
      MatrixTranslate((float)x, (float)y, (float)z));
  i1(s->landShader, "uFarPass", 0);
  i1(s->landShader, "uKind", 4);
  i1(s->landShader, "uShadowPass", shadow);
  i1(s->landShader, "uClip", clip);
  v3(s->landShader, "uEye", cam.position);
  v3(s->landShader, "uSun", s->sun);
  SetShaderValueMatrix(
      s->landShader, GetShaderLocation(s->landShader, "uLightVP"), s->lightVP);
  SetShaderValueMatrix(s->landShader,
                       GetShaderLocation(s->landShader, "uObject"), model);
  bindTexture(s->landShader, "uShadow", s->shadowMap.depth, 9);
  s->material.shader = s->landShader;
  rlDisableBackfaceCulling();
  DrawMesh(w->railcar, s->material, MatrixIdentity());
  rlEnableBackfaceCulling();
}

int sr_journey_draw(WalkWorld *w, double x, double y, double z, double yaw,
                    double pitch, double seconds, float left, float top,
                    int width, int height) {
  if (!sr_journey_ready(w) || width < 1 || height < 1)
    return 0;
  selectWorld(w->seed);
  rlDrawRenderBatchActive();
  int framebuffer = 0, viewport[4];
  glGetIntegerv(0x8CA6, &framebuffer);
  glGetIntegerv(0x0BA2, viewport);
  Matrix parentView = rlGetMatrixModelview(),
         parentProjection = rlGetMatrixProjection();
  int parentW = rlGetFramebufferWidth(), parentH = rlGetFramebufferHeight();
  Nature *s = w->n;
  if (streamOne(w, x, z) < 0 || streamFar(w, x, z) < 0)
    return 0;
  w->nearMissing = findChunk(w, (int)floor(x / TILE), (int)floor(z / TILE)) < 0;
  if (w->nearMissing)
    return 0;
  float scale = w->quality == 2 ? 1.5f : 1.0f;
  float limitW = w->quality == 2 ? 2880 : w->quality == 1 ? 2560 : 1600;
  float limitH = w->quality == 2 ? 1620 : w->quality == 1 ? 1440 : 900;
  float ratio = fminf(scale, fminf(limitW / width, limitH / height));
  int rw = (int)fmaxf(1, width * ratio), rh = (int)fmaxf(1, height * ratio);
  if (rw != s->currentWidth || rh != s->currentHeight) {
    freeTarget(s->hdr);
    freeTarget(s->reflection);
    s->hdr = target(s, rw, rh, 1);
    float reflectionScale = w->quality == 2 ? 1 : w->quality == 1 ? .75f : .5f;
    s->reflection = target(s, (int)ceilf(rw * reflectionScale),
                           (int)ceilf(rh * reflectionScale), 1);
    s->currentWidth = rw;
    s->currentHeight = rh;
  }
  Shader qualityShaders[] = {s->landShader, s->waterShader, s->skyShader, s->postShader};
  for (int i = 0; i < 4; i++) i1(qualityShaders[i], "uQuality", w->quality);
  Vector3 eye = {(float)x, (float)y, (float)z};
  Vector3 f = {(float)(sin(yaw) * cos(pitch)), (float)sin(pitch),
               (float)(-cos(yaw) * cos(pitch))};
  s->camera =
      (Camera3D){eye, Vector3Add(eye, f), {0, 1, 0}, 62, CAMERA_PERSPECTIVE};
  float t = (float)seconds;
  const float texel = 96.0f / s->shadowMap.depth.width;
  Vector3 center = {floorf(eye.x / texel) * texel, eye.y - 1.68f,
                    floorf(eye.z / texel) * texel};
  Camera3D light = {Vector3Add(center, Vector3Scale(s->sun, 130)),
                    center,
                    {0, 1, 0},
                    96,
                    CAMERA_ORTHOGRAPHIC};
  BeginTextureMode(s->shadowMap);
  ClearBackground(WHITE);
  BeginMode3D(light);
  s->lightVP = MatrixMultiply(rlGetMatrixModelview(), rlGetMatrixProjection());
  allLand(w, t, 1, 0, light);
  drawRailcar(w, 1, 0, light);
  EndMode3D();
  EndTextureMode();
  Camera3D mirror = s->camera;
  mirror.position.y = -mirror.position.y;
  mirror.target.y = -mirror.target.y;
  BeginTextureMode(s->reflection);
  ClearBackground(BLACK);
  sky(s, mirror, s->reflection.texture.width, s->reflection.texture.height);
  BeginMode3D(mirror);
  s->reflectionVP =
      MatrixMultiply(rlGetMatrixModelview(), rlGetMatrixProjection());
  allLand(w, t, 0, 1, mirror);
  drawRailcar(w, 0, 1, mirror);
  EndMode3D();
  EndTextureMode();
  BeginTextureMode(s->hdr);
  ClearBackground(BLACK);
  sky(s, s->camera, rw, rh);
  BeginMode3D(s->camera);
  allLand(w, t, 0, 0, s->camera);
  drawRailcar(w, 0, 0, s->camera);
  s->material.shader = s->waterShader;
  v3(s->waterShader, "uEye", eye);
  v3(s->waterShader, "uSun", s->sun);
  f1(s->waterShader, "uTime", t);
  i1(s->waterShader, "uLandscape", 2);
  SetShaderValueMatrix(s->waterShader,
                       GetShaderLocation(s->waterShader, "uReflectionVP"),
                       s->reflectionVP);
  bindTexture(s->waterShader, "uReflection", s->reflection.texture, 10);
  rlDisableBackfaceCulling();
  for (int pass = 1; pass >= 0; pass--) {
    i1(s->waterShader, "uFarPass", pass);
    int count = pass ? FAR_CAPACITY : CAPACITY, span = pass ? FAR_TILE : TILE;
    float texel = pass ? 8 : 1;
    for (int i = 0; i < count; i++) {
      Chunk *c = pass ? &w->farChunks[i] : &w->chunks[i];
      if (!c->valid || !c->g.water.vaoId)
        continue;
      float rect[4] = {c->x * span - texel * .5f, c->z * span - texel * .5f,
                       span + texel, span + texel};
      SetShaderValue(s->waterShader,
                     GetShaderLocation(s->waterShader, "uHeightRect"), rect,
                     SHADER_UNIFORM_VEC4);
      bindTexture(s->waterShader, "uHeight", c->g.height, 11);
      DrawMesh(c->g.water, s->material, MatrixIdentity());
    }
  }
  rlEnableBackfaceCulling();
  EndMode3D();
  EndTextureMode();
  rlEnableFramebuffer((unsigned int)framebuffer);
  rlViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
  rlSetFramebufferWidth(parentW);
  rlSetFramebufferHeight(parentH);
  rlSetMatrixModelview(parentView);
  rlSetMatrixProjection(parentProjection);
  BeginShaderMode(s->postShader);
  Vector2 outputResolution = {(float)width, (float)height};
  SetShaderValue(s->postShader, GetShaderLocation(s->postShader, "uOutputResolution"),
                 &outputResolution, SHADER_UNIFORM_VEC2);
  DrawTexturePro(s->hdr.texture, (Rectangle){0, 0, rw, -rh},
                 (Rectangle){left, top, width, height}, (Vector2){0, 0}, 0,
                 WHITE);
  EndShaderMode();
  w->frames++;
  unsigned int err = glGetError();
  if (err)
    snprintf(s->errorText, sizeof(s->errorText), "OpenGL error %x", err);
  return sr_journey_ready(w);
}
const char *sr_journey_info(WalkWorld *w) {
  int resident = 0, trees = 0, grass = 0, triangles = 0, farResident = 0,
      farTriangles = 0;
  for (int i = 0; i < FAR_CAPACITY; i++)
    if (w->farChunks[i].valid) {
      farResident++;
      for (int j = 0; j < 4; j++)
        farTriangles += w->farChunks[i].g.meshes[j].triangleCount;
    }
  for (int i = 0; i < CAPACITY; i++)
    if (w->chunks[i].valid) {
      resident++;
      trees += w->chunks[i].g.treeCount;
      grass += w->chunks[i].g.grassCount;
      for (int j = 0; j < 4; j++)
        triangles += w->chunks[i].g.meshes[j].triangleCount;
    }
  snprintf(w->info, sizeof(w->info),
           "{\"quality\":%d,\"rasterWidth\":%d,\"rasterHeight\":%d,\"shadowWidth\":%d,\"reflectionWidth\":%d,\"farResident\":%d,\"farCapacity\":%d,\"farTriangles\":%d,"
           "\"farGenerated\":%u,\"farEvicted\":%u,\"resident\":%d,\"capacity\":"
           "%d,\"trees\":%d,\"blades\":%d,"
           "\"triangles\":%d,\"generated\":%u,\"evicted\":%u,\"frames\":%u,"
           "\"maxGenerationMs\":%.4f,\"lastGenerationMs\":%.4f,\"nearMissing\":"
           "%d,\"renderer\":\"%s\"}",
           w->quality, w->n->currentWidth, w->n->currentHeight, w->n->shadowMap.depth.width,
           w->n->reflection.texture.width, farResident, FAR_CAPACITY, farTriangles, w->farGenerated,
           w->farEvicted, resident, CAPACITY, trees, grass, triangles,
           w->generated, w->evicted, w->frames, w->maxGenerationMs,
           w->lastGenerationMs, w->nearMissing, glGetString(0x1F01));
  return w->info;
}
