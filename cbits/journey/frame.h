// Versioned value protocol from Stillroom.Runtime.Journey.Packet.
// There is no native camera, light-grid or object-transform calculation here.
#define SR_FRAME_FLOATS 207
#define SR_FRAME_OBJECT 16
#define SR_FRAME_LIGHT 32
#define SR_FRAME_REFLECTION 48
#define SR_FRAME_SHADOW_UNITS 64
#define SR_FRAME_MAIN_PASS 68
#define SR_FRAME_SHADOW_PASS 109
#define SR_FRAME_REFLECTION_PASS 150
#define SR_FRAME_MAIN_CLIP 191

static Matrix frameMatrix(const float *p) {
  // OpenGL column order -> raylib's named Matrix fields, whose C layout differs.
  return (Matrix){p[0], p[4], p[8], p[12], p[1], p[5], p[9], p[13],
                  p[2], p[6], p[10], p[14], p[3], p[7], p[11], p[15]};
}
static Vector3 frameVector(const float *p) {
  return (Vector3){p[0], p[1], p[2]};
}
static Camera3D frameCamera(const float *p) {
  Vector3 eye = frameVector(p + 32), forward = frameVector(p + 35);
  return (Camera3D){eye, Vector3Add(eye, forward), frameVector(p + 38),
                    62, CAMERA_PERSPECTIVE};
}
static void beginFramePass(const float *p) {
  rlDrawRenderBatchActive();
  rlMatrixMode(RL_PROJECTION);
  rlPushMatrix();
  rlSetMatrixProjection(frameMatrix(p + 16));
  rlMatrixMode(RL_MODELVIEW);
  rlSetMatrixModelview(frameMatrix(p));
  rlEnableDepthTest();
}
static int validFrame(const float *p, int count, Nature *s) {
  if (!p || count != SR_FRAME_FLOATS || p[0] != 1) return 0;
  for (int i = 0; i < count; i++) if (!isfinite(p[i])) return 0;
  return p[1] == s->currentWidth && p[2] == s->currentHeight &&
         p[3] == s->shadowMap.depth.width &&
         p[4] == s->reflection.texture.width && p[5] == s->reflection.texture.height;
}
