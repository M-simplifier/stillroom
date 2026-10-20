// Small authored procedural shapes: shared world alignment, no image/model
// assets.
static Vector3 railPoint(float z, float side, float above) {
  double d = railSlopeX(z), m = sqrt(1 + d * d);
  return (Vector3){(float)railX(z) + side / (float)m, (float)railY(z) + above,
                   z - side * (float)d / (float)m};
}
static void railQuad(Builder *b, Vector3 a, Vector3 c, Vector3 d, Vector3 e,
                     Color color) {
  Vector3 n = Vector3Normalize(
      Vector3CrossProduct(Vector3Subtract(c, a), Vector3Subtract(d, a)));
  Vertex av = vert(a, n, (Vector2){2, 0}, -1, 0, color),
         cv = vert(c, n, (Vector2){2, 0}, -1, 0, color),
         dv = vert(d, n, (Vector2){2, 0}, -1, 0, color),
         ev = vert(e, n, (Vector2){2, 0}, -1, 0, color);
  tri(b, av, cv, dv);
  tri(b, av, dv, ev);
}
static void railPrism(Builder *b, Vector3 corners[8], Color color) {
  const int faces[6][4] = {{0, 3, 2, 1}, {4, 5, 6, 7}, {0, 1, 5, 4},
                           {3, 7, 6, 2}, {0, 4, 7, 3}, {1, 2, 6, 5}};
  for (int i = 0; i < 6; i++)
    railQuad(b, corners[faces[i][0]], corners[faces[i][1]],
             corners[faces[i][2]], corners[faces[i][3]], color);
}
static void railBox(Builder *b, float z0, float z1, float x0, float x1,
                    float y0, float y1, Color color) {
  Vector3 p[8] = {railPoint(z0, x0, y0), railPoint(z0, x1, y0),
                  railPoint(z1, x1, y0), railPoint(z1, x0, y0),
                  railPoint(z0, x0, y1), railPoint(z0, x1, y1),
                  railPoint(z1, x1, y1), railPoint(z1, x0, y1)};
  railPrism(b, p, color);
}
static void railChunk(Nature *s, int cx, int cz, int span, int far) {
  Builder *b = &s->builders[0];
  Color ballast = tint(.26, .245, .21, 1), steel = tint(.31, .34, .33, 1),
        wood = tint(.22, .16, .10, 1), beam = tint(.18, .235, .22, 1);
  int step = far ? 4 : 2;
  for (int iz = cz * span; iz < (cz + 1) * span; iz += step) {
    float z = (float)iz;
    if ((int)floor(railX(z + step * .5) / span) != cx)
      continue;
    railBox(b, z, z + step, -1.65f, 1.65f, -.32f, -.20f, ballast);
    for (int side = -1; side <= 1; side += 2) {
      float x = side * .54f;
      railBox(b, z, z + step, x - .035f, x + .035f, -.13f, -.035f, wood);
      railBox(b, z, z + step, x - .055f, x + .055f, -.045f, .015f, steel);
    }
    if (!far)
      for (int t = 0; t < 2; t++)
        railBox(b, z + t, z + t + .17f, -.91f, .91f, -.21f, -.12f, wood);
    double gap = railY(z + 1) - ground(railX(z + 1), z + 1);
    if (gap > 1.6) {
      for (int side = -1; side <= 1; side += 2) {
        float x = side * 1.42f;
        railBox(b, z, z + step, x - .10f, x + .10f, -.67f, -.29f, beam);
        railBox(b, z, z + step, x - .035f, x + .035f, .64f, .70f, beam);
        if (!far)
          railBox(b, z, z + .07f, x - .04f, x + .04f, -.27f, .70f, beam);
      }
      if (iz % 8 == 0)
        for (int side = -1; side <= 1; side += 2) {
          Vector3 a = railPoint(z, side * 1.25f, -.56f);
          float foot = (float)ground(a.x, a.z) - .35f;
          Vector3 p[8] = {
              {a.x - .15f, foot, a.z - .15f}, {a.x + .15f, foot, a.z - .15f},
              {a.x + .15f, foot, a.z + .15f}, {a.x - .15f, foot, a.z + .15f},
              {a.x - .15f, a.y, a.z - .15f},  {a.x + .15f, a.y, a.z - .15f},
              {a.x + .15f, a.y, a.z + .15f},  {a.x - .15f, a.y, a.z + .15f}};
          railPrism(b, p, beam);
        }
    }
  }
}
