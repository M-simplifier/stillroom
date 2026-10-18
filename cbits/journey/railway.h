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
static void carBox(Builder *b, float x, float y, float z, float w, float h,
                   float d, Color c) {
  Vector3 p[8] = {
      {x - w / 2, y - h / 2, z - d / 2}, {x + w / 2, y - h / 2, z - d / 2},
      {x + w / 2, y - h / 2, z + d / 2}, {x - w / 2, y - h / 2, z + d / 2},
      {x - w / 2, y + h / 2, z - d / 2}, {x + w / 2, y + h / 2, z - d / 2},
      {x + w / 2, y + h / 2, z + d / 2}, {x - w / 2, y + h / 2, z + d / 2}};
  railPrism(b, p, c);
}
static void carWheel(Builder *b, float x, float z) {
  Color c = tint(.10, .13, .14, 1), rim = tint(.29, .32, .31, 1);
  for (int i = 0; i < 20; i++) {
    float a = i * 2 * PI / 20, q = (i + 1) * 2 * PI / 20;
    Vector3 p[4] = {{x - .075f, .28f + .29f * sinf(a), z + .29f * cosf(a)},
                    {x + .075f, .28f + .29f * sinf(a), z + .29f * cosf(a)},
                    {x + .075f, .28f + .29f * sinf(q), z + .29f * cosf(q)},
                    {x - .075f, .28f + .29f * sinf(q), z + .29f * cosf(q)}};
    railQuad(b, p[0], p[1], p[2], p[3], c);
    for (int side = -1; side <= 1; side += 2) {
      Vector3 n = {(float)side, 0, 0}, center = {x + side * .077f, .28f, z};
      Vertex v = vert(center, n, (Vector2){2, 0}, -1, 0, rim);
      tri(b, v,
          vert((Vector3){center.x, p[0].y, p[0].z}, n, (Vector2){2, 0}, -1, 0,
               c),
          vert((Vector3){center.x, p[3].y, p[3].z}, n, (Vector2){2, 0}, -1, 0,
               c));
    }
  }
}
static Mesh createRailcar(Nature *s) {
  Builder b = {0};
  Color paint = tint(.105, .245, .225, 1), dark = tint(.065, .12, .12, 1),
        wood = tint(.47, .30, .14, 1), brass = tint(.53, .39, .18, 1),
        cream = tint(.65, .64, .51, 1);
  carBox(&b, 0, .37f, 0, 1.70f, .22f, 4.9f, dark);
  carBox(&b, 0, .56f, 0, 2.18f, .16f, 5.2f, paint);
  for (int k = 0; k < 25; k++)
    carBox(&b, 0, .655f, -2.45f + k * .20f, 2.06f, .035f, .19f, wood);
  for (int side = -1; side <= 1; side += 2) {
    carWheel(&b, side * .54f, -1.65f);
    carWheel(&b, side * .54f, 1.65f);
    // Low side panels leave a broad open window; center doorway remains open.
    for (int end = -1; end <= 1; end += 2) {
      carBox(&b, side * 1.04f, .95f, end * 1.66f, .095f, .63f, 1.78f, paint);
      carBox(&b, side * 1.04f, 1.29f, end * 1.66f, .13f, .07f, 1.80f, wood);
      carBox(&b, side * 1.035f, 1.79f, end * 2.46f, .075f, 2.32f, .075f, brass);
    }
    // A useful step, with no invisible wall over the doorway.
    carBox(&b, side * 1.20f, .34f, 0, .42f, .09f, .86f, wood);
    carBox(&b, side * .84f, 1.02f, 1.15f, .35f, .12f, 1.60f, wood);
    carBox(&b, side * .97f, 1.36f, 1.15f, .065f, .65f, 1.60f, wood);
    carBox(&b, side * 1.045f, 2.91f, 0, .09f, .13f, 5.25f, brass);
  }
  for (int end = -1; end <= 1; end += 2) {
    carBox(&b, 0, .98f, end * 2.46f, 2.07f, .65f, .10f, paint);
    carBox(&b, 0, 1.33f, end * 2.46f, 2.14f, .09f, .13f, wood);
    carBox(&b, 0, .41f, end * 2.72f, .75f, .12f, .22f, dark);
    carBox(&b, 0, 2.91f, end * 2.46f, 2.17f, .13f, .10f, brass);
  }
  // Slightly arched canopy, entirely procedural, warm timber underside.
  for (int k = 0; k < 16; k++) {
    float x = -1.20f + k * .15f, y = 2.96f + .14f * (1 - x * x / (1.2f * 1.2f));
    carBox(&b, x + .075f, y, 0, .151f, .07f, 5.48f, cream);
  }
  return upload(s, &b);
}
