// Local procedural materials and plant silhouettes. No image assets.
static Color habitatTint(Habitat b, int grass) {
  const float soil[HABITATS][3] = {{.13, .205, .062},  {.205, .171, .064},
                                   {.057, .102, .071}, {.13, .145, .085},
                                   {.14, .178, .075},  {.40, .302, .164}};
  const float leaves[HABITATS][3] = {{.24, .36, .073}, {.40, .32, .085},
                                     {.12, .23, .14},  {.29, .22, .28},
                                     {.30, .37, .11},  {.50, .39, .19}};
  float r = 0, g = 0, bl = 0;
  for (int i = 0; i < HABITATS; i++) {
    const float *c = grass ? leaves[i] : soil[i];
    r += b.w[i] * c[0];
    g += b.w[i] * c[1];
    bl += b.w[i] * c[2];
  }
  return tint(r, g, bl, 1);
}
static Color terrainTint(float x, float z) {
  return habitatTint(habitat(x, z), 0);
}
static float coarseHeight(float x, float z) {
  double ox = floor(x / 8) * 8, oz = floor(z / 8) * 8, u = (x - ox) / 8,
         v = (z - oz) / 8;
  double a = ground(ox, oz), b = ground(ox + 8, oz), c = ground(ox, oz + 8),
         d = ground(ox + 8, oz + 8);
  return (float)(u + v <= 1 ? a + (b - a) * u + (c - a) * v
                            : d + (c - d) * (1 - u) + (b - d) * (1 - v));
}
static float patchHeight(const float *heights, float x, float z, int spacing) {
  x = Clamp(x, 0, 32);
  z = Clamp(z, 0, 32);
  int ix = (int)fminf(32 / spacing - 1, floorf(x / spacing)),
      iz = (int)fminf(32 / spacing - 1, floorf(z / spacing));
  float u = (x - ix * spacing) / spacing, v = (z - iz * spacing) / spacing;
  int cell = iz * spacing * 33 + ix * spacing;
  float a = heights[cell], b = heights[cell + spacing],
        c = heights[cell + spacing * 33], d = heights[cell + spacing * 34];
  return u + v <= 1 ? a + (b - a) * u + (c - a) * v
                    : d + (c - d) * (1 - u) + (b - d) * (1 - v);
}
static void blossom(Nature *s, float x, float z, float y, float h,
                    Color color) {
  Vector3 root = {x, y, z}, top = {x, y + h, z};
  float radius = range(s, .045, .085), a = range(s, 0, 2 * PI);
  // Packed in the existing grass stream: identical world-root wind and fade.
  Vertex center = {top, {0, 1, 0}, {1, root.y}, {x, z}, color};
  for (int i = 0; i < 7; i++) {
    float p = a + i * 2 * PI / 7, q = a + (i + 1) * 2 * PI / 7;
    Vertex v = {
        Vector3Add(top, (Vector3){radius * cosf(p), -.012f, radius * sinf(p)}),
        {0, 1, 0},
        {1, root.y},
        {x, z},
        color};
    Vertex w = {
        Vector3Add(top, (Vector3){radius * cosf(q), -.012f, radius * sinf(q)}),
        {0, 1, 0},
        {1, root.y},
        {x, z},
        color};
    tri(&s->builders[3], center, v, w);
  }
}
static void vegetation(Nature *s, float ox, float oz, const float *heights) {
  for (int i = 0; i < 6500; i++) {
    float x = range(s, ox, ox + 32), z = range(s, oz, oz + 32),
          y = patchHeight(heights, x - ox, z - oz, 1);
    if (y < .18 || railDistance(x, z) < 2.8)
      continue;
    Habitat b = habitat(x, z);
    float cover =
        .72f - .42f * b.w[PINE] - .10f * b.w[HEATH] - .54f * b.w[DUNES];
    float patch = .22f + .78f * (float)wnoise(x * .14, z * .14);
    if (randf(s) > patch * cover)
      continue;
    float h = range(s, .22, .49) * (1 + 2.2 * b.w[WETLAND] + .65 * b.w[DUNES]);
    Color color = habitatTint(b, 1);
    float shade = range(s, .78, 1.12);
    color.r = (unsigned char)(color.r * shade);
    color.g = (unsigned char)(color.g * shade);
    color.b = (unsigned char)(color.b * shade);
    for (int k = 0; k < 3; k++) {
      float bx = x + range(s, -.08, .08), bz = z + range(s, -.08, .08);
      blade(s, bx, bz, patchHeight(heights, bx - ox, bz - oz, 1),
            h * range(s, .8, 1.2), range(s, .018, .033), color);
    }
    if (randf(s) < .15 * b.w[MEADOW] + .50 * b.w[HEATH]) {
      Color bloom = b.w[HEATH] > .45
                        ? tint(.43, .20, .48, 1)
                        : (randf(s) > .45 ? tint(.90, .84, .53, 1)
                                          : tint(.52, .41, .72, 1));
      blossom(s, x, z, y, h + .025, bloom);
    }
  }
}
// Stable coarse crowns use the same tree centers, heights and species as near
// geometry. Geometry is deliberately sparse beyond the individual-leaf range.
static void crown(Nature *s, Vector3 center, float radius, float vertical,
                  Color c, int cone) {
  // Three intersecting planes with procedural, leaf-shaped cutouts in GLSL.
  // No baked billboards or closed, spherical proxy crowns.
  for (int i = 0; i < 3; i++) {
    float angle = i * PI / 3 + center.x * .03f;
    Vector3 side = {cosf(angle), 0, sinf(angle)},
            front = {-sinf(angle), 0, cosf(angle)};
    Vertex v[4];
    for (int k = 0; k < 4; k++) {
      float u = k % 2 ? 1 : -1, w = k / 2 ? 1 : -1;
      Vector3 p = {center.x + side.x * u * radius,
                   center.y + vertical * (w + 1) * .5f,
                   center.z + side.z * u * radius};
      Vector3 n = Vector3Normalize(Vector3Add(
          Vector3Add(Vector3Scale(side, u), Vector3Scale(front, .45f)),
          (Vector3){0, w * .7f, 0}));
      v[k] = vert(p, n, (Vector2){u, w}, -1, (float)cone, c);
    }
    tri(&s->builders[2], v[0], v[2], v[1]);
    tri(&s->builders[2], v[1], v[2], v[3]);
  }
}
static void distantTree(Nature *s, float x, float z, float h, int species) {
  Vector3 root = surface(s, x, z);
  root.y = coarseHeight(x, z);
  s->treeCount++;
  tube(s, root, Vector3Add(root, (Vector3){0, h * .88f, 0}), h * .023f, .025f,
       -1, tint(.30, .27, .19, 1), 4);
  if (species == PINE) {
    for (int k = 0; k < 3; k++)
      crown(s, Vector3Add(root, (Vector3){h * .015f, h * (.2f + .21f * k), 0}),
            h * (.21f - .048f * k), h * (.45f - .06f * k),
            tint(.058, .135, .090, .75), 1);
  } else {
    Habitat b = habitat(x, z);
    float gold = (float)(b.w[BIRCH] * (.65 + .35 * wnoise(x * .009, z * .009)));
    Color c =
        tint(.12 + .42 * gold, .255 + .09 * gold, .075 * (1 - .55 * gold), .8);
    for (int k = 0; k < 6; k++) {
      float a = k * 2.39996f + x * .01f;
      crown(
          s,
          Vector3Add(root, (Vector3){cosf(a) * h * .09f, h * (.38f + .075f * k),
                                     sinf(a) * h * .09f}),
          h * (.20f - .013f * k), h * .26f, c, 0);
    }
  }
}
