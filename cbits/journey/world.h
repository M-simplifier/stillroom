#ifndef WALK_WORLD_H
#define WALK_WORLD_H
#include <math.h>
#include <stdint.h>
#include <limits.h>
static double wclamp(double x, double a, double b) {
  return fmax(a, fmin(b, x));
}
static double wsmooth(double a, double b, double x) {
  double t = wclamp((x - a) / (b - a), 0, 1);
  return t * t * (3 - 2 * t);
}
static _Thread_local uint32_t worldSeed = 0x73419a21u;
static void selectWorld(uint32_t seed) { worldSeed = seed; }
static uint32_t whash(int x, int z) {
  uint32_t h =
      (uint32_t)x * 0x9e3779b9u ^ (uint32_t)z * 0x85ebca6bu ^ worldSeed;
  h ^= h >> 16;
  h *= 0x7feb352du;
  h ^= h >> 15;
  h *= 0x846ca68bu;
  return h ^ (h >> 16);
}
static double wrandom(int x, int z) {
  return (whash(x, z) >> 8) * (1.0 / 16777216.0);
}
static double wnoise(double x, double z) {
  int i = (int)floor(x), j = (int)floor(z);
  double u = x - i, v = z - j;
  u = u * u * (3 - 2 * u);
  v = v * v * (3 - 2 * v);
  double a = wrandom(i, j) * (1 - u) + wrandom(i + 1, j) * u,
         b = wrandom(i, j + 1) * (1 - u) + wrandom(i + 1, j + 1) * u;
  return a * (1 - v) + b * v;
}
enum { MEADOW, BIRCH, PINE, HEATH, WETLAND, DUNES, HABITATS };
typedef struct {
  double w[HABITATS];
  int dominant;
} Habitat;
// Jittered ecological regions, softened across 48 m of the nearest-site
// boundary. Terrain is independent, so the same habitat can occupy a ridge or a
// lakeside. No field depends on load order, mutable random state, or camera
// position.
static Habitat habitat(double x, double z) {
  Habitat b = {{0}, 0};
  x += 22 * (wnoise(x * .003 + 39, z * .003 - 21) - .5);
  z += 22 * (wnoise(x * .003 - 81, z * .003 + 47) - .5);
  int cx = (int)floor(x / 190), cz = (int)floor(z / 190), ids[9], k = 0;
  double ds[9], best = 1e9;
  for (int j = cz - 1; j <= cz + 1; j++)
    for (int i = cx - 1; i <= cx + 1; i++) {
      double dx = x - (i + .5) * 190 - 76 * (wrandom(i + 323, j - 71) - .5),
             dz = z - (j + .5) * 190 - 76 * (wrandom(i - 99, j + 653) - .5);
      ds[k] = sqrt(dx * dx + dz * dz);
      ids[k] = (int)(whash(i - 173, j + 281) % HABITATS);
      best = fmin(best, ds[k]);
      k++;
    }
  double sum = 0;
  for (k = 0; k < 9; k++) {
    double a = 1 - wsmooth(0, 48, ds[k] - best);
    a *= a;
    b.w[ids[k]] += a;
    sum += a;
  }
  for (k = 0; k < HABITATS; k++) {
    b.w[k] /= sum;
    if (b.w[k] > b.w[b.dominant])
      b.dominant = k;
  }
  return b;
}
static double lakeX(int ix, int iz) {
  return ix * 240 + 90 * (wrandom(ix + 91, iz - 12) - .5);
}
static double lakeZ(int ix, int iz) {
  return iz * 240 + 90 * (wrandom(ix - 41, iz + 77) - .5);
}
static double shoreDistance(double x, double z) {
  int ix = (int)floor((x + 120) / 240), iz = (int)floor((z + 120) / 240);
  double best = 240;
  for (int j = iz - 1; j <= iz + 1; j++)
    for (int i = ix - 1; i <= ix + 1; i++) {
      double lx = lakeX(i, j), lz = lakeZ(i, j), dx = x - lx, dz = z - lz;
      // The early-out must include the ENTIRE shore-to-upland blend. An
      // elongated lake can still influence height well outside its water.
      if (dx * dx + dz * dz > 260 * 260)
        continue;
      Habitat b = habitat(lx, lz);
      if (wrandom(i + 213, j - 774) > .68 + .32 * b.w[WETLAND] && (i || j))
        continue;
      double rx = 24 + 24 * wrandom(i + 141, j - 20) + 28 * b.w[WETLAND],
             rz = 17 + 18 * wrandom(i - 108, j + 49) + 23 * b.w[WETLAND];
      double a = wrandom(i + 67, j - 81) * 3.141592653589793,
             u = dx * cos(a) + dz * sin(a), v = -dx * sin(a) + dz * cos(a);
      double d =
          (sqrt(u * u / (rx * rx) + v * v / (rz * rz)) - 1) * fmin(rx, rz);
      d += 2.5 * (wnoise(x * .035, z * .035) - .5);
      best = fmin(best, d);
    }
  return best;
}
static double naturalGround(double x, double z) {
  Habitat b = habitat(x, z);
  double warp = 45 * (wnoise(x * .003 - 4, z * .003 + 18) - .5);
  double upland = wsmooth(.30, .73, wnoise(x * .0026 + 2.3, z * .0029 - 7.6));
  double ridge =
      1 -
      fabs(2 * wnoise((x + warp) * .0073 + 16, (z - warp) * .0061 - 31) - 1);
  double mountains = 63 * upland * ridge * ridge * ridge;
  double hills = 4 + 14 * wnoise(x * .0041 - 29, z * .0043 + 5) +
                 3 * wnoise(x * .016 + 37, z * .014 - 72);
  double plain =
      (hills + mountains) * (1 - .88 * b.w[WETLAND]) + .8 * b.w[WETLAND];
  double dunes =
      2.3 * (1 + sin((x + warp) * .055 + 3 * wnoise(x * .008, z * .008)));
  plain = plain * (1 - .48 * b.w[DUNES]) + dunes * b.w[DUNES];
  plain += .20 * wnoise(x * .19 + 53, z * .19 - 26);
  double shore = shoreDistance(x, z), edge = shore * .25,
         blend = wsmooth(5, 70, shore);
  return fmax(-4.5, edge * (1 - blend) + plain * blend);
}

// One C2 uniform cubic B-spline describes an unbounded railway alignment.
// Its controls are global seeded coordinates, independent of chunk lifetime.
static double railBlend(double a, double b, double c, double d, double t) {
  double u = 1 - t;
  return (u * u * u * a + (3 * t * t * t - 6 * t * t + 4) * b +
          (-3 * t * t * t + 3 * t * t + 3 * t + 1) * c + t * t * t * d) /
         6;
}
static double railControlX(int i) { return 280 * (wrandom(i, 1729) - .5); }
static double railX(double z) {
  double f = z / 256;
  int i = (int)floor(f);
  double t = f - i;
  return railBlend(railControlX(i - 1), railControlX(i), railControlX(i + 1),
                   railControlX(i + 2), t);
}
static double railSlopeX(double z) {
  return (railX(z + .1) - railX(z - .1)) / .2;
}
static double railControlY(int i) {
  double z = i * 256.0;
  // Limit grades before smoothing. This is a scenic light railway, not a
  // physical civil-engineering optimizer. Terrain cuts and trestles meet it.
  return 2.0 + fmin(24.0, fmax(0, naturalGround(railX(z), z)) * .65);
}
static double railY(double z) {
  double f = z / 256;
  int i = (int)floor(f);
  double t = f - i;
  // A bounded per-thread memo of deterministic controls, not world state.
  // It is safe across FFI callers and eviction cannot affect the result.
  static _Thread_local int cached = INT_MIN;
  static _Thread_local double ys[4];
  static _Thread_local uint32_t cachedSeed;
  if (cached != i || cachedSeed != worldSeed) {
    for (int k = 0; k < 4; k++)
      ys[k] = railControlY(i + k - 1);
    cached = i;
    cachedSeed = worldSeed;
  }
  return railBlend(ys[0], ys[1], ys[2], ys[3], t);
}
static double railNearestZ(double x, double z) {
  double d = railSlopeX(z);
  return z + (x - railX(z)) * d / (1 + d * d);
}
static double railDistance(double x, double z) {
  double d = railSlopeX(z);
  return fabs(x - railX(z)) / sqrt(1 + d * d);
}
static double ground(double x, double z) {
  double g = naturalGround(x, z), d = railDistance(x, z);
  if (d >= 18)
    return g;
  double rz = railNearestZ(x, z), bed = railY(rz) - .36;
  double influence = 1 - wsmooth(2.1, 18, d);
  // Excavate high ground. Small fills become embankments, deep gaps remain
  // open beneath the bridge. The transition is continuous in both axes.
  if (bed > g)
    influence = (1 - wsmooth(2.1, 7, d)) * (1 - wsmooth(1.2, 3.0, bed - g));
  return g + (bed - g) * influence;
}
static double forestDensity(double x, double z) {
  Habitat b = habitat(x, z);
  double d = .05 * b.w[MEADOW] + .78 * b.w[BIRCH] + .91 * b.w[PINE] +
             .055 * b.w[HEATH] + .18 * b.w[WETLAND] + .025 * b.w[DUNES];
  return d * (.55 + .45 * wnoise(x * .024 + 2, z * .024 - 1));
}
static int treeSpecies(double x, double z) {
  Habitat b = habitat(x, z);
  return wrandom((int)floor(x * 3) + 412, (int)floor(z * 3) - 817) <
                 b.w[PINE] + .55 * b.w[HEATH]
             ? PINE
             : BIRCH;
}
static double pathWeight(double x, double z) {
  (void)x;
  (void)z;
  return 0;
}
static int treePlace(int ix, int iz, double *x, double *z, double *h) {
  *x = ix * 8 + 1 + wrandom(ix + 713, iz - 127) * 6;
  *z = iz * 8 + 1 + wrandom(ix - 117, iz + 821) * 6;
  if (railDistance(*x, *z) < 6.0 || wrandom(ix, iz) > forestDensity(*x, *z) ||
      ground(*x, *z) < .75)
    return 0;
  *h = treeSpecies(*x, *z) == PINE ? 9 + wrandom(ix + 31, iz - 89) * 8
                                   : 6 + wrandom(ix + 31, iz - 89) * 5;
  return 1;
}
// Shared geometry and collision footprint for actual large boulders.
static int boulderPlace(int ix, int iz, double *x, double *z, double *r) {
  *x = ix * 12 + 2 + 8 * wrandom(ix + 921, iz - 81);
  *z = iz * 12 + 2 + 8 * wrandom(ix - 71, iz + 982);
  Habitat b = habitat(*x, *z);
  double chance = .035 + .22 * b.w[HEATH] + .08 * b.w[PINE];
  if (railDistance(*x, *z) < 5.0 || wrandom(ix - 553, iz + 91) > chance ||
      ground(*x, *z) < .4)
    return 0;
  *r = .65 + 1.65 * wrandom(ix + 325, iz - 274);
  return 1;
}
static int standable(double x, double z) {
  if (!isfinite(x) || !isfinite(z) || fabs(x) > 10000000 ||
      fabs(z) > 10000000 || ground(x, z) < .16)
    return 0;
  double dx = ground(x + .15, z) - ground(x - .15, z),
         dz = ground(x, z + .15) - ground(x, z - .15);
  if (dx * dx + dz * dz > .075)
    return 0;
  int ix = (int)floor(x / 8), iz = (int)floor(z / 8);
  for (int i = ix - 1; i <= ix + 1; i++)
    for (int j = iz - 1; j <= iz + 1; j++) {
      double tx, tz, h;
      if (treePlace(i, j, &tx, &tz, &h)) {
        double r = .34 + h * (treeSpecies(tx, tz) == PINE ? .10 : .025);
        if ((x - tx) * (x - tx) + (z - tz) * (z - tz) < r * r)
          return 0;
      }
    }
  ix = (int)floor(x / 12);
  iz = (int)floor(z / 12);
  for (int i = ix - 1; i <= ix + 1; i++)
    for (int j = iz - 1; j <= iz + 1; j++) {
      double rx, rz, r;
      if (boulderPlace(i, j, &rx, &rz, &r) &&
          ((x - rx) * (x - rx) + (z - rz) * (z - rz) <
           (r * 1.1 + .27) * (r * 1.1 + .27)))
        return 0;
    }
  return 1;
}
#endif
