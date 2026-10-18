#include "world.h"
double sr_journey_ground(uint32_t seed, double x, double z) {
  selectWorld(seed);
  return ground(x, z);
}
int sr_journey_stand(uint32_t seed, double x, double z) {
  selectWorld(seed);
  return standable(x, z);
}
int sr_journey_biome(uint32_t seed, double x, double z) {
  selectWorld(seed);
  return habitat(x, z).dominant;
}
double sr_journey_interest(uint32_t seed, double x, double z) {
  selectWorld(seed);
  double d = (shoreDistance(x, z) - 12) / 22;
  Habitat b = habitat(x, z);
  double ecotone = 1;
  for (int k = 0; k < HABITATS; k++)
    ecotone -= b.w[k] * b.w[k];
  return .35 * exp(-d * d) + .25 * ecotone +
         .25 * wclamp(ground(x, z) / 55, 0, 1) +
         .15 * (1 - forestDensity(x, z));
}
double sr_journey_start_x(uint32_t seed) {
  selectWorld(seed);
  return -18;
}
double sr_journey_start_z(uint32_t seed) {
  selectWorld(seed);
  return -36;
}
double sr_journey_shore(uint32_t seed, double x, double z) {
  selectWorld(seed);
  return shoreDistance(x, z);
}

double sr_car_track_x(uint32_t seed, double s) {
  selectWorld(seed);
  return railX(-s);
}
double sr_car_track_y(uint32_t seed, double s) {
  selectWorld(seed);
  return railY(-s);
}
double sr_car_natural_ground(uint32_t seed, double x, double z) {
  selectWorld(seed);
  return naturalGround(x, z);
}
