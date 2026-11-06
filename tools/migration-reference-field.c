/* Acquisition-only oracle. Compile against cbits/journey/world.h at f72f242;
 * never link this file into the application. The checked-in CSV is frozen. */
#include <stdio.h>
#include <stdlib.h>
#ifndef SR_REFERENCE_WORLD
#error Define SR_REFERENCE_WORLD as the frozen baseline world.h path
#endif
#include SR_REFERENCE_WORLD

static unsigned long rows;
static void sample(const char *group, uint32_t seed, double x, double z) {
    selectWorld(seed);
    Habitat b = habitat(x, z);
    double d = (shoreDistance(x, z) - 12) / 22, ecotone = 1;
    for (int i = 0; i < HABITATS; ++i) ecotone -= b.w[i] * b.w[i];
    double interest = .35 * exp(-d * d) + .25 * ecotone +
        .25 * wclamp(ground(x, z) / 55, 0, 1) + .15 * (1 - forestDensity(x, z));
    printf("%s,%u,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%.17g,%d,%d",
        group, seed, x, z, ground(x,z), naturalGround(x,z), shoreDistance(x,z),
        railX(z), railY(z), forestDensity(x,z), standable(x,z), b.dominant);
    for (int i = 0; i < HABITATS; ++i) printf(",%.17g", b.w[i]);
    printf(",%.17g\n", interest);
    ++rows;
}

int main(void) {
    const uint32_t seeds[] = {0,1,2,17,123,1729,0x73419a21u,0xdeadbeefu,
        0x80000000u,0x7fffffffu,0xfffffffeu,0xffffffffu};
    const double coords[] = {-512,-256,-240,-190,-128,-120,-32,-12,-8,-1,-1e-9,
        0,1e-9,1,8,12,32,120,128,190,240,256,512};
    const double offsets[] = {-18.000001,-18,-17.999999,-7.000001,-7,-6,-5,
        -2.100001,-2.1,-2.099999,0,2.099999,2.1,2.100001,5,6,7,17.999999,18,18.000001};
    const double zs[] = {-1024.000001,-1024,-1023.999999,-512,-256.000001,-256,
        -255.999999,-128,-32,-1e-9,0,1e-9,32,128,255.999999,256,256.000001,512,1024};
    puts("group,seed,x,z,ground,naturalGround,shoreDistance,railX,railY,forestDensity,standable,dominant,meadow,birch,pine,heath,wetland,dunes,interest");
    for (size_t s=0; s<sizeof seeds/sizeof *seeds; ++s) {
        uint32_t seed = seeds[s];
        selectWorld(seed);
        for (size_t i=0; i<sizeof coords/sizeof *coords; ++i)
            for (size_t j=0; j<sizeof coords/sizeof *coords; ++j)
                sample("grid",seed,coords[i],coords[j]);
        for (size_t j=0; j<sizeof zs/sizeof *zs; ++j)
            for (size_t i=0; i<sizeof offsets/sizeof *offsets; ++i)
                sample("rail-boundary",seed,railX(zs[j])+offsets[i],zs[j]);
        /* Follow rays from actual deterministic lake centres and bisect the
         * shore field at both the waterline and the 5/70m blend boundaries. */
        for(int iz=-1; iz<=1; ++iz) for(int ix=-1; ix<=1; ++ix) {
            double lx=lakeX(ix,iz), lz=lakeZ(ix,iz);
            sample("lake-centre",seed,lx,lz);
            for(int direction=0; direction<4; ++direction) {
                double angle=direction*1.5707963267948966;
                const double targets[]={0,5,70};
                for(size_t k=0;k<sizeof targets/sizeof *targets;++k) {
                    double lo=0,hi=200;
                    for(int n=0;n<55;++n) {
                        double mid=(lo+hi)*.5;
                        if(shoreDistance(lx+cos(angle)*mid,lz+sin(angle)*mid)<targets[k])lo=mid;
                        else hi=mid;
                    }
                    for(int e=-1;e<=1;++e) {
                        double distance=(lo+hi)*.5+e*1e-6;
                        sample("shore-boundary",seed,lx+cos(angle)*distance,lz+sin(angle)*distance);
                    }
                }
            }
        }
        for(int iz=-12;iz<=12;++iz) for(int ix=-12;ix<=12;++ix) {
            double x,z,h;
            if(treePlace(ix,iz,&x,&z,&h)) sample("tree-centre",seed,x,z);
            if(boulderPlace(ix,iz,&x,&z,&h)) sample("boulder-centre",seed,x,z);
        }
        sample("stand-limit",seed,10000000,0);
        sample("stand-limit",seed,10000000.000001,0);
        sample("stand-limit",seed,-10000000,0);
        sample("stand-limit",seed,-10000000.000001,0);
    }
    fprintf(stderr,"field fixture rows=%lu\n",rows);
    return ferror(stdout)?1:0;
}
