"""Acquire CPU geometry from the frozen v0.11.0 C source, before GPU upload.

This is a test oracle generator, never part of Stillroom's build. It preserves
the baseline source arithmetic and evaluation order. Source slices end at the
first upload; the small capture tail only serializes those completed buffers.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess


def between(text, start, end):
    a = text.index(start)
    return text[a:text.index(end, a)]


CAPTURE = r'''
#include <time.h>
__declspec(dllimport) int __stdcall QueryPerformanceCounter(int64_t *);
__declspec(dllimport) int __stdcall QueryPerformanceFrequency(int64_t *);
static double nowMs(void) {int64_t tick,frequency;QueryPerformanceCounter(&tick);QueryPerformanceFrequency(&frequency);return tick*1000.0/frequency;}
static uint64_t hashBytes(uint64_t h,const void *data,size_t size) {
 const unsigned char *bytes=data;
 for(size_t k=0;k<size;++k){h^=bytes[k];h*=1099511628211ULL;}return h;
}
static void save(const char *folder,const char *name,const char *part,const void *data,size_t bytes) {
 char path[4096];snprintf(path,sizeof path,"%s/%s-%s.bin",folder,name,part);
 FILE *file=fopen(path,"wb");if(!file){perror(path);exit(2);}
 if(bytes&&fwrite(data,1,bytes,file)!=bytes){perror(path);exit(2);}fclose(file);
}
static void capture(const char *folder,const char *name,Nature *s,const float *height,int width,int heightRows,int water,double began) {
 uint64_t signature=1469598103934665603ULL;
 size_t boneBytes=(size_t)s->boneCount*sizeof(Bone),heightBytes=(size_t)width*heightRows*sizeof(float);
 signature=hashBytes(signature,s->bones,boneBytes);
 signature=hashBytes(signature,height,heightBytes);
 for(int i=0;i<4;++i)signature=hashBytes(signature,s->builders[i].v,(size_t)s->builders[i].count*sizeof(Vertex));
 Mesh packed[4]={{0}};
 for(int i=0;i<4;++i)if(s->builders[i].count)packed[i]=packCpu(s,&s->builders[i]);
 fprintf(stderr,"%s,%.6f\n",name,nowMs()-began);
 for(int i=0;i<4;++i){free(packed[i].vertices);free(packed[i].normals);free(packed[i].texcoords);free(packed[i].texcoords2);free(packed[i].colors);}
 if(!getenv("SR_REFERENCE_TIMING_ONLY")) {
 save(folder,name,"bones",s->bones,boneBytes);save(folder,name,"height",height,heightBytes);
 for(int i=0;i<4;++i){char part[32];snprintf(part,sizeof part,"material%d",i);
 size_t bytes=(size_t)s->builders[i].count*sizeof(Vertex);save(folder,name,part,s->builders[i].v,bytes);}
 }
 printf("%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%llu\n",name,s->boneCount,s->treeCount,s->grassCount,width,heightRows,water,
 s->builders[0].count,s->builders[1].count,s->builders[2].count,s->builders[3].count,(unsigned long long)signature);
 for(int i=0;i<4;++i)free(s->builders[i].v);free(s);
}
'''


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--source-root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--clang', type=Path, required=True)
    parser.add_argument('--timings-only', action='store_true')
    args = parser.parse_args()
    source = args.source_root.resolve()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    nature = (source/'cbits/nature.c').read_text(encoding='utf-8')
    journey = (source/'cbits/journey/render.c').read_text(encoding='utf-8')
    geometry = (source/'cbits/journey/geometry.h').read_text(encoding='utf-8')
    nature_helpers = nature[:nature.index('static Mesh upload(')]
    fixed_generation = between(nature, 'static void generate(Nature *s) {', '    for (int i = 0; i < 4; i++) {\n        s->meshes[i] = upload')
    fixed_generation = fixed_generation.replace('double began = GetTime();', '') + '\n}\n'
    fixed_height = between(nature, '    const int size = 512;', '    Image hm =').replace('return;', 'return 2;')
    packing = between(nature, 'static Mesh upload(Nature *s, Builder *b) {', '    UploadMesh(&m, false);')
    packing = packing.replace('static Mesh upload(', 'static __attribute__((noinline)) Mesh packCpu(').replace('MemAlloc', 'malloc').replace('MemFree', 'free') + '\n return m;\n}\n'
    fixed_main = r'''
int main(int argc,char **argv) {
 if(argc!=2)return 2;
 puts("name,bones,trees,grass,heightWidth,heightRows,water,terrain,wood,foliage,grassVertices,signature");
 for(int scene=0;scene<3;++scene) {
 double began=nowMs();
 Nature *s=calloc(1,sizeof *s);s->scene=scene;s->seed=0x73419a21u+scene*1949u;
 generate(s);
 HEIGHT
 char name[64];snprintf(name,sizeof name,"fixed-%d",scene);
 capture(argv[1],name,s,height,size,size,1,began);free(height);
 }
 return 0;
}
'''.replace('HEIGHT', fixed_height)
    fixed = nature_helpers + fixed_generation + packing + CAPTURE + fixed_main

    journey_helpers = geometry[:geometry.index('static Mesh upload(')]
    journey_types = between(journey, '#define SIDE', 'static int makeChunk(')
    chunk_generation = between(journey, 'static int makeChunk(', '  _Static_assert(sizeof(Vertex)')
    chunk_generation = chunk_generation.replace('double began = GetTime();', 'double began = nowMs();')
    chunk_generation = chunk_generation.replace('static int makeChunk(WalkWorld *w, Chunk *c, int cx, int cz, int far)', 'static int makeChunk(WalkWorld *w, Chunk *c, int cx, int cz, int far, const char *folder, const char *name)')
    chunk_generation += '\n  capture(folder,name,s,heights,stride,stride,low<0,began); return 1;\n}\n'
    chunk_main = r'''
int main(int argc,char **argv) {
 if(argc!=2)return 2;
 puts("name,bones,trees,grass,heightWidth,heightRows,water,terrain,wood,foliage,grassVertices,signature");
 const uint32_t seeds[]={0x73419a21u,1u,0xffffffffu};
 const int points[][2]={{-1,-2},{0,0},{1,-1},{-3,2}};
 for(int k=0;k<3;++k)for(int p=0;p<4;++p)for(int far=0;far<2;++far) {
 selectWorld(seeds[k]);Nature *s=calloc(1,sizeof *s);WalkWorld w={0};w.n=s;w.seed=seeds[k];Chunk c={0};
 char name[128];snprintf(name,sizeof name,"chunk-%u-%d-%d-%d",seeds[k],points[p][0],points[p][1],far);
 makeChunk(&w,&c,points[p][0],points[p][1],far,argv[1],name);
 }
 return 0;
}
'''
    streamed = journey_helpers + '\n#include "ecology.h"\n#include "railway.h"\n#include "frame.h"\n' + journey_types + packing + CAPTURE + chunk_generation + chunk_main
    for name, text in [('fixed', fixed), ('streamed', streamed)]:
        c_file = out/f'{name}-reference.c'
        c_file.write_text(text, encoding='utf-8')
        exe_file = out/f'{name}-reference.exe'
        subprocess.run([str(args.clang), '-O2', '-std=c11',
            '-I'+str(source/'cbits'), '-I'+str(source/'cbits/journey'),
            str(c_file), '-o', str(exe_file)], check=True)
        with (out/f'{name}.csv').open('wb') as stream, (out/f'{name}-cpu-ms.csv').open('wb') as timing:
            environment = dict(os.environ, SR_REFERENCE_TIMING_ONLY='1') if args.timings_only else None
            subprocess.run([str(exe_file), str(out)], stdout=stream, stderr=timing, env=environment, check=True)
    hashes = {str(p.relative_to(source)): hashlib.sha256(p.read_bytes()).hexdigest()
        for p in [source/'cbits/nature.c',source/'cbits/journey/world.h',source/'cbits/journey/geometry.h',source/'cbits/journey/ecology.h',source/'cbits/journey/railway.h',source/'cbits/journey/render.c']}
    (out/'provenance.json').write_text(json.dumps({'sourceRevision':'f72f2420ab91956b9ba76998899971e0b9ab82d2','sourceHashes':hashes,'compiler':str(args.clang),'flags':['-O2','-std=c11'],'scope':'CPU buffers immediately before baseline upload; timing includes generation, full FNV signature and exact legacy upload CPU SoA packing, but excludes GPU transfer and serialization. Water topology is separately generated by raylib.'},indent=2),encoding='utf-8')
    print(json.dumps({'output':str(out),'files':len(list(out.glob('*.bin'))),'bytes':sum(p.stat().st_size for p in out.glob('*.bin'))}))


if __name__ == '__main__':
    main()
