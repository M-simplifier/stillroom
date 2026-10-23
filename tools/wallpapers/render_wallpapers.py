"""Stillroom: six authored, reproducible static landscapes.

All scenery pixels originate here. No photographs, generated bitmap input, or
external scenery assets are read. NumPy supplies seeded fields; Pillow rasterizes
silhouettes and writes images. The seed varies natural structure inside an
explicit composition; it does not choose the composition.

Example: python render_wallpapers.py --width 2560 --output final
Requires Python 3.12+, NumPy 2.3.5, Pillow 12.3.0 (versions used for acceptance).
"""
from __future__ import annotations

import argparse
from dataclasses import asdict, dataclass
import hashlib
import json
from pathlib import Path
import time

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont


Color = tuple[int, int, int]


@dataclass(frozen=True)
class Ridge:
    heights: tuple[float, ...]
    color: Color
    roughness: float = .005
    trees: int = 0
    tree_height: float = .024
    tree_shape: str = "pine"
    fade: float = .04
    relief: float = 2.0
    groves: tuple[tuple[float, float, float], ...] = ((.5, .65, 1.),)
    softness: float = .65


@dataclass(frozen=True)
class Scene:
    id: str
    title: str
    seed: int
    sky_top: Color
    sky_low: Color
    light: Color
    light_x: float
    light_y: float
    light_strength: float
    cloud_strength: float
    ridges: tuple[Ridge, ...]
    fog_color: Color
    fogs: tuple[tuple[float, float, float], ...]
    lake: float | None = None
    water_bottom: Color = (62, 89, 93)
    water_reflect: float = .6
    grain: float = .24
    vignette: float = .035
    foreground: Ridge | None = None
    warm_scatter: float = 0.


SCENES = (
    Scene(
        "01-first-light", "First light / luminous forest", 843021,
        (125, 151, 149), (210, 216, 193), (247, 232, 193), .68, .38, .39, 5.2,
        (
            Ridge((.54,.50,.47,.49,.52,.53,.50,.44,.42,.45,.48), (156,177,161), .002, 45, .006, fade=.12, softness=1.6),
            Ridge((.65,.69,.73,.77,.82,.84,.83,.79,.73,.68,.65), (81,122,112), .004, 180, .026, fade=.10, relief=4., softness=1.,
                  groves=((.13,.18,1),(.91,.15,.8))),
        ),
        (185,203,183), ((.80,.11,.45),),
        warm_scatter=.12,
    ),
    Scene(
        "02-cedar-breath", "Cedar breath / deep forest", 592074,
        (72,105,103), (171,193,168), (221,221,179), .60, .45, .29, 3.8,
        (
            Ridge((.46,.45,.48,.53,.56,.55,.52,.47,.46,.49,.52), (117,150,133), .004, 180, .011, fade=.13),
            Ridge((.51,.54,.58,.62,.68,.71,.67,.63,.59,.55,.53), (79,122,110), .005, 300, .024, fade=.11),
            Ridge((.65,.64,.69,.74,.82,.86,.88,.82,.77,.73,.72), (42,83,77), .007, 250, .045, fade=.04,
                  groves=((.15,.18,1),(.87,.16,1))),
        ),
        (144,178,155), ((.69,.05,.29),(.82,.065,.51),(.99,.12,.27)),
        vignette=.055,
        foreground=Ridge((.82,.84,.88,.95,1.08,1.1,1.1,1.1,1.1,1.1,1.1), (24,57,55), .005, 110, .10, fade=.015,
                         relief=3., groves=((.08,.08,1),(.18,.04,.38))),
    ),
    Scene(
        "03-open-water", "Open water / pearl morning", 620318,
        (122,153,164), (209,220,213), (241,231,206), .63, .42, .28, 5.0,
        (
            Ridge((.51,.50,.52,.54,.545,.54,.52,.49,.46,.45,.47), (158,182,180), .002, 0, fade=.09, relief=2.1,softness=1.9),
            Ridge((.50,.48,.51,.54,.56,.573,.57,.558,.54,.51,.49), (122,155,153), .003, 140, .007, fade=.09, relief=2.5,softness=1.5),
            Ridge((.51,.53,.55,.578,.59,.61,.64,.65,.62,.59,.565), (82,126,124), .003, 170, .013, fade=.04, relief=3.,softness=1.0,
                  groves=((.08,.11,1),(.93,.06,.7))),
        ),
        (191,207,199), ((.577,.012,.35),), .588, (65,106,117), .57,
        vignette=.028,
    ),
    Scene(
        "04-honey-mist", "Honey mist / warm water", 781205,
        (130,145,150), (203,196,177), (253,209,144), .24, .52, .37, 4.1,
        (
            Ridge((.55,.56,.57,.57,.55,.52,.47,.43,.41,.39,.40), (159,168,157), .002, 0, fade=.13, relief=2.,softness=2.),
            Ridge((.57,.58,.59,.59,.58,.56,.54,.50,.46,.45,.43), (125,144,134), .003, 70, .006, fade=.09, relief=3.2,softness=1.5,
                  groves=((.85,.20,1),)),
            Ridge((.60,.61,.62,.63,.63,.62,.60,.58,.56,.54,.52), (80,111,104), .002, 140, .012, fade=.045, relief=4.,softness=1.,
                  groves=((.94,.13,1),)),
        ),
        (182,187,168), ((.594,.026,.36),), .613, (70,102,105), .49,
        vignette=.025, warm_scatter=.64,
    ),
    Scene(
        "05-blue-distance", "Blue distance / ink ridges", 316880,
        (27,44,65), (84,111,131), (155,178,188), .71, .37, .14, 3.3,
        (
            Ridge((.49,.48,.50,.49,.44,.42,.45,.47,.51,.55,.56), (68,98,119), .002, 0, fade=.12, relief=2.2,softness=1.8),
            Ridge((.51,.54,.59,.63,.65,.64,.62,.57,.54,.52,.53), (43,73,97), .002, 0, fade=.085, relief=3.2,softness=1.5),
            Ridge((.70,.69,.66,.65,.69,.74,.79,.82,.81,.77,.74), (28,55,78), .002, 70, .008, fade=.06, relief=3.1,softness=1.2),
            Ridge((.74,.77,.83,.89,.94,.95,.92,.86,.82,.83,.85), (17,36,55), .002, 90, .010, fade=.03, relief=2.7,softness=1.),
        ),
        (74,105,125), ((.65,.05,.16),(.80,.052,.16),(.96,.075,.12)),
        vignette=.025,
    ),
    Scene(
        "06-rain-valley", "Rain valley / an inhabited distance", 904126,
        (108,130,136), (182,198,187), (230,218,179), .46, .39, .20, 3.6,
        (
            Ridge((.40,.38,.42,.45,.49,.50,.46,.43,.39,.37,.40), (136,160,156), .005, 0, fade=.12, relief=2.8),
            Ridge((.48,.46,.50,.53,.56,.55,.52,.49,.46,.43,.41), (102,133,129), .006, 130, .009, fade=.09, relief=3.),
            Ridge((.51,.55,.60,.64,.66,.65,.64,.62,.58,.54,.51), (65,106,101), .005, 250, .018, fade=.05, relief=3.5),
            Ridge((.60,.63,.67,.70,.72,.72,.72,.71,.68,.63,.58), (44,82,79), .005, 220, .027, fade=.03, relief=2.3,
                  groves=((.13,.15,1),(.88,.16,1))),
        ),
        (164,190,175), ((.63,.025,.21),(.72,.027,.28)), .738, (46,83,88), .54,
        vignette=.04,
        foreground=Ridge((1.06,1.06,1.06,1.06,1.06,1.06,1.01,.95,.88,.82,.79), (26,57,55), .005, 100, .095, fade=.012,
                         relief=2.4, groves=((.95,.07,1),(.84,.06,.45))),
    ),
)

FINAL_SCENE_IDS = ("01-first-light", "03-open-water", "04-honey-mist", "05-blue-distance")


def rgb(color: Color) -> np.ndarray:
    return np.asarray(color, dtype=np.float32)


def smooth(t):
    t = np.clip(t, 0, 1)
    return t * t * (3 - 2 * t)


def lattice(width: int, height: int, nx: int, ny: int, seed: int) -> np.ndarray:
    """Bicubic value field; fixed lattice size makes the world scale invariant."""
    values = np.random.default_rng(seed).uniform(-1, 1, (ny, nx)).astype(np.float32)
    return np.asarray(Image.fromarray(values).resize((width,height), Image.Resampling.BICUBIC))


def field(width, height, seed, octaves=((5,4,1.),(12,8,.45),(31,18,.20),(79,44,.07))):
    value = np.zeros((height, width), np.float32)
    for i,(nx,ny,amplitude) in enumerate(octaves):
        value += lattice(width,height,nx,ny,seed+i*137)*amplitude
    return value


def curve(control: tuple[float, ...], width: int) -> np.ndarray:
    """Catmull-Rom interpolation through the authored profile."""
    t = np.linspace(0,len(control)-1,width,dtype=np.float32)
    k = np.minimum(np.floor(t).astype(int),len(control)-2)
    u = t-k
    p = np.pad(np.asarray(control,dtype=np.float32),(1,2),mode="edge")
    a,b,c,d = p[k],p[k+1],p[k+2],p[k+3]
    return .5*((2*b)+(-a+c)*u+(2*a-5*b+4*c-d)*u*u+(-a+3*b-3*c+d)*u*u*u)


def rough_curve(ridge: Ridge, width: int, seed: int) -> np.ndarray:
    out = curve(ridge.heights,width)
    for i,(cells,amp) in enumerate(((37,1.),(101,.43),(283,.16))):
        xs=np.linspace(0,1,cells)
        vals=np.random.default_rng(seed+i*107).normal(0,amp,cells)
        out += np.interp(np.linspace(0,1,width),xs,vals)*ridge.roughness
    return out


def pine(draw, x, base, height, rng):
    """Asymmetric tiered branches around a slightly leaning trunk."""
    lean=rng.uniform(-.09,.09)*height
    tip=base-height
    trunk=max(.6,height*.025)
    draw.line((x,base,x+lean,tip),fill=255,width=max(1,round(trunk)))
    for j in range(14):
        t=(j+1)/15
        yy=tip+height*t
        cx=x+lean*(1-t)
        breadth=height*(.22*t**.7)*rng.uniform(.8,1.1)
        rise=height*rng.uniform(.035,.09)
        droop=height*rng.uniform(.02,.055)
        draw.polygon([(cx,yy-rise),(cx-breadth*rng.uniform(.7,1.15),yy+droop),(cx,yy+height*.075)],fill=255)
        draw.polygon([(cx,yy-rise*.8),(cx+breadth*rng.uniform(.7,1.15),yy+droop*.8),(cx,yy+height*.075)],fill=255)


def broadleaf(draw, x, base, height, rng):
    draw.line((x,base,x,base-height*.56),fill=255,width=max(1,round(height*.05)))
    for j in range(10):
        cx=x+rng.normal(0,.13)*height
        cy=base-height*rng.uniform(.43,.90)
        rad=height*rng.uniform(.12,.20)
        pts=[]
        for angle in np.linspace(0,2*np.pi,13,endpoint=False):
            radius=rad*rng.uniform(.82,1.15)
            pts.append((cx+np.cos(angle)*radius,cy+np.sin(angle)*radius))
        draw.polygon(pts,fill=255)


class Painter:
    def __init__(self, scene: Scene, width: int):
        self.s=scene
        self.w=width
        self.h=round(width*9/16)
        self.x=np.linspace(0,1,width,dtype=np.float32)[None,:]
        self.y=np.linspace(0,1,self.h,dtype=np.float32)[:,None]
        self.ambient=field(self.w,self.h,scene.seed+870)
        self.image=np.zeros((self.h,self.w,3),np.float32)

    def blend(self, color, opacity):
        a=np.clip(opacity,0,1)
        if np.ndim(a)>0: a=a[...,None]
        self.image=self.image*(1-a)+np.asarray(color,dtype=np.float32)*a

    def sky(self):
        s=self.s
        u=smooth(self.y/.76)**.8
        self.image[:]=rgb(s.sky_top)*(1-u[...,None])+rgb(s.sky_low)*u[...,None]
        light=np.exp(-((self.x-s.light_x)/.32)**2-((self.y-s.light_y)/.32)**2)*s.light_strength
        self.blend(rgb(s.light),light)
        clouds=field(self.w,self.h,s.seed+28,((5,14,1),(13,37,.42),(37,91,.13),(93,127,.035)))
        self.image += (clouds*s.cloud_strength*(.6+.4*np.clip(1-self.y/.7,0,1)))[...,None]
        # A faint high, horizontal veil makes illumination belong to an atmosphere.
        veil=np.exp(-((self.y-(.23+.028*np.sin(self.x*6+1)))/.075)**2)*.045
        self.blend(rgb(s.sky_low),veil)

    def ridge(self, ridge: Ridge, index: int):
        s=self.s
        seed=s.seed+index*1009
        profile=rough_curve(ridge,self.w,seed)
        mask=Image.new("L",(self.w,self.h))
        draw=ImageDraw.Draw(mask)
        draw.polygon([(0,self.h)]+[(i,float(v*self.h)) for i,v in enumerate(profile)]+[(self.w,self.h)],fill=255)
        rng=np.random.default_rng(seed+81)
        # Positions are samples of a authored grove-density field, not a grid.
        positions=np.sort(rng.uniform(-.025,1.025,ridge.trees*2))
        for px in positions:
            density=max(a*np.exp(-((px-m)/sd)**2/2) for m,sd,a in ridge.groves)
            if rng.random()>density*.62: continue
            h=ridge.tree_height*self.h*rng.uniform(.58,1.40)
            at=np.interp(px,np.linspace(0,1,self.w),profile)*self.h
            if ridge.tree_shape=="pine": pine(draw,px*self.w,at+h*.10,h,rng)
            else: broadleaf(draw,px*self.w,at+h*.07,h,rng)
        mask=mask.filter(ImageFilter.GaussianBlur(max(.32,self.w/1280*ridge.softness)))
        a=np.asarray(mask,dtype=np.float32)/255
        # Relief is tied to downslope position and erodes into the local mist.
        descent=np.maximum(0,self.y-profile[None,:])
        ridges=field(self.w,self.h,seed+132,((19,4,1),(43,11,.42),(101,31,.15)))
        slope=np.gradient(profile)*self.w
        vertical_ribs=np.sin(self.x*173+np.sin(self.x*41)*3+self.y*(12+np.tanh(slope)[None,:]*16))
        relief=(ridges+vertical_ribs*.15)*ridge.relief
        lighting=np.exp(-((self.x-s.light_x)/.40)**2)*2.5
        foliage=field(self.w,self.h,seed+648,((139,85,1),(283,181,.32)))
        canopy=np.clip(descent/.025,0,1)*np.exp(-descent*6)
        shade=relief*np.exp(-descent*4)+foliage*ridge.relief*.22*canopy+lighting
        base=rgb(ridge.color)[None,None,:]+shade[...,None]
        haze=smooth(descent/.20)*ridge.fade*2.8
        layer=base*(1-haze[...,None])+rgb(s.fog_color)*haze[...,None]
        self.blend(layer,a)

    def fog(self, center, spread, amount, index):
        # A wind-swept sheet has a broad contour; tiny texture never drives it.
        tilt=.008*np.sin(self.x*(6+index)+index*1.8)+.006*self.ambient
        bands=np.exp(-((self.y-center-tilt)/spread)**2)
        density=bands*amount*(.89+.16*self.ambient)
        light=np.exp(-((self.x-self.s.light_x)/.24)**2)*self.s.warm_scatter
        color=rgb(self.s.fog_color)*(1-light[...,None])+rgb(self.s.light)*light[...,None]
        self.blend(color,density)

    def lake(self):
        s=self.s
        if s.lake is None:return
        horizon=s.lake
        start=round(horizon*(self.h-1))
        depth=np.clip((self.y-horizon)/(1-horizon),0,1)
        # Horizontal expansion and subpixel vertical distortion grow toward us.
        displacement=(np.sin(self.y*740+np.sin(self.x*28)*2)*.00065+
                      np.sin(self.y*291+self.x*16)*.0011)*depth**.85
        reflect_y=np.clip(horizon-(self.y-horizon)*.84+displacement,0,1)
        reflect_x=np.clip(self.x+depth**1.3*(np.sin(self.y*1073)*.003+np.sin(self.y*537+self.x*23)*.001),0,1)
        rows=reflect_y*(self.h-1)
        columns=np.minimum(self.w-1,(reflect_x*(self.w-1)).astype(int))
        low=np.floor(rows).astype(int)
        high=np.minimum(low+1,self.h-1)
        frac=(rows-low)[...,None]
        # Slight diffusion preserves the large masses, softening mirrored needles.
        reflected_source=np.asarray(Image.fromarray(np.uint8(np.clip(self.image,0,255))).filter(
            ImageFilter.GaussianBlur(self.w/1280*.58)),dtype=np.float32)
        reflected=reflected_source[low,columns]*(1-frac)+reflected_source[high,columns]*frac
        water=rgb(s.fog_color)*(1-depth[...,None]**.7)+rgb(s.water_bottom)*depth[...,None]**.7
        reflection_amount=s.water_reflect*(1-depth*.60)
        reflected=reflected*reflection_amount[...,None]+water*(1-reflection_amount[...,None])
        # Quiet surface: very low amplitude, horizontally correlated capillary ripples.
        wave=np.sin(self.y*1051+np.sin(self.x*14)*.8)+.45*np.sin(self.y*1739+self.x*28)
        breakup=field(self.w,self.h,s.seed+790,((7,35,1),(27,111,.35)))
        waves=wave*(.16+depth*.50)*(1+breakup*.24)
        reflected += waves[...,None]
        glow=np.exp(-((self.x-s.light_x)/(.11+.12*depth))**2)*np.exp(-depth*1.9)*(.045+s.warm_scatter*.18)
        reflected=reflected*(1-glow[...,None])+rgb(s.light)*glow[...,None]
        # Long sparse near-surface ripples break the mathematical mirror gently.
        ripples=np.exp(-((self.y-(horizon+.14+.018*np.sin(self.x*4)))/.073)**2)
        reflected += (np.sin(self.y*661+self.x*12)*np.clip(breakup+.25,0,1)*ripples*.55)[...,None]
        self.image[start:]=reflected[start:]
        # A narrow mist veil removes a hard geometric hinge at the far shore.
        self.fog(horizon,.009,.19,8)

    def finish(self):
        s=self.s
        edge=((self.x-.5)**2/.5+(self.y-.48)**2/.8)
        self.image *= 1-s.vignette*edge[...,None]
        # Dither is below one display-code value. It prevents gradient banding.
        rng=np.random.default_rng(s.seed+99881)
        self.image += rng.normal(0,s.grain,(self.h,self.w,1)).astype(np.float32)
        return Image.fromarray(np.uint8(np.clip(self.image+.5,0,255)),"RGB")

    def render(self):
        self.sky()
        for i,ridge in enumerate(self.s.ridges):
            self.ridge(ridge,i)
        for i,fog in enumerate(self.s.fogs):
            self.fog(*fog,i)
        self.lake()
        if self.s.foreground:
            self.ridge(self.s.foreground,19)
        return self.finish()


def font(size):
    for path in ("C:/Windows/Fonts/segoeui.ttf","/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"):
        if Path(path).exists():return ImageFont.truetype(path,size)
    return ImageFont.load_default(size=size)


def contact_sheet(paths: list[tuple[Scene,Path]], output: Path):
    thumb_w=640
    thumb_h=360
    gap=20
    caption=53
    cols=2
    rows=(len(paths)+cols-1)//cols
    sheet=Image.new("RGB",(cols*thumb_w+(cols+1)*gap,rows*(thumb_h+caption)+gap*2),(21,28,32))
    draw=ImageDraw.Draw(sheet)
    for index,(scene,path) in enumerate(paths):
        col=index%cols;row=index//cols
        xx=gap+col*(thumb_w+gap);yy=gap+row*(thumb_h+caption)
        with Image.open(path) as source:
            sheet.paste(source.resize((thumb_w,thumb_h),Image.Resampling.LANCZOS),(xx,yy))
        draw.text((xx,yy+thumb_h+9),scene.title,font=font(19),fill=(208,217,215))
        draw.text((xx,yy+thumb_h+32),f"{scene.id}  |  seed {scene.seed}",font=font(12),fill=(125,145,147))
    sheet.save(output,quality=92,subsampling=0)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--width",type=int,default=1280)
    parser.add_argument("--output",type=Path,default=Path("iteration01"))
    parser.add_argument("--scenes",nargs="*",help="ID prefixes, e.g. 01 03; all includes the two exploratory scenes")
    args=parser.parse_args()
    if args.width<320 or args.width>7680:parser.error("width must be between 320 and 7680")
    root=Path(__file__).resolve().parent
    out=args.output if args.output.is_absolute() else root/args.output
    out=out.resolve()
    if not out.is_relative_to(root):parser.error("output must remain inside the generator directory")
    out.mkdir(parents=True,exist_ok=True)
    previews=out/"previews"
    previews.mkdir(exist_ok=True)
    wanted=args.scenes or FINAL_SCENE_IDS
    selected=[s for s in SCENES if "all" in wanted or any(s.id.startswith(p) for p in wanted)]
    if not selected:parser.error("no scene matches the requested IDs")
    paths=[]
    records=[]
    for scene in selected:
        started=time.perf_counter()
        painting=Painter(scene,args.width).render()
        path=out/f"{scene.id}.png"
        painting.save(path,optimize=True)
        preview=painting.copy()
        preview.thumbnail((1280,720),Image.Resampling.LANCZOS)
        preview.save(previews/f"{scene.id}.jpg",quality=91,subsampling=0)
        data=np.asarray(painting,dtype=np.float32)
        luma=data@np.array([.2126,.7152,.0722],np.float32)
        records.append({"scene":asdict(scene),"path":path.name,"width":painting.width,"height":painting.height,
                        "sha256":hashlib.sha256(path.read_bytes()).hexdigest(),
                        "luma_percentiles_0_255":dict(zip(("p01","p10","p50","p90","p99"),map(float,np.percentile(luma,[1,10,50,90,99])))),
                        "render_seconds":round(time.perf_counter()-started,3)})
        paths.append((scene,path))
        print(f"Rendered {scene.id}: {painting.width} x {painting.height}, {records[-1]['render_seconds']} s",flush=True)
    contact_sheet(paths,out/"contact-sheet.jpg")
    import PIL
    manifest={"generator":Path(__file__).name,"generator_sha256":hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
              "algorithm":"Authored Catmull-Rom landforms, seeded asymmetric conifers, spatial atmospheric transmittance, distorted reflected sky and land, display-code dither.",
              "inputs":"Only scalar/vector declarations and seeds in this source; no scenery image inputs.",
              "dependencies":{"numpy":np.__version__,"pillow":PIL.__version__},"images":records}
    (out/"manifest.json").write_text(json.dumps(manifest,indent=2),encoding="utf-8")
    (out/"generator-used.py").write_bytes(Path(__file__).read_bytes())
    print(f"Contact sheet: {out/'contact-sheet.jpg'}",flush=True)


if __name__=="__main__":
    main()
