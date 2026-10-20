in vec3 world;
in vec3 normal;
in vec3 original;
in vec3 originalNormal;
in vec2 uv;
in vec4 tint;
in vec4 lightSpace;
in float foliageShape;
uniform vec3 uEye;
uniform sampler2D uShadow;
uniform int uKind;
uniform int uShadowPass;
uniform int uClip;
uniform int uFarPass;
// x = world metres/texel; y = light depth span; z,w = bias in metres.
uniform vec4 uShadowUnits;
out vec4 finalColor;
float depthComparison(ivec2 cell, vec3 p, vec2 gradient, float bias) {
    ivec2 size=textureSize(uShadow,0);
    if(any(lessThan(cell,ivec2(0)))||any(greaterThanEqual(cell,size)))return 1.0;
    // Compare at the actual texel centre on the RECEIVER plane. A wide
    // filter must not compare every neighbouring depth to the central depth.
    vec2 offset=(vec2(cell)+.5)/vec2(size)-p.xy;
    float receiver=p.z+dot(gradient,offset)-bias;
    return step(receiver,texelFetch(uShadow,cell,0).r);
}
float filteredComparison(vec2 sampleUV, vec3 p, vec2 gradient, float bias) {
    vec2 texel=sampleUV*vec2(textureSize(uShadow,0))-.5;
    ivec2 base=ivec2(floor(texel));
    vec2 f=fract(texel);
    float a=depthComparison(base,p,gradient,bias);
    float b=depthComparison(base+ivec2(1,0),p,gradient,bias);
    float c=depthComparison(base+ivec2(0,1),p,gradient,bias);
    float d=depthComparison(base+ivec2(1,1),p,gradient,bias);
    // Filter visibility, never interpolated raw depths.
    return mix(mix(a,b,f.x),mix(c,d,f.x),f.y);
}
float shadow(vec3 n) {
    vec3 p=lightSpace.xyz/lightSpace.w*.5+.5;
    vec3 dx=dFdx(p),dy=dFdy(p);
    // Remove screen magnification before testing singularity. An absolute
    // determinant cutoff rejects perfectly regular planes near the camera.
    // Scale z as well: this leaves the receiver gradient unchanged.
    float uvScale=max(max(abs(dx.x),abs(dx.y)),max(abs(dy.x),abs(dy.y)));
    if(uvScale>0.0){dx/=uvScale;dy/=uvScale;}
    float det=dx.x*dy.y-dx.y*dy.x;
    // About eight binary32 epsilons in the normalized Jacobian.
    vec2 gradient=abs(det)>1e-6
        ? vec2(dy.y*dx.z-dx.y*dy.z,dx.x*dy.z-dy.x*dx.z)/det : vec2(0);
    if(p.x<0.0||p.x>1.0||p.y<0.0||p.y>1.0||p.z<0.0||p.z>1.0)return 1.0;
    float bias=max(uShadowUnits.z,uShadowUnits.w*(1.0-dot(n,uSun)))/uShadowUnits.y;
    vec2 px=1.0/vec2(textureSize(uShadow,0));
    float v=0.0;
    // A fixed world-space footprint has no blocker-count-driven radius jumps.
    // Higher tiers spend samples on a smooth tent filter, not animated noise.
    if(uQuality==0) v=filteredComparison(p.xy,p,gradient,bias);
    else {
        float radius=clamp(.055/uShadowUnits.x,.75,2.5);
        for(int x=-1;x<=1;x++)for(int y=-1;y<=1;y++) {
            float weight=float((x==0?2:1)*(y==0?2:1));
            v+=weight*filteredComparison(p.xy+vec2(x,y)*px*radius,p,gradient,bias);
        }
        v/=16.0;
    }
    float edge=min(min(p.x,1.0-p.x),min(p.y,1.0-p.y));
    return mix(1.0,.14+.86*v,smoothstep(.01,.10,edge));
}
void main() {
    if(uClip==1&&world.y<.025) discard;
    if(uKind==3 && length(world-uEye)>47.0) discard;
    if(uShadowPass==1){finalColor=vec4(1.0);return;}
    float rangeXZ=distance(world.xz,uEye.xz);
    if(uFarPass==1 && uKind==2) {
        float leaves=valueNoise(uv*vec2(18.0,27.0)+original.xz*.07);
        float outline=foliageShape>.5 ? abs(uv.x)/(1.04-(uv.y*.5+.5)*.96) : length(uv);
        if(outline>1.0-.19*leaves || leaves>.79)discard;
    }
    if(uKind==0 && uv.x<.5) {
        if(uFarPass==1 && rangeXZ<90.0)discard;
        if(uFarPass==0 && rangeXZ>=90.0)discard;
    } else if(uKind!=3 && uKind!=4) {
        float lod=smoothstep(77.0,102.0,rangeXZ);
        float threshold=fract(dot(floor(gl_FragCoord.xy),vec2(.754877666,.569840296)));
        if(uFarPass==1 ? threshold>=lod : threshold<lod)discard;
    }
    vec3 n=normalize(normal); if(!gl_FrontFacing)n=-n;
    vec3 view=normalize(uEye-world),c=tint.rgb;
    float ao=1.0;
    if(uKind==0) {
        float footprint=max(length(dFdx(original.xz)),length(dFdy(original.xz)));
        float patches=fbm(original.xz*.28),grain=mix(.5,valueNoise(original.xz*7.5),exp(-footprint*10.0));
        c=tint.rgb*(.65+.55*patches)*(.85+.28*grain);
        c=mix(vec3(.22,.19,.13),c,smoothstep(-.12,.50,original.y));
        float stone=smoothstep(.38,.70,1.0-n.y)*smoothstep(3.0,10.0,original.y);
        if(uLandscape==1)stone=max(stone,smoothstep(5.0,21.0,original.y));
        c=mix(c,vec3(.19,.205,.20)*(.75+.38*patches),stone);
        float exposed=smoothstep(40.0,64.0,original.y)*smoothstep(.12,.50,1.0-n.y);
        c=mix(c,vec3(.24,.25,.245)*(.75+.38*patches),exposed);
        c=mix(c,vec3(.69,.75,.78),smoothstep(69.0,78.0,original.y+patches*4.0)*smoothstep(.65,.91,n.y));
        if(uLandscape==1) {
            float strata=valueNoise(vec2(original.x*.24,original.y*.82+original.z*.13));
            vec3 rock=mix(vec3(.12,.14,.15),vec3(.30,.30,.27),strata*.6+patches*.4);
            c=mix(c,rock,stone);
            float snowLine=original.y+patches*8.0+valueNoise(original.xz*.11)*5.0;
            float settled=smoothstep(.40,.83,n.y);
            c=mix(c,vec3(.73,.79,.83),smoothstep(34.0,40.0,snowLine)*settled);
        }
        if(uv.x>.5)c=vec3(.20,.215,.19)*(.65+.5*patches)*mix(.45,1.0,smoothstep(0.0,.4,world.y));
        c*=mix(.65,1.0,smoothstep(0.0,.65,original.y));
        if(uv.x<.5)c=mix(c,vec3(.24,.205,.13)*(.78+.30*grain),uv.y*.87);
    } else if(uKind==1) {
        float r=valueNoise(vec2(uv.x*8.0,original.y*2.5));
        float marks=step(.70,valueNoise(vec2(uv.x*20.0,original.y*11.0)))*.55*smoothstep(.32,.50,tint.r);
        c*=.64+.45*r-marks;
    } else if(uKind==2) {
        float mid=abs(uv.y);
        c*=.79+.22*uv.x+.12*(1.0-mid);
        ao=mix(.58,1.0,tint.a);
    } else if(uKind==3) {
        c*=mix(.46,1.15,sqrt(uv.x)); ao=mix(.47,1.0,uv.x);
    }
    if(uKind==4 || (uKind==0 && uv.x>1.5)) {
        vec2 coordinates=original.xz*vec2(4.0,45.0);
        float footprint=max(length(dFdx(coordinates)),length(dFdy(coordinates)));
        float grain=mix(.5,valueNoise(coordinates),exp(-2.0*footprint*footprint));
        c=tint.rgb*(.93+.10*grain);
    }
    if(uQuality>0) {
        float roughness=.84;
        if(uKind==0 && uv.x<1.5) {
            float footprint=max(length(dFdx(original.xz)),length(dFdy(original.xz)));
            float detail=valueNoise(original.xz*5.5)*exp(-footprint*6.0);
            float micro=detail*.028+valueNoise(original.xz*1.7)*.035;
            vec3 dx=dFdx(world),dy=dFdy(world),rx=cross(dy,n),ry=cross(n,dx);
            float det=dot(dx,rx);
            if(abs(det)>1e-7)n=normalize(abs(det)*n-sign(det)*(dFdx(micro)*rx+dFdy(micro)*ry));
            float wet=1.0-smoothstep(.05,.65,original.y);
            c*=mix(1.0,.64,wet);
            roughness=mix(.86,.24,wet);
        }
        if(uKind==1)roughness=.78;
        if(uKind==2 || uKind==3)roughness=.52;
        if(uKind==4 || (uKind==0 && uv.x>1.5)) {
            roughness=.43;
            // The railcar is generated too. Broad grain follows each wooden
            // surface and fades below a pixel instead of adding a noisy image.
            float wood=smoothstep(.08,.24,tint.r-tint.b);
            vec2 grainPlane=abs(originalNormal.y)>.5?original.xz:vec2(original.x+original.z,original.y);
            float warp=valueNoise(grainPlane*vec2(2.0,.8));
            vec2 grainCoordinates=vec2(grainPlane.x*2.5,grainPlane.y*55.0+warp*6.0);
            float grainFootprint=max(length(dFdx(grainCoordinates)),length(dFdy(grainCoordinates)));
            float lines=valueNoise(grainCoordinates);
            float fineGrain=(lines-.5)*exp(-2.0*grainFootprint*grainFootprint);
            c*=1.0+wood*(fineGrain*.22+(warp-.5)*.055);
            roughness=mix(roughness,.67,wood);
        }
        float sh=shadow(n),nl=max(dot(n,uSun),0.0),nv=max(dot(n,view),.03);
        float up=clamp(n.y*.5+.5,0.0,1.0);
        vec3 ambient=mix(vec3(.16,.19,.13),vec3(.36,.47,.59),up)*ao;
        vec3 sunlight=vec3(1.85,1.53,1.08);
        vec3 color=c*(ambient+sunlight*nl*sh);
        vec3 h=normalize(view+uSun);
        float nh=max(dot(n,h),0.0),vh=max(dot(view,h),0.0);
        float a=roughness*roughness,a2=a*a;
        float den=nh*nh*(a2-1.0)+1.0;
        float distribution=a2/(3.14159*den*den+.0001);
        float k=(roughness+1.0)*(roughness+1.0)/8.0;
        float vis=nv/(nv*(1.0-k)+k)*nl/(nl*(1.0-k)+k+.0001);
        float fresnel=.035+.965*pow(1.0-vh,5.0);
        color+=sunlight*distribution*vis*fresnel/(4.0*nv+.0001)*sh;
        if(uKind==2 || uKind==3) {
            float thin=uKind==2?mix(.55,1.0,tint.a):mix(.35,.85,uv.x);
            float forward=pow(max(dot(view,-uSun),0.0),4.0);
            float back=max(-dot(n,uSun),0.0);
            color+=c*vec3(1.30,1.14,.54)*(forward*.72+back*.34)*thin*sh;
        }
        finalColor=vec4(fogged(color,world,uEye),1.0);
        return;
    }
    float sh=shadow(n),ndl=max(dot(n,uSun),0.0),up=.5+.5*n.y;
    vec3 ambient=mix(vec3(.20,.22,.13),vec3(.40,.51,.63),up)*ao;
    vec3 direct=vec3(1.42,1.25,.91)*ndl*sh;
    vec3 color=c*(ambient+direct);
    if(uKind==2||uKind==3) {
        float trans=pow(max(dot(view,-uSun),0.0),3.0);
        color+=c*vec3(.85,1.02,.36)*(trans*.65+max(-dot(n,uSun),0.0)*.30)*sh;
        vec3 halfDir=normalize(uSun+view);
        color+=vec3(.42,.46,.24)*pow(max(dot(n,halfDir),0.0),34.0)*sh*.14;
    }
    if(uKind==4 || (uKind==0 && uv.x>1.5))color+=vec3(.20,.18,.12)*pow(max(dot(n,normalize(uSun+view)),0.0),48.0)*sh;
    finalColor=vec4(fogged(color,world,uEye),1.0);
}
