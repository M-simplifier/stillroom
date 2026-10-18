in vec3 world;
in vec3 normal;
in vec3 original;
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
out vec4 finalColor;
float shadow(vec3 n) {
    vec3 p=lightSpace.xyz/lightSpace.w*.5+.5;
    if(p.x<0.0||p.x>1.0||p.y<0.0||p.y>1.0||p.z>1.0) return 1.0;
    float bias=max(.00023,.0011*(1.0-dot(n,uSun))),v=0.0;
    for(int x=-1;x<=1;x++)for(int y=-1;y<=1;y++) {
        float d=texture(uShadow,p.xy+vec2(x,y)/vec2(textureSize(uShadow,0))).r;
        v+=step(p.z-bias,d);
    }
    float edge=min(min(p.x,1.0-p.x),min(p.y,1.0-p.y));
    return mix(1.0,.18+.82*v/9.0,smoothstep(.01,.10,edge));
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
        float grain=valueNoise(original.xz*vec2(4.0,45.0));
        c=tint.rgb*(.93+.10*grain);
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
