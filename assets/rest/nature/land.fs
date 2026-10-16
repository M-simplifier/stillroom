in vec3 world;
in vec3 normal;
in vec3 original;
in vec2 uv;
in vec4 tint;
in vec4 lightSpace;
uniform vec3 uEye;
uniform sampler2D uShadow;
uniform int uKind;
uniform int uShadowPass;
uniform int uClip;
out vec4 finalColor;
float shadow(vec3 n) {
    vec3 p=lightSpace.xyz/lightSpace.w*.5+.5;
    if(p.x<0.0||p.x>1.0||p.y<0.0||p.y>1.0||p.z>1.0) return 1.0;
    float bias=max(.00023,.0011*(1.0-dot(n,uSun))),v=0.0;
    for(int x=-1;x<=1;x++)for(int y=-1;y<=1;y++) {
        float d=texture(uShadow,p.xy+vec2(x,y)/vec2(textureSize(uShadow,0))).r;
        v+=step(p.z-bias,d);
    }
    return .18+.82*v/9.0;
}
void main() {
    if(uClip==1&&world.y<.025) discard;
    if(uShadowPass==1){finalColor=vec4(1.0);return;}
    vec3 n=normalize(normal); if(!gl_FrontFacing)n=-n;
    vec3 view=normalize(uEye-world),c=tint.rgb;
    float ao=1.0;
    if(uKind==0) {
        float footprint=max(length(dFdx(original.xz)),length(dFdy(original.xz)));
        float patches=fbm(original.xz*.28),grain=mix(.5,valueNoise(original.xz*7.5),exp(-footprint*10.0));
        c=mix(vec3(.060,.105,.023),vec3(.17,.24,.057),patches)*(.85+.28*grain);
        c=mix(vec3(.22,.19,.13),c,smoothstep(-.12,.50,original.y));
        float stone=smoothstep(.38,.70,1.0-n.y)*smoothstep(3.0,10.0,original.y);
        if(uLandscape==1)stone=max(stone,smoothstep(5.0,21.0,original.y));
        c=mix(c,vec3(.19,.205,.20)*(.75+.38*patches),stone);
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
    } else if(uKind==1) {
        float r=valueNoise(vec2(uv.x*8.0,original.y*2.5));
        float marks=step(.70,valueNoise(vec2(uv.x*20.0,original.y*11.0)))*.55;
        c*=.64+.45*r-marks;
    } else if(uKind==2) {
        float mid=abs(uv.y);
        c*=.79+.22*uv.x+.12*(1.0-mid);
        ao=mix(.58,1.0,tint.a);
    } else {
        c*=mix(.46,1.15,sqrt(uv.x)); ao=mix(.47,1.0,uv.x);
        c=mix(vec3(.115,.18,.035),c,exp(-length(uEye-world)*.010));
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
    finalColor=vec4(fogged(color,world,uEye),1.0);
}
