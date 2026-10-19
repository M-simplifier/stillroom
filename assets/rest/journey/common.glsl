uniform float uTime;
uniform vec3 uSun;
uniform int uLandscape;
uniform int uQuality;
float hash12(vec2 p) { vec3 q=fract(vec3(p.xyx)*0.1031); q+=dot(q,q.yzx+33.33); return fract((q.x+q.y)*q.z); }
float valueNoise(vec2 p) { vec2 i=floor(p),f=fract(p); f=f*f*(3.0-2.0*f); return mix(mix(hash12(i),hash12(i+vec2(1,0)),f.x),mix(hash12(i+vec2(0,1)),hash12(i+1.0),f.x),f.y); }
float fbm(vec2 p) { float v=0.0,a=0.5; for(int i=0;i<5;i++){v+=a*valueNoise(p);p=mat2(1.6,1.2,-1.2,1.6)*p+17.1;a*=0.5;}return v; }
float wind(vec2 p,float t) { return 0.47+0.24*sin(dot(p,vec2(.082,.047))-t*.58)+0.13*sin(dot(p,vec2(.21,-.13))-t*1.13)+.10*sin(t*.23); }
// Art-directed single-scattering approximation. The same sky illuminates the
// landscape, water and aerial perspective; no photographic sky or LUT asset.
vec3 clearSky(vec3 d) {
    float h=max(d.y,0.0),mu=clamp(dot(d,uSun),-1.0,1.0);
    float air=1.0/(max(h,.0)+.20);
    vec3 extinction=vec3(.075,.145,.265);
    vec3 trans=exp(-extinction*air);
    vec3 rayleigh=vec3(.22,.43,.76)*(1.0+mu*mu)*(.40+.26*air);
    float mie=.055/pow(max(.14,1.0+.72*.72-1.44*mu),1.3);
    vec3 c=rayleigh*trans+vec3(.76,.76,.66)*(1.0-trans)*.59;
    c+=vec3(1.0,.78,.48)*mie;
    c+=vec3(5.0,3.8,2.35)*smoothstep(.99965,.99992,mu);
    return c;
}
vec3 detailedSky(vec3 d) {
    vec3 c=clearSky(d);
    if(d.y>.015) {
        vec2 p=d.xz/(d.y+.20)*3.0+vec2(3.2,7.8);
        float n=fbm(p),coverage=smoothstep(.48,.70,n);
        float lit=clamp((n-fbm(p+normalize(uSun.xz)*.28))*.95+.66,.25,1.0);
        vec3 cloud=mix(vec3(.54,.62,.66),vec3(1.42,1.37,1.21),lit);
        cloud+=vec3(.58,.39,.18)*pow(max(dot(d,uSun),0.0),12.0)*.24;
        c=mix(c,cloud,coverage*smoothstep(.01,.18,d.y)*.85);
    }
    return c;
}
vec3 skyColor(vec3 d) {
    if(uQuality>0)return detailedSky(d);
    float h=max(d.y,0.0),sun=max(dot(d,uSun),0.0);
    vec3 c=mix(vec3(.48,.65,.76),vec3(.085,.26,.49),pow(h,.5));
    if(uLandscape==1)c=mix(vec3(.40,.57,.69),vec3(.055,.19,.38),pow(h,.5));
    c+=vec3(1.0,.75,.43)*pow(sun,12.0)*.19+vec3(3.5,2.7,1.7)*pow(sun,1400.0);
    if(d.y>.015) {
        vec2 p=d.xz/(d.y+.12)*2.8+vec2(3.2,7.8);
        float n=fbm(p),cl=smoothstep(.51,.73,n)*smoothstep(.01,.15,d.y);
        c=mix(c,vec3(.93,.94,.89)+vec3(.17)*n,cl*.75);
    }
    return c;
}
vec3 fogged(vec3 c,vec3 p,vec3 eye) {
    if(uQuality>0) {
        float distanceToEye=length(p-eye);
        vec3 direction=normalize(p-eye);
        float dh=(p.y-eye.y)*.024;
        float integrated=abs(dh)<.001?1.0:(1.0-exp(-dh))/dh;
        float optical=distanceToEye*.0017*exp(-max(eye.y,0.0)*.024)*integrated;
        vec3 transmission=exp(-max(optical,0.0)*vec3(.76,1.0,1.32));
        vec3 air=clearSky(direction);
        vec3 result=c*transmission+air*(1.0-transmission);
        return mix(result,skyColor(direction),smoothstep(380.0,500.0,distanceToEye));
    }
    float d=length(p-eye),f=1.0-exp(-d*.0024*exp(-max(p.y,0.0)*.009));
    f=max(f,smoothstep(380.0,500.0,d));
    return mix(c,skyColor(normalize(p-eye)),clamp(f,0.0,1.0));
}
