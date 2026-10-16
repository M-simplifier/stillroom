uniform float uTime;
uniform vec3 uSun;
uniform int uLandscape;
float hash12(vec2 p) { vec3 q=fract(vec3(p.xyx)*0.1031); q+=dot(q,q.yzx+33.33); return fract((q.x+q.y)*q.z); }
float valueNoise(vec2 p) { vec2 i=floor(p),f=fract(p); f=f*f*(3.0-2.0*f); return mix(mix(hash12(i),hash12(i+vec2(1,0)),f.x),mix(hash12(i+vec2(0,1)),hash12(i+1.0),f.x),f.y); }
float fbm(vec2 p) { float v=0.0,a=0.5; for(int i=0;i<5;i++){v+=a*valueNoise(p);p=mat2(1.6,1.2,-1.2,1.6)*p+17.1;a*=0.5;}return v; }
float wind(vec2 p,float t) { return 0.47+0.24*sin(dot(p,vec2(.082,.047))-t*.58)+0.13*sin(dot(p,vec2(.21,-.13))-t*1.13)+.10*sin(t*.23); }
vec3 skyColor(vec3 d) {
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
    float d=length(p-eye),f=1.0-exp(-d*.0016*exp(-max(p.y,0.0)*.018));
    return mix(c,skyColor(normalize(p-eye)),clamp(f,0.0,.92));
}
