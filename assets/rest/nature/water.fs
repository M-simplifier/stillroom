in vec3 world;
in vec4 reflectionSpace;
uniform vec3 uEye;
uniform sampler2D uReflection;
uniform sampler2D uHeight;
out vec4 finalColor;
void main() {
    float ground=texture(uHeight,(world.xz-vec2(-130.0,-200.0))/vec2(260.0,224.0)).r;
    float depth=-ground;
    if(depth<.01)discard;
    vec2 d=vec2(0);
    float gust=wind(world.xz,uTime),amp=.008;
    float footprint=max(length(dFdx(world.xz)),length(dFdy(world.xz)));
    for(int i=0;i<7;i++) {
        float k=1.2*pow(1.65,float(i));
        float a=.42+float(i)*1.8;
        vec2 dir=vec2(cos(a),sin(a));
        vec2 current=world.xz+vec2(0,uLandscape==2?uTime*.23:0.0);
        if(uQuality>0)current+=vec2(valueNoise(world.xz*.075),valueNoise(world.xz*.075+37.0))*.55;
        float ph=dot(current,dir)*k-uTime*sqrt(9.81*k)*.31;
        float atten=exp(-.25*k*k*footprint*footprint);
        d+=dir*cos(ph)*amp*k*atten*(.7+.35*gust);
        amp*=.60;
    }
    vec3 n=normalize(vec3(-d.x,1.0,-d.y)),v=normalize(uEye-world);
    float fres=.025+.975*pow(1.0-max(dot(n,v),0.0),5.0);
    vec2 uv=reflectionSpace.xy/reflectionSpace.w*.5+.5;
    uv+=d*vec2(.012,.021)*clamp(depth,0.0,1.0);
    vec3 reflected=texture(uReflection,clamp(uv,.002,.998)).rgb;
    if(uQuality>0) {
        // Filter the reflection cone as well as the short waves. This avoids
        // turning extra resolution into sparkling subpixel noise.
        vec2 px=1.0/vec2(textureSize(uReflection,0));
        vec2 spread=px*(.65+1.3*clamp(1.0-v.y,0.0,1.0));
        reflected*=.40;
        reflected+=texture(uReflection,clamp(uv+vec2(spread.x,0),.002,.998)).rgb*.15;
        reflected+=texture(uReflection,clamp(uv-vec2(spread.x,0),.002,.998)).rgb*.15;
        reflected+=texture(uReflection,clamp(uv+vec2(0,spread.y),.002,.998)).rgb*.15;
        reflected+=texture(uReflection,clamp(uv-vec2(0,spread.y),.002,.998)).rgb*.15;
        float path=depth/max(v.y,.22);
        float pebbles=valueNoise(world.xz*4.5);
        float fine=mix(.5,valueNoise(world.xz*19.0),exp(-footprint*25.0));
        vec3 bed=mix(vec3(.16,.185,.12),vec3(.37,.34,.23),pebbles)*(.84+.24*fine);
        vec3 absorption=exp(-path*vec3(.43,.16,.10));
        vec3 below=bed*absorption*.82+vec3(.018,.105,.116)*(1.0-absorption);
        float caustic=pow(max(0.0,sin(world.x*2.3+sin(world.z*1.7+uTime*.22))*sin(world.z*2.5-sin(world.x*1.3-uTime*.19))),5.0);
        below+=vec3(.12,.14,.07)*caustic*exp(-path*.8)*exp(-footprint*8.0);
        float reflectionWeight=clamp(fres+.035,0.0,1.0);
        vec3 color=mix(below,reflected,reflectionWeight);
        vec3 halfVector=normalize(v+uSun);
        float nv=max(dot(n,v),.025),nl=max(dot(n,uSun),.001),nh=max(dot(n,halfVector),0.0);
        float rough=.15+.025*gust;
        // Include the normal's pixel footprint in the specular width.
        float a2=rough*rough+min(.035,dot(dFdx(n),dFdx(n))+dot(dFdy(n),dFdy(n)));
        float distribution=a2/(3.14159*pow(nh*nh*(a2-1.0)+1.0,2.0));
        float k=rough*.5;
        float visibility=nv/(nv*(1.0-k)+k)*nl/(nl*(1.0-k)+k);
        float sunFres=.02+.98*pow(1.0-max(dot(v,halfVector),0.0),5.0);
        color+=vec3(1.85,1.53,1.08)*distribution*visibility*sunFres/(4.0*nv);
        color=mix(bed*.42,color,smoothstep(.01,.12,depth));
        finalColor=vec4(fogged(color,world,uEye),1.0);
        return;
    }
    float speck=valueNoise(world.xz*5.0);
    vec3 bed=mix(vec3(.22,.22,.105),vec3(.40,.39,.20),speck);
    vec3 absorb=exp(-depth*vec3(.40,.18,.12));
    vec3 below=mix(vec3(.022,.105,.105),bed*.75,absorb);
    float caustic=pow(.5+.5*sin(world.x*5.0+sin(world.z*3.0+uTime*.4)+uTime*.7),7.0);
    below+=vec3(.08,.10,.05)*caustic*exp(-depth*1.4);
    vec3 c=mix(below,reflected,clamp(fres+.12,0.0,1.0));
    vec3 h=normalize(v+uSun);
    float rough=.12+.035*gust,a2=rough*rough,nh=max(dot(n,h),0.0);
    float distribution=a2/(3.14159*pow(nh*nh*(a2-1.0)+1.0,2.0));
    float nv=max(dot(n,v),.001),nl=max(dot(n,uSun),.001),k=rough*.5;
    float visibility=nv/(nv*(1.0-k)+k)*nl/(nl*(1.0-k)+k);
    float fresnel=.02+.98*pow(1.0-max(dot(v,h),0.0),5.0);
    c+=vec3(1.3,1.12,.70)*distribution*visibility*fresnel/(4.0*nv);
    c=mix(bed*.35,c,smoothstep(.01,.16,depth));
    finalColor=vec4(fogged(c,world,uEye),1.0);
}
