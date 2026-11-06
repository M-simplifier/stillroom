in vec3 vertexPosition;
in vec3 vertexNormal;
in vec2 vertexTexCoord;
in vec2 vertexTexCoord2;
in vec4 vertexColor;
layout(location=6) in vec4 grassRootHeight;
layout(location=7) in vec4 grassParameters;
uniform int uGrassInstances;
uniform mat4 uClipFromRender;
uniform mat4 uObject;
uniform mat4 uLightVP;
uniform sampler2D uBones;
uniform int uKind;
uniform int uFarPass;
uniform vec3 uReferenceEye;
uniform vec3 uOrigin;
out vec3 world;
out vec3 normal;
out vec3 original;
out vec3 originalNormal;
out vec2 uv;
out vec4 tint;
out vec4 lightSpace;
out float foliageShape;
vec4 bone(int index) { return texelFetch(uBones,ivec2(index%256,index/256),0); }
void bend(inout vec3 p,inout vec3 n,vec3 origin,float angle) {
    vec3 axis=normalize(vec3(-.26,0.0,-.966));
    vec3 v=p-origin;
    p=origin+v*cos(angle)+cross(axis,v)*sin(angle)+axis*dot(axis,v)*(1.0-cos(angle));
    n=n*cos(angle)+cross(axis,n)*sin(angle)+axis*dot(axis,n)*(1.0-cos(angle));
}
void main() {
    vec3 shapePosition=vertexPosition,shapeNormal=vertexNormal;
    vec2 shapeUV=vertexTexCoord,shapeExtra=vertexTexCoord2;
    if(uGrassInstances!=0) grassShape(vertexPosition,grassRootHeight,grassParameters,true,shapePosition,shapeNormal,shapeUV,shapeExtra);
    vec3 p=shapePosition,n=shapeNormal;
    if(uKind==0 && shapeUV.x<.5 && uFarPass==0) {
        p.y=mix(p.y,shapeExtra.x,smoothstep(55.0,85.0,distance(p.xz,uReferenceEye.xz)));
    }
    if(uKind==1||uKind==2) {
        if(uKind==2) {
            float flutter=sin(uTime*3.8+shapeExtra.y)*.014*shapeUV.x;
            p+=n*flutter;
            n=normalize(n+vec3(flutter*2.8,0.0,-flutter));
        }
        int id=int(shapeExtra.x+.5);
        for(int i=0;i<5;i++) {
            if(id<0) break;
            vec4 b=bone(id*2),q=bone(id*2+1);
            float gust=wind(b.xz,uTime);
            float a=(.35+gust)*sin(uTime*.66+q.y)*q.x;
            a+=gust*q.x*.65;
            bend(p,n,b.xyz,a);
            id=int(b.w)-1;
        }
    } else if(uKind==3) {
        float t=shapeUV.x,phase=shapeExtra.y;
        float fade=1.0-smoothstep(27.0,46.0,distance(p.xz,uReferenceEye.xz));
        p.xz=mix(shapeExtra,p.xz,fade);
        p.y=mix(shapeUV.y,p.y,fade);
        float gust=wind(shapeExtra,uTime);
        float a=(.28+gust*.65+.16*sin(uTime*2.1+phase))*t*t*fade;
        p.x+=a*.32; p.z-=a*.085; p.y-=a*a*.055;
        n=normalize(n+vec3(a*.45,.1,0));
    }
    // The object translation was made relative in Double before Float lowering.
    // Static fields retain their world-space phase, then enter render space.
    if(uKind==4){p=(uObject*vec4(p,1.0)).xyz;n=mat3(uObject)*n;}
    else p-=uOrigin;
    world=p;normal=n;original=shapePosition;originalNormal=shapeNormal;uv=shapeUV;tint=vertexColor;foliageShape=shapeExtra.y;
    lightSpace=uLightVP*vec4(p,1.0);
    gl_Position=uClipFromRender*vec4(p,1.0);
}
