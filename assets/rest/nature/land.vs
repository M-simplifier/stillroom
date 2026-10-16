in vec3 vertexPosition;
in vec3 vertexNormal;
in vec2 vertexTexCoord;
in vec2 vertexTexCoord2;
in vec4 vertexColor;
uniform mat4 mvp;
uniform mat4 uLightVP;
uniform sampler2D uBones;
uniform int uKind;
out vec3 world;
out vec3 normal;
out vec3 original;
out vec2 uv;
out vec4 tint;
out vec4 lightSpace;
vec4 bone(int index) { return texelFetch(uBones,ivec2(index%256,index/256),0); }
void bend(inout vec3 p,inout vec3 n,vec3 origin,float angle) {
    vec3 axis=normalize(vec3(-.26,0.0,-.966));
    vec3 v=p-origin;
    p=origin+v*cos(angle)+cross(axis,v)*sin(angle)+axis*dot(axis,v)*(1.0-cos(angle));
    n=n*cos(angle)+cross(axis,n)*sin(angle)+axis*dot(axis,n)*(1.0-cos(angle));
}
void main() {
    vec3 p=vertexPosition,n=vertexNormal;
    if(uKind==1||uKind==2) {
        if(uKind==2) {
            float flutter=sin(uTime*3.8+vertexTexCoord2.y)*.035*vertexTexCoord.x;
            p+=n*flutter;
            n=normalize(n+vec3(flutter*2.8,0.0,-flutter));
        }
        int id=int(vertexTexCoord2.x+.5);
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
        float t=vertexTexCoord.x,phase=vertexTexCoord2.y;
        float gust=wind(vertexTexCoord2,uTime);
        float a=(.28+gust*.65+.16*sin(uTime*2.1+phase))*t*t;
        p.x+=a*.32; p.z-=a*.085; p.y-=a*a*.055;
        n=normalize(n+vec3(a*.45,.1,0));
    }
    world=p;normal=n;original=vertexPosition;uv=vertexTexCoord;tint=vertexColor;
    lightSpace=uLightVP*vec4(p,1.0);
    gl_Position=mvp*vec4(p,1.0);
}
