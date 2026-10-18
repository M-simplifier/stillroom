in vec3 vertexPosition;
uniform mat4 mvp;
uniform mat4 uReflectionVP;
out vec3 world;
out vec4 reflectionSpace;
void main() {
    vec3 p=vertexPosition;
    p.y=.012*sin(dot(p.xz,vec2(.6,.18))-uTime*.9)+.008*sin(dot(p.xz,vec2(-.23,.73))-uTime*.67);
    world=p;reflectionSpace=uReflectionVP*vec4(p,1.0);
    gl_Position=mvp*vec4(p,1.0);
}
