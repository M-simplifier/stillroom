uniform vec2 uResolution;
uniform vec3 uForward;
uniform vec3 uRight;
uniform vec3 uUp;
uniform float uTanFov;
out vec4 finalColor;
void main() {
    vec2 p=(gl_FragCoord.xy/uResolution*2.0-1.0);
    vec3 d=normalize(uForward+uRight*p.x*(uResolution.x/uResolution.y)*uTanFov+uUp*p.y*uTanFov);
    finalColor=vec4(skyColor(d),1.0);
}
