in vec2 fragTexCoord;
uniform sampler2D texture0;
out vec4 finalColor;
vec3 aces(vec3 x){return clamp((x*(2.51*x+.03))/(x*(2.43*x+.59)+.14),0.0,1.0);}
vec3 displayed(vec2 p){return pow(aces(texture(texture0,p).rgb*.88),vec3(1.0/2.2));}
float brightness(vec3 c){return dot(c,vec3(.299,.587,.114));}
// Contrast-adaptive subpixel smoothing after tone mapping. This is a small
// dedicated filter, not the complete FXAA edge-search algorithm.
vec3 smoothDetail(vec2 p){
    vec2 px=1.0/vec2(textureSize(texture0,0));
    vec3 c=displayed(p),n=displayed(p+vec2(0,px.y)),s=displayed(p-vec2(0,px.y));
    vec3 e=displayed(p+vec2(px.x,0)),w=displayed(p-vec2(px.x,0));
    float lc=brightness(c),ln=brightness(n),ls=brightness(s),le=brightness(e),lw=brightness(w);
    float hi=max(lc,max(max(ln,ls),max(le,lw))),lo=min(lc,min(min(ln,ls),min(le,lw)));
    if(hi-lo<max(.035,hi*.10))return c;
    float contrast=abs(lc-(ln+ls+le+lw)*.25)/(hi-lo);
    vec3 corners=displayed(p+px)+displayed(p-px)+displayed(p+vec2(px.x,-px.y))+displayed(p+vec2(-px.x,px.y));
    return mix(c,(c+n+s+e+w+corners)/9.0,clamp((contrast-.12)*1.35,0.0,.70));
}
void main(){
    vec3 c=smoothDetail(fragTexCoord);
    vec2 p=fragTexCoord-.5;c*=1.0-.105*dot(p,p);
    finalColor=vec4(c,1.0);
}
