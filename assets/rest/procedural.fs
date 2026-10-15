#version 330

// Original Stillroom scenes. No image, noise texture, video or external input.
uniform vec2 resolution;
uniform vec2 origin;
uniform float phase;
uniform int scene;
out vec4 finalColor;

const float TAU = 6.28318530718;

float hash(vec2 p) {
    vec3 q = fract(vec3(p.xyx) * vec3(0.1031, 0.1030, 0.0973));
    q += dot(q, q.yzx + 19.19);
    return fract((q.x + q.y) * q.z);
}

float noise(vec2 p) {
    vec2 i = floor(p), f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);
    return mix(mix(hash(i), hash(i + vec2(1, 0)), u.x),
               mix(hash(i + vec2(0, 1)), hash(i + vec2(1, 1)), u.x), u.y);
}

float mist(vec2 p) {
    float value = 0.0, weight = 0.5;
    mat2 turn = mat2(0.80, -0.60, 0.60, 0.80);
    for (int i = 0; i < 5; i++) {
        value += weight * noise(p);
        p = turn * p * 2.03 + vec2(7.2, 4.1);
        weight *= 0.5;
    }
    return value;
}

// Coherent environment lighting: the very same sky is visible and reflected.
// This is an artistic analytic atmosphere, not a multiple-scattering solver.
vec3 daylight(vec3 direction, bool evening, float blur) {
    float elevation = max(direction.y, 0.0);
    vec3 horizon = evening ? vec3(0.79, 0.40, 0.22) : vec3(0.60, 0.73, 0.79);
    vec3 zenith = evening ? vec3(0.14, 0.21, 0.34) : vec3(0.13, 0.32, 0.52);
    vec3 color = mix(horizon, zenith, pow(clamp(elevation * 1.7, 0.0, 1.0), 0.45));
    vec3 sun = normalize(evening ? vec3(-0.34, 0.10, 1.0) : vec3(-0.46, 0.42, 1.0));
    float facing = max(dot(direction, sun), 0.0);
    color += (evening ? vec3(0.60, 0.29, 0.10) : vec3(0.26, 0.24, 0.19)) * pow(facing, 12.0);
    // Broad cirrus, painted in world directions so it reflects consistently.
    vec2 cloudUV = direction.xz / (0.25 + abs(direction.y)) * 1.3;
    float cloud = mist(cloudUV * vec2(0.7, 2.2) + vec2(11.0, 5.0));
    float wisps = smoothstep(0.43, 0.78, cloud) * smoothstep(0.0, 0.16, elevation);
    color = mix(color, evening ? vec3(0.78, 0.60, 0.48) : vec3(0.86, 0.88, 0.87), wisps * (0.65 - blur * 0.4));
    float disk = smoothstep(0.9995 - blur * 0.002, 0.9999, facing);
    color += (evening ? vec3(1.7, 1.0, 0.46) : vec3(1.0, 0.85, 0.58)) * disk;
    return color;
}

// A continuous heightfield and its analytic derivative. Height and normal use
// the same wave family; small unresolved waves are filtered by pixel footprint.
vec3 waves(vec2 p, float footprint, int count) {
    vec3 result = vec3(0.0);
    float k = 0.62, amplitude = 0.105;
    for (int i = 0; i < 13; i++) {
        if (i >= count) break;
        float fi = float(i);
        float angle = fi * 2.39996 + 0.4 * sin(fi * 1.7);
        vec2 d = vec2(cos(angle), sin(angle));
        float theta = dot(p, d) * k + phase * floor(8.0 * sqrt(k) + 0.5) + fi * 2.83;
        float filter = exp(-k * k * footprint * footprint * 0.5);
        // A weak second harmonic sharpens crests without storm-like breakers.
        float height = sin(theta) - 0.18 * cos(2.0 * theta);
        float slope = cos(theta) + 0.36 * sin(2.0 * theta);
        result += amplitude * filter * vec3(height, d * k * slope);
        k *= 1.63;
        amplitude *= 0.56;
    }
    return result;
}

float caustics(vec2 p) {
    vec2 flow = vec2(sin(phase * 3.0), cos(phase * 2.0)) * 0.25;
    p += flow + vec2(noise(p * 0.6), noise(p * 0.6 + 8.0)) * 0.9;
    vec2 cell = floor(p), local = fract(p);
    float nearest = 10.0, second = 10.0;
    for (int y = -1; y <= 1; y++) for (int x = -1; x <= 1; x++) {
        vec2 offset = vec2(x, y), id = cell + offset;
        vec2 point = 0.5 + 0.35 * sin(vec2(hash(id), hash(id + 12.1)) * TAU + phase * 2.0);
        float d = length(offset + point - local);
        if (d < nearest) { second = nearest; nearest = d; }
        else second = min(second, d);
    }
    // An intentionally bounded caustic approximation, not photon tracing.
    return exp(-28.0 * (second - nearest)) * 0.7;
}

vec3 waterScene(vec2 uv, int kind) {
    bool evening = kind == 1, shallow = kind == 2;
    vec2 screen = (uv - 0.5) * vec2(resolution.x / resolution.y, 1.0);
    vec3 eye = vec3(0.0, shallow ? 3.6 : 1.55, 0.0);
    vec3 ray = normalize(vec3(screen.x * 0.95, shallow ? uv.y * 0.7 - 1.05 : (uv.y - 0.65) * 0.88, 1.0));
    vec3 skyColor = daylight(ray, evening, 0.0);
    if (ray.y > -0.001) return 1.0 - exp(-skyColor * 1.25);
    // Bracket the heightfield, then use safeguarded interpolation. Unlike a
    // normal-only plane this locates an actual displaced surface along the ray.
    float lo = (eye.y - 0.32) / -ray.y;
    float hi = min((eye.y + 0.32) / -ray.y, 1600.0);
    float travel = eye.y / -ray.y;
    float flo = 0.32 - waves((eye + ray * lo).xz, 0.0, 7).x;
    float fhi = eye.y + ray.y * hi - waves((eye + ray * hi).xz, 0.0, 7).x;
    for (int j = 0; j < 7; j++) {
        travel = mix(lo, hi, clamp(flo / max(flo - fhi, 0.00001), 0.1, 0.9));
        vec3 q = eye + ray * travel;
        float f = q.y - waves(q.xz, 0.0, 7).x;
        if (f > 0.0) { lo = travel; flo = f; } else { hi = travel; fhi = f; }
    }
    vec3 point = eye + ray * travel;
    float footprint = travel / resolution.y / max(-ray.y, 0.035);
    vec3 wave = waves(point.xz, footprint, 13);
    vec3 normal = normalize(vec3(-wave.y, 1.0, -wave.z));
    float facing = max(dot(normal, -ray), 0.0);
    float fresnel = 0.0204 + 0.9796 * pow(1.0 - facing, 5.0);
    vec3 reflection = daylight(reflect(ray, normal), evening, clamp(footprint * 0.5, 0.0, 1.0));
    vec3 volume = evening ? vec3(0.018, 0.075, 0.095) : vec3(0.018, 0.15, 0.19);
    volume += vec3(0.02, 0.08, 0.07) * (0.5 + 0.5 * wave.x / 0.25);
    if (shallow) {
        vec3 underRay = refract(ray, normal, 1.0 / 1.333);
        float depth = 2.1 + 0.25 * noise(point.xz * 0.32);
        vec2 floorPoint = point.xz + underRay.xz * depth / max(-underRay.y, 0.1);
        float grain = noise(floorPoint * 28.0) * 0.035;
        float ridges = 0.5 + 0.5 * sin(floorPoint.x * 6.0 + mist(floorPoint * 0.7) * 4.0);
        vec3 sand = vec3(0.55, 0.49, 0.32) * (0.82 + ridges * 0.1 + grain);
        sand += vec3(0.40, 0.44, 0.31) * caustics(floorPoint * 1.45);
        vec3 transmission = exp(-vec3(0.30, 0.10, 0.055) * depth / max(-underRay.y, 0.1));
        volume = sand * transmission + vec3(0.02, 0.26, 0.27) * (1.0 - transmission);
    }
    vec3 color = mix(volume, reflection, fresnel);
    vec3 sun = normalize(evening ? vec3(-0.34, 0.10, 1.0) : vec3(-0.46, 0.42, 1.0));
    vec3 halfVector = normalize(sun - ray);
    float nh = max(dot(normal, halfVector), 0.0);
    float rough = 0.055 + min(0.09, footprint * 0.14);
    float a2 = rough * rough;
    float denominator = nh * nh * (a2 - 1.0) + 1.0;
    float specular = min(1.3, a2 / (3.14159 * denominator * denominator) * 0.018);
    color += (evening ? vec3(1.0, 0.57, 0.24) : vec3(0.88, 0.86, 0.73)) * specular;
    float haze = 1.0 - exp(-travel * 0.004);
    color = mix(color, daylight(normalize(vec3(ray.x, 0.0, ray.z)), evening, 1.0), haze);
    // Fixed exposure; no adaptation, random flicker or postprocess bloom.
    return pow(1.0 - exp(-color * 1.55), vec3(0.88));
}

vec3 veilScene(vec2 uv) {
    vec2 p = (uv - 0.5) * vec2(resolution.x / resolution.y, 1.0);
    vec3 color = mix(vec3(0.018, 0.035, 0.065), vec3(0.038, 0.085, 0.11), uv.y);
    float glow = exp(-1.7 * dot(p - vec2(-0.25, 0.1), p - vec2(-0.25, 0.1)));
    color += vec3(0.015, 0.035, 0.045) * glow;
    // Four translucent, softly folded sheets; no particles or sudden events.
    for (int i = 0; i < 4; i++) {
        float n = float(i);
        float center = -0.18 + n * 0.105
            + 0.13 * sin(p.x * 2.1 + phase * 2.0 + n * 0.72)
            + 0.055 * sin(p.x * 4.0 - phase * 3.0 + n * 1.33);
        float width = 0.085 + 0.025 * sin(p.x * 1.9 + phase + n);
        float d = (p.y - center) / width;
        float body = exp(-d * d * 0.70);
        float foldDistance = (p.y - center + width * 0.32) / (width * 0.18);
        float fold = exp(-foldDistance * foldDistance);
        float along = 0.6 + 0.4 * cos(p.x * 1.4 + n * 0.7);
        vec3 tint = mix(vec3(0.12, 0.40, 0.41), vec3(0.42, 0.29, 0.38), n / 3.0);
        color += tint * (body * 0.42 + fold * 0.25) * along;
    }
    return color;
}

vec3 stars(vec2 p, float scale, float seed) {
    vec2 cell = floor(p * scale), local = fract(p * scale);
    vec3 light = vec3(0.0);
    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            vec2 offset = vec2(x, y);
            vec2 id = cell + offset + seed;
            float chance = hash(id);
            vec2 center = vec2(hash(id + 5.3), hash(id + 17.1)) * 0.8 + 0.1;
            vec2 delta = (offset + center - local) / scale;
            float radius = mix(0.00045, 0.00105, pow(hash(id + 29.7), 3.0));
            // Antialias using the pixel footprint, including compact windows.
            radius = max(radius, 0.58 / resolution.y);
            float core = exp(-dot(delta, delta) / (radius * radius));
            float halo = exp(-dot(delta, delta) / (radius * radius * 9.0)) * 0.085;
            float breathe = 0.94 + 0.06 * sin(phase * (5.0 + floor(chance * 5.0)) + chance * TAU);
            float intensity = mix(0.18, 0.70, pow(hash(id + 43.3), 4.0));
            vec3 tint = mix(vec3(0.58, 0.72, 0.90), vec3(0.96, 0.81, 0.64), hash(id + 31.9));
            light += tint * (core + halo) * intensity * breathe * step(0.72, chance);
        }
    }
    return light;
}

vec3 nightScene(vec2 uv) {
    vec2 p = (uv - 0.5) * vec2(resolution.x / resolution.y, 1.0);
    vec3 color = mix(vec3(0.012, 0.022, 0.045), vec3(0.029, 0.048, 0.085), uv.y);
    vec2 galactic = mat2(0.85, -0.53, 0.53, 0.85) * p;
    float bandDistance = (galactic.y - 0.04) / 0.19;
    float band = exp(-bandDistance * bandDistance);
    vec2 drift = 0.022 * vec2(sin(phase), cos(phase));
    float dust = mist(galactic * vec2(3.4, 7.8) + vec2(13.0, 3.0) + drift);
    float fine = mist(galactic * 17.0 + vec2(23.0, 8.0));
    color += mix(vec3(0.055, 0.095, 0.115), vec3(0.12, 0.075, 0.12), dust)
           * band * (0.25 + 0.8 * dust) * (0.5 + fine) * 1.8;
    color += (stars(p, 35.0, 0.0) + stars(p, 62.0, 87.0) * 0.52) * 1.35;
    return color;
}

float volumeNoise(vec3 p) {
    vec3 i = floor(p), f = fract(p);
    vec3 u = f * f * (3.0 - 2.0 * f);
    vec2 cell = i.xy + i.z * vec2(37.0, 61.0);
    float low = mix(mix(hash(cell), hash(cell + vec2(1, 0)), u.x),
                    mix(hash(cell + vec2(0, 1)), hash(cell + vec2(1, 1)), u.x), u.y);
    cell += vec2(37.0, 61.0);
    float high = mix(mix(hash(cell), hash(cell + vec2(1, 0)), u.x),
                     mix(hash(cell + vec2(0, 1)), hash(cell + vec2(1, 1)), u.x), u.y);
    return mix(low, high, u.z);
}

float cloudDensity(vec3 p) {
    vec3 drift = vec3(sin(phase), 0.0, cos(phase)) * 0.22;
    vec3 q = p * vec3(0.52, 0.65, 0.52) + drift;
    vec2 cell = floor(q.xz), local = fract(q.xz);
    float shape = 10.0;
    // Rounded governing forms first, small erosion second. Pure fBM alone
    // produced a flat fog sheet; overlapping lobes supply readable billows.
    for (int z = -1; z <= 1; z++) for (int x = -1; x <= 1; x++) {
        vec2 offset = vec2(x, z), id = cell + offset;
        vec2 center = 0.12 + 0.76 * vec2(hash(id), hash(id + 18.0));
        float altitude = 0.25 + hash(id + 29.0) * 0.40;
        vec2 delta = local - offset - center;
        float radius = 0.50 + hash(id + 43.0) * 0.28;
        float d = length(vec3(delta, (q.y - altitude) * 0.90)) - radius;
        float blend = max(0.22 - abs(shape - d), 0.0) / 0.22;
        shape = min(shape, d) - blend * blend * 0.055;
    }
    float erosion = volumeNoise(p * 2.6 + drift) * 0.105 + volumeNoise(p * 5.2) * 0.035;
    return clamp((-shape - erosion) * 6.0, 0.0, 1.0) * smoothstep(0.0, 0.25, p.y);
}

vec3 cloudScene(vec2 uv) {
    vec2 screen = (uv - 0.5) * vec2(resolution.x / resolution.y, 1.0);
    vec3 ray = normalize(vec3(screen.x, (uv.y - 0.68) * 1.0, 1.0));
    vec3 skyColor = mix(vec3(0.66, 0.73, 0.76), vec3(0.15, 0.33, 0.51), smoothstep(0.0, 0.45, ray.y));
    if (ray.y >= -0.01) return skyColor;
    vec3 eye = vec3(0.0, 3.0, 0.0);
    float travel = (eye.y - 2.4) / -ray.y;
    float stride = min(0.0375 / -ray.y, 0.55);
    vec3 color = vec3(0.0);
    float transmission = 1.0;
    // Fixed steps and continuous density avoid stochastic temporal noise.
    for (int i = 0; i < 64; i++) {
        vec3 p = eye + ray * travel;
        float density = cloudDensity(p);
        if (density > 0.001) {
            vec3 sun = normalize(vec3(-0.8, 0.45, 0.35));
            float shade = cloudDensity(p + sun * 0.22) * 0.3
                        + cloudDensity(p + sun * 0.60) * 0.55
                        + cloudDensity(p + sun * 1.2) * 0.8;
            float light = exp(-shade * 2.1);
            vec3 tint = mix(vec3(0.27, 0.38, 0.51), vec3(0.93, 0.90, 0.80), light);
            tint = mix(tint, skyColor, 1.0 - exp(-travel * 0.012));
            float alpha = 1.0 - exp(-density * stride * 2.6);
            color += transmission * alpha * tint;
            transmission *= 1.0 - alpha;
        }
        travel += stride;
        if (transmission < 0.015) break;
    }
    return color + skyColor * transmission;
}

vec3 auroraScene(vec2 uv) {
    vec2 p = (uv - 0.5) * vec2(resolution.x / resolution.y, 1.0);
    vec3 color = mix(vec3(0.016, 0.034, 0.055), vec3(0.008, 0.018, 0.042), uv.y);
    color += stars(p, 42.0, 16.0) * 0.65;
    // Integrate translucent folded curtains along a shallow view direction.
    for (int i = 0; i < 12; i++) {
        float z = float(i) / 11.0;
        float x = p.x * (1.0 + z * 0.15) + z * 0.12;
        float hem = -0.10 + 0.14 * sin(x * 2.5 + phase * 2.0 + z * 0.4)
                   + 0.055 * sin(x * 5.7 - phase + z * 0.7);
        float height = p.y - hem + z * 0.06;
        float body = smoothstep(-0.025, 0.025, height) * exp(-max(height, 0.0) * 7.5);
        float fibers = 0.45 + 0.55 * noise(vec2(x * 34.0 + sin(phase), z * 7.0));
        vec3 tint = mix(vec3(0.12, 0.68, 0.38), vec3(0.30, 0.20, 0.55), clamp(height * 3.5, 0.0, 1.0));
        color += tint * body * fibers * 0.18;
    }
    float ridge = -0.37 + 0.028 * noise(vec2(p.x * 6.0, 3.0));
    color = mix(vec3(0.012, 0.026, 0.033), color, smoothstep(ridge, ridge + 0.004, p.y));
    return color;
}

vec3 inkScene(vec2 uv) {
    vec2 p = (uv - 0.5) * vec2(resolution.x / resolution.y, 1.0) * 2.0;
    vec2 drift = vec2(sin(phase), cos(phase)) * 0.24;
    vec2 flow = vec2(mist(p + drift + 4.0), mist(p + drift + vec2(8.0, 1.0)));
    vec2 folded = vec2(mist(p + flow * 2.5 + vec2(5.0, 9.0)), mist(p - flow * 2.0 + 14.0));
    float pigment = mist(p + folded * 3.2 + drift * 0.3);
    vec3 color = mix(vec3(0.055, 0.12, 0.14), vec3(0.64, 0.72, 0.64), smoothstep(0.22, 0.77, pigment));
    float contour = exp(-abs(pigment - 0.48) * 75.0);
    color += vec3(0.17, 0.13, 0.065) * contour;
    return color;
}

void main() {
    vec2 uv = (gl_FragCoord.xy - origin) / resolution;
    vec3 color;
    if (scene == 0) color = waterScene(uv, 0);
    else if (scene == 1) color = veilScene(uv);
    else if (scene == 2) color = nightScene(uv);
    else if (scene == 3) color = waterScene(uv, 1);
    else if (scene == 4) color = waterScene(uv, 2);
    else if (scene == 5) color = cloudScene(uv);
    else if (scene == 6) color = auroraScene(uv);
    else color = inkScene(uv);
    // Stationary, shallow vignette. No exposure pulsing or temporal noise.
    vec2 edge = (uv - 0.5) * 2.0;
    color *= 1.0 - 0.10 * dot(edge, edge);
    finalColor = vec4(clamp(color, 0.0, 1.0), 1.0);
}
