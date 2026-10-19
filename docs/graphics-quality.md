# Graphics quality

Stillroom 0.9 adds **Auto, Low, Standard, High and Ultra** to Settings → Rest.
The setting is saved. Existing installations start on Auto and retain their
music, background, brightness, motion preference and journey bookmark.

Auto starts at Standard and uses completed OpenGL timer queries during moving
rest scenes. It does not run a startup benchmark, inspect GPU marketing names,
change the GPU assignment, or wait for a query to finish. An unavailable timer
leaves it at Standard. Preview windows and frozen frames do not train the policy.

After 90 settling observations, each 120-observation window uses its 90th
percentile. Below 14 ms it may move up one tier; above 25 ms it moves down one.
Each decision starts another settling interval. A reduction also caps subsequent
promotion for that scene and viewport session, avoiding repeated oscillation.
A different scene, viewport or preference starts a fresh decision. This is a
conservative policy for the existing 30 FPS rest loop, not an FPS guarantee or
a power/battery optimization. A single terrain-generation stall is not enough
to lower quality. Choose Low explicitly when reduced graphics work matters.

| Tier | Landscape raster cap | Shadow map | Reflection per dimension |
| --- | --- | --- | --- |
| Low | 1120 × 630 | 1024² | ½ |
| Standard | 1600 × 900 | 2048² | ½ |
| High | 2560 × 1440 | 3072² | ¾ |
| Ultra | 3840 × 2160 | 4096² | full |

Aspect ratio is preserved. Ultra renders at up to 1.5 times the output size in
each dimension, within its cap, then filters down spatially. It has no temporal
history or ghost trails. At a 4K output it renders at 4K rather than above 4K.
Atmosphere scenes have separate bounded raster budgets (1120×630, 1920×1200,
2560×1440, 3840×2160); the more expensive Cloud sea retains a lower width cap.
Working wallpapers and music are unaffected.

The landscape update includes coherent sky and aerial colour, directional cloud
shading, filtered water ripples and reflections, depth-dependent water absorption,
subtle bed detail, material roughness, leaf transmission and softer shadows.
Standard and above enable the richer material and water model; High and Ultra
add blocker-based shadow filtering. No photograph or image-specific deformation
is involved. These are stylized analytic approximations, not ray tracing or a
complete physical atmosphere solver.

Quality never changes the world seed, terrain meshes, collision, navigation,
railway or semantic time. The native targets are replaced on the GL-owning
window thread and released before the context closes. The frozen nature and
atmosphere caches include representation changes. Timer queries have a fixed
four-slot ring; only available results are read. See the
[Khronos query documentation](https://wikis.khronos.org/opengl/Timer_Query).

If a new graphics target cannot be allocated, the renderer tries a smaller
budget and keeps a valid old target until replacement succeeds. Native scenes
remember the lower ceiling for their resource lifetime. Atmosphere targets also
shrink on failure and keep the previous image if all replacements fail. The
diagnostic harness injects allocation limits into all three render paths; this
does not simulate every possible driver failure or total device loss.

## Reproducible checks

- `tools/dev.ps1 Build`: domain, preference migration and governor laws.
- `tools/dev.ps1 Capture` and `Check`: real settings layouts and existing audio,
  save/load, rest/working and journey acceptance checks.
- `stillroom.exe --quality-capture OUTPUT`: identical journey pose, clock and
  near/far geometry fingerprints at all four tiers, with resize/downshift checks.
- `stillroom.exe --check-graphics OUTPUT`: all eleven quiet scenes at four tiers,
  frozen-cache checks, and paced automatic selection in a lake and moving journey.

Diagnostics use isolated state. Synchronized capture timings and paced frame
timings have different meanings: the former include submission and a GPU wait;
the latter include the 30 FPS limiter and possible streaming work. Neither is a
promise for all GPUs, power modes, monitor sizes or long-running sessions.
