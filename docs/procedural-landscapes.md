# Procedural landscapes for a quiet pause

The three landscapes in Stillroom 0.7 are generated locally from code: birch
lake, alpine lake and meadow stream. Their terrain, tree structure, foliage,
grass, stones, material colors and movement have no photograph, generated image,
authored model or texture dependency. Numeric height/bone textures, reflection
targets and thumbnails are intermediate results of this generation.

The design question was not how many effects could fit in a scene. It was how
to give the eye a coherent place to settle without another task to perform.
The camera stays fixed, the center remains open, wind is gentle, and there is
no scene cycling or new control during rest. These are design decisions, not
evidence of therapeutic or attention benefits.

## Different structures, shared renderer

| Landscape | Shape and composition | Generation rules |
| --- | --- | --- |
| Birch lake | A still center framed by nearby trees and unequal distant groves | An irregular elliptical basin, recursive broadleaf branches, folded polygon leaves and patchy shore grass |
| Alpine lake | Unequal peaks behind a low, reflective lake | Overlapping asymmetric ridges and drainage-like grooves; rock/snow depend on height and slope; conifer crowns overlap rather than forming regular shelves |
| Meadow stream | A winding line through open grass | A variable-width sinuous channel, low banks and a few tree groups; water phase moves along the channel |

Randomness is seeded once per scene. It changes the distribution of growth,
not the identity of the view each frame. Groves have different centers, sizes
and densities. Distance reduces small detail: distant conifers use volumes of
pointed foliage sprays; nearby trees also show branches and individual foliage.
Grass height, density, shore wetness and distance fade vary spatially. This
reduces the hard edge of a uniform grass carpet without adding a moving camera.

The current meshes contain approximately 550k, 797k and 397k triangles. They
are fixed-camera scenes, not an open-world vegetation system or botanical
simulation. Mountains are a constructed heightfield, not a geological erosion
solver. Trees remain stylized, and fine foliage can shimmer in motion.

## Research that changed the implementation

- **Tree motion needs hierarchy.** GPU Gems 3 distinguishes movement at tree,
  branch and leaf scales. Stillroom keeps branch origins and parent links in a
  generated numeric texture, applies bounded rotations in the vertex shader,
  and adds smaller leaf flutter. A shared wind field relates neighboring plants;
  independent random vertex offsets would break that coherence.
  [GPU-generated procedural wind animations of trees](https://developer.nvidia.com/gpugems/gpugems3/part-i-geometry/chapter-6-gpu-generated-procedural-wind-animations-trees).
- **Batch vegetation, but choose geometry for the constraint.** The grass chapter
  demonstrates batching and world-position wind with textured cards. This app
  instead uses actual tapered triangles, because scenery image assets are outside
  the chosen design. The transferable idea is shared motion and batching, not
  copying its texture-based representation.
  [Rendering countless blades of waving grass](https://developer.nvidia.com/gpugems/gpugems/part-i-natural-effects/chapter-7-rendering-countless-blades-waving-grass).
- **Water benefits from separate scales.** Large geometric waves and smaller
  analytic normal waves serve different purposes. Stillroom combines these with
  a planar reflected view, terrain-derived depth color, Fresnel weighting and
  a small microfacet specular model. Pixel-footprint filtering attenuates normal
  detail in the distance. This is a controlled approximation, not a full physical
  water solver or the complete Disney BRDF.
  [Effective water simulation](https://developer.nvidia.com/gpugems/gpugems/part-i-natural-effects/chapter-1-effective-water-simulation-physical-models),
  [Disney BRDF notes](https://media.disneyanimation.com/uploads/production/publication_asset/48/asset/s2012_pbs_disney_brdf_notes_v3.pdf).
- **Terrain variation should affect structure.** GPU terrain work motivates
  separating large forms from smaller detail. A fixed scene does not need its
  full volumetric GPU terrain pipeline; an explicit heightfield suffices here.
  Observations of subalpine forest/meadow patterns also motivated unequal tree
  groups and open ground, rather than a uniform row of conifers. Neither source
  makes the resulting scenery an ecological or geological simulation.
  [Generating complex procedural terrains](https://developer.nvidia.com/gpugems/gpugems3/part-i-geometry/chapter-1-generating-complex-procedural-terrains-using-gpu),
  [NPS subalpine landscape](https://www.nps.gov/olym/learn/nature/subalpine.htm).
- **Use inspectable rendering techniques.** raylib's shadow-map example supplies
  a useful depth-target/light-matrix reference. Our independently implemented
  renderer uses a small filtered shadow map. A contrast-adaptive smoothing pass
  follows tone mapping and gamma; it is not the complete FXAA algorithm.
  [raylib shadow mapping](https://github.com/raysan5/raylib/blob/5.5/examples/shaders/shaders_shadowmap.c),
  [FXAA white paper](https://developer.download.nvidia.com/assets/gamedev/files/sdk/11/FXAA_WhitePaper.pdf).

The space-colonization paper was considered through its abstract as an alternative
tree-growth model. It was not implemented: the current recursive branch generator
already serves the accepted composition at a smaller implementation cost.
[Space colonization algorithm](https://www.algorithmicbotany.org/papers/colonization.egwnp2007.html).

## Ownership and the Haskell/C boundary

`Runtime.Nature` brackets an opaque C context inside the raylib window. Haskell
loads shader contents through Unicode-safe file APIs; C borrows them only while
compiling. Frames pass a scene, time and rectangle, not lists of vertices.
Long operations use `safe` FFI on the bound rendering thread. Batching avoids
conversion overhead without relying on an unsafe long call that delays GC.

The C context owns meshes, generated numeric textures, shaders and targets.
Three bounded geometry sets are prepared while startup thumbnails are drawn,
before music begins. Selecting another landscape reuses its GPU geometry.
Shaders, shadow/reflection targets and the final surface are shared. CPU upload
arrays are freed after upload. All GPU resources are released before closing
the OpenGL context; no GPU finalizer runs on a GC thread.

This deliberately trades startup time and resident memory for predictable scene
switching. Regenerating the landscape inside an ordinary frame could delay the
audio pump. Shared terrain vertices are evaluated once during generation instead
of repeating the same height/normal calculations at every triangle corner.

The renderer can run inside the gallery's framebuffer. It saves and restores
the caller's framebuffer, viewport, render dimensions and matrices before
compositing. Testing a standalone viewer alone would not establish this contract.
A missing source, failed shader/uniform or invalid target produces the normal
opaque fallback and an exit route; it must not turn into a successful blank frame.
An intentionally invalid fragment shader was exercised in an isolated extracted
package: the fallback and return path worked, and the harness completed with the
expected rendering-readiness failures. GPU allocation failure was not injected.

## Frame budget and measurement

Normal controls target 60 FPS. Rest, landscape previews and invisible windows
target 30 FPS; audio continues to be pumped. Invisible windows skip scene drawing.
Still mode reuses the rendered frame. Scene, size and the 30 Hz time sample
invalidate the cache. Brightness is applied afterward, preserving opaque output.

The production profile caps the internal raster at 1600×900, preserves aspect,
uses a half-size reflection and a 1536-square shadow map. The benchmark-only
reference profile uses the same geometry, camera, shaders and clocks with a full
reflection, uncapped raster and 2048 shadow map. This comparison measures the
cost of a **quality tradeoff**, not identical pixels or a change in FFI overhead.

Use the existing development entrypoint:

```powershell
./tools/dev.ps1 Capture
./tools/dev.ps1 Build
./tools/dev.ps1 Benchmark
./tools/dev.ps1 Check
```

`Capture` checks the real product renderer at 720p/1080p and both gallery
collections at compact/desktop sizes. `Benchmark` records the GL renderer,
triangle counts, generation times, allocations and synchronized frame timings.
Its landscape samples include `EndDrawing` and `glFinish`, with no audio,
screen capture, VSync request or frame limiter. It also checks frozen reuse,
clock advancement, resize bounds and selection invalidation.

`Check` exercises the actual application loop, audio state, selection, saved
preferences and normal shutdown. It records app-entry-to-ready-loop time,
selection-to-first-`EndDrawing` latency and process peak working set. Those are
not first audible sample latency, input-device latency, GPU memory or energy
measurements. Captures inside this harness affect frame-time and memory peaks.
Verify extracted releases with `CheckPackage`, not a development asset override.

See [measured results and limits](performance.md) and
[current acceptance scope](../STATUS.md). Do not compare an integrated GPU
prototype with a discrete GPU product run as though it were an optimization.
Source review, numeric checks, actual rendered frames and the viewer's aesthetic
judgment answer different questions; none alone establishes the whole result.
