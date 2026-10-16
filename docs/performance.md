# Rendering cost and the Haskell/C boundary

## 0.7 landscape quality and cost

The new landscape renderer batches geometry behind one native call. It retains
three generated scenes and shares render targets; scene selection does not
rebuild trees while audio is pumping. GPU work stays inside a window-scoped
bracket. Shader compilation, generation/drawing and destruction use safe FFI
calls on the bound render thread. The optimization is retained data and less
work, not changing long native calls to unsafe.

Measured on Windows 11, GHC 9.6.7, h-raylib 5.6.0.0 and an **NVIDIA GeForce RTX
3050 6GB Laptop GPU**. Each entry below is the median of two 30-frame run medians,
after eight warmup frames. The product renderer, scene geometry, shaders, camera
and 30 Hz clock are the same. Reference uses uncapped raster, full-size reflection
and 2048 shadows; production uses a maximum 1600×900 raster, half-size reflection
and 1536 shadows. Values include render submission, swap and `glFinish`, with
no audio, VSync request, FPS limiter or capture in the timed region.

| Scene | Output | Reference, ms | Production, ms | Frozen production, ms |
| --- | --- | ---: | ---: | ---: |
| Birch lake | 1280×720 | 6.47 | 5.52 | 1.07 |
| Birch lake | 1920×1080 | 9.10 | 7.17 | 2.41 |
| Alpine lake | 1280×720 | 9.49 | 8.40 | 1.35 |
| Alpine lake | 1920×1080 | 13.61 | 8.99 | 2.12 |
| Meadow stream | 1280×720 | 4.87 | 4.01 | 0.88 |
| Meadow stream | 1920×1080 | 8.15 | 5.71 | 2.03 |

Production reduced these 1080p medians by approximately **21–34%** relative to
the reference profile. The largest production run p95 was 12.37 ms. This is a
quality/cost tradeoff, not an identical-pixel speedup, exclusive GPU time, visible
frame rate or energy measurement. Another run on the same GPU was faster in
absolute terms; clock state and other desktop load were not isolated. The
standalone prototype ran on an Intel GPU and is not a controlled baseline here.

An extracted candidate package, using only its own assets/DLL and two generated
MP3 fixtures with Japanese filenames, passed all 49 native checks. Its app-entry
to ready-loop time was **2.32 seconds**, including approximately 0.44/1.02/0.49
seconds for the three geometry generations/uploads. Selection to the first
`EndDrawing` was **31.9–32.1 ms**. Peak process working set was **517 MiB** during
the full test, including PNG captures; this is not steady-state memory or GPU
memory. First audible sample latency and battery use were not measured. Retaining
three geometry sets has a real memory/startup cost, even though it removes
generation from scene switching. Shared terrain vertices are evaluated once at
startup instead of repeating the same height and normal calculations per corner.

Rest and landscape previews target 30 FPS. Frozen views reuse their scene, and
hidden/minimized windows skip rendering while the audio loop continues. Four
benchmark contracts check frozen reuse, clock advancement, resize invalidation
and raster bounds, and selection invalidation. Actual 720p/1080p frames and the
minimum-size galleries were inspected separately. The earlier eight shader
scenes and their existing resolution budgets remain available.

For reproduction and generation choices, see [landscape implementation and
research](procedural-landscapes.md). The remaining sections describe the earlier
0.6 font/FFI optimization; their before/after workloads are distinct from the
new landscape profile comparison.

Stillroom keeps its font in native memory and reuses text measurements. This
avoids rebuilding every glyph bitmap whenever a label is drawn. Frozen rest
scenes reuse their rendered surface; an opaque rest view does not draw the
working wallpaper underneath it. The procedural shader and its resolution are
unchanged by these optimizations.

## Why these changes

Inspecting the pinned **h-raylib 5.6.0.0** source showed that `drawTextEx` and
`measureTextEx` call `withFreeable font`. `Storable Font.poke` allocates the
rectangle and glyph arrays, including the glyph images, and releases them after
each call. Stillroom's Japanese labels made that representation conversion much
more expensive than the actual text drawing. Wrapping a paragraph by measuring
successive prefixes multiplied the work.

`Runtime.Typeface` now retains the result of `c'loadFontFromMemory`. Its
`ForeignPtr` owns CPU glyph data only. The window owns the GPU atlas, and texture
deletion stays on the rendering thread. Drawing uses the binding's existing
native calls. A bounded, font-specific measurement cache is discarded when the
font changes. No `safe` import was changed to `unsafe` for this improvement.

## A controlled comparison

Measured on one Windows setup with GHC 9.6.7 and h-raylib 5.6.0.0. The baseline
was the v0.5.0 renderer with the benchmark harness added. The optimized run used
the **same layout, labels, shader and render dimensions**, before the subsequent
UI redesign. Each case had 8 warmup frames and 60 measured frames, repeated three
times. Values below are the median of each run's frame-time median. There was
no FPS target or VSync request, no audio, and no screen capture in the timed
region. This measures the real renderer in a hidden OpenGL window, not the whole
application, energy use, GPU execution time alone, or input latency.

| Workload | Before, ms/frame | After, ms/frame | Haskell allocation before → after, bytes/frame |
| --- | ---: | ---: | ---: |
| Player, 760×500 | 29.14 | 0.34 | 636,081 → 26,795 |
| Display settings, 760×500 | 94.60 | 0.32 | 1,789,963 → 77,085 |
| Rest gallery, 760×500 | 117.35 | 0.37 | 2,263,605 → 118,771 |
| Moving water over illustrated lake, 1920×1080 | 2.92 | 2.25 | 327,977 → 21,671 |
| Frozen water, 1920×1080 | 2.32 | 0.97 | 21,661 → 1,852 |

The gallery's allocation fell about 95%. GHC allocation statistics exclude
native and GPU allocations. Process CPU counters were too coarse for individual
sub-millisecond frames, so they are not presented as precise zero-cost results.
The unchanged quiet-lake control also varied in elapsed time between runs; small
GPU-bound timing differences should not be attributed entirely to these changes.
The large text-heavy improvement is consistent with removal of the inspected
font conversion path. Normal playback still targets 60 FPS; this is not a claim
of hundreds of visible frames per second or a measured battery-life gain.

For future comparisons, run the renderer with RTS allocation statistics:

```powershell
stillroom.exe --benchmark benchmark-output +RTS -T -RTS
```

The JSON includes all repetitions, frame dimensions, wall-time median/p95,
process CPU time and Haskell allocated bytes. Compare identical workloads before
changing the UI or interpreting a speedup. The command uses no personal settings.
Use `--check` as well to exercise real playback, font replacement, scene changes,
window changes, persistence and cleanup; a renderer benchmark cannot certify them.

## Reusable guidance and prior work

- **Inspect the binding, not only the C API.** Count representation conversion,
  ownership transfers and temporary arrays. h-raylib explicitly exposes native
  functions for performance-sensitive use, with manual ownership obligations.
  [h-raylib documentation](https://github.com/Anut-py/h-raylib/blob/master/DOCUMENTATION.md)
  and [pinned package source](https://hackage.haskell.org/package/h-raylib-5.6.0.0/src/).
- **Classify foreign calls by duration, callbacks and thread affinity.** A short
  non-callback call may suit `unsafe`; a blocking dialog or long computation
  needs a different decision. In GHC, a long unsafe call can delay global GC.
  Keeping buffers alive and keeping them at a stable address are separate
  requirements. [GHC 9.6.7 FFI guide](https://downloads.haskell.org/ghc/9.6.7/docs/users_guide/exts/ffi.html).
- **Do not infer FFI cost from a Haskell time profile alone.** Safe foreign-call
  visibility has limitations. Well-Typed's eventlog tracing work provides a
  concrete way to inspect native-call duration when that is the unresolved
  question. [Tracing foreign function invocations](https://www.well-typed.com/blog/2025/01/trace-foreign-calls/).
- **Make ownership explicit before avoiding copies.** Retain native allocations
  behind a narrow typed boundary, and distinguish borrowed defaults, owned CPU
  memory and context-bound GPU resources. The same design issue appears in
  [Well-Typed's Haskell/Rust binding case](https://www.well-typed.com/blog/2023/03/purgatory/).
- **Skip unchanged work before batching more work.** Render-to-texture can cache
  a static result. Invalidate it for scene, dimensions and time; apply independent
  tint afterward. [raylib author's render-target examples](https://gist.github.com/raysan5/17392498d40e2cb281f5d09c0a4bf798/3a440dfa57d2fd32c2405313925aaa3417fd23e3).
- **Treat language-binding benchmarks as workload examples.** The Python binding
  demonstrates a lower-level hot-loop API and different runtime costs, but its
  bunnymark numbers do not predict this application's text, audio or shaders.
  [raylib-python-cffi performance notes](https://github.com/electronstudio/raylib-python-cffi).

The existing audio synthesis already supplies contiguous storable-vector PCM
blocks to C. Image decoding already retains native pixels rather than roundtripping
them through a Haskell byte list. Those boundaries remain in place. This work
does not establish that every library should bypass its high-level API, upgrade
its compiler, tune the GC, use instancing or replace safe FFI calls.

The completed 0.6 interface was also measured with the same harness: player
0.28 ms, background settings 0.30 ms, and rest gallery 0.36 ms per frame
(median of three run medians at 760×500). These are a final rendering-cost check;
the redesigned layout is not an identical-workload comparison with 0.5.
