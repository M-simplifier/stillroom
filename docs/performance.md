# Rendering cost and the Haskell/C boundary

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
