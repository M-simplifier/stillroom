# Architecture

Stillroom separates decisions from their execution.

The journey renderer follows the same principle. Haskell computes the sampled
landscape, retains its mesh data, and executes the drawing passes. Its typed
geometry and observation core, product frame policy and GPU interpreter are described in
[Functional graphics foundation](graphics-foundation.md).

```
input / time -> Event -> step -> Session + [Effect]
                           |              |
                           v              v
                      scene view      Windows / audio / JSON
```

## Pure modules

`Domain` keeps finite, bounded volume and nonnegative time behind smart
constructors. Backgrounds, transport, presets and transitions are algebraic data
types. A crossfade uses a cosine/sine pair with extra output headroom in the
runtime. Its overlap is at most six seconds and half the length of either track,
so a short incoming track cannot finish before the handover.

`Session` reduces input and elapsed time to the next state and explicit effects.
It knows nothing about raylib, Windows or files. Inactivity controls visibility.
`ViewingMode` separates Working/Resting from transport and window state; rest
scene, motion and shade are independent of the working picture. Leaving rest has
no audio/window effect. There is no countdown or feed. The only animation is
optional procedural motion in rest and its preview.

`Interface` is a pure description of bounds, labels, actions and visual roles.
Drawing, hit testing and keyboard traversal consume it. A stable action identifies
keyboard focus across page changes. Layout properties check supported sizes,
distinct targets, complete traversal and exit-only rest.

`Shuffle` uses a seeded generator and a nonempty catalogue. A normal cycle is a
permutation, and cycle boundaries avoid immediate repeats. Excluding a failed
candidate starts a new cycle and preserves the last accepted item. Removing the
last candidate produces `Nothing`, not a retry loop.

`Audio.Composition` queries bounded windows of an evolving score. Rational beats
become Integer sample positions; seeded phrases vary without a growing history.
`Audio.Mix` validates immutable sample banks once and sums overlapping voices.
The piano uses credited Salamander recordings; other instruments are generated
from code. `Synthesis` retains the earlier oscillator path for compatibility.
`Audio.Environment` advances loop phases, smooths gains and places bounded body
sounds by travel distance. Geometry queries remain on the frame thread.
See [audio design and reproduction](builtin-world.md).

`Landscape.Field` evaluates terrain, shores, rail alignment, habitat weights
and collision from an explicit `Journey.World.Seed`. `Landscape.Geometry`
turns a fixed scene or seeded chunk into immutable contiguous vertex streams,
compact grass instances, branch-bone pixels, height samples and a water mesh. Its bounded builders use
local `ST`; CPU generation needs no window or OpenGL context. Constructors and
record labels stay internal so callers cannot detach a mesh from its attributes
or replace validated texture dimensions.

`Landscape.Grass.Kernel.bladeCorner` defines a blade's arithmetic once. Evaluating
it with Float produces the CPU reference; evaluating it with expressions emits
the GLSL used by both landscape renderers. Each 40-byte blade record supplies
root, height, width, direction, curve and color to a shared fifteen-corner buffer.
The last four bytes record the original vertex order for diagnostics. Blossoms
remain ordinary Grass-material triangles. `materialMesh` returns those
ordinary streams; `grassBlades` supplies instances and `materialVertexCount`
counts both. Only `diagnosticMaterialMesh` reconstructs the former interleaved
vertex stream for frozen-buffer comparisons. Normal upload keeps the compact
representation and performs no per-frame expansion or upload.

## Effect boundary

`Runtime.Audio` owns two music decks, independent ambience and the device on a
bound supplier thread. Supply and controls are serialized, independently of
drawing. Continuous sources have no duration or automatic advance. Ambience
eases music to 0.40 and restores it on exit; master and pause affect all streams.
Replacement
decks are acquired before old ones are released. Decoder input bytes remain alive
until the decoder closes; reading through Haskell also supports Unicode paths.
Synchronous decoding failures remove candidates; asynchronous cancellation
propagates. Native EOF detection is separate from position, since raylib resets
its position at the end of a recording.

`Desktop` requests VSync and uses raylib's 60 FPS pacing for ordinary controls,
30 FPS for rest, landscape previews and invisible windows. Hidden/minimized
windows skip drawing while the input/audio loop continues. `Render` consumes the pure interface and draws the scene, controls and focus ring. `Storage` validates JSON and replaces settings
through a temporary file. `Native` is a small Win32 FFI for monitors, a folder
dialog, sleep requests and a single-instance signal. The folder dialog runs on a
bound worker thread while the audio loop continues. The selected and previously
used folder are stored separately, so choosing procedural music does not discard
the user's location. Render dimensions are queried after window-changing effects.

Fullscreen keeps GLFW's monitor association null. The OpenGL surface overscans
each monitor edge by one pixel; a Win32 window region clips it to exactly the
selected display. This avoids exact surface/display dimensions while leaving
adjacent displays untouched. Compact mode removes the region. Native checks
read back both the outer rectangle and clip rather than relying on application
state alone. The workaround follows [GLFW's explanation of driver fullscreen
promotion](https://discourse.glfw.org/t/fullscreen-window-mode-but-not-the-current-way/2595)
and [Win32 region ownership](https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setwindowrgn).
Driver behavior still requires visual verification on the affected system.

h-raylib is pinned to 5.6.0.0. `Typeface` retains the native font and detects raylib's borrowed fallback
before transferring glyph ownership. CPU glyph data has a `ForeignPtr` owner;
the window owns the GPU atlas. Finalizers never delete GPU resources. Drawing
avoids per-label glyph-array marshalling, and text metrics have a bounded cache.
Each audio-session attempt acquires a new font reference and closes its current
font before retry or window shutdown. See [performance](performance.md). WAV cleanup also frees the context omitted
by that bundled raylib destructor. Recheck both boundaries before dependency
updates. Only the enabled MP3, WAV and OGG formats are exposed.

## Verification

`Runtime.Pictures` owns the image library. File IDs resolve only against discovered
bundled assets and private imported copies. CPU decoding uses native memory on a
worker; texture uploads stay on the rendering thread. `ForeignPtr` owns decoded
pixels until upload, and window resources own GPU textures. Replacements load
before the old texture is released. Image controls are separate reducer events
and cannot reconfigure playback or window placement. Preferences version 9 accepts
versions 1–8, mapping retired local photo choices to procedural scenes at the
JSON boundary without discarding other settings. Working view is always restored at
startup, independently of the saved scene and motion preference.

Image and music-picker requests carry generation numbers. Selecting a background,
switching to built-in music, or entering rest invalidates the relevant pending
work. Stale completions release prepared pixels and clear the busy flag without
replacing textures, audio or saved choices. Native checks inject delayed results
at the real completion boundary to verify this ordering.

`Runtime.Rest` loads the procedural renderers before starting playback. Window resources
own the program, output surface and generated thumbnails. A missing shader or
required uniform shows an opaque dark field with an explanation and an exit route.
The runtime exits rest on a focused-to-unfocused edge. Hit testing uses the view
from before mouse activity, preventing hidden buttons from consuming the first click.

Eight `RestScene` constructors use this texture-free fragment program.
Resolution, viewport origin, scene choice and a bounded phase are passed to GLSL.
The fragment shader derives coordinates from `gl_FragCoord`, independently of the
working image. Heightfield intersections, analytic wave derivatives, reflected
sky, refracted sand, integrated cloud density, folded light and deterministic
stars are separate functions. All temporal frequencies share a 240-second cycle.
No new random data is sampled each frame.

The gallery renders actual 320×180 thumbnails once at startup and one live
preview. The rest clock also advances while this gallery is open. Still freezes
the preview without affecting audio; each new rest begins at time zero. Large
procedural views render into a reusable, window-owned target with bounded size
(1920×1200, or width1120 for clouds), then scale to the window. Shade is applied
after compositing the opaque target so its alpha cannot expose the wallpaper.
Scene, dimensions and time invalidate the target; a frozen frame reuses it.
An opaque rest view skips the hidden working-background pass.

Three additional landscapes use `Runtime.Nature` and the Haskell
`Runtime.Graphics.Landscape` controller inside the same window. It retains three
uploaded geometry sets for immediate selection. `Graphics.Resource` owns meshes,
numeric textures and shader handles; `Graphics.Target` replaces render targets
after a successful allocation. Context-thread cleanup closes these resources
before the window. Scene/size/time invalidate a 30 Hz cache. Standard quality
bounds the landscape raster at 1600×900 and uses a half-resolution reflection.
Nested thumbnail rendering restores its caller's framebuffer, viewport and
matrices. See [generation and ownership details](procedural-landscapes.md).

`RestCollection` groups the gallery without changing the selected scene. The
selected scene determines the initial collection when preferences are restored;
collection browsing itself is transient and has no persistence or audio effect.

For the eleven quiet scenes, both keyboard routing and the pure reducer enforce exit-only rest. Only Elapse,
LeaveRest, ToggleRest and Escape are admitted while Resting; every other event
leaves both state and effects unchanged. The gallery hides the transport panel,
and its hit testing cannot reach the hidden volume bar.

The native check also captures the same frozen procedural scene over two different
working backgrounds. Pixel equality checks that a texture is not leaking through.
Diagnostic frame timings include capture frames and report sample count, median
and p95; they are observations of the test machine, not universal performance bounds.

The PNG/ICO icon is generated from editable geometry. The packaging build embeds
the icon and an `asInvoker` manifest; the runtime also assigns its window icon.

Properties exercise shuffle conservation and boundaries, rejected-candidate
progress, volume validation, pause/resume, visibility, synthesis chunk independence,
finite signal bounds and crossfades with short tracks.

The native harness runs the real loop in a hidden, muted window. It checks audio
clock progression, pause/resume, crossfade state, automatic transition, control
visibility, compact mode and successful process shutdown, saving screenshots and
JSON. This verifies native control and lifecycle, not perceived audio quality or
clinical effectiveness.

Interface inspection is a separate `--capture-ui OUTPUT` path through the same
production renderer. It draws fixed EN/JP states at minimum and desktop sizes,
owns its font and GPU resources for one hidden window, and opens no audio device
or personal settings. Separating it from `--check` keeps repeated layout captures
small without replacing state, audio or native-lifetime acceptance. The
[development entrypoint](development.md) records logs and restores its environment;
package acceptance explicitly excludes development data paths.


## Continuous journey

`Journey.World.Seed` is an explicit value passed to pure `Landscape.Field`
queries. Each generated chunk shares a bounded immutable set of rail-height
controls. There is no selected global world or thread-local terrain cache.
`Journey.Ride` and `Journey.Walk` query the same field used by CPU generation.
Walking keeps its analytical Double queries; mesh generation samples that field
at Float raster coordinates. Grass and LOD morphing interpolate the actual
triangles. These are related surfaces, not an assertion that an analytical
field equals its piecewise-linear approximation between samples.

`Journey.Ride` and `Journey.Walk` are fixed 60 Hz reducers. Frame input accumulates
until a semantic tick; edges are consumed once, relative look is distributed,
and rendering interpolates poses. A journey contains both a parked/moving car and
the walker. Safe nearby transfer has a short fade, with no distant teleport.
`Runtime.Journey` owns input/cursor state and the lazy GPU context. It never owns
the music device. Esc/focus loss suspends it; music remains in `Runtime.Audio`.
The main Session reducer still rejects music/settings events during any break.
Journey-specific actions go only to the journey reducer.

There are 49 nearby 32 m chunks and 81 distant 128 m chunks. Preparation generates
at most one of each per frame, leaving the desktop loop to pump music between
steps. After preparation, travel streams/evicts chunks under the same bounds.
Returning to work retains the cache; choosing a quiet scene frees it. Re-entry
and new seeds never accumulate worlds. HDR, reflection and shadow budgets follow
the selected [graphics quality](graphics-quality.md). Nested previews restore the parent framebuffer,
viewport and matrices before compositing. The gallery's journey thumbnail is a
small captured poster so startup never generates the world merely for its card.

Version 1 journey bookmarks store the seed, car progress/pace, seat and walker
pose/intent, separately from version 9 music/display preferences. Loading checks
finite bounds and valid ground before restoring a position. Velocity,
temporary transfer, routes, and generated meshes are not serialized. Writes use
a sibling temporary file and rename. Versions 1–8 preferences still migrate.

`Runtime.Graphics.Journey` executes the validated `Journey.Frame.FramePlan`
directly. Each observation supplies its target and typed projection together;
`Graphics.Encoding` lowers matrix columns to the pinned raylib representation.
`Runtime.Journey.Packet` remains a diagnostic encoding, outside production
pass selection. GLSL still runs on the GPU and h-raylib remains the native
graphics dependency. Application C is restricted to `cbits/native.c` for the
Windows platform boundary; the former C world and renderers are test oracles,
not production fallbacks.
