# The quiet line

Stillroom 0.8 adds a single connected journey to the rest gallery. A small
observation railcar moves through generated countryside. Stop near safe ground,
step outside, walk freely or use automatic strolling, then return to the waiting
car. Esc returns to the working view and keeps the place for the next break.
The eleven earlier quiet scenes remain available with their exit-only controls.

## Why one world

The railcar supplies a path and a steady changing view without requiring steering.
Walking supplies curiosity and freedom in the same landscape. Keeping them
together preserves the relationship between the place seen from the car and
the place reached on foot. There is no score, destination list, driving challenge
or car mode. New landscape is a deliberate gallery action, not a random reset
each time the user rests. The first seed matches the original journey study;
new seeds change both terrain and rail alignment.

The app's existing music continues. The prototype's separate ambient audio
device is not included: the production journey shares the listening room's
window, lifecycle and music controls. The site film is silent native footage
from the rail prototype, shown with explicit playback controls.

## Terrain, track and walking

Six ecological weights blend meadow, birchwood, pines, heath, wetland and dunes.
Terrain noise, ridges, lake basins and these ecological weights combine rather
than selecting one prefab scene. Trees and large rocks have the same placement
rules in collision queries and mesh generation. The walker plans short routes
around non-walkable ground, remembers recent places and limits turning speed.
Input immediately returns automatic strolling to manual walking.

Rail alignment and elevation use continuous cubic B-splines. Cuttings and small
embankments adapt the terrain near the track; deep gaps use trestles. The track
is a designed corridor, not a globally optimized railway network. The passenger
camera stays level, with acceleration and jerk limits. Three paces target 2.8,
4.6 and 6.2 metres per second. A transfer checks nearby dry ground, height and
clearance, then uses a brief fade. It refuses unsafe bridge-side exits. A parked
car blocks the walker; boarding requires proximity. Bridges are vehicle-only.

The world is deterministic from a saved 32-bit seed. Nearby and distant mesh
caches contain at most 49 and 81 chunks. Evicted places are generated again from
the same seed. The practical coordinate range is finite; this release does not
claim mathematical infinity, a globally connected walkable continent or endless
single-precision accuracy. It is an open, streamed landscape for ordinary breaks.

The current implementation keeps these rules in Haskell. `Landscape.Field`
takes the saved seed for every terrain, track and habitat query.
`Landscape.Geometry.seededChunkGeometry` returns a near or far chunk's immutable
mesh streams, branch hierarchy and sampled heights. `Runtime.Graphics.Journey`
uploads that result, retains the bounded near/far caches and executes the shared
`Journey.Frame.FramePlan` for shadow, reflection and main views. Changing the
quality budget changes targets and shading while keeping the seed and geometry.

Walking queries the analytical Double field. The renderer samples Float raster
coordinates and uses actual triangle interpolation for grass roots and LOD
morphing. A cell's centre can differ from the analytical surface between grid
vertices; tests preserve this approximation boundary rather than changing the
accepted walking behavior to force exact equality.

## References and what was adopted

- [Slow Roads](https://slowroads.io/) demonstrates the appeal of continuous
  procedural travel. This work takes the unhurried journey as an experience
  reference; no source code or art is copied.
- [Tracks](https://store.steampowered.com/app/657240/Tracks__The_Train_Set_Game/)
  is a reference for a small train's scale and viewpoint. Stillroom does not
  include track construction or a toy-building game.
- [Galin et al., Procedural Generation of Roads](https://perso.liris.cnrs.fr/egalin/Articles/2010-roads.pdf)
  motivates considering route, terrain and structures together. The implemented
  spline corridor is not the paper's anisotropic shortest-path/clothoid method;
  there are no tunnels or network planning.
- [Reynolds, Steering Behaviors](https://www.red3d.com/cwr/steer/gdc99/) and
  [Fray, Context Behaviours](https://andrewfray.wordpress.com/2013/03/26/context-behaviours-know-how-to-share/)
  inform local walking decisions and smooth turning. Stillroom uses a bounded
  local route planner, not a global navmesh or guaranteed complete pathfinder.
- [Red Blob Games, Terrain from Noise](https://www.redblobgames.com/maps/terrain-from-noise/)
  and [Minecraft's world-generation discussion](https://www.minecraft.net/en-us/article/new-world-generation-java-available-testing)
  are references for separating terrain shape and ecological variation.

## Verification and limits

The maintained Haskell migration checks are `tools/check-haskell-migration.ps1`
for frozen-baseline CPU comparisons and
`cabal test migration-native -fnative-graphics-tests` for actual
raylib water topology, drawing and resource lifetime. They complement ordinary
laws and the executable's `--check-journey` route. Baseline fixtures identify
the 0.11.0 source revision and keep C solely as an independent porting oracle.
The numerical comparison, rendered journey and viewer's judgment remain
separate evidence.
The native suite is opt-in; ordinary `cabal test` keeps the pure laws usable
without an OpenGL context.

### Local Haskell migration, 2026-11-06

The same 960×540 Standard-quality diagnostic was run from the frozen public
0.11.0 executable and the local Haskell build on the same Windows 11 / RTX 3050
Laptop host. Each includes initial preparation, 1,800 rendered travel frames,
regeneration, seed changes and walking/boarding checks. The semantic route and
resident geometry counts match; this accelerated workload is not ordinary
real-time playback.

| Whole diagnostic | Process wall time | Largest travel-world generation/upload step |
| --- | ---: | ---: |
| Local Haskell renderer, effective GHC `-O2` | 72.03 s | 74.77 ms |
| Public 0.11.0 C renderer, run immediately afterward | 40.43 s | 50.67 ms |

The final comparison ran these two executables consecutively to limit drift
from clocks and other desktop work. The Stillroom package sets `optimization: 2`
in `cabal.project`; verification checked actual library and executable compiler
calls for `-O2`. Dependency settings remain unchanged. Cabal's overall profile
banner can still show the dependency default `-O1`, so that banner alone does
not establish the application's effective optimization.

Earlier default-`-O1` local runs took 89.68 s before, and 73.62 s after, moving
shared land-pass shader state outside the chunk loop; the initial public C
control took 42.52 s. Those earlier values remain development observations.
All twelve final `-O2` journey quality captures stayed pixel-identical to the
baseline and uniform readback error remained zero. The final 1.78× diagnostic
wall cost is a measured limitation, not a performance improvement claim.
The final travel-world counters record 25.13 s of strict CPU generation,
2.54 s of GPU upload calls and 29.99 s of render submission.
Submission excludes `EndDrawing`/presentation
and can include waits inside GL calls; it is not a pure CPU or GPU timer.
These totals include initial residency preparation and the travel world, not
the later recreated worlds used by the same diagnostic.

The near/far caches still hold at most 49/81 chunks. The test observed 692/226
evictions, reproduced both starting signatures after reload and rendered all
six habitats. Ordinary warm-frame captures and the full diagnostic answer
different performance questions; generation hitches remain possible.

Five further CPU candidates were rejected by measurement and reverted: vertex
unpacking, removal of vector bounds checks, shared vegetation rows, an alternative
habitat accumulator, and prepared rail controls inside placement queries.
The final nine landscape modules retain the accepted source hashes. The last
placement probe saved about 7.8 ms across 1,040 representative chunk cases; it did not explain the
25 s generation total or justify another implementation. This migration is a
local trial and does not replace the public release.

### Original 0.8 study

The 0.8 development build passed 42 property/regression groups, including seed
interleaving, distinct seeded terrain, bookmark round-trips/rejection, fixed-tick
partition invariance and safe rail/walk transfers. The integrated desktop check
passed 56 assertions, including music during preparation, saved location,
work/rest suspension and new-world rendering inside the settings preview.

The long journey check rendered 1,800 frames representing 600 seconds of travel
through all six habitats. Near/far caches remained at 49/81 chunks, with 692/226
evictions. Reloading the starting location reproduced identical near/far mesh
fingerprints. Seeds 0, 1 and 4294967295 rendered successfully and shared their
terrain with navigation. Safe exit, walking and reboarding were rendered too.

On the Windows 11 / NVIDIA RTX 3050 Laptop 6 GB test host, this accelerated check
took about 53 seconds. Its maximum observed single-chunk generation was 77 ms;
occasional generation hitches remain possible. A separate integrated desktop
check peaked at about 1.03 GB process working set, including fixed landscapes,
fonts and music. These are test-host observations, not a minimum specification
or a promise of smooth performance on every GPU. No continuous-hour stability,
OS-level input automation, battery measurement or perceptual recovery trial was
performed. The ordinary audio check is muted and does not establish audibility.

Streaming is bounded, but coordinates are not mathematically infinite. The
supported range is one million terrain units from the origin on either axis.
At the rail limit the car stops; walking cannot cross the boundary. The place
remains saveable and an on-screen hint points to New landscape. Origin rebasing
and long-distance visual precision beyond the measured route are not verified.

See [architecture](architecture.md) for ownership and seed boundaries,
[development](development.md) for repeatable checks, and [the guide](guide.md)
for controls and save locations.
