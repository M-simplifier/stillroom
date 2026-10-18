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
