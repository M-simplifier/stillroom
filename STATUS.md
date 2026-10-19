# Status

Stillroom 0.8.2 targets Windows x64, tested on Windows 11 with GHC 9.6.7 and
h-raylib 5.6.0.0. It is a small utility with best-effort maintenance, not a
medical device or a measured productivity intervention.

The listening room provides procedural music, MP3/WAV/OGG folder playback,
shuffle, crossfade, pause/next, saved settings, local background images, six
bundled wallpapers, monitor selection, fullscreen/compact mode, English/Japanese
controls and single-instance activation. Image import and a local library,
brightness controls and an embedded Windows icon are included.

Rest settings offer three collections. Journey connects a procedural railcar
ride, free walking and automatic strolling in one world of six habitats,
lakes and hills. It saves the seed and place between breaks and launches;
New landscape explicitly replaces that journey. Nature and Light & sky retain
the eleven earlier exit-only scenes, including three generated landscapes.
Music continues in every scene. Journey permits movement and looking around;
Esc/R, focus loss and its visible return button return to work. The quiet scenes
also accept a background click to return.

The current source passes 42 property/regression groups. They cover the pure
session/interface, seeds and interleaved native field queries, bookmark
validation, fixed-tick input, cross-seed automatic walking and rail-to-foot
transfers. The native acceptance harness has 56 assertions covering audio
clocks, MP3/procedural playback, preferences, scene selection, rendering,
journey preparation, movement, save/reload and rest/work transitions.
Twenty-eight captures cover English/Japanese settings at 700 by 500 and larger
sizes, quiet landscapes, the journey and all six bundled wallpapers. Six
development-tool boundary checks cover error propagation, environment
restoration and incomplete packages.

A separate long journey check renders 1,800 frames representing ten minutes
of travel, visits all six habitats, and checks the 49 near / 81 distant chunk
limits, eviction and byte-identical regeneration after returning. It also
checks seed changes, matching navigation/rendered terrain and disembarking /
walking / boarding. This is accelerated semantic time, not an all-day test.
Development measurements and the known generation spikes are recorded in
[journeys](docs/journeys.md). Native tests use a hidden muted window with a real
graphics context and audio device. They do not simulate physical key presses,
OS-level Alt-Tab, or certify perceived audio quality.

Journey allocation is lazy. Initial preparation generates bounded amounts of
terrain per frame while audio continues; leaving a journey for a quiet scene
releases its world. Returning to work freezes movement, releases mouse capture
and saves the place. Working previews do not advance the journey. Earlier
quiet-scene renderer measurements remain in [performance](docs/performance.md),
with their original conditions and version scope.

Presentation uses a clipped oversized borderless surface with VSync. Controls
target 60 FPS; rest and landscape previews target 30 FPS. Hidden/minimized
windows skip drawing while audio continues. The earlier owner-confirmed fix for
focus-change blackouts/audio interruption is prior release evidence. Other
monitor, driver and GPU combinations remain unverified. Procedural scenery is
stylized; photographic realism is not a verified claim.

The Windows executable requests a high-performance GPU on NVIDIA Optimus and
AMD PowerXpress hybrid systems. Explicit OS/user and device power policies can
override this preference. It does not edit graphics settings or require a
discrete GPU. GPU selection occurs at process startup, not when entering rest.

Distribution uses the pinned Windows CI build, replaceable GMP DLL and matching
source notices. The official English/Japanese page has an explicitly played
journey film; it does not autoplay. The binary is unsigned. Custom-drawn controls
have keyboard navigation and shortcuts but no complete screen-reader control
tree. No loudness normalization, FLAC, recursive music folders, streaming
service, installer or automatic updater is included. The checks do not certify
attention or health effects, every audio device, power/battery cost, GPU
allocation failures or all-day stability.
