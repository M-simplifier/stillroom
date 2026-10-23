# Status

Stillroom 0.11.0 targets Windows x64, tested on Windows 11 with GHC 9.6.7 and
h-raylib 5.6.0.0. It is a small utility with best-effort maintenance, not a
medical device or a measured productivity intervention.

The listening room provides four continuously composed music moods, MP3/WAV/OGG
folder playback, shuffle, crossfade, pause/next, saved settings, local background images, ten
bundled wallpapers, monitor selection, fullscreen/compact mode, English/Japanese
controls and single-instance activation. Image import and a local library,
brightness controls and an embedded Windows icon are included.
Fresh settings start with soft piano and a still water landscape. Existing
preferences and personal music folders are retained. The new sample banks and
four static landscapes have reproducible generation tools; the piano samples
carry a CC BY 3.0 attribution alongside the project's MIT license.

Rest settings offer three collections. Journey connects a procedural railcar
ride, free walking and automatic strolling in one world of six habitats,
lakes and hills. It saves the seed and place between breaks and launches;
New landscape explicitly replaces that journey. Nature and Light & sky retain
the eleven earlier exit-only scenes, including three generated landscapes.
Music continues in every scene. Journey adds wind, leaves, water and vehicle
sounds, with quieter music while these sounds are enabled. The layer follows
the terrain, movement and seat, and can be disabled in Rest settings.
Journey permits movement and looking around;
Esc/R, focus loss and its visible return button return to work. The quiet scenes
also accept a background click to return.

Rest settings also offer saved Auto, Low, Standard, High and Ultra quality.
Auto observes moving scenes with nonblocking GPU queries and conservative
hysteresis. Quality changes render targets and shading, never navigation or
world geometry. See [graphics quality](docs/graphics-quality.md) for budgets
and measurement boundaries.

The current source passes 71 property/regression groups. They cover the pure
session/interface, seeds and interleaved native field queries, bookmark
validation, fixed-tick input, cross-seed automatic walking and rail-to-foot
transfers, exact box boundaries, emitted mesh winding, projection and origin
laws, rejected frame encodings, and independence of reflection resolution.
Audio checks cover sample alignment, partition-independent rendering, bounded
composition queries, environment transitions and saved source selection.
See [built-in music and scenery](docs/builtin-world.md) for the design,
reproduction commands and bounded audio evidence.
The journey's functional graphics foundation and scoped acceptance evidence
are described in [graphics foundation](docs/graphics-foundation.md).
The build also accepts a valid graphics API client and rejects four invalid
clients, protecting coordinate-space roles and the opaque plan boundaries.
The native acceptance harness has 56 assertions covering audio
clocks, MP3/procedural playback, preferences, scene selection, rendering,
journey preparation, movement, save/reload and rest/work transitions.
Thirty-two captures cover English/Japanese settings at 700 by 500 and larger
sizes, quiet landscapes, the journey and all ten bundled wallpapers. Six
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
