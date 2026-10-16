# Status

Stillroom 0.7.0 targets Windows x64, tested on Windows 11 with GHC
9.6.7 and h-raylib 5.6.0.0. It is a small utility with best-effort maintenance,
not a medical device or a measured productivity intervention.

Implemented: procedural music; MP3/WAV/OGG folder playback; shuffle, crossfade,
pause and next; saved settings; static backgrounds; monitor choice; fullscreen
and compact mode; English/Japanese controls; single-instance activation.
Image import and a persistent local library, four bundled wallpapers, manual
selection, brightness control, and an embedded Windows icon are included.
Rest mode provides separate brightness and motion choices, and click/R entry
with click/Esc/R/focus-loss return. Music continues.
Three generated landscapes add birch lake, alpine lake and meadow stream, with
terrain, plants, water and wind produced from code. Eight further procedural
scenes generate water, sunset, shallows, clouds, aurora, ink, light veils and
stars in a shader. The two-collection thumbnail gallery provides a live preview,
motion and brightness controls. Rest itself is exit-only, including keyboard input.

Three settings pages separate rest, working backgrounds and music/display.
A larger scene preview, direct motion/brightness choices, a slimmer player and
Tab/Shift-Tab focus navigation share one pure interface description. Native font
ownership and frozen-surface reuse reduce rendering overhead; see the
[controlled measurements](docs/performance.md) and [design rationale](docs/design.md).

The current source passes 33 property/regression groups. The native harness has
49 assertions covering procedural/MP3 playback, audio clocks, transitions,
visibility, preferences, generated thumbnails, shader uniforms and normal exit, including font replacement followed by a failed
library and a fresh rendering session. English/Japanese captures cover every
settings page at the minimum 700×500 window size. A separate `--capture-ui` path
produces these UI captures without running the audio/native acceptance sequence;
the native harness checks all eleven scene selections. The development entrypoint keeps
full logs with concise summaries, and six tooling boundary checks cover failure
propagation, environment restoration and incomplete-package rejection.
Captured frames compare all eleven procedural scenes and verify a frozen frame
over different working backgrounds. Retired local image-based rest settings
migrate to procedural scenes while preserving music and display preferences.
Gallery hit targets are checked at compact and desktop dimensions. Blocked rest
actions are verified at the reducer and real effect boundary, including late
image and music-picker completions after entering rest or selecting a background.
The focus-loss event's state transition
is checked; the harness does not simulate an OS-level Alt-Tab.

Earlier releases also received independent source review and native checks with
Unicode/mixed-case WAV paths, invalid music files and fallback. Those are prior
release evidence. CI builds and tests the pinned source.

Presentation uses a clipped, oversized borderless surface with VSync. Controls
target 60 FPS; rest and landscape previews target 30 FPS. Hidden/minimized
windows skip drawing while audio continues. The affected user confirmed in the
earlier release that switching focus no longer causes the
reported blackout or audio interruption on their Windows setup. Native checks
also verify monitor clipping and its removal when returning to compact mode;
other display and driver combinations remain unverified.

Landscape captures cover 720p/1080p and the English/Japanese minimum-size gallery.
The renderer benchmark checks frozen reuse, clock advancement, resize bounds and
scene invalidation, with a same-scene quality/cost comparison on an NVIDIA RTX
3050 Laptop GPU. Three geometry sets remain resident to avoid generation during
playback; startup and memory costs are recorded in [performance](docs/performance.md).
This is stylized procedural scenery, not demonstrated photographic realism.
An intentionally invalid landscape shader in a separate extracted package reached
the unavailable-scene fallback and completed the native harness with the expected
four rendering-readiness failures. No GPU allocation-failure injection, full graphics-driver matrix or power/battery
measurement was performed. The standalone integrated-GPU prototype is separate
evidence and is not a controlled performance baseline for the product.

Limits: these checks do not certify perceived audio quality, attention effects,
every audio device, every monitor arrangement, or all-day stability. The binary
is unsigned. Custom-drawn controls have keyboard focus navigation and shortcuts but no complete
screen-reader control tree. No loudness normalization, FLAC, recursive folders,
streaming service, installer or automatic updater.
