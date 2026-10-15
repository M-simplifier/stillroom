# Status

Stillroom 0.5.0 targets Windows x64, tested on Windows 11 with GHC
9.6.7 and h-raylib 5.6.0.0. It is a small utility with best-effort maintenance,
not a medical device or a measured productivity intervention.

Implemented: procedural music; MP3/WAV/OGG folder playback; shuffle, crossfade,
pause and next; saved settings; static backgrounds; monitor choice; fullscreen
and compact mode; English/Japanese controls; single-instance activation.
Image import and a persistent local library, four bundled wallpapers, manual
selection, brightness control, and an embedded Windows icon are included.
Rest mode provides separate brightness and motion choices, and click/R entry
with click/Esc/R/focus-loss return. Music continues.
Eight procedural scenes generate water, sunset, shallows, clouds, aurora, ink,
light veils and stars in a shader. The thumbnail gallery provides a live preview,
motion and brightness controls. Rest itself is exit-only, including keyboard input.

The current source passes 29 property/regression groups. The native harness has
42 assertions covering procedural/MP3 playback, audio clocks, transitions,
visibility, preferences, generated thumbnails, shader uniforms and normal exit.
Captured frames compare all eight procedural scenes and verify a frozen frame
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

Presentation uses a clipped, oversized borderless surface with VSync and 60 FPS
pacing. The affected user confirmed that switching focus no longer causes the
reported blackout or audio interruption on their Windows setup. Native checks
also verify monitor clipping and its removal when returning to compact mode;
other display and driver combinations remain unverified.

Limits: these checks do not certify perceived audio quality, attention effects,
every audio device, every monitor arrangement, or all-day stability. The binary
is unsigned. Custom-drawn controls have keyboard shortcuts but no complete
screen-reader control tree. No loudness normalization, FLAC, recursive folders,
streaming service, installer or automatic updater.
