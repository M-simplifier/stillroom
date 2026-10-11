# Status

Stillroom 0.1.0 is an initial Windows x64 release, tested on Windows 11 with GHC
9.6.7 and h-raylib 5.6.0.0. It is a small utility with best-effort maintenance,
not a medical device or a measured productivity intervention.

Implemented: procedural music; MP3/WAV/OGG folder playback; shuffle, crossfade,
pause and next; saved settings; static backgrounds; monitor choice; fullscreen
and compact mode; English/Japanese controls; single-instance activation.

Release verification includes properties, independent source review and native
muted checks with procedural music, MP3, Unicode/mixed-case WAV paths, invalid
files and fallback. Checks cover audio clocks, transitions, visibility and normal
termination. CI builds and tests the pinned source.

Limits: these checks do not certify perceived audio quality, attention effects,
every audio device, every monitor arrangement, or all-day stability. The binary
is unsigned. Custom-drawn controls have keyboard shortcuts but no complete
screen-reader control tree. No loudness normalization, FLAC, recursive folders,
streaming service, installer or automatic updater.
