# Website film

The product page's `journey.mp4` was recorded on 2026-10-20 from the published
Windows x64 **v0.10.0** package, with its bundled assets and no source override.
The executable's SHA-256 is
`324874c1b2bff5c7808472dd053965c01d0fa2fc347b73bca10d1735e6b9bc85`.

The `--quality-film supersampled` diagnostic uses the production renderer at
**Ultra** quality with a disposable world and a fixed seed and camera sequence.
It opens neither personal settings nor the audio device. It writes 480 RGBA
frames at 1280×720 to standard output. Encode these as 30 fps H.264 / yuv420p
with fast-start metadata; this film uses x264's slow preset and CRF 17.

The resulting 16-second film is silent and starts only when the visitor plays
it. It presents the scene at fixed semantic timestamps; it does not measure
real-time frame rate. The poster is extracted at two seconds.

Film SHA-256:
`42e690934d7e0790169f396f30747e6aa2e667e35c25ed31adf3d4ff66849089`.

The graphics article keeps its separate 0.9/0.10 **High** comparison films so
that the two versions use the same quality setting.
