# Changelog

## 0.2.0 — local update, 2026-10-11

- Add a persistent image library: import PNG/JPEG/BMP, browse previous/next,
  remove imported copies, and select 100%, 75% or 50% brightness.
- Include four generated static backgrounds: mist lake, quiet forest, soft dunes,
  and night field. Image selection stays fixed until changed by the user.
- Copy imports into the private settings directory; originals remain untouched.
  Decode on a worker while the playback/rendering loop continues. Keep the working
  image on failed imports and release replaced texture allocations.
- Add a multiresolution Windows icon, an asInvoker manifest and Unicode-safe
  build artifact lookup. Preserve version 1 settings during migration.

## 0.1.1 — 2026-10-11

- Keep fullscreen as an undecorated desktop window, with a clipped one-pixel
  overscan to avoid an exactly monitor-sized OpenGL surface. This targets
  focus-switch blackouts without changing the display mode or neighboring screens.
- Request synchronized presentation and use raylib's 60 FPS pacing to improve
  fullscreen frame consistency. The affected user confirmed that focus-switch
  blackouts and audio interruptions stopped on their Windows setup.
- Draw with the updated dimensions immediately after changing window mode or monitor.
- Open the music picker at the selected folder. Keep that location when switching
  to built-in music, so it can be selected again without navigating from Desktop.

## 0.1.0 — 2026-10-11

Initial Windows release: pure Haskell core, four procedural presets, local music
folders, two-deck playback, static scenery, compact controls and saved preferences.
No external music recordings are included.
