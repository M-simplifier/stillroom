# Changelog

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
