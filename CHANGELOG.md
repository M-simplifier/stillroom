# Changelog

## 0.6.0 — 2026-10-15

- Keep native font glyphs resident and cache measurements instead of rebuilding
  font data for each label; retain safe FFI calls and explicit CPU/GPU ownership.
- Reuse frozen rest frames and skip wallpaper drawing beneath opaque rest.
- Rebuild settings around Rest, Background and Music & app, with a large preview,
  all eight scenes visible, and direct motion and brightness choices.
- Reduce the player panel height from 160 to 98 pixels; keep the primary rest
  action and a consistent, readable palette.
- Add Tab/Shift-Tab traversal, visible focus and Enter/Space activation. Rest
  remains exit-only.
- Add rendering benchmarks, interface properties, English/Japanese minimum-size
  captures, and a font replacement / library recovery regression check.
- Document performance evidence and the interface research and design decisions.

## 0.5.0 — 2026-10-15

- Add eight procedural rest scenes: Water and light, Amber sea, Sunlit shallows,
  Sea of clouds, Northern lights, Light veil, Starry night and Flowing ink.
  Water uses intersected wave heights, analytic normals, coherent sky reflections
  and filtered distant detail. No rest image or video assets are needed.
- Add a thumbnail gallery with a live preview, motion and brightness controls.
- Click the background or press R to rest, keeping music and the window in place.
  Returning restores the working background; there is no timer or automatic finish.
- Make rest exit-only: click, Esc/R or focus loss returns to work. Other scene,
  transport, settings and window actions have no effect during rest.
- Save version 6 preferences while accepting versions 1–5. Retired image-based
  rest choices from local previews migrate to procedural scenes without losing
  music, volume, display or background settings. Always start in the working view.
- Bound procedural rendering resolution; generate thumbnails only once. Apply
  dimming after opaque compositing to prevent background leakage.
- Add shader research notes, gallery checks, rendered-frame verification and
  exit-only state properties.
- Add a persistent image library: import PNG/JPEG/BMP, browse previous/next,
  remove imported copies, and select 100%, 75% or 50% brightness.
- Include four generated static backgrounds: mist lake, quiet forest, soft dunes,
  and night field. Image selection stays fixed until changed by the user.
- Copy imports into the private settings directory; originals remain untouched.
  Decode on a worker while the playback/rendering loop continues. Keep the working
  image on failed imports and release replaced texture allocations.
- Ignore pending image and music-picker results when a newer choice or entering
  rest has superseded them, so delayed IO cannot overwrite the working view.
- Add a multiresolution Windows icon, an asInvoker manifest and Unicode-safe
  build artifact lookup.

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
