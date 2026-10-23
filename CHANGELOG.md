# Changelog

## 0.11.0 — 2026-10-24

- Start with soft piano and a still, code-generated water landscape. Include
  four reproducible working backgrounds while preserving existing choices.
- Replace the three-minute built-in loops with four evolving Haskell scores.
  Choose a mood directly; Next changes mood. Personal folders keep their playlists.
- Mix pre-rendered instruments in bounded sample windows. Keep exact musical
  time, validated sample banks and playback independent of drawing stalls.
- Add wind, leaves, nearby water and railcar sound driven by the journey.
  Ease music down during the journey and restore it on return; allow sounds off.
- Include offline generators, pinned piano-source hashes, attribution and
  regression checks for mixing, composition, environment phases and migration.

## 0.10.0 — 2026-10-20

- Stabilize the journey's railcar and shadows while preserving the original
  vehicle dimensions, palette, movement and saved journeys.
- Plan geometry references, relative coordinates, cameras and light-space
  grids in Haskell. Main, shadow and reflection views consume the same frame
  description; shadow writing and sampling share the same lowered projection.
- Compile the original railcar's box assembly into exact, uniquely owned
  exterior surfaces. Correct wheel winding and filter fine material detail.
- Use receiver-plane shadow comparisons with visibility filtering, including
  close-up surfaces. Derive the sky basis directly from the planned view.
- Separate reusable space, box-union, mesh and observation modules from
  Stillroom's scene policy and native encoding. Document laws, measurements,
  and the remaining limits of the graphics foundation.

## 0.9.0 — 2026-10-19

- Add saved Auto / Low / Standard / High / Ultra graphics quality in Rest settings.
- Adapt moving rest scenes from asynchronous rendering measurements, with settling,
  headroom and hysteresis; keep music, movement and world generation independent.
- Bring the accepted lighting study into the journey and all three nature scenes:
  clearer water, coherent sky and distance, richer materials and softer shadows.
- Add bounded spatial supersampling, reflection and shadow budgets, and extend
  quality selection to the eight atmosphere scenes.
- Preserve existing settings/bookmarks and the quiet rest interface. Add product
  graphics checks, preference migration and automatic-policy regression laws.

## 0.8.2 — 2026-10-19

- Request the high-performance GPU at startup on NVIDIA Optimus and AMD
  PowerXpress hybrid systems. Explicit Windows, driver and device power policies
  still take precedence; no system preferences are changed.
- Verify that both GPU preference exports survive linking into the distributed
  executable, so the setting also applies outside the development environment.

## 0.8.1 — 2026-10-18

- Include Afternoon lake and Woodland pond as optional static working
  backgrounds, bringing the bundled image library to six. Keep existing image
  choices and brightness settings, and retain procedural scenes for rest.
- Preserve the two original generated outputs, prompts and provenance; include
  all six wallpapers in native rendering captures and package checks.

## 0.8.0 — 2026-10-18

- Integrate one continuous journey: an observation railcar, safe stops and boarding,
  free walking and automatic strolling through six generated habitats.
- Save the world seed and current place between breaks and application launches.
  Choose a new landscape from the rest gallery; keep the eleven quiet scenes.
- Load journey meshes lazily in bounded steps while music continues. Cap nearby
  and distant chunk caches, and release them when choosing a quiet scene.
- Keep Esc/R, focus-loss return and music/settings isolation; add a visible return
  button, a short fading control guide, mouse look and keyboard-only look controls.
- Add seeded-field, bookmark and movement laws; integrated native acceptance,
  long-distance streaming/regeneration checks, and EN/JA journey captures.
- Redesign the official page around the railcar film, with Japanese/English pages,
  responsive layouts and explicit video playback.

## 0.7.0 — 2026-10-16

- Add three locally generated 3D rest landscapes: Birch lake, Alpine lake and
  Meadow stream. Terrain, plants, stones, materials and motion come from code.
- Group the gallery into Landscapes and Light & sky, retaining the eight earlier
  scenes, large preview, direct settings and exit-only rest.
- Add hierarchical vegetation wind, reflected scenery, filtered water detail,
  terrain shadows and tone mapping. Use distance-appropriate conifer foliage,
  irregular ridges and patchy shore vegetation.
- Warm bounded scene geometry before music starts; reuse it on selection. Share
  render targets, cap landscape raster size, reuse frozen frames and target
  30 FPS in rest/previews. Skip invisible drawing while continuing audio.
- Add actual landscape captures, audio/selection checks, startup/switch timing,
  cache invariants and a synchronized same-scene quality/cost comparison.
- Document procedural generation research, resource ownership and measured limits.

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
