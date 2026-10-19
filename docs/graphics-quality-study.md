# Graphics quality study — local prototype

2026-10-19. This branch is an owner-review prototype based on 0.8.2, not a
released quality-settings feature. Automatic selection and persisted settings
are deliberately awaiting the visual experiment's outcome.

`stillroom --quality-preview` opens a disposable journey at the same lakeside.
It uses the released Haskell journey model and native renderer, without opening
an audio device or reading/writing personal preferences or journey bookmarks.

- **1 / 2 / 3:** released-style / detailed / supersampled rendering, preserving
  the current camera, clock and generated geometry.
- **P:** freeze the camera and scene clock for comparison; press again to resume.
- **Space / E / WASD / click:** existing ride, walk and look controls.
- **R:** return to the comparison's initial lake. **H:** reveal instructions.
- **F:** maximize/restore the desktop window. **Esc:** close this prototype.

Detailed shading adds integrated height haze, a coherent clear-sky light model,
cloud shading, filtered planar reflection, depth-dependent water absorption,
filtered procedural surface detail, a wider shadow filter and roughness-based
surface lighting. This is an art-directed approximation, not a complete
atmosphere solver, temporal antialiasing or hardware ray tracing implementation.

Standard retains the previous shader branches and 1600×900 cap. Detailed uses
native raster up to 2560×1440, three-quarter-size reflection and a 3072 shadow.
Supersampled uses 1.5× raster up to 2880×1620, full-size reflection and a 4096
shadow. Resolution caps retain aspect ratio. Quality affects no collision,
navigation, rail route or world seed. These budgets are experimental.

`--quality-capture OUTPUT` captures three fixed viewpoints in all three levels,
checks that the journey values and cached geometry fingerprints are unchanged,
then exercises resize and return to Standard. Per-frame synchronized render
times are diagnostic, not isolated GPU times or moving-world FPS guarantees.

`--quality-film standard` and `--quality-film supersampled` stream 480 RGBA
frames of the same 16-second journey to stdout (1280×720, 30 fps). This is an
offline render at fixed semantic timestamps. Both swap buffers are populated
before capture. Keep stderr separate and pipe stdout to a video encoder.

Local captures, videos, comparison viewer, build receipts and a separate preview
copy are under `scratch/graphics-quality-prototype-2026-10-19/`. The source
passes the existing 42 law/regression groups. Final native checks and exact
artifact identity are recorded there. Perceived quality and input feel remain
for the owner's trial; production quality selection follows that feedback.
