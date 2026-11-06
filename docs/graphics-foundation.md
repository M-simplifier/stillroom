# Functional graphics foundation

The graphics foundation introduced in Stillroom 0.10 keeps the original railcar and scenery, while making the
relationship between geometry, its observations, and GPU execution explicit.
The work targets a stable moving view: changing the rendering pass must not
accidentally change the terrain it projects.

[Illustrated article](https://m-simplifier.github.io/stillroom/graphics-foundation-en.html)
· [日本語](https://m-simplifier.github.io/stillroom/graphics-foundation.html)
· [Developer notes](https://m-simplifier.github.io/stillroom/developers.html)

## Read the code in this order

| Module | Responsibility |
| --- | --- |
| [`Graphics.Space`](../src/Stillroom/Graphics/Space.hs) | Points, directions and homogeneous maps with nominal coordinate-space roles. Compose only matching spaces. |
| [`Graphics.BoxUnion`](../src/Stillroom/Graphics/BoxUnion.hs) | Exact axis-aligned box boundaries and triangulation, independent of the material interpretation. |
| [`Graphics.Mesh`](../src/Stillroom/Graphics/Mesh.hs) | Three-vertex triangles, parameterized by coordinates and appearance. |
| [`Graphics.Observation`](../src/Stillroom/Graphics/Observation.hs) | An image target description together with its writing/sampling projection. No fixed pass enumeration. |
| [`Journey.Railcar`](../src/Stillroom/Journey/Railcar.hs) | The original dimensions, part ordering, palette and wheel design. |
| [`Journey.Frame`](../src/Stillroom/Journey/Frame.hs) | Stillroom's shared geometry context and passenger, light and reflection observations. |
| [`Landscape.Field`](../src/Stillroom/Landscape/Field.hs) | Explicit-seed analytical terrain, shores, habitats, railway and collision. |
| [`Landscape.Geometry`](../src/Stillroom/Landscape/Geometry.hs) | Pure bounded construction of fixed scenes and seeded near/far meshes, branch data and height samples. |
| [`Runtime.Graphics.Journey`](../runtime/Stillroom/Runtime/Graphics/Journey.hs) | Retained chunk resources and direct execution of the validated frame and its observations. |
| [`Runtime.Graphics.Resource`](../runtime/Stillroom/Runtime/Graphics/Resource.hs) | Context-thread ownership, uploads, shader values and typed GPU handles. |
| [`Runtime.Graphics.Target`](../runtime/Stillroom/Runtime/Graphics/Target.hs) | Target selection, allocation fallback and replacement. |
| [`Runtime.Journey.Packet`](../runtime/Stillroom/Runtime/Journey/Packet.hs) | Validated finite diagnostic encoding, outside production pass selection. |
| [`Graphics.Encoding.Mesh`](../src/Stillroom/Graphics/Encoding/Mesh.hs) | Validated native vertex encoding, retained and uploaded once per world. |

The reusable core imports no railcar, fixed three-pass schedule, or native frame
packet. `Journey.Frame` makes the product choices; `Runtime.Journey` owns the
effect boundary. Main, reflection and shadow use one geometry-selection
reference and clock. A future unrelated camera can belong to another geometry
observation group rather than being forced to use the passenger's LOD policy.

## Assemble before extracting a boundary

`BoxUnion.boundary` removes covered interior surfaces and gives coincident
exterior material to the later part. Coordinates remain Rational until mesh
lowering. The authored 71-box railcar is separate from the algorithm.

```haskell
-- A different material vocabulary requires no change to the geometry kernel.
touchingParts = do
  a <- cuboid (0, 0, 0) (1, 1, 1) "cedar"
  b <- cuboid (1, 0, 0) (2, 1, 1) "iron"
  pure (boundary [a, b])
```

Extracting both boundaries first and concatenating them would retain the two
internal contact faces. Assembly and boundary extraction therefore have
different types and different composition laws. Part concatenation is
associative; reordering can change the chosen exterior material.

## One projection reaches both producer and consumer

An `Observation target` stores its view, projection, combined clip map and
target description. Queries are ordinary functions rather than exported record
selectors, so clients cannot replace the combined map independently through
record update. `FramePlan` and its geometry context have the same boundary.

At execution, each typed `Observation` combines the selected GPU image's target
description with its map for the frame. The shadow pass writes with the same lowered
matrix used to sample shadow depth. The reflection pass and water sampling
likewise use one pair. Resource allocation and destruction remain in the native
Haskell runtime; the pure values do not own GPU handles.

Transforms are computed in Double. Object translations become relative to a
nearby origin before conversion to Float. The light grid is anchored in the
light's basis, rather than rounded along world X/Z. `Space.matrixElements`
exposes four total columns; `Runtime.Graphics.Encoding` lowers those to raylib's
named matrix fields. The renderer consumes `FramePlan` directly. The 207-float
versioned packet remains available for diagnostics; target fallback is resolved
before planning.

## Visible stability

The terrain shader uses the shared shape-selection view, not the current light
or reflection camera. A synthetic GPU witness using the production shader found
a 3 m main/shadow height disagreement in the old path and zero after separating
those roles. That is a bounded vertex experiment, not a claim about every
rendered scene.

Shadows compare depth at each sampled texel centre on the receiver plane, then
filter visibility. The footprint has no blocker-count-dependent radius jump.
The gradient calculation tests relative Jacobian degeneracy, so a close-up
surface is not rejected merely because its UV derivatives are small. Sky axes
come directly from the view matrix rather than a lossy eye/target round trip.
Fine railcar grain is filtered using its pixel footprint.

## Verification and limits

The Haskell migration retains frozen 0.11.0 field and geometry
fixtures. `tools/check-haskell-migration.ps1` compares 17,137 field samples and
all buffers from three fixed scenes and 24 seeded near/far chunks. Twenty-six
geometries are byte-identical. Alpine terrain alone has a documented one-ULP
`atan2` difference: height error stays below 0.00001 m and normal components
below 0.00004; topology, colors, branch data and vegetation remain exact.
The fixture provenance records the exact differing input and output bits.

The same tool accepts ordinary public queries and rejects attempts to replace
geometry dimensions or mesh position arrays through record update.
`cabal test migration-native -fnative-graphics-tests` compares water with actual pinned raylib, draws
the indexed upload, repeats acquisition/release and checks recovery after a
shader failure. Product captures and actual framebuffer/uniform readback remain
necessary; internal path equality alone does not establish image preservation.
The native suite is disabled by default, so ordinary `cabal test` does not
require a GPU context.

The following counts and measurements describe the bounded **2026-10-20**
foundation acceptance study. They are retained historical evidence, not new
migration performance claims.

- 55 property/regression groups include exact rectangle subtraction, analytic
  union volume, surface ownership, mesh winding, projections, relative-origin
  behavior and reflection-resolution independence.
- Compiler acceptance includes a positive client and four rejected clients:
  replacing part of a validated plan, replacing an observation's clip map,
  coercing distinct coordinate spaces, and composing mismatched spaces.
  `tools/build.ps1` runs these checks against the actual public modules.
- Independent acceptance examined the actual Haskell output: 1,798 triangles,
  zero positive-area coplanar overlap, zero degeneration and zero
  winding/normal disagreement. A separate occupancy oracle matched all 44,248
  exterior subdivisions of the 71 boxes.
- The independent geometry suite used 1,499 rectangle cases and 859 box cases,
  including negative/fractional coordinates, contacts, containment and material
  priority. Actual Haskell-to-C vertex transfer matched 215,760 bytes.
- The frame audit executed 5,670 inputs. After module separation, all 5,654
  accepted packets retained identical numerical values; nonzero Float bits
  matched. Some zero signs changed with compilation, so the whole frame byte
  sequence is not claimed identical. Double clip results also matched.
- Both reported boundary defects were replayed against corrected source.
  The extracted C sky code preserved orthonormal axes for 16,962 observations.
  The corrected shader gradient fragment recovered the close-plane gradient
  with maximum component error below 3e-8 in CPU Float arithmetic.
- A transform-feedback audit used the frozen foundation, the actual 5,394
  car vertices, and the production vertex shader on an RTX 3050 / OpenGL 3.3.
  Across 67 synthetic frames and three passes, shadow-pass clip coordinates
  and main-pass shadow coordinates were bit-identical. Relative to the
  Double reference, maximum main-view error was 0.02410 px at 1600×900;
  eight origin-crossing sequences had a maximum residual change of 0.00924 px.
  Pixel comparisons use the audit's in-frustum, `w >= 0.5` selection; they are
  projected vertex errors, not image-pixel readback. Both deliberately wrong
  matrix and wrong object-kind controls were detected.
- The same GPU audit's eight static-world control points lost up to 0.03 world
  units before origin subtraction, with up to 3.97 projected pixels of error.
  Passing a numerical rounding bound does not make that loss acceptable for
  every application. The audit covered the frozen vertex path; its vertex
  shader remains byte-identical in production. It did not test fragment
  visibility, frame rate, power, every world position, or other drivers.
- The production build passed 56 native application assertions from an
  extracted ZIP with a restricted Windows PATH, 28 UI captures, and an
  accelerated 1,800-frame / 600-semantic-second journey across all six habitats.
  Three journey scenes at all four manual tiers retained the exact native
  packet and zero uniform-readback difference. Graphics allocation fallback,
  frozen-scene caches and automatic-quality query progress also passed.

These are finite tests and scoped numerical observations, not a formal proof
of Haskell, GLSL, drivers, or visual quality. The original wheel cap/tread gaps
are retained; the whole vehicle is not claimed to be a watertight CSG union.
Static terrain and vegetation still enter their world-field path as Float
before origin subtraction. The car's relative-position guarantee must not be
generalized to every static mesh at arbitrarily distant coordinates.

The implementation is a reusable core exercised by Stillroom, not yet a
separately released general engine. A second real consumer should determine
which policies and representations deserve further generalization.

Run `tools/build.ps1`, `tools/dev.ps1 CheckPackage`, and the executable's
`--check-journey`, `--quality-capture`, and `--check-graphics` commands for the
maintained product checks. The independent audit counts above describe the
bounded 2026-10-20 acceptance study, not extra tests run by ordinary CI.

## Sources

- [Functional Images](https://conal.net/papers/functional-images/): give the
  generated image a meaning before choosing its implementation.
- [Halide](https://people.csail.mit.edu/jrk/halide12/): distinguish what is
  computed from its execution schedule; output-changing LOD remains a policy.
- [Microsoft shadow-map techniques](https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps):
  stable projections, depth comparisons and receiver-plane bias.
- [GPU Gems: shadow-map antialiasing](https://developer.nvidia.com/gpugems/gpugems/part-ii-lighting-and-shadows/chapter-11-shadow-map-antialiasing):
  filter the comparison results rather than raw depth values.
