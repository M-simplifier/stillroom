# Development and verification

Use PowerShell on Windows x64 with GHC 9.6.7, Cabal 3.12.1.0 and GHCup's MSYS2
tools installed. The script discovers the pinned versions in GHCup locations
or PATH; `-Ghc` and `-Cabal` accept explicit executable paths. It does not install
tools. Run from the repository root:

```powershell
./tools/dev.ps1 Build -Online
./tools/dev.ps1 Capture
```

`-Online` updates the pinned package index and allows missing dependencies to be
downloaded. Omit it after the first successful build. Every development action
performs an incremental build of the current source, checks package metadata and
GMP linkage, and supplies the source assets. It restores PATH and
`stillroom_datadir` on success and failure, including an initially absent override.
No action reads or writes your normal Stillroom settings.

| Action | Purpose and evidence |
| --- | --- |
| `Build` | Build, metadata/linkage checks and the complete law/regression suite. |
| `Capture` | 18 deterministic UI/landscape PNGs through the production renderer, with no audio device or native acceptance sequence. |
| `Check` | The real native loop: 49 assertions for state, input/effects, audio, persistence, shader resources, font replacement/restart and shutdown. Startup/switch timing and peak process working set are recorded. |
| `Benchmark` | 18 interface/procedural samples plus 36 synchronized landscape samples and four cache contracts, with RTS allocation statistics; no audio. |
| `CheckPackage` | Check an extracted executable with only its own assets/DLL and a restricted Windows PATH. No build or development toolchain. |

Each run gets a unique directory under `scratch/dev`. To choose a location, pass
`-OutputDirectory` with a fresh directory. An existing summary is never overwritten.
The one-line console result identifies the repository commit, dirty status,
executable SHA-256, counts, failures and report path. The JSON report records stage
durations and full log locations; native failures also preserve their assertion
names. `CheckPackage`'s commit describes the calling checkout, while the executable
hash identifies the package actually tested. A dirty commit is not a claim that
the binary was built from that exact commit.

## Inspecting the interface

`Capture` writes `capture-ui/ui-capture.json` and PNGs for the player and all
three settings pages in English and Japanese at 700×500, plus the Japanese rest
gallery at 1280×800. It adds all three landscapes at 1280×720 and 1920×1080,
and the landscape gallery in both languages at minimum size plus desktop size.
It uses a fixed scene time, bundled forest background and
original preset title. It requires a real graphics context and fails if the
procedural shader or thumbnails cannot load. It opens no audio device, picker
or personal library. Inspect the PNGs for layout, clipping and legibility;
producing the files alone does not certify a good interface.

The executable also accepts `--capture-ui OUTPUT` directly. For a source build,
prefer the wrapper so that it cannot accidentally inspect a stale binary or
inherit missing dependency paths. Screenshots are diagnostic artifacts, not
input tests or performance measurements.

## Choose checks for the changed boundary

- For visual iteration, use `Capture` while adjusting the layout. Before merging
  changes to controls or layout, run `Build` for the pure interface/hit-target
  properties. Exercise `Check` when input routing or native behavior changes.
- Changes to state, persistence, asynchronous work, audio, native ownership or
  FFI lifetimes still require the law suite and native checks. Use `Check
  -MusicFolder C:/path/to/music` as well when folder playback is affected.
- Renderer optimizations need a controlled before/after benchmark with the same
  workload, plus captures and native checks for the affected boundary. See
  [measurement limits](performance.md); a fast capture is not a benchmark.
- Packaging, loaders, dependency paths or asset changes require validation of
  the actual extracted package. Development checks cannot replace it.

## Check an actual package

After building, run `tools/package.ps1` in a shell where the pinned GHC is on
PATH. Choose an empty packaging destination, extract the generated ZIP to a
fresh directory, and test that extracted executable:

```powershell
./tools/dev.ps1 CheckPackage -Executable C:/path/to/extracted/stillroom.exe
./tools/dev.ps1 CheckPackage -Executable C:/path/to/extracted/stillroom.exe -MusicFolder C:/path/to/music
```

`CheckPackage` clears the development data override, restricts PATH to Windows
system directories and checks the font, wallpapers, shader, icon and GMP DLL
beside the executable before launching. The caller's environment is restored
afterward. `tools/verify-native.ps1` enforces the same package boundary by default;
only the development `Check` action opts into `-DevelopmentAssetRoot`.

For shader work, run `tools/verify-rest-frames.py` on a native check directory
with Pillow and NumPy installed. It compares moving/still frames for all eleven
scenes and the frozen scene over different wallpapers. Native checks still need
an available audio device and graphics driver; they do not certify perceived
audio quality or every device configuration.

`tools/test-dev.ps1` exercises failure propagation, environment restoration,
missing-package rejection despite a source data override, and evidence retention
without starting a graphics/audio process. CI runs these boundary checks as well
as the full build/law suite and packaging. Desktop captures and native runtime
acceptance remain local checks on a machine with real devices.
