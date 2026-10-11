# Stillroom

A small Windows listening room, written in Haskell. Open it and music starts;
leave it alone and the controls disappear into a quiet, static screen.

[Download for Windows](https://github.com/M-simplifier/stillroom/releases/latest)
· [Website](https://m-simplifier.github.io/stillroom/)
· [Guide](docs/guide.md) · [Architecture](docs/architecture.md)

![Stillroom in compact mode](docs/player.png)

- Four original procedural presets, synthesized locally. No account or downloads.
- Or choose a folder of your own MP3, WAV or OGG files.
- Shuffle without immediate repeats when at least two playable tracks exist.
- Continuous playback with equal-power crossfades limited by both track lengths.
- Static green, slate, black or illustrated lake backgrounds. No visual animation.
- Four quiet image backgrounds, or import your own PNG, JPEG or BMP. Choose
  an image and its brightness; neither track changes nor time switch it for you.
- A native app icon for Explorer, shortcuts and the taskbar.
- Choose a display; use fullscreen or a small window. Settings persist.
- English and Japanese controls. No network requests, analytics or updater.

Extract the ZIP and open **stillroom.exe**. No Haskell toolchain or .NET installation
is needed to use the Windows binary. The executable is unsigned.

## A functional core

`step :: Event -> Session -> (Session, [Effect])` describes state changes without
performing them. Validated volume/time values, seeded shuffle, crossfade curves,
and sample-based composition are ordinary pure functions. A small imperative
boundary owns the window, two audio decks, native handles and preferences.
See the [architecture](docs/architecture.md) for the ownership rules.

## Build

On Windows x64, install GHC **9.6.7**, Cabal **3.12.1.0** and GHCup's MSYS2 tools:

```powershell
./tools/build.ps1
cabal run stillroom
```

The package index and dependencies are pinned. The first native dependency build
can take several minutes. Run `./tools/package.ps1` to package the already-built
executable with its font, replaceable GMP DLL, corresponding library source and
notices. See [GMP distribution](docs/gmp.md). Native checks need a real graphics/audio device:

```powershell
./tools/verify-native.ps1 -Executable (cabal list-bin exe:stillroom)
```

## Scope and license

This is an early **Windows-only** utility. It makes no clinical or measured
productivity claims. See [status and verification limits](STATUS.md).

Code, original synthesis and original scenery are **MIT** licensed. Dependencies
retain their licenses; the font is **SIL OFL 1.1**. No Suno recordings are
distributed. User music retains its original rights and is never uploaded.
See [music licensing](docs/music-licensing.md), [asset provenance](docs/assets.md),
and [third-party notices](licenses/README.md).

Ideas and bug reports are welcome through issues. Read
[CONTRIBUTING.md](CONTRIBUTING.md) before starting a substantial pull request.
