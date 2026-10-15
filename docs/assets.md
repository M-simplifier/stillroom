# Asset provenance

- **Music:** original sample-free algorithms in `Stillroom.Synthesis`, MIT.
  No bundled recordings or third-party samples.
- **Scenery:** original static geometry in `Runtime.Render`, MIT.
- **Wallpapers:** four assets created with the built-in OpenAI image generation
  tool on 2026-10-11. No reference images or third-party source material were
  supplied. Prompts are in [background-prompts.md](background-prompts.md).
  Included under this project's MIT grant to the extent applicable.
- **Icon:** original window-and-horizon geometry, MIT. SVG source and PNG/ICO
  outputs are in `assets/icon/`; regenerate with `tools/make-icon.ps1`.
- **Procedural rest scenes:** original GLSL in `assets/rest/procedural.fs`, MIT.
  Eight scenes use heightfields, analytic waves, volume integration, warped noise,
  smooth fields and deterministic stars. No image/video/noise textures or external
  shader source files are sampled. Output render targets and thumbnails are
  generated locally from the same shader. [Research notes](shader-research.md)
  distinguish references from the independent implementation; no Seascape code is
  bundled.
- **Font:** unmodified Noto Sans CJK JP Regular, renamed on disk to
  `NotoSansJP.otf`. SIL Open Font License 1.1, separate from MIT.
  [Upstream file](https://github.com/notofonts/noto-cjk/blob/f8d157532fbfaeda587e826d4cd5b21a49186f7c/Sans/OTF/Japanese/NotoSansCJKjp-Regular.otf)
  and [license](https://github.com/notofonts/noto-cjk/blob/f8d157532fbfaeda587e826d4cd5b21a49186f7c/Sans/LICENSE).
  SHA-256: `68a3fc98800b2a27b371f2fb79991daf3633bd89309d4ffaa6946fd587f375b5`.
- **Screenshots:** captured with original presets; no user titles or private paths.

The release ZIP retains `LICENSE`, `licenses/`, and `assets/fonts/OFL.txt`.
