# Asset provenance

- **Music:** original sample-free algorithms in `Stillroom.Synthesis`, MIT.
  No bundled recordings or third-party samples.
- **Scenery:** original static geometry in `Runtime.Render`, MIT.
- **Wallpapers:** six assets created with the built-in OpenAI image generation
  tool: four on 2026-10-11, plus Afternoon lake and Woodland pond on 2026-10-15.
  No reference images or third-party source material were supplied. The latter
  two were originally made for a rest-mode experiment and are now included,
  unchanged, as optional static working backgrounds. Prompts and output hashes
  are in [background-prompts.md](background-prompts.md).
  Included under this project's MIT grant to the extent applicable. The
  [OpenAI Terms of Use](https://openai.com/policies/terms-of-use/), checked on
  2026-10-18, assign OpenAI's rights in output, if any, to the user to the extent
  permitted by law. This does not establish copyrightability, exclusivity or
  clearance of third-party rights. These generated images are not photographs.
- **Icon:** original window-and-horizon geometry, MIT. SVG source and PNG/ICO
  outputs are in `assets/icon/`; regenerate with `tools/make-icon.ps1`.
- **Procedural rest scenes:** original GLSL in `assets/rest/procedural.fs`, MIT.
  Eight scenes use heightfields, analytic waves, volume integration, warped noise,
  smooth fields and deterministic stars. No image/video/noise textures or external
  shader source files are sampled. Output render targets and thumbnails are
  generated locally from the same shader. [Research notes](shader-research.md)
  distinguish references from the independent implementation; no Seascape code is
  bundled.
- **Procedural landscapes:** original generation in `cbits/nature.c` and shaders
  in `assets/rest/nature/`, MIT. Three scenes generate terrain, vegetation, stones,
  height/bone data and reflections; no landscape image, authored model, video or
  noise texture is distributed. [Landscape research](procedural-landscapes.md)
  distinguishes references from implemented techniques. The unmodified raylib,
  raymath and rlgl headers in `cbits/include/` come from pinned h-raylib 5.6.0.0;
  their zlib notices remain in the headers and `licenses/raylib-native.txt`.
- **Journey:** original Haskell reducers, terrain/rail/plant generation in
  `cbits/journey/` and shaders in `assets/rest/journey/`, MIT. No model, terrain
  image or video is sampled by the live journey. `poster.png` is a 320×180 native
  capture used only by the gallery card. The official site's 16-second silent
  film uses the production Ultra renderer, captured at a fixed clock and encoded
  offline. It contains no third-party recordings or personal data; it is a visual
  preview, not a measurement of real-time rendering performance.
- **Font:** unmodified Noto Sans CJK JP Regular, renamed on disk to
  `NotoSansJP.otf`. SIL Open Font License 1.1, separate from MIT.
  [Upstream file](https://github.com/notofonts/noto-cjk/blob/f8d157532fbfaeda587e826d4cd5b21a49186f7c/Sans/OTF/Japanese/NotoSansCJKjp-Regular.otf)
  and [license](https://github.com/notofonts/noto-cjk/blob/f8d157532fbfaeda587e826d4cd5b21a49186f7c/Sans/LICENSE).
  SHA-256: `68a3fc98800b2a27b371f2fb79991daf3633bd89309d4ffaa6946fd587f375b5`.
- **Screenshots:** captured with original presets; no user titles or private paths.

The release ZIP retains `LICENSE`, `licenses/`, and `assets/fonts/OFL.txt`.
