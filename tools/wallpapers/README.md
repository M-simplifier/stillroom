# Static working landscapes

These four bundled PNGs are rendered from the compositions and seeds in
`render_wallpapers.py`. No photographs, generated bitmap inputs or network
resources are used. The app loads the finished image; it does not render the
landscape on startup or animate it while working.

From this directory, install `requirements.txt`, then run:

```sh
python render_wallpapers.py --width 2560 --output rendered
```

The output includes a contact sheet and a manifest with parameters, dependency
versions and hashes. Copy the four PNGs into `assets/backgrounds`, removing
the numeric prefixes: `first-light.png`, `open-water.png`, `honey-mist.png`
and `blue-distance.png`. Use the pinned dependencies for byte reproduction.
The source and these four images use the repository's MIT license.
