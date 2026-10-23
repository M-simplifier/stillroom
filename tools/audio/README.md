# Rebuild the bundled audio banks

These offline tools recreate Stillroom's 62 music one-shots and seven environmental
samples. Musical composition stays in Haskell. The application plays the bundled
WAVs and needs neither Python nor network access.

The tested environment is CPython 3.12.14 on Windows x86-64, with the versions in
`requirements.txt` and libsndfile 1.2.2. Use a Python 3.12 virtual environment:

```sh
python -m pip install -r tools/audio/requirements.txt
```

Fetch the nine pinned Salamander Grand Piano v3 source candidates explicitly, or
use an existing directory containing those exact files. The helper checks their
SHA-256 values and includes the upstream license. It does not download the whole
instrument library.

```sh
python tools/audio/fetch_piano_samples.py --out rebuild/piano
python tools/audio/make_instruments.py --piano-samples rebuild/piano --out rebuild/music
python tools/audio/make_environment.py --out rebuild/environment
python tools/audio/verify_banks.py --music rebuild/music --environment rebuild/environment
```

Choose fresh output directories: the generators reject existing bank files.
`--keys` defaults to `tools/audio/music-keys.txt`, exported from
`Stillroom.Audio.Composition.requiredKeys`. A custom key list may be supplied with
`--keys`; the verifier intentionally checks the complete shipped banks.

`piano-sources.json` pins the upstream commit, license and source-file hashes.
The fixed nine-file candidate pool preserves the audition's nearest-note choices;
unrelated files in the input directory are ignored. The shipped nine felt notes
use six of those source files. Source SHA-256 values are validated before rendering.
The environmental bank uses original seeded synthesis and needs no recordings.

Each generated manifest records sample hashes, frame counts, source attribution,
generator/helper hashes and numerical-library versions. Regeneration on the tested
environment was byte-identical for all 69 shipped WAVs. FFT, resampling, floating
point and WAV-library changes can affect bytes on other environments; run the hash
verifier instead of assuming cross-platform identity from a seed alone.

See [audio attribution and licenses](../../licenses/builtin-audio.md). The tools,
generated non-piano sounds and Stillroom code are MIT; modified Salamander piano
samples retain their separately stated CC BY 3.0 license.
