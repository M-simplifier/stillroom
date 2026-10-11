# Architecture

Stillroom separates decisions from their execution.

```
input / time -> Event -> step -> Session + [Effect]
                           |              |
                           v              v
                      static view     Windows / audio / JSON
```

## Pure modules

`Domain` keeps finite, bounded volume and nonnegative time behind smart
constructors. Backgrounds, transport, presets and transitions are algebraic data
types. A crossfade uses a cosine/sine pair with extra output headroom in the
runtime. Its overlap is at most six seconds and half the length of either track,
so a short incoming track cannot finish before the handover.

`Session` reduces input and elapsed time to the next state and explicit effects.
It knows nothing about raylib, Windows or files. Inactivity controls visibility;
there is no countdown, feed or animated focus object.

`Shuffle` uses a seeded generator and a nonempty catalogue. A normal cycle is a
permutation, and cycle boundaries avoid immediate repeats. Excluding a failed
candidate starts a new cycle and preserves the last accepted item. Removing the
last candidate produces `Nothing`, not a retry loop.

`Synthesis` maps a preset and an integer sample index to stereo samples using
oscillators, envelopes, deterministic noise and chord/beat schedules. Adjacent
blocks are equivalent to their concatenation. No external samples, synthesizer
service or music model is involved. A storable vector provides bounded PCM
buffers without making composition depend on wall time.

## Effect boundary

`Runtime.Audio` owns two decks and the audio device through `bracket`. Replacement
decks are acquired before old ones are released. Decoder input bytes remain alive
until the decoder closes; reading through Haskell also supports Unicode paths.
Synchronous decoding failures remove candidates; asynchronous cancellation
propagates. Native EOF detection is separate from position, since raylib resets
its position at the end of a recording.

`Desktop` interprets effects at 30 Hz. `Render` draws static geometry and shares
button bounds with hit testing. `Storage` validates JSON and replaces settings
through a temporary file. `Native` is a small Win32 FFI for monitors, a folder
dialog, sleep requests and a single-instance signal. The folder dialog runs on a
bound worker thread while the audio loop continues.

h-raylib is pinned to 5.6.0.0. The font loader detects raylib's borrowed fallback
before transferring glyph ownership. WAV cleanup also frees the context omitted
by that bundled raylib destructor. Recheck both boundaries before dependency
updates. Only the enabled MP3, WAV and OGG formats are exposed.

## Verification

Properties exercise shuffle conservation and boundaries, rejected-candidate
progress, volume validation, pause/resume, visibility, synthesis chunk independence,
finite signal bounds and crossfades with short tracks.

The native harness runs the real loop in a hidden, muted window. It checks audio
clock progression, pause/resume, crossfade state, automatic transition, control
visibility, compact mode and successful process shutdown, saving screenshots and
JSON. This verifies native control and lifecycle, not perceived audio quality or
clinical effectiveness.
