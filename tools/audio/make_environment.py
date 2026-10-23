"""Offline, seeded environment bank maker. Code and generated sounds are MIT.

Only the approved bank is generated; there is no route, video, preview gallery,
encoder, network access or application dependency.
"""
import argparse
import json
from pathlib import Path

import numpy as np
import soundfile as sf

from audio_common import dependencies, fresh_output, sha256_file, write_manifest

SR = 44100
RNG_SEED = 20261024

def amp(value):
    return 10 ** (value / 20)


def rms(a):
    return float(np.sqrt(np.mean(np.asarray(a, dtype=np.float64) ** 2)))


def scale_rms(a, level):
    return a * (amp(level) / rms(a))


def ease(a):
    a = np.clip(a, 0, 1)
    return a * a * (3 - 2 * a)


def spectral_texture(seconds, seed, low, high, tilt, shoulders=()):
    """One exactly periodic coloured stochastic carrier, not white-noise output."""
    n = int(round(seconds * SR))
    f = np.fft.rfftfreq(n, 1 / SR)
    safe = np.maximum(f, 1)
    # Deliberately remove sub-bass and the brittle high end. Broad shoulders
    # have no sustained oscillator / narrowly resonant bird or engine tone.
    curve = 1 / np.sqrt(1 + (low / safe) ** 8)
    curve *= 1 / np.sqrt(1 + (safe / high) ** 10)
    curve *= (safe / 600) ** (-tilt)
    for centre, width_octaves, strength in shoulders:
        curve *= 1 + strength * np.exp(-0.5 * (np.log2(safe / centre) / width_octaves) ** 2)
    curve[0] = 0
    rng = np.random.default_rng(seed)
    spectrum = (rng.normal(size=f.size) + 1j * rng.normal(size=f.size)) * curve
    spectrum[-1] = spectrum[-1].real
    a = np.fft.irfft(spectrum, n=n)
    return a / rms(a)


def periodic_envelope(n, seed, harmonics, depth):
    phase = np.arange(n, dtype=np.float64) * (2 * np.pi / n)
    rng = np.random.default_rng(seed)
    a = np.zeros(n)
    for h, weight in harmonics:
        a += weight * np.sin(phase * h + rng.uniform(0, 2 * np.pi))
    a /= max(float(np.max(np.abs(a))), 1)
    return 1 + depth * a


def loop(seconds, seed, low, high, tilt, level, width, modulation, shoulders=()):
    common = spectral_texture(seconds, seed, low, high, tilt, shoulders)
    left = spectral_texture(seconds, seed + 1, low, high, tilt, shoulders)
    right = spectral_texture(seconds, seed + 2, low, high, tilt, shoulders)
    a = np.column_stack((common + width * left, common + width * right))
    a *= periodic_envelope(len(a), seed + 3, modulation, 0.28)[:, None]
    return scale_rms(a, level).astype(np.float32)


def body_shot(seed, seconds=0.57):
    """A brush-like, felt-damped wooden flex, with no impulse attack."""
    a = loop(seconds, seed, 155, 1080, 0.58, -43, 0.12, [(1, 0.1)],
             [(260, 0.28, 0.8), (480, 0.25, 0.4)])
    t = np.arange(len(a)) / SR
    env = ease(t / 0.055) * np.exp(-t / 0.15) * ease((seconds - t) / 0.10)
    a *= env[:, None]
    a[0] = a[-1] = 0
    return scale_rms(a, -39).astype(np.float32)


def water_loop():
    """Broad little washes with a delayed upper texture; no pitched droplets."""
    seconds = 10.19
    lower = loop(seconds, 307, 240, 1450, 0.50, -37, 0.45, [(1, .15)])
    upper = loop(seconds, 317, 680, 2950, 0.20, -40, 0.55, [(1, .1)])
    t = np.arange(len(lower)) / SR
    low_env = np.full(len(t), .17)
    upper_env = np.full(len(t), .10)
    for start, duration, strength in [(.3, 1.65, .85), (2.7, 2.15, 1.0),
                                      (5.2, 1.8, .70), (8.1, 2.10, .90)]:
        local = (t - start) % seconds
        # Smooth half-wave bell: rise, broad crest, recede, and silence between
        # some laps. The upper wash comes later than the water-body movement.
        u = np.clip(local / duration, 0, 1)
        low_env += strength * np.sin(np.pi * u) ** 2
        v = np.clip((local - .17) / (duration * .74), 0, 1)
        upper_env += strength * np.sin(np.pi * v) ** 2
    return scale_rms(lower * low_env[:, None] + upper * upper_env[:, None], -37).astype(np.float32)


def build_bank():
    # Incommensurate lengths prevent all three environmental layers from
    # returning to the same configuration after one obvious short cycle.
    return {
        "wind": loop(11.31, 101, 135, 1450, 0.64, -34.5, 0.42,
                     [(1, 0.72), (2, 0.24), (5, 0.09)], [(390, 1.0, 0.4)]),
        "leaves": loop(13.73, 211, 480, 3600, 0.35, -40.0, 0.65,
                       [(1, 0.4), (3, 0.5), (7, 0.2), (19, 0.13), (47, 0.06)]),
        "water": water_loop(),
        "wheel": loop(7.91, 409, 125, 1550, 0.72, -34.0, 0.16,
                      [(1, 0.28), (3, 0.25), (17, 0.10), (31, 0.04)],
                      [(285, 0.68, 0.85), (730, 0.42, 0.20)]),
        "body-1": body_shot(503),
        "body-2": body_shot(509, 0.64),
        "body-3": body_shot(521, 0.52),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()
    bank = build_bank()
    out = fresh_output(args.out, bank)
    rows = []
    for key, samples in bank.items():
        if not np.isfinite(samples).all() or np.abs(samples).max() >= .99:
            raise ValueError("Invalid environmental sample: " + key)
        target = out / (key + ".wav")
        sf.write(target, samples, SR, subtype="PCM_16")
        rows.append({"key": key, "frames": len(samples),
                     "loop": not key.startswith("body-"),
                     "peak": float(np.abs(samples).max()),
                     "sha256": sha256_file(target), "license": "MIT"})
    write_manifest(out / "manifest.json", {
        "sample_rate": SR, "channels": 2, "format": "WAV PCM_16",
        "generator": "tools/audio/make_environment.py",
        "generator_sha256": sha256_file(Path(__file__)),
        "helpers_sha256": sha256_file(Path(__file__).with_name("audio_common.py")),
        "dependencies": dependencies(), "seed": RNG_SEED,
        "source": "Seeded original synthesis; no borrowed recordings or samples.",
        "samples": rows,
    })
    print(json.dumps({"samples": len(rows),
                      "bytes": sum((out / (row["key"] + ".wav")).stat().st_size for row in rows)}))


if __name__ == "__main__":
    main()
