"""Offline one-shot instrument maker; composition remains in Haskell.

The felt samples adapt Alexander Holm's CC-BY-3.0 Salamander Grand Piano v3.
Other instruments and the generator code are MIT. See licenses/builtin-audio.md.
No audio device, application startup, private path or network is involved here.
"""
from pathlib import Path
from fractions import Fraction
import argparse
import hashlib
import json
import re
import sys

import numpy as np
import soundfile as sf
from scipy import signal

from audio_common import dependencies, fresh_output, sha256_file, write_manifest

ROOT = Path(__file__).resolve().parent
SR = 44100

def smooth(x):
    x = np.clip(x, 0, 1)
    return x*x*(3-2*x)

def lowpass(y, hz, order=2):
    return signal.sosfilt(signal.butter(order, hz, fs=SR, output='sos'), y, axis=0)

def highpass(y, hz):
    return signal.sosfilt(signal.butter(2, hz, fs=SR, btype='high', output='sos'), y, axis=0)

def stereo(y):
    return np.column_stack([y, y]) if y.ndim == 1 else y[:, :2]

def room_ir():
    rng = np.random.default_rng(219183)
    t = np.arange(round(SR*2.8))/SR
    ir = rng.normal(size=(len(t), 2))
    # A dense, dark late field, preceded by sparse early wall reflections.
    ir = lowpass(ir, 3300)
    ir *= (np.exp(-3.5*t) * smooth((t-.035)/.08))[:, None]
    ir /= max(np.linalg.norm(ir[:, 0]), np.linalg.norm(ir[:, 1]))
    ir *= 0.52
    for delay, amplitude in [(.027,.29),(.043,.21),(.061,.15),(.083,.09)]:
        ir[round(delay*SR),0] += amplitude
        ir[round((delay+.007)*SR),1] += amplitude*.93
    return ir

IR = room_ir()

def reverberate(dry, wet):
    dry = stereo(dry)
    mono = dry.mean(axis=1)
    tail = np.column_stack([signal.fftconvolve(mono, IR[:,c]) for c in range(2)])
    tail *= wet
    tail[:len(dry)] += dry
    return tail

def felt(midi, piano_samples, piano_inputs):
    options = [(r['native_midi'], piano_samples / r['file']) for r in piano_inputs]
    native,path=min(options,key=lambda p:abs(p[0]-midi))
    y,sr=sf.read(path,dtype='float64',always_2d=True)
    energy=np.mean(y*y,axis=1)
    threshold=max(float(energy.max())*.0002,1e-9)
    first=np.flatnonzero(energy>threshold)
    trim=max(0,int(first[0])-int(sr*.009)) if len(first) else 0
    y=y[trim:trim+round(sr*7.2)]
    ratio=Fraction(SR/(sr*2**((midi-native)/12))).limit_denominator(1200)
    y=signal.resample_poly(y,ratio.numerator,ratio.denominator,axis=0)
    y=highpass(lowpass(y,2700),35)
    t=np.arange(len(y))/SR
    y*= (smooth(t/.014)*smooth((len(y)/SR-t)/1.4)*np.exp(-t*.08))[:,None]
    y *= .16/max(float(np.sqrt(np.mean(y[:SR]**2))),1e-7)
    y *= min(1,.46/max(float(np.abs(y).max()),1e-7))
    return reverberate(y,.24),dict(file=path.name,sha256=hashlib.sha256(path.read_bytes()).hexdigest(),native_midi=native,license='CC-BY-3.0',author='Alexander Holm')

def instrument(kind,midi,piano_samples,piano_inputs):
    if kind=='felt':
        return felt(midi,piano_samples,piano_inputs)
    f=440*2**((midi-69)/12)
    length={'keys':6.5,'wood':4.5,'pluck':4.5,'air':19.0,'glass':9.0,'bass':9.0,'kick':.7,'brush':.6}[kind]
    t=np.arange(round(SR*length))/SR
    rng=np.random.default_rng(7741+midi*23+sum(map(ord,kind)))
    if kind=='keys':
        body=np.sin(2*np.pi*f*t + .48*np.exp(-t*1.8)*np.sin(2*np.pi*2*f*t))
        tine=.11*np.sin(2*np.pi*3*f*t)*np.exp(-t*2.6)
        y=(body+tine)*np.exp(-t*.68)*smooth(t/.012)
        y=lowpass(y,3200)*.19
        wet=.20
    elif kind=='wood':
        y=(np.sin(2*np.pi*f*t)*np.exp(-t*1.5)+.17*np.sin(2*np.pi*f*3.997*t)*np.exp(-t*5.1)
           +.045*np.sin(2*np.pi*f*9.12*t)*np.exp(-t*10))
        y=lowpass(y*smooth(t/.009),2800)*.22
        wet=.27
    elif kind=='pluck':
        y=sum((1/k**1.8)*np.sin(2*np.pi*f*k*t+.05*k)*np.exp(-t*(1.1+.52*k)) for k in range(1,9) if f*k<9000)
        y=lowpass(y*smooth(t/.011),2700)*.22
        wet=.22
    elif kind=='air':
        envelope=smooth(t/2.6)*smooth((length-t)/5.5)
        drift=.45*np.sin(2*np.pi*.071*t)+.16*np.sin(2*np.pi*.113*t+.8)
        a=np.sin(2*np.pi*f*t+drift*.23)+.09*np.sin(2*np.pi*2*f*t+.3)
        b=np.sin(2*np.pi*f*1.0006*t+drift*.19)+.07*np.sin(2*np.pi*2.001*f*t+1.1)
        y=np.column_stack([a,b])*envelope[:,None]*.15
        wet=.31
    elif kind=='glass':
        y=sum(a*np.sin(2*np.pi*f*k*t)*np.exp(-t*d) for k,a,d in [(1,1,.7),(2.001,.14,1.0),(3.998,.035,1.8)])
        y*=smooth(t/.04)*.14
        wet=.46
    elif kind=='bass':
        y=(np.sin(2*np.pi*f*t)+.11*np.sin(2*np.pi*2*f*t)+.025*np.sin(2*np.pi*3*f*t))
        y*=smooth(t/.12)*np.exp(-t*.39)*smooth((length-t)/2)*.22
        wet=.07
    elif kind=='kick':
        phase=2*np.pi*(46*t + (18/27)*(1-np.exp(-27*t)))
        y=np.sin(phase)*np.exp(-t*12)*smooth(t/.009)*.30
        wet=.01
    else:
        y=highpass(lowpass(rng.normal(size=len(t)),2600),700)
        y*=np.exp(-t*23)*smooth(t/.012)*.12
        wet=.06
    y=stereo(y)*smooth((length-t)/.2)[:,None]
    return reverberate(y,wet),dict(generator='make_instruments.py',license='MIT')


def read_keys(path):
    keys = sorted({line.strip() for line in path.read_text(encoding="utf-8").splitlines()
                   if line.strip() and not line.lstrip().startswith("#")})
    if not keys:
        raise ValueError("The key list is empty.")
    for key in keys:
        match = re.fullmatch(r"(felt|keys|wood|pluck|air|glass|bass|kick|brush)-(\d+)", key)
        if not match or not 0 <= int(match[2]) <= 127:
            raise ValueError("Invalid instrument key: " + key)
    return keys


def verify_piano_inputs(directory, pins):
    # Preserve the audition's candidate pool. Unrelated additional samples in
    # the supplied directory cannot change nearest-note selection.
    for item in pins["selection_pool"]:
        name = item["file"]
        if Path(name).name != name or sha256_file(directory / name) != item["sha256"]:
            raise ValueError("Missing or changed pinned piano input: " + name)
    return pins["selection_pool"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--piano-samples", required=True, type=Path,
                        help="Directory containing the pinned Salamander FLAC files.")
    parser.add_argument("--keys", type=Path, default=ROOT / "music-keys.txt")
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()
    keys = read_keys(args.keys)
    pins = json.loads((ROOT / "piano-sources.json").read_text(encoding="utf-8"))
    inputs = verify_piano_inputs(args.piano_samples, pins)
    out = fresh_output(args.out, keys)
    rows = []
    for key in keys:
        kind, note = key.rsplit("-", 1)
        y, source = instrument(kind, int(note), args.piano_samples, inputs)
        # Keep the approved bank's exact finishing/quantization sequence.
        y = highpass(y, 20)
        y[:128] *= smooth(np.arange(128) / 127)[:, None]
        y[-2048:] *= smooth(np.arange(2048)[::-1] / 2047)[:, None]
        if not np.isfinite(y).all() or np.abs(y).max() >= .99:
            raise ValueError("Invalid instrument: " + key)
        target = out / (key + ".wav")
        sf.write(target, y, SR, subtype="PCM_16")
        rows.append({"key": key, "frames": len(y), "peak": float(np.abs(y).max()),
                     "source": source, "sha256": sha256_file(target)})
    write_manifest(out / "manifest.json", {
        "sample_rate": SR, "channels": 2, "format": "WAV PCM_16",
        "generator": "tools/audio/make_instruments.py",
        "generator_sha256": sha256_file(Path(__file__)),
        "helpers_sha256": sha256_file(ROOT / "audio_common.py"),
        "keys_sha256": sha256_file(args.keys),
        "dependencies": dependencies(),
        "piano_source": {key: value for key, value in pins.items() if key != "selection_pool"},
        "instruments": rows,
    })
    print(json.dumps({"samples": len(rows), "same_rate": SR,
                      "bytes": sum((out / (r["key"] + ".wav")).stat().st_size for r in rows)}))


if __name__ == "__main__":
    main()

