"""Compare regenerated WAVs against the shipped, hashed bank manifests. MIT."""
import argparse
import json
from pathlib import Path
import wave

from audio_common import sha256_file


ROOT = Path(__file__).resolve().parents[2]


def verify(directory, manifest, rows_key):
    expected = json.loads(manifest.read_text(encoding="utf-8"))
    rows = expected[rows_key]
    names = {row["key"] + ".wav" for row in rows}
    actual_names = {path.name for path in directory.glob("*.wav")}
    if actual_names != names:
        raise ValueError("Bank WAV filenames differ from the shipped manifest.")
    for row in rows:
        path = directory / (row["key"] + ".wav")
        if sha256_file(path) != row["sha256"]:
            raise ValueError("WAV SHA-256 differs: " + path.name)
        with wave.open(str(path), "rb") as source:
            actual = (source.getnchannels(), source.getsampwidth(),
                      source.getframerate(), source.getnframes())
        if actual != (2, 2, expected["sample_rate"], row["frames"]):
            raise ValueError("Unexpected PCM16 format or frame count: " + path.name)
    return len(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--music", required=True, type=Path)
    parser.add_argument("--environment", required=True, type=Path)
    args = parser.parse_args()
    counts = {
        "music": verify(args.music, ROOT / "assets/audio/music/manifest.json", "instruments"),
        "environment": verify(args.environment, ROOT / "assets/audio/environment/manifest.json", "samples"),
    }
    print(json.dumps({"byte_identical": True, "verified_wavs": counts}))


if __name__ == "__main__":
    main()
