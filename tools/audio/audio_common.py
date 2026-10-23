"""Small offline provenance helpers shared by the two bank makers. MIT."""
import hashlib
import json
import platform
from pathlib import Path


def sha256_file(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def dependencies():
    import numpy
    import scipy
    import soundfile
    return {"python": platform.python_version(), "numpy": numpy.__version__,
            "scipy": scipy.__version__, "soundfile": soundfile.__version__,
            "libsndfile": soundfile.__libsndfile_version__}


def write_manifest(path, value):
    Path(path).write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n",
                          encoding="utf-8", newline="\n")


def fresh_output(path, keys):
    path = Path(path)
    targets = [path / (key + ".wav") for key in keys] + [path / "manifest.json"]
    if any(target.exists() for target in targets):
        raise ValueError("Choose a fresh output directory; bank files will not be overwritten.")
    path.mkdir(parents=True, exist_ok=True)
    return path
