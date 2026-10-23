"""Explicit, optional download of pinned public Salamander inputs. MIT.

The application never runs this tool or downloads assets. All downloads are
read-only public requests; hashes are checked before files are written.
"""
import argparse
import hashlib
import json
from pathlib import Path
from urllib.parse import quote
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parent


def download(url, expected, target):
    if target.exists():
        actual = hashlib.sha256(target.read_bytes()).hexdigest()
        if actual != expected:
            raise ValueError("Existing file has the wrong SHA-256: " + target.name)
        return 0
    request = Request(url, headers={"User-Agent": "Stillroom-offline-audio-source-verifier"})
    with urlopen(request, timeout=60) as response:
        data = response.read(8 * 1024 * 1024 + 1)
    if len(data) > 8 * 1024 * 1024:
        raise ValueError("Unexpectedly large input: " + target.name)
    if hashlib.sha256(data).hexdigest() != expected:
        raise ValueError("Downloaded SHA-256 does not match: " + target.name)
    with target.open("xb") as output:
        output.write(data)
    return len(data)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", required=True, type=Path,
                        help="Directory for the nine pinned FLAC files and LICENSE.")
    args = parser.parse_args()
    pins = json.loads((ROOT / "piano-sources.json").read_text(encoding="utf-8"))
    args.out.mkdir(parents=True, exist_ok=True)
    base = "https://raw.githubusercontent.com/sfzinstruments/SalamanderGrandPiano/" + pins["commit"]
    transferred = 0
    for item in pins["selection_pool"]:
        name = item["file"]
        if Path(name).name != name or name in (".", ".."):
            raise ValueError("Input filename must be a basename.")
        transferred += download(base + "/Samples/" + quote(name, safe=""),
                                item["sha256"], args.out / name)
    transferred += download(base + "/LICENSE", pins["license_sha256"], args.out / "LICENSE")
    print(json.dumps({"verified_samples": len(pins["selection_pool"]),
                      "downloaded_bytes": transferred, "commit": pins["commit"]}))


if __name__ == "__main__":
    main()
