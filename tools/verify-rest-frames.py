"""Check frames from --check. Development only; requires Pillow and NumPy.

These invariants detect missing rendering, frozen animation, alpha leakage,
accidental camera/sky movement and an ineffective Still setting. They do not
score beauty, comfort or health effects.
"""

import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image


def pixels(path):
    return np.asarray(Image.open(path).convert("RGB"))


folder = Path(sys.argv[1])
report = {}
scenes = ["water", "amber", "shallows", "clouds", "aurora", "ink", "veil", "stars", "birch", "alpine", "stream", "still"]
for scene in scenes:
    a = pixels(folder / f"procedural-{scene}-a.png")
    b = pixels(folder / f"procedural-{scene}-b.png")
    assert a.shape == b.shape
    changed = np.any(a != b, axis=2)
    delta = np.abs(a.astype(float) - b.astype(float))
    result = {
        "size": [a.shape[1], a.shape[0]],
        "changed_pixels": int(changed.sum()),
        "mean_channel_delta": float(delta.mean()),
        "max_channel_delta": int(delta.max()),
        "image_standard_deviation": float(a.std()),
    }
    assert result["image_standard_deviation"] > 2, (scene, "blank output", result)
    if scene == "still":
        assert not changed.any(), ("Still or opaque compositing failed", result)
    else:
        assert result["changed_pixels"] > 100, (scene, "motion missing", result)
        assert result["mean_channel_delta"] < 12, (scene, "large frame discontinuity", result)
    if scene in ("water", "amber"):
        assert not changed[: int(a.shape[0] * 0.30)].any(), (scene, "sky moved")
    report[scene] = result

returned = pixels(folder / "rest-return.png")
rest = pixels(folder / "rest-alternate-b.png")
assert np.abs(returned.astype(float) - rest.astype(float)).mean() > 10
report["working_background_restored"] = True
(folder / "frame-check.json").write_text(json.dumps(report, indent=2), encoding="utf-8")
print(json.dumps(report, indent=2))
