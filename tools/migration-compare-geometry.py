"""Compare complete migrated CPU buffers against the frozen native oracle."""

import argparse
import csv
import json
import math
from pathlib import Path
import struct


def compare_payload(expected, actual, material):
    if len(expected) != len(actual):
        return {'exact': False, 'expectedBytes': len(expected), 'actualBytes': len(actual)}
    if expected == actual:
        return {'exact': True, 'bytes': len(expected)}
    names = ['x','y','z','normalX','normalY','normalZ','u','v','extraX','extraY','red','green','blue','alpha'] if material else ['value']
    fmt = '<10f4B' if material else '<f'
    stats = {name: {'different': 0, 'maxAbsolute': 0.0, 'first': None} for name in names}
    invalid = False
    for i, (a, b) in enumerate(zip(struct.iter_unpack(fmt, expected), struct.iter_unpack(fmt, actual))):
        for component, (x, y) in enumerate(zip(a, b)):
            if not math.isfinite(y):
                invalid = True
            if x != y:
                stat = stats[names[component]]
                stat['different'] += 1
                stat['maxAbsolute'] = max(stat['maxAbsolute'], abs(x-y))
                if stat['first'] is None:
                    stat['first'] = {'record': i, 'expected': x, 'actual': y}
    return {'exact': False, 'bytes': len(expected), 'invalidActual': invalid,
        'components': {k: v for k, v in stats.items() if v['different']}}


def read_rows(paths):
    result = {}
    for path in paths:
        with path.open(encoding='utf-8-sig', newline='') as stream:
            for row in csv.DictReader(stream):
                result[row['name']] = row
    return result


def accepted_rounding(name, result):
    """The one reviewed ordinary-libm difference; every other attribute is exact.

    At Alpine vertex35, atan2 Float bits differ c0220b87 (native clang libm)
    versus c0220b88 (ordinary GHC Float). This propagates through ridge drainage
    into height/normals, while topology, all vegetation, bones and colors match.
    Actual Alpine captures remain a separate acceptance check.
    """
    if result.get('exact'):
        return True
    if result.get('invalidActual') or 'components' not in result:
        return False
    limits = {'value': 1e-5} if name == 'fixed-1-height.bin' else (
        {'y': 1e-5, 'normalX': 4e-5, 'normalY': 4e-5, 'normalZ': 4e-5}
        if name == 'fixed-1-material0.bin' else {})
    return bool(limits) and all(component in limits and stat['maxAbsolute'] <= limits[component]
        for component, stat in result['components'].items())


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--actual', type=Path, required=True)
    parser.add_argument('--actual-csv', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    expected_rows = read_rows([args.reference/'fixed.csv', args.reference/'streamed.csv'])
    actual_rows = read_rows([args.actual_csv])
    mismatches = []
    for name, expected in expected_rows.items():
        actual = actual_rows.get(name, {})
        for key, value in expected.items():
            if key not in ('name', 'signature') and actual.get(key) != value:
                mismatches.append({'name': name, 'field': key, 'expected': value, 'actual': actual.get(key)})
    buffers = {}
    for path in sorted(args.reference.glob('*.bin')):
        actual = args.actual/path.name
        buffers[path.name] = compare_payload(path.read_bytes(), actual.read_bytes(), 'material' in path.name) if actual.is_file() else {'exact': False, 'missing': True}
    # A shared wrong geometry path must not pass by agreeing with itself.
    # Corrupt a real reference vertex by one metre and feed the same comparator.
    example = (args.reference/'fixed-0-material0.bin').read_bytes()[:44]
    corrupted = bytearray(example)
    struct.pack_into('<f', corrupted, 0, struct.unpack_from('<f', corrupted)[0]+1)
    negative_rejected = not compare_payload(example, corrupted, True)['exact']
    report = {'passed': not mismatches and all(accepted_rounding(k,v) for k,v in buffers.items()) and negative_rejected,
        'cases': len(expected_rows), 'buffers': len(buffers), 'exactBuffers': sum(v['exact'] for v in buffers.values()),
        'referenceBytes': sum(p.stat().st_size for p in args.reference.glob('*.bin')),
        'countMismatches': mismatches, 'perturbedVertexRejected': negative_rejected,
        'exactSignatureCases': sum(actual_rows.get(name,{}).get('signature') == row['signature'] for name,row in expected_rows.items()),
        'boundedRoundingBuffers': [k for k,v in buffers.items() if not v['exact'] and accepted_rounding(k,v)],
        'differences': {k: v for k, v in buffers.items() if not v['exact']},
        'acceptance': 'All counts and CPU buffer bytes match except the reviewed Alpine ordinary-libm atan2 rounding: height <=1e-5m and normal components <=4e-5; every other attribute exact. Actual scene capture acceptance is separate.'}
    args.output.write_text(json.dumps(report, indent=2, allow_nan=False), encoding='utf-8')
    print(json.dumps({k: report[k] for k in ['passed','cases','buffers','exactBuffers','referenceBytes','countMismatches','perturbedVertexRejected']}))
    raise SystemExit(0 if report['passed'] else 1)


if __name__ == '__main__':
    main()
