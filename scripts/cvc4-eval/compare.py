#!/usr/bin/env python3

import csv
import os
import sys
import json
import statistics

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def load_report(filename):
    with open(filename) as f:
        return json.load(f)

def make_same_row(diff):
    diff = load_report(diff)
    ruler_fname = diff['files'][0]
    assert('ruler' in ruler_fname)
    cvc_fname = diff['files'][1]
    # assert('cvc4' in cvc_fname)
    ruler = load_report(ruler_fname)
    cvc = load_report(cvc_fname)

    base = os.path.basename(ruler_fname)
    domain = base.split('-')[0]

    if 'params' in cvc:
        for field in ['iters', 'variables']:
            assert ruler['params'][field] == cvc['params'][field]

    # assert len(diff['forward'].get('bad', [])) == 0
    # assert len(diff['reverse'].get('bad', [])) == 0

    data = {
        'domain': domain,
        'vars': ruler['params']['variables'],
        'iterations': ruler['params']['iters'],
        'ruler time': ruler['time'],
        'ruler rules': len(ruler['eqs']),
        # 'ruler bad': len(diff['forward']['bad']),
        'ruler prove power': len(diff['forward']['derivable']) / len(cvc['eqs']),
        'cvc time': cvc['time'],
        'cvc rules': len(cvc['eqs']),
        # 'cvc bad': len(diff['reverse']['bad']),
        'cvc prove power': len(diff['reverse']['derivable']) / len(ruler['eqs']),
        # 'ruler cannot prove': len(diff['forward']['not_derivable']),
        # 'cvc cannot prove': len(diff['reverse']['not_derivable']),
        'ruler time pct': pct(ruler['time'], cvc['time']),
        'ruler rules pct': pct(len(ruler['eqs']), len(cvc['eqs'])),
    }

    return data

def pct(a, b):
    return a / b
    # pct = int(a * 100 / b)
    # return "({}%)".format(pct)

def fmt(x):
    if x == 0.0:
        return 0
    elif x == 1.0:
        return 1
    elif type(x) is float:
        return "{:.2f}".format(round(x, 2))
    return x

if __name__ == '__main__':
    diffs = sys.argv[1:]
    headers = ['ruler time', 'cvc time']

    rows = [make_same_row(d) for d in diffs]

    avg_time_diff = statistics.harmonic_mean([r['ruler time pct'] for r in rows])
    avg_rule_diff = statistics.harmonic_mean([r['ruler rules pct'] for r in rows])

    writer = csv.DictWriter(sys.stdout, rows[0].keys())
    writer.writeheader()
    for row in rows:
        row = {k: fmt(v) for k,v in row.items()}
        writer.writerow(row)

    eprint('avg time diff', 'avg rule diff',
          avg_time_diff, '{:.1f}x'.format(1/avg_time_diff),
          avg_rule_diff, '{:.1f}x'.format(1/avg_rule_diff))
