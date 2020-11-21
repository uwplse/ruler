#!/usr/bin/env python3

import json

def print_rules(filename):
    with open(filename) as f:
        j = json.load(f)
    for eq in j['eqs']:
        l, r = sorted([eq['lhs'], eq['rhs']], key=len, reverse=True)
        s = '"{}" &=& "{}" \\\\'.format(l, r)
        s = s.replace('?', '').replace('15', 'true').replace('0', 'false')
        print(s)


if __name__ == "__main__":
    import sys
    for filename in sys.argv[1:]:
        print_rules(filename)
