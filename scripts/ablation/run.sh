#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

TIMESTAMP="$(date "+%Y-%m-%d_%H%M")"

"$MYDIR/run_ruler.sh" -d bv4 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/compare" -r 1
"$MYDIR/run_ruler.sh" -d bv32 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/compare" -r 1
"$MYDIR/run_ruler.sh" -d rational -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/compare" -r 1 --use-smt 

"$MYDIR/run_ruler_rr.sh" -d bv4 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/no-rr" -r 1
"$MYDIR/run_ruler_rr.sh" -d bv32 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/no-rr" -r 1
"$MYDIR/run_ruler_rr.sh" -d rational -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/no-rr" -r 1 --use-smt

node parse.js "$MYDIR/output/$TIMESTAMP/compare/"
node parse.js "$MYDIR/output/$TIMESTAMP/no-rr/" yes

python visualize.py "$MYDIR/output/$TIMESTAMP/compare/" "$MYDIR/output/$TIMESTAMP/no-rr/"
