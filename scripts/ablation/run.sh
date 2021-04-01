#!/usr/bin/env bash

# determine physical directory of this script
# h/t Zach
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

TIMESTAMP="$(date "+%Y-%m-%d_%H%M")"

"$MYDIR/run_ruler.sh" -d bool -o "$MYDIR/output/$TIMESTAMP" -v 1 -i 2 -r 2
# "$MYDIR/run_ruler.sh" -d bv4ns -o "$MYDIR/output/$TIMESTAMP"
# "$MYDIR/run_ruler.sh" -d rational -o "$MYDIR/output/$TIMESTAMP"
node parse.js "$MYDIR/output/$TIMESTAMP/"
python visualize.py