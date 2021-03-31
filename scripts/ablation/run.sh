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

$MYDIR/run_ruler.sh -d bool -o $MYDIR/output -v 1 -i 2 -r 2
# $MYDIR/run_ruler.sh -d bv4ns -o $MYDIR/output
# $MYDIR/run_ruler.sh -d rational -o $MYDIR/output
node parse.js
python visualize.py