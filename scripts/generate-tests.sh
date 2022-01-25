#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# change to top-level directory
cd $MYDIR/..

# generate rulesets
echo "Generating test files..."
echo "] rational / 2 iters"
cargo rational --iters 2 --variables 3 --num-fuzz 50 --do-final-run --outfile tests/rat.json
echo "] rational / 2 iters / EMA (after 1 iter)"
cargo rational --iters 2 --variables 3 --ema-above-iter 1 --num-fuzz 50 --do-final-run --outfile tests/rat-ema.json
echo "] rational / 2 iters / EMA (after 1 iter) / No consts"
cargo rational --iters 2 --variables 3 --ema-above-iter 1 --no-constants-above-iter 1 --num-fuzz 50 --do-final-run --outfile tests/rat-ema-no-const.json
echo "] bv4 / 2 iters"
cargo bv4 --iters 2 --variables 3 --outfile tests/bv4.json
echo "] bv32 / 2 iters"
cargo bv4 --iters 2 --variables 3 --outfile tests/bv32.json
echo "] real / 2 iters / rule lifting from rational"
cargo real-rat --iters 2 --variables 3 --no-constants-above-iter 1 --prior-rules tests/rat.json --outfile tests/real.json
