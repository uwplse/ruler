#!/usr/bin/env bash

# h/t Zach
# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

DIR="$MYDIR/output/workload-eval"
mkdir -p $DIR

pushd "$DIR"
# clone Herbie inside the output directory
if [ -d "$DIR/herbie" ]
then
    echo "Herbie is already cloned. Skipping a fresh clone."
else
    git clone git@github.com:uwplse/herbie.git
fi
popd

# set herbie path
export HERBIE="$DIR/herbie"

# filter FPCores with only these ops and put in out.fpcore
racket filter.rkt --operators "+ - * / fabs" $HERBIE/bench/hamming $HERBIE/bench/libraries $HERBIE/bench/mathematics $HERBIE/bench/numerics $HERBIE/bench/physics > out.fpcore
