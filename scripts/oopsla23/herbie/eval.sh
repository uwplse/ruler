#!/usr/bin/env bash

##
## Runs the Herbie eval
## Requires clone access to remote Herbie repo https://github.com/herbie-fp/herbie
## Requires rules to be generated and placed under `rules/`
##

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# Constants
HERBIE_DIR="$MYDIR/herbie"
BENCH_DIR="$MYDIR/bench"
THREADS=4   # 4 is a good number for consistent performance
FLAGS="-o generate:taylor --timeout 300"
GROUP="main"
RULES="$MYDIR/rules/output.json"

if [ -z "$NUM_SEEDS" ]; then
  NUM_SEEDS=1
fi

echo "Running with $NUM_SEEDS seeds"

# Output
tstamp="$(date "+%Y-%m-%d_%H%M")"
OUTDIR="$MYDIR/reports/$GROUP/$tstamp"

#
#   Install Herbie
#

BUILD_DIR=$HERBIE_DIR bash install.sh

#
#   Make benchmark set
#

rm -rf $BENCH_DIR
mkdir -p $BENCH_DIR

cp -r "$HERBIE_DIR/bench/hamming" \
      "$HERBIE_DIR/bench/mathematics" \
      "$HERBIE_DIR/bench/numerics" \
      "$HERBIE_DIR/bench/physics" \
      "$HERBIE_DIR/bench/pbrt.fpcore" \
      "$BENCH_DIR/"

# FPCORES=$(find $BENCH_DIR -name "*.fpcore")
# for fpcore in $FPCORES; do
#   echo "filtering $fpcore"
#   racket "$MYDIR/filter.rkt" \
#     --names "raw-angle from scale-rotated-ellipse ; \
#              a from scale-rotated-ellipse ; \
#              b from scale-rotated-ellipse ; \
#              Simplification of discriminant from scale-rotated-ellipse ; \
#              Distance on a great circle ; \
#              Harley's example" \
#     $fpcore >> "$fpcore.tmp"
#   mv "$fpcore.tmp" $fpcore
# done

#
#   Run the branches!!!
#

function do_branch {
  name="$1"; shift
  flags="$@"

  # Prepare run
  pushd $HERBIE_DIR

  # Generate rules if necessary
  git checkout $HERBIE_DIR/src/syntax/rules.rkt
  if [[ "$name" != "main" ]]; then
    racket $MYDIR/gen-rules.rkt $name $RULES $MYDIR/rules.rkt
  fi

  # Install
  # make install
  popd

  # Run the seed survey
  if [ -z "$NO_RUN" ]; then
    HERBIE=$HERBIE_DIR \
      PARALLEL_SEEDS=$PARALLEL_SEEDS \
      THREADS=$THREADS \
      BENCH=$BENCH_DIR \
      HERBIE_FLAGS="$FLAGS $flags" \
      bash seed-variance.sh $NUM_SEEDS "$OUTDIR/$name" $name
  fi
}

# Run each configuration

do_branch main
do_branch enumo
do_branch enumo-no-ff
do_branch enumo-rat
do_branch ruler

#
#   Plots
#

if [ -z "$NO_RUN" ]; then
  bash plot.sh $OUTDIR
fi
