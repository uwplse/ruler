#!/usr/bin/env bash

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
tstamp="$(date "+%Y-%m-%d_%H%M")"

# Constants
HERBIE_DIR="$MYDIR/herbie"
BENCH_DIR="$MYDIR/bench"
OUTDIR="$MYDIR/reports/$tstamp"
NUM_SEEDS=1
PARALLEL_SEEDS=1
THREADS=4

# install herbie
BUILD_DIR=$HERBIE_DIR bash install.sh

# Form the benchmark set
mkdir -p $BENCH_DIR
cp -r "$HERBIE_DIR/bench/hamming" \
      "$HERBIE_DIR/bench/libraries" \
      "$HERBIE_DIR/bench/mathematics" \
      "$HERBIE_DIR/bench/numerics" \
      "$HERBIE_DIR/bench/physics" \
      "$HERBIE_DIR/bench/pbrt.fpcore" \
      "$BENCH_DIR/"

# Run the branches!!!

function do_branch {
  branch="$1"; shift
  outdir="$1"; shift
  flags="$@"

  pushd $HERBIE_DIR
  git checkout $branch
  make install
  popd

  if [ -z "$PARALLEL_SEEDS" ]; then
    HERBIE=$HERBIE_DIR \
      THREADS=$THREADS \
      BENCH=$BENCH_DIR \
      HERBIE_FLAGS=$flags \
      bash seed-variance.sh $NUM_SEEDS "$OUTDIR/$outdir"
  else
    HERBIE=$HERBIE_DIR \
      PARALLEL_SEEDS=$PARALLEL_SEEDS \
      THREADS=$THREADS \
      BENCH=$BENCH_DIR \
      HERBIE_FLAGS=$flags \
      bash seed-variance.sh $NUM_SEEDS "$OUTDIR/$outdir"
  fi
}

if [ -z "$NO_RUN" ]; then
  do_branch main main
  do_branch main no-rules -o generate:rr -o generate:simplify
  do_branch using-ruler-nightlies enumo
  do_branch using-ruler-baseline ruler
fi

# Plots

bash plot.sh $OUTDIR
