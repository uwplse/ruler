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

# Constants
HERBIE_DIR="$MYDIR/herbie"
BENCH_DIR="$MYDIR/bench"
THREADS=4
GROUP="main"

if [ -z "$NUM_SEEDS" ]; then
  NUM_SEEDS=1
fi

echo "Running with $NUM_SEEDS seeds"

# Output
tstamp="$(date "+%Y-%m-%d_%H%M")"
OUTDIR="$MYDIR/reports/$GROUP/$tstamp"

# install herbie
BUILD_DIR=$HERBIE_DIR bash install.sh

# Form the benchmark set
mkdir -p $BENCH_DIR
cp -r "$HERBIE_DIR/bench/hamming" "$BENCH_DIR/"

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

# Run enumo branch

function do_branch {
  name="$1"; shift
  flags="$@"

  # Checkout and build
  pushd $HERBIE_DIR
  git checkout "$HERBIE_DIR/src/syntax/rules.rkt"

  # In case of broken nightlies, target different branch
  sed -i 's/oflatt-better-herbie/main/g' "$HERBIE_DIR/src/syntax/rules.rkt"
  if [[ -n "$COMMIT" ]]; then
    sed -i "s/(define nightly-commit #f)/(define nightly-commit \"$COMMIT\")/g" "$HERBIE_DIR/src/syntax/rules.rkt"
  fi

  if [[ "$flags" == *"-o rules:numerics"* ]]; then
    echo "skipping numerics patch"
  else
    cat "$MYDIR/numerics.patch" >> "$HERBIE_DIR/src/syntax/rules.rkt"
    echo "" >> "$HERBIE_DIR/src/syntax/rules.rkt"
  fi

  cat "$MYDIR/expansive.patch" >> "$HERBIE_DIR/src/syntax/rules.rkt"
  if [[ "$name" = "enumo-only-rat" ]]; then
    sed -i 's#(ruler-manifest "exponential"#; (ruler-manifest "exponential"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
    sed -i 's#(ruler-manifest "trig"#; (ruler-manifest "trig"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
  fi

  make install
  popd

  flags="$flags --timeout 600"
  if [ -z "$PARALLEL_SEEDS" ]; then
    HERBIE=$HERBIE_DIR \
      THREADS=$THREADS \
      BENCH=$BENCH_DIR \
      HERBIE_FLAGS=$flags \
      bash seed-variance.sh $NUM_SEEDS "$OUTDIR/$name" $name
  else
    HERBIE=$HERBIE_DIR \
      PARALLEL_SEEDS=$PARALLEL_SEEDS \
      THREADS=$THREADS \
      BENCH=$BENCH_DIR \
      HERBIE_FLAGS=$flags \
      bash seed-variance.sh $NUM_SEEDS "$OUTDIR/$name" $name
  fi
}

pushd $HERBIE_DIR
git checkout using-ruler-nightlies
popd

if [ -z "$NO_RUN" ]; then
  rm $HERBIE_DIR/egg-herbie/src/*
  cp $MYDIR/egg-herbie/* "$HERBIE_DIR/egg-herbie/src/"
  cp $MYDIR/egg-herbie.rkt "$HERBIE_DIR/src/core/egg-herbie.rkt"

  do_branch sat-2500 -o generate:taylor --num-enodes 2500
  do_branch sat-5000 -o generate:taylor --num-enodes 5000
  do_branch sat-10000 -o generate:taylor --num-enodes 10000
  do_branch sat-20000 -o generate:taylor --num-enodes 20000

  # rm $HERBIE_DIR/egg-herbie/src/*
  # pushd $HERBIE_DIR
  # git checkout $HERBIE_DIR/egg-herbie/src/
  # popd

  # cp "$MYDIR/lib.rs" "$HERBIE_DIR/egg-herbie/src/lib.rs"

  # do_branch default-2500 -o generate:taylor --num-enodes 2500
  # do_branch default-5000 -o generate:taylor --num-enodes 5000
  # do_branch default-10000 -o generate:taylor --num-enodes 10000
  # do_branch default-20000 -o generate:taylor --num-enodes 20000
fi

# Plots

bash plot.sh $OUTDIR
