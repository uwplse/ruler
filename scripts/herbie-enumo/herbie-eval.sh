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

cp -r "$HERBIE_DIR/bench/hamming" \
      "$HERBIE_DIR/bench/mathematics" \
      "$HERBIE_DIR/bench/numerics" \
      "$HERBIE_DIR/bench/physics" \
      "$HERBIE_DIR/bench/pbrt.fpcore" \
      "$BENCH_DIR/"

FPCORES=$(find $BENCH_DIR -name "*.fpcore")
for fpcore in $FPCORES; do
  echo "filtering $fpcore"
  racket "$MYDIR/filter.rkt" \
    --names "raw-angle from scale-rotated-ellipse ; \
             a from scale-rotated-ellipse ; \
             b from scale-rotated-ellipse ; \
             Simplification of discriminant from scale-rotated-ellipse ; \
             Distance on a great circle ; \
             Harley's example" \
    $fpcore >> "$fpcore.tmp"
  mv "$fpcore.tmp" $fpcore
done


# "$HERBIE_DIR/bench/libraries" \

# Run the branches!!!

function do_branch {
  branch="$1"; shift
  name="$1"; shift
  flags="$@"

  # Checkout and build
  pushd $HERBIE_DIR
  git checkout "$HERBIE_DIR/src/syntax/rules.rkt"
  git checkout "$HERBIE_DIR/src/config.rkt"
  git checkout $branch

  # Change commit for main
  if [[ "$branch" == "main" ]]; then
    git checkout 5a1accbc5ebcb1311f85421a3c6dd73e4f8575be
  fi

  cp "$MYDIR/new-config.rkt" "$HERBIE_DIR/src/config.rkt"
  cp "$MYDIR/egg-herbie.rkt" "$HERBIE_DIR/src/core/egg-herbie.rkt"
  # cp "$MYDIR/lib.rs" "$HERBIE_DIR/egg-herbie/src/lib.rs"

  # Patch ruler-autogen rule branches
  if [[ "$branch" == "using-ruler-baseline" || "$branch" == "using-ruler-nightlies" || "$branch" == "ruler-no-fast-forwarding" ]]; then
    # In case of broken nightlies, target different branch
    sed -i 's/main/oflatt-conditional-herbie/g' "$HERBIE_DIR/src/syntax/rules.rkt"
    if [[ "$flags" == *"-o rules:numerics"* ]]; then
      echo "skipping numerics patch"
    else
      cat "$MYDIR/numerics.patch" >> "$HERBIE_DIR/src/syntax/rules.rkt"
      echo "" >> "$HERBIE_DIR/src/syntax/rules.rkt"
    fi

    if [[ "$name" = "enumo-only-rat" ]]; then
      sed -i 's#(ruler-manifest "exponential"#; (ruler-manifest "exponential"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
      sed -i 's#(ruler-manifest "trig"#; (ruler-manifest "trig"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
    fi
  # Patch no-rules branch
  elif [[ "$name" == "no-rules" ]]; then
    cp "$MYDIR/empty-rules.rkt" "$HERBIE_DIR/src/syntax/rules.rkt"
  # Patch any other branch
  else
    # Patch bug where rules cannot be disabled
    if [[ "$flags" == *"-o rules:numerics"* ]]; then
      cp "$MYDIR/no-numerics.rkt" "$HERBIE_DIR/src/syntax/rules.rkt"
    fi
  fi

  make install
  popd

  flags="$flags --timeout 300"
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

if [ -z "$NO_RUN" ]; then
  ### Debugging ###
  # do_branch main main
  # do_branch main main-n -o rules:numerics
  # do_branch main main-t -o generate:taylor
  # do_branch main main-n-t -o rules:numerics -o generate:taylor
  # do_branch using-ruler-nightlies enumo
  # do_branch using-ruler-nightlies enumo-n -o rules:numerics
  # do_branch using-ruler-nightlies enumo-t -o generate:taylor
  # do_branch using-ruler-nightlies enumo-n-t -o rules:numerics -o generate:taylor
  # do_branch using-ruler-baseline ruler
  # do_branch using-ruler-baseline ruler-n -o rules:numerics
  # do_branch using-ruler-baseline ruler-t -o generate:taylor
  # do_branch using-ruler-baseline ruler-n-t -o rules:numerics -o generate:taylor
  # do_branch main no-rules -o generate:rr -o generate:simplify
  # do_branch ruler-no-fast-forwarding ruler-no-ff
  # do_branch ruler-no-fast-forwarding ruler-no-ff-n -o rules:numerics
  # do_branch ruler-no-fast-forwarding ruler-no-ff-t -o generate:taylor
  # do_branch ruler-no-fast-forwarding ruler-no-ff-n-t -o rules:numerics -o generate:taylor
  ### Final ###
  do_branch main main-t -o generate:taylor
  do_branch using-ruler-nightlies enumo-t -o generate:taylor
  do_branch ruler-no-fast-forwarding ruler-no-ff-t -o generate:taylor
  do_branch using-ruler-baseline enumo-only-rat -o generate:taylor
  do_branch using-ruler-baseline ruler-t -o generate:taylor
fi

# Plots

# bash plot.sh $OUTDIR
