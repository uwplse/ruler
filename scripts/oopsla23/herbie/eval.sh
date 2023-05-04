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

#
#   Run the branches!!!
#

function do_branch {
  name="$1"; shift
  flags="$@"

  # Checkout and build
  pushd $HERBIE_DIR

#   # Change commit for main
#   if [[ "$branch" == "main" ]]; then
#     git checkout 5a1accbc5ebcb1311f85421a3c6dd73e4f8575be
#   fi

#   if [[ "$name" == "main-lim" || "$name" == "enumo-lim" || "$name" == "enumo-rat-lim" ]]; then
#     cp "$MYDIR/lib.rs" "$HERBIE_DIR/egg-herbie/src/lib.rs"
#   fi

  # Patch ruler-autogen rule branches
#   if [[ "$branch" == "using-ruler-baseline" || "$branch" == "using-ruler-nightlies" || "$branch" == "ruler-no-fast-forwarding" ]]; then
#     # In case of broken nightlies, target different branch
#     sed -i 's/main/oflatt-error-unsound/g' "$HERBIE_DIR/src/syntax/rules.rkt"
#     # cp "$MYDIR/lib.rs" "$HERBIE_DIR/egg-herbie/src/lib.rs"
#     if [[ -n "$COMMIT" ]]; then
#       sed -i "s/(define nightly-commit #f)/(define nightly-commit \"$COMMIT\")/g" "$HERBIE_DIR/src/syntax/rules.rkt"
#     fi

#     if [[ "$flags" == *"-o rules:numerics"* ]]; then
#       echo "skipping numerics patch"
#     else
#       cat "$MYDIR/numerics.patch" >> "$HERBIE_DIR/src/syntax/rules.rkt"
#       echo "" >> "$HERBIE_DIR/src/syntax/rules.rkt"
#     fi

#     if [[ "$name" == "ruler" ]]; then
#       cp "$MYDIR/egg-herbie.rkt" "$HERBIE_DIR/src/core/egg-herbie.rkt"
#     else
#       cat "$MYDIR/expansive.patch" >> "$HERBIE_DIR/src/syntax/rules.rkt"
#       sed -i 's#(ruler-manifest "rational_best" "oopsla"#(ruler-manifest "rational_best" "rational_best"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
#     fi

#     if [[ "$name" == "enumo-only-rat" ]]; then
#       sed -i 's#(ruler-manifest "exponential"#; (ruler-manifest "exponential"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
#       sed -i 's#(ruler-manifest "trig"#; (ruler-manifest "trig"#g' "$HERBIE_DIR/src/syntax/rules.rkt"
#     fi
#   # Patch no-rules branch
#   elif [[ "$name" == "no-rules" ]]; then
#     cp "$MYDIR/egg-herbie.rkt" "$HERBIE_DIR/src/core/egg-herbie.rkt"
#     cp "$MYDIR/empty-rules.rkt" "$HERBIE_DIR/src/syntax/rules.rkt"
#   # Patch any other branch
#   else
#     cp "$MYDIR/egg-herbie.rkt" "$HERBIE_DIR/src/core/egg-herbie.rkt"
#     if [[ "$flags" == *"-o rules:numerics"* ]]; then
#       cp "$MYDIR/no-numerics.rkt" "$HERBIE_DIR/src/syntax/rules.rkt"
#     fi
#   fi

  make install
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
