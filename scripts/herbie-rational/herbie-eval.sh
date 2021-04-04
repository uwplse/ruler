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

# default values
NSEEDS=${1:-1}
NUM_NODES=${2:-5000}

if [ "$#" -eq 1 ]
then
    NSEEDS="$1"
elif [ "$#" -eq 2 ]
then
    NEEDS="$1"
    NUM_NODES="$2"
else
    if [ "$#" -gt 2 ]
    then
        echo "Illegal number of arguments."
        echo "Usage: $0 NSEEDS NUM_NODES."
        exit 1
    fi
fi

echo "Running $0 with $NSEEDS seeds and $NUM_NODES enodes."

tstamp="$(date "+%Y-%m-%d_%H%M")"
DIR="$MYDIR/output/ruler-herbie-eval"

mkdir -p "$DIR"

echo "Running Ruler"
# run ruler and put rules in output directory
cargo rational --iters 2 --variables 3 --rules-to-take 999999999999 > $DIR/ruler-rules-"$tstamp".txt

# go to output directory
pushd "$DIR"

# clone Herbie inside the output directory
if [ -d "$DIR/herbie" ]
then
    echo "Herbie is already cloned. Skipping a fresh clone."
else
    git clone git@github.com:uwplse/herbie.git
    cd herbie
    # TODO: remove this once merged.
    git checkout egg-interface-fixes
    make install
    cd ..
fi

# Filter rational rules
pushd herbie
if [ -f "bench/rat-reduced.fpcore" ]
then
    echo "Rational FPCore Exists. Not Filtering."
else
    echo "Filtering all stable benches..."
    racket $MYDIR/filter.rkt \
    --operators "let let* fabs + - * / neg" \
    --names "Octave 3.8, jcobi/3 ; Rosa's FloatVsDoubleBenchmark ; (- (/ x0 (- 1 x1)) x0) ; Expression 4, p15" \
    bench/hamming \
    bench/mathematics \
    bench/libraries \
    bench/numerics \
    bench/physics > bench/rat-reduced.fpcore
fi
popd

# set herbie path
export HERBIE="$DIR/herbie"

# pop back to MYDIR
popd

# all configs
configs=("herbie-no-simpl" "herbie-only" "ruler-only" "herbie-ruler")

# for all the configs
for c in ${configs[@]}; do
    # run python script with this config
    echo "Running with config: $c"
    python3 preprocess.py $DIR/ruler-rules-"$tstamp".txt "$HERBIE"/src/syntax/rules.rkt $c
    # do the herbie runs with this config
    # seed-variance.sh will put its output in the right directory
    ./seed-variance.sh "$NSEEDS" "$NUM_NODES" $c $tstamp
done

# make unified all_all.json with all.json from all configs
jq -s . \
    $DIR/configs/$tstamp/herbie-only/all.json \
    $DIR/configs/$tstamp/herbie-no-simpl/all.json \
    $DIR/configs/$tstamp/herbie-ruler/all.json \
    $DIR/configs/$tstamp/ruler-only/all.json \
    > $DIR/configs/$tstamp/all-all.json

pushd "$DIR/configs/$tstamp" 

jq 'flatten' all-all.json > all-all.json.tmp
mv all-all.json.tmp all-all.json

# first group by test, then by seed within each test
jq 'group_by(.test) | [ .[] | group_by(.seed) ]' all-all.json > by_test_then_seed.json

# if length of this test-seed is 4, that means all configs succeeded
# keep it
jq '[ .[] | .[] | select(length == 4) ]' by_test_then_seed.json > all-good.json
jq 'flatten' all-good.json > all-good.json.tmp
mv all-good.json.tmp all-good.json

# if length is not 4 that means this test-seed did not succeed for all configs
# discard
jq '[ .[] | .[] | select(length != 4) ]' by_test_then_seed.json > all-bad.json
jq 'flatten' all-bad.json > all-bad.json.tmp
mv all-bad.json.tmp all-bad.json

popd
#
#   PLOT RESULTS
#

$MYDIR/seed-stats-per-test.sh "$DIR/configs/$tstamp"
$MYDIR/plots/plot-results.sh "$DIR/configs/$tstamp"
