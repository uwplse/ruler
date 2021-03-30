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

tstamp="$(date "+%Y-%m-%d_%H%M")"
DIR="$MYDIR/output/ruler-herbie-eval"

mkdir -p "$DIR"

# run ruler and put rules in output directory
cargo run --bin float --release -- --no-conditionals > $DIR/ruler-rules-"$tstamp".txt

# go to output directory
pushd "$DIR"

# clone Herbie inside the output directory
if [ -d "$DIR/herbie" ]
then
    echo "Herbie is already cloned. Skipping a fresh clone."
else
    git clone git@github.com:uwplse/herbie.git
    cd herbie
    make install
    cd ..
fi

# set herbie path
export HERBIE="$DIR/herbie"

# pop back to MYDIR
popd

# all configs
configs=("herbie-only" "herbie-ruler" "herbie-no-fp-simpl" "ruler-only")

# for all the configs
for c in ${configs[@]}; do
    # run python script with this config
    echo "Running with config: $c"
    python3 preprocess.py $DIR/ruler-rules-"$tstamp".txt "$HERBIE"/src/syntax/rules.rkt $c
    # do the herbie runs with this config
    # seed-variance.sh will put its output in the right directory
    ./seed-variance.sh 1 $c $tstamp
done

# make unified all_all.json with all.json from all configs
jq -s . \
    $DIR/configs/$tstamp/herbie-only/all.json \
    $DIR/configs/$tstamp/herbie-no-fp-simpl/all.json \
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
