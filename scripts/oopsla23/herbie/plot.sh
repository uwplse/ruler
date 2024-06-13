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

if [ -z "$1" ]; then
  echo "expected an output directory"
  exit 1
else
  OUTPUT_DIR="$1"
fi

JSONS=$(find $OUTPUT_DIR -maxdepth 2 -name all.json)
NUM_JSONS=$(find $OUTPUT_DIR -maxdepth 2 -name all.json | wc -l)

echo "Found $NUM_JSONS jsons at"
echo "$JSONS"

# make unified all_all.json with all.json from all configs
jq -s . $JSONS > $OUTPUT_DIR/all-all.json

pushd "$OUTPUT_DIR"

jq 'flatten' all-all.json > all-all.json.tmp
mv all-all.json.tmp all-all.json

# first group by test, then by seed within each test
jq 'group_by(.test) | [ .[] | group_by(.seed) ]' all-all.json > by_test_then_seed.json

# if length of this test-seed is $NUM_JSONS, that means all configs succeeded
# keep it
jq --argjson NUM_JSONS "$NUM_JSONS" \
  '[ .[] | .[] | select(length == $NUM_JSONS) ]' \
  by_test_then_seed.json \
  > all-good.json
jq 'flatten' all-good.json > all-good.json.tmp
mv all-good.json.tmp all-good.json

# if length is not $NUM_JSONS that means this test-seed did not succeed for all configs
# discard
jq --argjson NUM_JSONS "$NUM_JSONS" \
  '[ .[] | .[] | select(length != $NUM_JSONS) ]' \
  by_test_then_seed.json \
  > all-bad.json
jq 'flatten' all-bad.json > all-bad.json.tmp
mv all-bad.json.tmp all-bad.json

popd

bash $MYDIR/plot/plot-results.sh "$OUTPUT_DIR"
