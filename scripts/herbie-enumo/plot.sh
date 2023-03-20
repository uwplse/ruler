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

# make unified all_all.json with all.json from all configs
jq -s . \
    $OUTPUT_DIR/main/all.json      \
    $OUTPUT_DIR/enumo/all.json     \
    $OUTPUT_DIR/ruler/all.json     \
    $OUTPUT_DIR/no-rules/all.json  \
    > $OUTPUT_DIR/all-all.json

pushd "$OUTPUT_DIR"

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

$MYDIR/src/plot-results.sh "$OUTPUT_DIR"
