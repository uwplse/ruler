#!/usr/bin/env bash

# using infra from Zach's scripts

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# caller should pass path to output from sampler
cd "$1"

if [ ! -f "all-good.json" ]; then
  echo "ERROR: no 'all-good.json' in '$1'"
  exit 1
fi


# munge and plot results
jq 'group_by(.seed)' all-good.json > by-seed.json
jq 'group_by(.test)' all-good.json > by-test.json
jq 'group_by(.config)' all-good.json > by-config.json

# group by config, then by seed, and then sum the field over all tests
function gen-config-seed-field {
  field="$1"

  cat by-config.json \
    | jq --arg FIELD "$field" \
        'map({ "config": .[0].config
             , "data": group_by(.seed) | map(map(.[$FIELD]) | add)
             })' \
    > "by-config-seed-${field}.json"

  python3 "$MYDIR/config-all-tests-box-plot.py" "by-config-seed-${field}.json" "$field"
}

# group by config, then by test, but do NOT add field value over seeds
function gen-config-test-field {
  field="$1"

  cat by-config.json \
    | jq --arg FIELD "$field" \
        'map({ config : .[0].config
             , tests : group_by(.test) |
                map ({ name : .[0].test
                     , data : map(.[$FIELD])})
            })' \
    > "by-config-test-${field}.json"

  python3 "$MYDIR/config-per-test-box-plot.py" "by-config-test-${field}.json" "$field"
  python3 "$MYDIR/config-per-test-bar.py" "by-config-test-${field}.json" "$field"
}

function gen-seed-field {
  field="$1"

  cat by-seed.json \
    | jq --arg FIELD "$field" \
        'map({ "seed": .[0].seed
             , "data": map(.[$FIELD]) | add
             })' \
    > "by-seed-${field}.json"
}

function gen-test-field {
  field="$1"

  cat by-test.json \
    | jq --arg FIELD "$field" \
        'map({ "test": .[0].test
             , "data": map(.[$FIELD])
             })' \
    > "by-test-${field}.json"
}

fields="
  output_parens
  avg_bits_err_improve
  time"

for f in $fields; do
  gen-config-seed-field "$f"
done

# for f in $fields; do
#   gen-config-test-field "$f"
# done

for f in $fields; do
  gen-seed-field "$f"
done

for f in $fields; do
  gen-test-field "$f"
done
