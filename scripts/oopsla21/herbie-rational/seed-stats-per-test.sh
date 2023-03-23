#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"


function gather_timeout {
    # only keep the name of the benchmark
    jq 'map ({ "seed" : .seed , "timeouts" : .timeouts | map (.name)})' \
        timeouts.json > timeouts-aggr.json

    # reshape the json to store name vs number of timeout seeds
    jq 'map(.seed as $seed | .timeouts | map({seed : $seed, name: .})) | 
    flatten | group_by(.name) | map({name : .[0].name, timeouts : map(.seed) | length})' \
        timeouts-aggr.json > timeouts-aggr.json.tmp
    mv timeouts-aggr.json.tmp timeouts-aggr.json
}

function gather_error {
    # only keep the name of the benchmark
    jq 'map ({ "seed" : .seed , "errors" : .errors | map (.name)})' \
        errors.json > errors-aggr.json
    
    # reshape the json to store name vs number of error seeds
    jq 'map(.seed as $seed | .errors | map({seed : $seed, name: .})) | 
    flatten | group_by(.name) | map({name : .[0].name, errors : map(.seed) | length})' \
        errors-aggr.json > errors-aggr.json.tmp
    mv errors-aggr.json.tmp errors-aggr.json
}

function gather_all {
    jq 'group_by(.test) | map ({name : .[0].test, all: map(.seed) | length })' \
        all.json > all-aggr.json
}

function slurp () {
    # slurping happens in order
    jq -s . all-aggr.json errors-aggr.json timeouts-aggr.json > summary-tmp.json
    jq 'flatten | group_by(.name) |
        map({name: .[0].name
        , all: .[0].all
        , errors: map(select(.errors != null)) | .[0].errors
        , timeouts: map(select(.timeouts != null)) | .[0].timeouts})' summary-tmp.json > summary.json
    jq --arg FIELD "$1" 'map( . += {config : $FIELD})' summary.json > "final-${1}.json"
}

configs=("herbie-only" "herbie-ruler" "herbie-no-simpl" "ruler-only")

# you can do all the above for each config
for c in ${configs[@]}; do
    pushd "$1/$c"
    gather_timeout
    gather_error
    gather_all
    slurp "$c"
    popd
done

pushd $1

jq -s . \
    "${configs[0]}/final-${configs[0]}.json" \
    "${configs[1]}/final-${configs[1]}.json" \
    "${configs[2]}/final-${configs[2]}.json" \
    "${configs[3]}/final-${configs[3]}.json" \
    > final.json

cat final.json \
    | jq 'flatten | group_by(.name)' \
    > final-by-test.json.tmp

# for each test, make a data field that will be a list (all, timeout, error, config), one for each config
jq 'map ({name: .[0].name, data : map({all: .all, timeouts: .timeouts, errors: .errors, config: .config}) })' \
    final-by-test.json.tmp > final-by-test.json

rm final-by-test.json.tmp

# number of seeds per test over all four configurations. Note the div by 4.
jq 'group_by(.test) | map ( {name: .[0].test, num_seed: map(.seed) | (length / 4)})' \
    all-good.json > num-seeds-per-test.json

# generate final seed-stats per test
# slurping happens in order
jq -s . num-seeds-per-test.json final-by-test.json > seed-stats.json
jq 'flatten | group_by(.name) | map({name: .[0].name, num_seeds: .[0].num_seed, data: .[1].data})' \
    seed-stats.json > seed-stats-per-test.json

popd

