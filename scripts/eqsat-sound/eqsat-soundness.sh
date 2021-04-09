#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

tstamp="$(date "+%Y-%m-%d_%H%M")"
DIR="$MYDIR/output/$tstamp"
mkdir -p "$DIR"

#TODO: also do 16 bit bv?
is=2
vs=3
# domain=("4" "32" "rational")
domain=("4" "32")
numfuzz=("0" "10" "100" "1000" "10000")
cvec=("cvec-none" "only-consts" "only-rand" "cross-prod" "rand-and-consts")

for d in ${domain[@]}; do
    for c in ${cvec[@]}; do
       for n in ${numfuzz[@]}; do
           resn="$DIR/$d-$c-$n"
           mkdir -p "$resn"
           pushd "$resn"
           if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
               cargo run --bin bvz3_"$d" --release -- synth --iters "$is" --variables "$vs" --"$c" --no-conditionals --num-fuzz $n
           else
               cargo run --bin "$d" --release -- synth --iters "$is" --variables "$vs" --"$c" --no-conditionals --num-fuzz $n
           fi
           post=$("$MYDIR"/postpass.sh out.json $d)
           cat out.json | jq --argjson POST "$post" --argjson DOM "$d" '. + {"domain": $DOM} + {"post_pass": $POST}' > tmp.json
           mv tmp.json out.json
           popd
       done
       # smt validation for each domain and cvec config
       res="$DIR/$d-$c"
       mkdir -p "$res"
       pushd "$res"
       if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
           cargo run --bin bvz3_"$d" --release -- synth --iters "$is" --variables "$vs" --"$c" --no-conditionals --use-smt
       else
           cargo run --bin "$d" --release -- synth --iters "$is" --variables "$vs" --"$c" --no-conditionals --use-smt
       fi
       # this should always be 0 for sure since the rules are already smt validated.
       post=$("$MYDIR"/postpass.sh out.json $d)
       cat out.json | jq --argjson POST "$post" --argjson DOM "$d" '. + {"domain": $DOM} + {"post_pass": $POST}' > tmp.json
       mv tmp.json out.json
       popd
    done

    # all fuzz configs with default cvec config, i.e., rand_and_cross
    for n in ${numfuzz[@]}; do
        def="$DIR/$d/rand-cross/$n"
        mkdir -p "$def"
        pushd "$def"
        if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
            cargo run --bin bvz3_"$d" --release -- synth --iters "$is" --variables "$vs" --no-conditionals --num-fuzz $n
        else
            cargo run --bin "$d" --release -- synth --iters "$is" --variables "$vs" --no-conditionals --num-fuzz $n
        fi
        post=$("$MYDIR"/postpass.sh out.json $d)
        cat out.json | jq --argjson POST "$post" --argjson DOM "$d" '. + {"domain": $DOM} + {"post_pass": $POST}' > tmp.json
        mv tmp.json out.json
        popd
    done

    # smt validation with default cvec config, i.e., rand_and_cross
    defs="$DIR/$d/rand-cross/smt"
    mkdir -p "$defs"
    pushd "$defs"
    if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
        cargo run --bin bvz3_"$d" --release -- synth --iters "$is" --variables "$vs" --no-conditionals --use-smt
    else
        cargo run --bin "$d" --release -- synth --iters "$is" --variables "$vs" --no-conditionals --use-smt
    fi
    post=$("$MYDIR"/postpass.sh out.json $d)
    cat out.json | jq --argjson POST "$post" --argjson DOM "$d" '. + {"domain": $DOM} + {"post_pass": $POST}' > tmp.json
    mv tmp.json out.json
    popd
done

pushd "$DIR"
# process all the jsons and populate a top-level all.json file
echo "[ " > all.json
first=true

for out in $(find . -name 'out.json' | sort); do
    if $first; then
      first=false
    else
      echo "," >> all.json
    fi
    # for Ruler crashes
    if [ -s "$out" ]; then
        jq '{"status": CRASHED, "domain" : .domain, "time": 0.0, "num_rules": 0.0, "post_unsound": 0, "params": {} }' >> all.json 
    else 
        cat "$out" \
            | jq '{status: SUCCESS, "domain" : .domain, "time": .time, "num_rules": .num_rules, "post_unsound": .post_pass, "params": .params}' \
            >> all.json
    fi
done

echo "]" >> all.json
popd