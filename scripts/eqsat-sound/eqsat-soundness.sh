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
domain=("4" "32" "rational")
numfuzz=("0" "10" "100" "1000")
consts=("1" "2" "3" "4" "5")

default_num_const=5
samples=10

# TODO: lkup table

for d in ${domain[@]}; do
    for c in ${consts[@]}; do
       for n in ${numfuzz[@]}; do
           resn="$DIR/$d-const-$c-fuzz-$n"
           mkdir -p "$resn"
           pushd "$resn"
           if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
               cargo run --bin bv"$d" --release -- synth \
                    --iters "$is" \
                    --variables "$vs" \
                    --important-cvec-offsets "$c" \
                    --no-conditionals \
                    --num-fuzz "$n"
           else
               cargo run --bin "$d" --release -- synth \
                    --iters "$is" \
                    --variables "$vs" \
                    --important-cvec-offsets "$c" \
                    --no-conditionals \
                    --num-fuzz "$n"
           fi
           if [ -s out.json ]; then
               post=$("$MYDIR"/postpass.sh out.json $d)
               unknown=$(echo $post | jq '.unknown')
               unsound=$(echo $post | jq '.unsound')
               cat out.json | \
                    jq --arg UNKNOWN "$unknown" --arg UNSOUND "$unsound" --arg DOM "$d" \
                    '. + {"domain": $DOM} + {"unsound": $UNSOUND} + {"unknown": $UNKNOWN}' > tmp.json
               mv tmp.json out.json
               echo "Generated $resn/out.json"
           else
               touch out.json
               echo "Generated empty $resn/out.json"
           fi
           popd
       done
       # smt validation for each domain and cvec config
       res="$DIR/$d-const-$c/smt"
       mkdir -p "$res"
       pushd "$res"
       if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
           cargo run --bin bv"$d" --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --important-cvec-offsets "$c" \
                --no-conditionals \
                --use-smt
       else
           cargo run --bin "$d" --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --important-cvec-offsets "$c" \
                --no-conditionals \
                --use-smt
       fi
       # this should always be 0 for sure since the rules are already smt validated.
       if [ -s out.json ]; then
           post=$("$MYDIR"/postpass.sh out.json $d)
           unknown=$(echo $post | jq '.unknown')
           unsound=$(echo $post | jq '.unsound')
           cat out.json | \
                jq --arg UNKNOWN "$unknown" --arg UNSOUND "$unsound" --arg DOM "$d" \
                '. + {"domain": $DOM} + {"unsound": $UNSOUND} + {"unknown": $UNKNOWN}' > tmp.json
           mv tmp.json out.json
           echo "Generated $res/out.json"
       else
           touch out.json
           echo "Generated empty $res/out.json"
       fi
       popd
    done

    # all fuzz configs with cvecs made by cross prod of default_num_const important numbers and samples
    for n in ${numfuzz[@]}; do
        def="$DIR/$d/sample-$samples-const-$default_num_const/$n"
        mkdir -p "$def"
        pushd "$def"
        if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
            cargo run --bin bv"$d" --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --no-conditionals \
                --num-fuzz "$n" \
                --important-cvec-offsets "$default_num_const" \
                --n-samples "$samples"
        else
            cargo run --bin "$d" --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --no-conditionals \
                --num-fuzz "$n" \
                --important-cvec-offsets "$default_num_const" \
                --n-samples "$samples"
        fi
        if [ -s out.json ]; then
            post=$("$MYDIR"/postpass.sh out.json $d)
            unknown=$(echo $post | jq '.unknown')
            unsound=$(echo $post | jq '.unsound')
            cat out.json | \
                 jq --arg UNKNOWN "$unknown" --arg UNSOUND "$unsound" --arg DOM "$d" \
                 '. + {"domain": $DOM} + {"unsound": $UNSOUND} + {"unknown": $UNKNOWN}' > tmp.json            
            mv tmp.json out.json
            echo "Generated $def/out.json"
        else
            touch out.json
            echo "Generated empty $def/out.json"
        fi
        popd
    done

    # smt validation with cvecs made by cross prod of default_num_const important numbers and samples
    defs="$DIR/$d/sample-$samples-const-$default_num_const/smt"
    mkdir -p "$defs"
    pushd "$defs"
    if [ "$d" -eq "4" ] || [ "$d" -eq "32" ]; then
        cargo run --bin bv"$d" --release -- synth \
            --iters "$is" \
            --variables "$vs" \
            --no-conditionals \
            --use-smt \
            --important-cvec-offsets "$default_num_const" \
            --n-samples "$samples"
    else
        cargo run --bin "$d" --release -- synth \
            --iters "$is" \
            --variables "$vs" \
            --no-conditionals \
            --use-smt \
            --important-cvec-offsets "$default_num_const" \
            --n-samples "$samples"
    fi
    if [ -s out.json ]; then
        post=$("$MYDIR"/postpass.sh out.json $d)
        unknown=$(echo $post | jq '.unknown')
        unsound=$(echo $post | jq '.unsound')
        cat out.json | \
             jq --arg UNKNOWN "$unknown" --arg UNSOUND "$unsound" --arg DOM "$d" \
             '. + {"domain": $DOM} + {"unsound": $UNSOUND} + {"unknown": $UNKNOWN}' > tmp.json
        mv tmp.json out.json
        echo "Generated $defs/out.json"
    else
        touch out.json
        echo "Generated empty $defs/out.json"
    fi
    popd
done

pushd "$DIR"
# process all the jsons and populate a top-level all.json file
echo "[ " > all.json
first=true

crash="crash"
success="success"

echo "Generating aggregate json"
for out in $(find . -name 'out.json' | sort); do
    if $first; then
      first=false
    else
      echo "," >> all.json
    fi
    # for Ruler crashes
    if [ -s "$out" ]; then
        cat "$out" \
            | jq --arg SUCCESS "$success" \
                '{"status": $SUCCESS, "domain" : .domain, "time": .time, 
                  "num_rules": .num_rules, "post_unsound": .unsound,
                  "post_unknown": .ununknown, "params": .params}' \
            >> all.json
    else
        jq --arg CRASH "$crash" \
            '{"status": $CRASH, "domain" : .domain, "time": 0.0,
              "num_rules": 0, "post_unsound": 0,
              "post_unknown": 0, "params": {} }' >> all.json
    fi
done

echo "]" >> all.json
popd