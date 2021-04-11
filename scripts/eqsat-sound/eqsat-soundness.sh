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

is=2
vs=3

domain=("4" "32" "rational")
numfuzz=("0" "10" "100" "1000")
consts=("1" "2" "3" "4" "5")
# to compare consts vs sampling
default_num_const=("0" "2" "5")
samples=("10" "50" "100")

# domain=("4")
# numfuzz=("100")
# consts=("1")
# # to compare consts vs sampling
# default_num_const=()
# samples=()

# TODO: lkup table

for d in ${domain[@]}; do
    for c in ${consts[@]}; do
       for n in ${numfuzz[@]}; do
           resn="$DIR/$d-const-$c-fuzz-$n"
           mkdir -p "$resn"
           pushd "$resn"
           if [ "$d" = "4" ] || [ "$d" = "32" ]; then
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
                    --num-fuzz "$n"
           fi
           if [ -s out.json ]; then
               "$MYDIR"/postpass.sh out.json "$d"
               unknown=$(cat post_pass.json | jq '.unknown')
               unsound=$(cat post_pass.json | jq '.unsound')
               # post=$("$MYDIR"/postpass.sh out.json $d)
               # unknown="$(echo $post | jq '.unknown')"
               # unsound="$(echo $post | jq '.unsound')"
               cat out.json | \
                    jq --argjson UNKNOWN "$unknown" \
                       --argjson UNSOUND "$unsound" \
                       --argjson DOM "$d" \
                       --argjson CONSTS "$c" \
                       --argjson FUZZ "$n" \
                    '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": 0} +
                     {"fuzz": $FUZZ} + {"smt": "NO"} +
                     {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + {"status": "SUCCESS"} + .' > tmp.json
               mv tmp.json out.json
               echo "Generated $resn/out.json"
           else
               echo "{\"status\" : \"CRASH\"}" > out.json
               cat out.json | \
                    jq --argjson UNKNOWN "0" \
                       --argjson UNSOUND "0" \
                       --argjson DOM "$d" \
                       --argjson CONSTS "$c" \
                       --argjson FUZZ "$n" \
                    '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": 0} +
                     {"fuzz": $FUZZ} + {"smt": "NO"} +
                     {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + .' > tmp.json 
               mv tmp.json out.json
               echo "Generated crashed $resn/out.json"
           fi
           popd
       done
       # smt validation for each domain and cvec config
       res="$DIR/$d-const-$c-smt"
       mkdir -p "$res"
       pushd "$res"
       if [ "$d" = "4" ] || [ "$d" = "32" ]; then
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
                --use-smt
       fi
       # this should always be 0 for sure since the rules are already smt validated.
       if [ -s out.json ]; then
             "$MYDIR"/postpass.sh out.json "$d"
             unknown=$(cat post_pass.json | jq '.unknown')
             unsound=$(cat post_pass.json | jq '.unsound')
        #    post=$("$MYDIR"/postpass.sh out.json $d)
        #    unknown="$(echo $post | jq '.unknown')"
        #    unsound="$(echo $post | jq '.unsound')"
           cat out.json | \
                jq --argjson UNKNOWN "$unknown" \
                   --argjson UNSOUND "$unsound" \
                   --argjson DOM "$d" \
                   --argjson CONSTS "$c" \
                '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": 0} +
                 {"fuzz": 0} + {"smt":  "YES"} +
                 {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + {"status": "SUCCESS"} + .' > tmp.json 
           mv tmp.json out.json
           echo "Generated $res/out.json"
       else
           echo "{\"status\" : \"CRASH\"}" > out.json
           cat out.json | \
                jq --argjson UNKNOWN "0" \
                   --argjson UNSOUND "0" \
                   --argjson DOM "$d" \
                   --argjson CONSTS "$c" \
                '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": 0} +
                 {"fuzz": 0} + {"smt" : "YES"} + 
                 {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + .' > tmp.json 
           mv tmp.json out.json
           echo "Generated crashed $res/out.json"
       fi
       popd
    done

      
    ### SAMPLING BASED CVECS

    # all fuzz configs with cvecs made by cross prod of default_num_const important numbers and samples
    for c in ${default_num_const[@]}; do
        for s in ${samples[@]}; do
            for n in ${numfuzz[@]}; do
                def="$DIR/$d-const-$c-sample-$s-fuzz-$n"
                mkdir -p "$def"
                pushd "$def"
                if [ "$d" = "4" ] || [ "$d" = "32" ]; then
                    cargo run --bin bv"$d" --release -- synth \
                        --iters "$is" \
                        --variables "$vs" \
                        --no-conditionals \
                        --num-fuzz "$n" \
                        --important-cvec-offsets "$c" \
                        --n-samples "$s"
                else
                    cargo run --bin "$d" --release -- synth \
                        --iters "$is" \
                        --variables "$vs" \
                        --num-fuzz "$n" \
                        --important-cvec-offsets "$c" \
                        --n-samples "$s"
                fi
                if [ -s out.json ]; then
                    "$MYDIR"/postpass.sh out.json "$d"
                    unknown=$(cat post_pass.json | jq '.unknown')
                    unsound=$(cat post_pass.json | jq '.unsound')
                    # post=$("$MYDIR"/postpass.sh out.json $d)
                    # unknown="$(echo $post | jq '.unknown')"
                    # unsound="$(echo $post | jq '.unsound')"
                    cat out.json | \
                        jq --argjson UNKNOWN "$unknown" \
                           --argjson UNSOUND "$unsound" \
                           --argjson DOM "$d" \
                           --argjson CONSTS "$c" \
                           --argjson SAMPLES "$s" \
                           --argjson FUZZ "$n" \
                           '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": $SAMPLES} +
                            {"fuzz": $FUZZ} + {"smt" : "NO"} + 
                            {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + {"status": "SUCCESS"} + .' > tmp.json  
                    mv tmp.json out.json
                    echo "Generated $def/out.json"
                else
                    echo "{\"status\" : \"CRASH\"}" > out.json
                    cat out.json | \
                        jq --argjson UNKNOWN "0" \
                           --argjson UNSOUND "0" \
                           --argjson DOM "$d" \
                           --argjson CONSTS "$c" \
                           --argjson SAMPLES "$s" \
                           --argjson FUZZ "$n" \
                           '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": $SAMPLES} +
                            {"fuzz": $FUZZ} + {"smt" : "NO"} + 
                            {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + .' > tmp.json  
                    mv tmp.json out.json
                    echo "Generated crashed $def/out.json"
                fi
                popd
            done

            # smt validation with cvecs made by cross prod of default_num_const important numbers and samples
            defs="$DIR/$d-const-$c-sample-$s-smt"
            mkdir -p "$defs"
            pushd "$defs"
            if [ "$d" = "4" ] || [ "$d" = "32" ]; then
                cargo run --bin bv"$d" --release -- synth \
                    --iters "$is" \
                    --variables "$vs" \
                    --no-conditionals \
                    --use-smt \
                    --important-cvec-offsets "$c" \
                    --n-samples "$s"
            else
                cargo run --bin "$d" --release -- synth \
                    --iters "$is" \
                    --variables "$vs" \
                    --use-smt \
                    --important-cvec-offsets "$c" \
                    --n-samples "$s"
            fi
            if [ -s out.json ]; then
                "$MYDIR"/postpass.sh out.json "$d"
                unknown=$(cat post_pass.json | jq '.unknown')
                unsound=$(cat post_pass.json | jq '.unsound')
                # post=$("$MYDIR"/postpass.sh out.json $d)
                # unknown="$(echo $post | jq '.unknown')"
                # unsound="$(echo $post | jq '.unsound')"
                cat out.json | \
                    jq --argjson UNKNOWN "$unknown" \
                       --argjson UNSOUND "$unsound" \
                       --argjson DOM "$d" \
                       --argjson CONSTS "$c" \
                       --argjson SAMPLES "$s" \
                       '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": $SAMPLES} +
                        {"fuzz": 0} + {"smt" : "YES"} + 
                        {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + {"status": "SUCCESS"} + .' > tmp.json                        
                mv tmp.json out.json
                echo "Generated $defs/out.json"
            else
                echo "{\"status\" : \"CRASH\"}" > out.json
                cat out.json | \
                    jq --argjson UNKNOWN "0" \
                       --argjson UNSOUND "0" \
                       --argjson DOM "$d" \
                       --argjson CONSTS "$c" \
                       --argjson SAMPLES "$s" \
                       '{"domain": $DOM} + {"num_consts": $CONSTS} + {"samples": $SAMPLES} +
                        {"fuzz": 0} + {"smt" : "YES"} + 
                        {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + .' > tmp.json                        
                mv tmp.json out.json
                echo "Generated crashed $defs/out.json"
            fi
            popd
        done
    done
done

$MYDIR/aggregate.sh "$DIR"