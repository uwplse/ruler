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
numfuzz=("0" "10" "100" "1000" "smt")
consts=("2" "3" "5")
# actual lengths of the cvecs, used for the sampling configs
bv32_cvec=("343" "1331" "6859")
bv4_cvec=("343" "1331" "4096")
rat_cvec=("27" "125" "729")

# domain cvec-offset fuzz
function run_bv_const () {
    if [ "$3" = "smt" ]; then
        if [ "$1" = "4" ] || [ "$1" = "32" ]; then
            cargo run --bin bv"$1" --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --important-cvec-offsets "$2" \
                --no-conditionals \
                --use-smt \
                --do-final-run
        # only showing this for bv32
        elif [ "$1" = "less-const" ]; then
             cargo run --bin bv32 --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --no-conditionals \
                --important-cvec-offsets "$2" \
                --use-smt \
                --no-constants-above-iter 1 \
                --do-final-run
        else
            echo "Only bv4, bv32, and bv32 with less constants supported."
        fi
    else
        if [ "$1" = "4" ] || [ "$1" = "32" ]; then
            cargo run --bin bv"$1" --release -- synth \
                 --iters "$is" \
                 --variables "$vs" \
                 --important-cvec-offsets "$2" \
                 --no-conditionals \
                 --num-fuzz "$3" \
                 --do-final-run
        # only showing this for bv32
        elif [ "$1" = "less-const" ]; then
            cargo run --bin bv32 --release -- synth \
                 --iters "$is" \
                 --variables "$vs" \
                 --important-cvec-offsets "$2" \
                 --no-conditionals \
                 --num-fuzz "$3" \
                 --no-constants-above-iter 1 \
                 --do-final-run
        else
            echo "Only bv4, bv32, and bv32 with less constants supported."
        fi
    fi
}

# domain samples fuzz
function run_bv_sampled () {
    if [ "$3" = "smt" ]; then
        if [ "$1" = "4" ] || [ "$1" = "32" ]; then
            cargo run --bin bv"$1" --release -- synth \
                 --iters "$is" \
                 --variables "$vs" \
                 --n-samples "$2" \
                 --important-cvec-offsets 0 \
                 --no-conditionals \
                 --use-smt \
                 --do-final-run
        # only showing this for bv32
        elif [ "$1" = "less-const" ]; then
            cargo run --bin bv32 --release -- synth \
                 --iters "$is" \
                 --variables "$vs" \
                 --n-samples "$2" \
                 --important-cvec-offsets 0 \
                 --no-conditionals \
                 --use-smt \
                 --no-constants-above-iter 1 \
                 --do-final-run
        else
            echo "Only bv4, bv32, and bv32 with less constants supported."
        fi
    else
        if [ "$1" = "4" ] || [ "$1" = "32" ]; then
            cargo run --bin bv"$1" --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --n-samples "$2" \
                --important-cvec-offsets 0 \
                --no-conditionals \
                --num-fuzz "$3" \
                --do-final-run
        # only showing this for bv32
        elif [ "$1" = "less-const" ]; then
            cargo run --bin bv32 --release -- synth \
                --iters "$is" \
                --variables "$vs" \
                --n-samples "$2" \
                --important-cvec-offsets 0 \
                --no-conditionals \
                --num-fuzz "$3" \
                --no-constants-above-iter 1 \
                --do-final-run
        else
            echo "Only bv4, bv32, and bv32 with less constants supported."
        fi
    fi
}

# cvec-offset, fuzz
function run_rats_const () {
    if [ "$2" == "smt" ]; then
        cargo run --bin rational --release -- synth \
        --iters "$is" \
        --variables "$vs" \
        --important-cvec-offsets "$1" \
        --use-smt \
        --do-final-run
    else
        cargo run --bin rational --release -- synth \
        --iters "$is" \
        --variables "$vs" \
        --important-cvec-offsets "$1" \
        --num-fuzz "$2" \
        --do-final-run
    fi
}

# samples, fuzz
function run_rats_sampled () {
    if [ "$2" == "smt" ]; then
        cargo run --bin rational --release -- synth \
        --iters "$is" \
        --variables "$vs" \
        --n-samples "$1" \
        --important-cvec-offsets 0 \
        --use-smt \
        --do-final-run
    else
        cargo run --bin rational --release -- synth \
        --iters "$is" \
        --variables "$vs" \
        --n-samples "$1" \
        --important-cvec-offsets 0 \
        --num-fuzz "$2" \
        --do-final-run
    fi
}

# path domain const samples fuzz 
function mk_report () {
    if [ "$2" = "less-const" ]; then
        DM="32"
    else
        DM="$2"
    fi
    if [ -s "$1/out.json" ]; then
        "$MYDIR"/postpass.sh "$1/out.json" "$DM"
        unknown=$(cat post_pass.json | jq '.unknown')
        unsound=$(cat post_pass.json | jq '.unsound')
        echo "$unknown"
        echo "$unsound"
        echo "$2"
        echo "$3"
        echo "$4"
        echo "$5"
        cat "$1/out.json" | \
             jq --argjson UNKNOWN "$unknown" \
                --argjson UNSOUND "$unsound" \
                --arg DOM "$2" \
                --argjson CONSTS "$3" \
                --argjson SAMPLES "$4" \
                --arg FUZZ "$5" \
             '{"domain": $DOM} + {"num_consts": $CONSTS} +
              {"samples": $SAMPLES} + {"fuzz": $FUZZ} +
              {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + {"status": "SUCCESS"} + .' > "$1/tmp.json"
        mv "$1/tmp.json" "$1/out.json"
        echo "Generated success $1/out.json"
    else
        echo "{\"status\" : \"CRASH\"}" > "$1/out.json"
        cat "$1/out.json" | \
             jq --argjson UNKNOWN "0" \
                --argjson UNSOUND "0" \
                --arg DOM "$2" \
                --argjson CONSTS "$3" \
                --argjson SAMPLES "$4" \
                --arg FUZZ "$5" \
             '{"domain": $DOM} + {"num_consts": $CONSTS} +
              {"samples": $SAMPLES} + {"fuzz": $FUZZ} +
              {"unsound": $UNSOUND} + {"unknown": $UNKNOWN} + .' > "$1/tmp.json" 
        mv "$1/tmp.json" "$1/out.json"
        echo "Generated crashed $1/out.json"
    fi
}

# const based cvec
for d in ${domain[@]}; do
    for n in ${numfuzz[@]}; do
        for c in ${consts[@]}; do
            res1="$DIR/$d-fuzz-$n-const-$c"
            mkdir -p "$res1"
            pushd "$res1"
            if [ "$d" = "rational" ]; then
                run_rats_const "$c" "$n"
            else
                run_bv_const "$d" "$c" "$n"
            fi
            mk_report "$res1" "$d" "$c" "0" "$n"
           popd
        done
    done
done

# sampling based cvec
for d in ${domain[@]}; do
    if [ "$d" = "32" ] ||  [ "$d" = "less-const" ]; then
        for s in ${bv32_cvec[@]}; do
            for n in ${numfuzz[@]}; do
                res1="$DIR/$d-fuzz-$n-samples-$s"
                mkdir -p "$res1"
                pushd "$res1"
                run_bv_sampled "$d" "$s" "$n"
                mk_report "$res1" "$d" "0" "$s" "$n"
                popd
            done
        done
    elif [ "$d" = "4" ]; then
        for s in ${bv4_cvec[@]}; do
            for n in ${numfuzz[@]}; do
                res2="$DIR/$d-fuzz-$n-samples-$s"
                mkdir -p "$res2"
                pushd "$res2"
                run_bv_sampled "$d" "$s" "$n"
                mk_report "$res2" "$d" "0" "$s" "$n"
                popd
            done
        done
    elif [ "$d" = "rational" ]; then
        for s in ${rat_cvec[@]}; do
            for n in ${numfuzz[@]}; do
                res3="$DIR/rational-fuzz-$n-samples-$s"
                mkdir -p "$res3"
                pushd "$res3"
                run_rats_sampled "$s" "$n"
                mk_report "$res3" "rational" "0" "$s" "$n"
                popd
            done
        done
    else
        echo "Domain not supported."
    fi
done

$MYDIR/aggregate.sh "$DIR"
