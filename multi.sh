#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "expected an iteration to run"
    exit 1
fi

if [ "$1" == "full" ]; then
    echo "Starting consecutive Ruler runs"
    ./$0 2
    ./$0 3
    ./$0 4
    ./$0 5
    ./$0 7
    exit 0
fi

# Problem: Rust doesn't like parsing negative numbers as arguments
# Solution: Prefix with an invalid constant
if [ $1 == "2" ]; then
    echo "Running 2 iteration"
    RUST_LOG="info" cargo rational --iters 2 --variables 3 --num-fuzz 30 --filtered-consts "2 1 0 -1 -2" --outfile iter2.json --do-final-run 2> ../test.txt
elif [ $1 == "3" ]; then
    echo "Running 3 iterations"
    RUST_LOG="info" cargo rational --iters 3 --variables 3 --num-fuzz 30 --no-constants-above-iter 3 --ema-above-iter 3 --filtered-consts "0 1 2" --prior-rules iter2.json --outfile iter3.json --do-final-run 2> ../test.txt
elif [ $1 == "4" ]; then
    echo "Running 4 iterations"
    RUST_LOG="info" cargo rational --iters 4 --variables 3 --num-fuzz 30 --no-constants-above-iter 3 --ema-above-iter 3 --filtered-consts "0 1 2" --disabled-ops "abs ~" --prior-rules iter3.json --outfile iter4.json --do-final-run 2> ../test.txt
elif [ $1 == "5" ]; then
    echo "Running 5 iterations"
    RUST_LOG="info" cargo rational --iters 5 --variables 3 --num-fuzz 30 --no-constants-above-iter 3 --ema-above-iter 3 --filtered-consts "0 1 2" --disabled-ops "abs ~ /" --prior-rules iter4.json --outfile iter5.json --do-final-run 2> ../test.txt
elif [ $1 == "7" ]; then
    echo "Running 7 iterations"
    RUST_LOG="info" cargo rational --iters 7 --variables 2 --num-fuzz 30 --no-constants-above-iter 3 --ema-above-iter 3 --filtered-consts "0 1 2" --disabled-ops "abs ~ - /" --prior-rules iter5.json --outfile iter7.json --do-final-run 2> ../test.txt
else
    echo "rule undefined for iter $1"
    exit 1
fi
