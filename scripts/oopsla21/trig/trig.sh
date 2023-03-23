#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "expected a phase to run"
    exit 1
fi

if [ "$1" == "full" ]; then
    echo "Starting consecutive Ruler runs"
    ./$0 1
    ./$0 2
    ./$0 3
    ./$0 4
    ./$0 5
    ./$0 6
    ./$0 7
    ./$0 8
    exit 0
fi

if [ $1 == "1" ]; then
    echo "Running phase 1"
    RUST_LOG=info cargo trig --iters 1 --variables 1 --no-constants-above-iter 0 --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --disabled-ops "tan" --prior-rules ../complex-trig.json --outfile rule1.json 2> ../rule1.log
elif [ $1 == "2" ]; then
    echo "Running phase 2"
    RUST_LOG=info cargo trig --iters 1 --variables 1 --no-constants-above-iter 0 --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --disabled-ops "sin cos" --prior-rules rule1.json --outfile rule2.json 2> ../rule2.log
elif [ $1 == "3" ]; then
    echo "Running phase 3"
    RUST_LOG=info cargo trig --iters 2 --variables 1 --no-constants-above-iter 2 --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules rule2.json --outfile rule3.json 2> ../rule3.log
elif [ $1 == "4" ]; then
    echo "Running phase 4"
    RUST_LOG=info cargo trig --iters 3 --variables 1 --no-constants-above-iter 2 --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules rule3.json --outfile rule4.json 2> ../rule4.log
elif [ $1 == "5" ]; then
    echo "Running phase 5"
    RUST_LOG=info cargo trig --iters 5 --variables 1 --no-constants-above-iter 1 --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --disabled-ops "tan" --prior-rules rule4.json --outfile rule5.json 2> ../rule5.log
elif [ $1 == "6" ]; then
    echo "Running phase 6"
    RUST_LOG=info cargo trig --iters 5 --variables 1 --no-constants-above-iter 1 --eqsat-iter-limit 3 --eqsat-node-limit 5000000 --eqsat-time-limit 150 --prior-rules rule5.json --outfile rule6.json 2> ../rule6.log
elif [ $1 == "7" ]; then
    echo "Running phase 7"
    RUST_LOG=info cargo trig --iters 5 --variables 2 --no-constants-above-iter 1 --eqsat-iter-limit 3 --eqsat-node-limit 5000000 --eqsat-time-limit 150 --disabled-ops "tan" --prior-rules rule6.json --outfile rule7.json 2> ../rule7.log
elif [ $1 == "8" ]; then
    echo "Running phase 8"
    RUST_LOG=info cargo trig --iters 7 --variables 1 --no-constants-above-iter 1 --eqsat-iter-limit 3 --eqsat-node-limit 5000000 --eqsat-time-limit 150 --disabled-ops "tan" --prior-rules rule7.json --outfile rule8.json 2> ../rule8.log
else
    echo "phase $1 undefined"
    exit 1
fi
