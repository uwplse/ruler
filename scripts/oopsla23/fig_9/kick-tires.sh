#!/bin/bash

pushd megalibm

# kick the tires
mkdir results
mkdir results/plots
python graph_against_baseline.py --directory=oopsla23/tool/

cp oopsla23/baseline/style.css results/style.css
cp -r oopsla23/baseline/baseline results/baseline
cp oopsla23/baseline/index.html results/baseline.html

popd
