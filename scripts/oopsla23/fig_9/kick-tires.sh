#!/bin/bash

# Start from clean slate
rm -rf out/
mkdir out/

pushd megalibm

# Clean megalibm
rm -rf results/

# kick the tires
mkdir results
mkdir results/plots
python graph_against_baseline.py --directory=oopsla23/tool/ &>> ../out/log.txt

cp oopsla23/baseline/style.css results/style.css
cp -r oopsla23/baseline/baseline results/baseline
cp oopsla23/baseline/index.html results/baseline.html
cp oopsla23/renumo_precomputed/renumo_run.html results/renumo_run.html
cp -r oopsla23/renumo_precomputed/generated/ results/generated/

cp oopsla23/precomputed_identities.log results/identities.log

popd

mv megalibm/results/* out/
