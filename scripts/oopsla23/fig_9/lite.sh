#!/bin/bash

# for this one, we're running megalibm from pre-set renumo rules
# we want to generate the comparison between baseline and new here.
# so, we want to run the script with locally-generated.
# use make nightly, get all the relevant json, and run the script that generates
# comparison plots. 

# Start from clean slate
rm -rf out/
mkdir out/

# Use precomputed Renumo rules
cp ../precomputed.json megalibm/output.json

pushd megalibm

# Clean megalibm slate
rm -rf results/
mkdir results/
mkdir results/plots

make
make nightly >> run.log 2>&1
cat run.log | awk '/we used the following/{flag=1} /megalibm_generate/{flag=0} flag' > results/identities.log

# now run comparison 
python graph_against_baseline.py --directory=results/run/generated/ &>> ../out/log.txt

cp -r results/run/generated results/generated
cp results/run/index.html results/renumo_run.html
cp results/run/style.css results/style.css

cp -r oopsla23/baseline/baseline results/baseline
cp oopsla23/baseline/index.html results/baseline.html

popd

mv megalibm/results/* out/
