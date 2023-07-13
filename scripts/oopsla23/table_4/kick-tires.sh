#!/usr/bin/env bash

echo "Starting Szalinski Experiment (Kick Tires)"

# Start from clean state
rm -rf out/
mkdir out/

# Copy precomputed table
cp precomputed.csv szalinski/out/aec-table2/table2.csv

pushd szalinski

# Generate Table
python3 to_latex.py
pdflatex table.tex > /dev/null

popd

cp szalinski/table.pdf out/table.pdf
