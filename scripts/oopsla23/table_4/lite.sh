#!/usr/bin/env bash

# Start from clean state
rm -rf out/
mkdir out/
rm -rf szalinski/out/
mkdir szalinski/out/

# Copy precomputed rules
cp precomputed.rules szalinski/out/rules.txt

pushd szalinski

# Copy synthesized rules
python3 copy-rules.py

# Run Szalinski eval
make -B out/aec-table2/table2.csv

# Restore changes to Szalinski rules
git restore src/rules.rs

# Generate Table
python3 to_latex.py
pdflatex -output-directory out out/table.tex > /dev/null

popd

cp szalinski/table.pdf out/table.pdf
