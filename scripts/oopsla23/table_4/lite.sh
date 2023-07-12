#!/usr/bin/env bash

# Start from clean state
rm -rf out/
mkdir out/

# Copy precomputed rules
cp precomputed.rules szalinski/rules.txt

pushd szalinski

# Copy synthesized rules
python3 copy-rules.py

# Run Szalinski eval
make -B out/aec-table2/table2.csv

# Generate Table
python3 to_latex.py
pdflatex table.tex > /dev/null

# Clean up Szalinski repo
git restore .
git clean -f

popd

cp szalinski/table.pdf out/table.pdf
