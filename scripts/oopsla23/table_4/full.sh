#!/usr/bin/env bash


# Start from clean state
rm -rf out/
mkdir out/

# Synthesize rules
RAYON_NUM_THREADS=1 cargo test --release --package ruler --test szalinski -- --ignored --nocapture > szalinski/rules.txt

# Check rule synthesis
if ! [ -f szalinski/rules.txt ]; then
    echo "szalinski/rules.txt not found. Did rule inference not run?"
    exit 1
fi

if ! grep -q "RULER RULES START"szalinski/rules.txt; then
    echo "Rule inference had malformed output!"
    exit 1
fi

pushd szalinski

# Copy synthesized rules
python3 copy-rules.py

# Run Szalinski eval
make -B out/aec-table2/table2.csv

# Generate Table
python3 to_latex.py
pdflatex table.tex > /dev/null

popd

cp szalinski/table.pdf out/table.pdf
