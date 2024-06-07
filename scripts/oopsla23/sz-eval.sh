#!/usr/bin/env bash

# Synthesize rules
cargo test --package ruler --test szalinski -- --ignored --nocapture > szalinski/rules.txt

# Check rule synthesis
if ! test -f szalinski/rules.txt; then
    echo "szalinski/rules.txt not found. Did rule inference not run?"
    exit 1
fi

if ! grep -q "RULER RULES START" szalinski/rules.txt; then
    echo "Rule inference had malformed output!"
    exit 1
fi

cd szalinski

# Copy synthesized rules
python3 copy-rules.py

# Run Szalinski eval
make -B out/aec-table2/table2.csv

# Print results
python3 to_latex.py
