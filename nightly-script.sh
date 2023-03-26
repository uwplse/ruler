#!/bin/bash
# The primary purpose of this script is to run all the tests and upload 
# the results to the nightly-results server. It also generates some HTML
# to display the equiderivability tests in a nicely-formatted way.

# Clean output directories
rm -rf rep/json rep/output

# Prepare output directories
mkdir -p rep/output
mkdir -p rep/json

# Run tests.
RUST_TEST_THREADS=1 cargo test --release

# Update HTML index page.
cp rep/*.js rep/output
python3 generatehtml.py

# Move json
mv rep/json rep/output/json

# This is the uploading part, copied directly from Herbie's nightly script.
DIR="rep/output"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

nightly-results publish --name "$RDIR" "$DIR"
