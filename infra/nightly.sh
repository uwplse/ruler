#!/bin/bash
# The primary purpose of this script is to run all the tests and upload 
# the results to the nightly-results server. It also generates some HTML
# to display the equiderivability tests in a nicely-formatted way.

echo "Beginning Ruler nightly script..."

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# Absolute directory paths
TOP_DIR="$MYDIR/.."
RESOURCE_DIR="$MYDIR/nightly-resources"
NIGHTLY_DIR="$TOP_DIR/nightly"

# Make sure we're in the right place
cd $MYDIR
echo "Switching to nighly script directory: $MYDIR"

# Clean previous nightly run
# CAREFUL using -f
rm -rf $NIGHTLY_DIR

# Prepare output directories
mkdir -p "$NIGHTLY_DIR/data" "$NIGHTLY_DIR/output"

# Run tests.
pushd $TOP_DIR
RUST_TEST_THREADS=1 cargo test --release -- --nocapture > log.txt

# Copy log
cp log.txt "$NIGHTLY_DIR/output"
popd

# Update HTML index page.
cp "$RESOURCE_DIR"/* "$NIGHTLY_DIR/output"

# Put the json data in a JS object for consumption by frontend
(echo "var data = "; cat "$NIGHTLY_DIR/data/output.json") > "$NIGHTLY_DIR/data/output.js"
# Copy json directory to the artifact
cp -r "$NIGHTLY_DIR/data" "$NIGHTLY_DIR/output/data"

# This is the uploading part, copied directly from Herbie's nightly script.
DIR="$NIGHTLY_DIR/output"
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="$(date +%s):$(hostname):$B:$C"

# Upload the artifact!
nightly-results publish --name "$RDIR" "$DIR"
