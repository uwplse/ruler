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

RESOURCES_DIR="$MYDIR/nightly-resources"
NIGHTLY_DIR="$MYDIR/../nightly"
OUTPUT_DIR="$NIGHTLY_DIR/output"

rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR/data"

cp $RESOURCES_DIR/* "$OUTPUT_DIR"
(echo "var data = "; cat $NIGHTLY_DIR/data/output.json) > $OUTPUT_DIR/data/output.js

cd $OUTPUT_DIR && python3 -m http.server 8002