echo "Starting Guided Search experiment (Kick Tires)"

# Go to the right directory
pushd scripts/oopsla23/table_2

# Start from clean state
rm -rf out/
mkdir out/

# Use the precomputed data
cp ../precomputed.json out/output.json

# Generate latex table from output json
node generateLatex.js
pdflatex -output-directory out out/table.tex


if [ -f "out/table.pdf" ]; then
  echo "Done! Results are shown in table.pdf"
fi

popd
