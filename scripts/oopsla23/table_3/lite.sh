echo "Starting Fast-Forwarding experiment (Lite)"

# Go to the right directory
pushd scripts/oopsla23/table_2

# Start from clean state
rm -rf out/
rm -f ../../../nightly/data/output.json
mkdir out/

# Generate rules and compute derivability
cargo test --release --package ruler --test exponential -- test::run --exact --nocapture >> out/log.txt

cp ../../../nightly/data/output.json out/

# Generate latex table from output json
node generateLatex.js
pdflatex -output-directory out out/table.tex


if [ -f "out/table.pdf" ]; then
  echo "Done! Results are shown in table.pdf"
fi

popd
