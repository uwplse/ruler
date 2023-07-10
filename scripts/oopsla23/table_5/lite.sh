echo "Starting BV experiment (Lite)"

# Start from clean state
rm -rf out/
rm -f ../../../nightly/data/output.json
mkdir out/

# Generate BV rules and compute derivability
cargo test --release --package ruler --test bv8  -- test::compare --exact --nocapture >> out/log.txt

cp ../../../nightly/data/output.json out/

# Generate latex table from output json
node generateLatex.js
pdflatex -output-directory out out/table.tex > /dev/null


if [ -f "out/table.pdf" ]; then
  echo "Done! Results are shown in table.pdf"
fi
