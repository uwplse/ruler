echo "Starting BV experiment"

# Clear logs
rm -f log.txt
rm -f output.json
rm -f ../../../nightly/data/output.json

# Generate BV rules and compute derivability
cargo test --release --package ruler --test bv8  -- test::compare --exact --nocapture >> log.txt
cargo test --release --package ruler --test bv16 -- test::compare --exact --nocapture >> log.txt

cp ../../../nightly/data/output.json .

# Generate latex table from output json
node generateLatex.js
pdflatex table.tex


echo "Done! Results are shown in table.pdf"