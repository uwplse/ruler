echo "Starting Herbie Experiment (Full)"

# Start from clean state
rm -rf out/
mkdir out/
rm -f ../../../nightly/data/output.json
rm -rf reports/
mkdir reports/

# Generate rules and compute derivability
cargo test --release --package ruler --test bool        -- test::run --exact --nocapture >> out/log.txt
cargo test --release --package ruler --test exponential -- test::run --exact --nocapture >> out/log.txt
cargo test --release --package ruler --test rational    -- test::run --exact --nocapture >> out/log.txt
cargo test --release --package ruler --test trig        -- test::run --exact --nocapture >> out/log.txt
mkdir rules/
cp ../../../nightly/data/output.json rules/

# Regenerate data for 30 seeds
bash eval.sh 30 hamming mathematics numerics physics pbrt.fpcore &>> out/log.txt

cp reports/*/*.pdf out/
