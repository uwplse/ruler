echo "Starting Herbie Experiment (Lite)"

# Start from clean state
rm -rf out/
mkdir out/
rm -rf reports/
mkdir reports/

# Copy in rules
mkdir -f rules/
cp ../precomputed.json rules/output.json

# Regenerate data for a single seed
bash eval.sh 1 hamming

cp reports/*/*.pdf out/
