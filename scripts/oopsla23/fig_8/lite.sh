echo "Starting Herbie Experiment (Lite)"

# Start from clean state
rm -rf out/
mkdir out/
rm -rf reports/
mkdir reports/
rm -rf rules/
mkdir rules/

# Copy in rules
cp ../precomputed.json rules/output.json

# Regenerate data for a single seed
bash eval.sh 5 hamming >> out/log.txt

cp reports/*/*.pdf out/
