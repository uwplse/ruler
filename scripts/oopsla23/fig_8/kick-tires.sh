echo "Starting Herbie experiment (Kick Tires)"

# Start from clean state
rm -rf out/
mkdir out/
rm -rf saved/
mkdir saved/

# Regenerate plots using precomputed data
./plot.sh saved/