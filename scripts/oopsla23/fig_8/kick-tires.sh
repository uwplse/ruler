echo "Starting Herbie experiment (Kick Tires)"

# Start from clean state
rm -rf out/
mkdir out/

# Regenerate plots using precomputed data
./plot.sh saved/ >> out/log.txt
cp saved/*.pdf out/
