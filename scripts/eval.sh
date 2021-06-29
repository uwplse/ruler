# dummy script for now.
# cvc4 experiment
cd ..
make
cd scripts

# herbie experiment
pushd herbie-rational
./herbie-eval.sh 30
popd

# search ablation
pushd ablation
./ablation.sh -r generate-new
popd

# eqsat soundness
pushd eqsat-sound
./eqsat-soundness.sh