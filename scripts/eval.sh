echo "Starting CVC4 comparison"
# cvc4 experiment
pushd cvc4-eval
make
popd

echo "Starting Herbie Eval with 15 seeds"
# herbie experiment
pushd herbie-rational
./herbie-eval.sh 15
popd

echo "Starting Search Parameter Analysis"
# search ablation
pushd ablation
./ablation.sh -r generate-new
popd

echo "Starting eqsat soundness"
# eqsat soundness
pushd eqsat-sound
./eqsat-soundness.sh

echo "Done"