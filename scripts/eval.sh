echo "Starting CVC4 comparison"
# cvc4 experiment using existing data.
pushd cvc4-eval
make > table.txt
popd

# echo "Starting Herbie Eval with 1 seed"
# # herbie experiment for 1 seed.
# pushd herbie-rational
# ./herbie-eval.sh 15
# popd

echo "Starting Search Parameter Analysis"
# search ablation using existing data.
pushd ablation
./ablation.sh -r use-existing
popd

echo "Starting eqsat soundness"
# eqsat soundness using existing data.
pushd eqsat-sound
python3 tabulate.py output/pre-gen-2021-07-06-2242/all.json

echo "Done"