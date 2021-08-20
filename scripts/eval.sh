echo "***Starting CVC4 comparison: TABLE 1***"
# cvc4 experiment using existing data.
pushd cvc4-eval
make
popd

echo "***Starting Herbie Eval: FIGURE 7***"
# herbie experiment using existing data.
pushd herbie-rational
plots/plot-results.sh output/ruler-herbie-eval/results/pre-gen-2021-04-13-1331
popd

echo "***Starting Search Parameter Analysis: FIGURE 8, 9***"
# search ablation using existing data.
pushd ablation
./ablation.sh -r use-existing
popd

echo "***Starting eqsat soundness: TABLE 2***"
# eqsat soundness using existing data.
pushd eqsat-sound
python3 tabulate.py output/pre-gen-2021-07-06-2242/all.json

echo "Done"