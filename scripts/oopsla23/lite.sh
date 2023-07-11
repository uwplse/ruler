echo "Starting Lite (All)"

pushd scripts/oopsla23

rm -rf out/
mkdir out/

# Table 2
pushd table_2
./lite.sh
cp out/table.pdf ../out/table2.pdf
popd

# Experiment 2 (6.1.2)
pushd exp_2
./lite.sh
cp out/table.pdf ../out/exp2.pdf
popd

# Table 3
pushd table_3
./lite.sh
cp out/table.pdf ../out/table3.pdf
popd

# Figure 8
pushd fig_8
./lite.sh
cp out/by-config-all-tests-time-boxplot.pdf ../out/fig8_time.pdf
cp out/by-config-all-tests-avg_bits_err_improve-boxplot.pdf ../out/fig8.pdf
popd

# Table 4
pushd table_4
./lite.sh
cp out/table.pdf ../out/table4.pdf
popd

# Table 5
pushd table_5
./lite.sh
cp out/table.pdf ../out/table5.pdf
popd

echo "Done! See scripts/ooopsla23/out for results"

popd
