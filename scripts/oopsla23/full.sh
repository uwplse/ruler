echo "Starting Full (All)"

pushd scripts/oopsla23

rm -rf out/
mkdir out/

# Table 2
pushd table_2
./full.sh
cp out/table.pdf ../out/table2.pdf
popd

# Experiment 2 (6.1.2)
pushd exp_2
./full.sh
cp out/table.pdf ../out/exp2.pdf
popd

# Table 3
pushd table_3
./full.sh
cp out/table.pdf ../out/table3.pdf
popd

# Figure 8
pushd fig_8
./full.sh
cp out/by-config-all-tests-time-boxplot.pdf ../out/fig8_time.pdf
cp out/by-config-all-tests-avg_bits_err_improve-boxplot.pdf ../out/fig8.pdf
popd

# Table 5
pushd table_5
./full.sh
cp out/table.pdf ../out/table5.pdf
popd

popd
