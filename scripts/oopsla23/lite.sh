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

# Table 5
pushd table_5
./lite.sh
cp out/table.pdf ../out/table5.pdf
popd

popd
