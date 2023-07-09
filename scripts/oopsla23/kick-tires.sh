echo "Starting Kick Tires (All)"

pushd scripts/oopsla23

rm -rf out/
mkdir out/

# Table 2
pushd table_2
./kick-tires.sh
cp out/table.pdf ../out/table2.pdf
popd

# Experiment 2 (6.1.2)
pushd exp_2
./kick-tires.sh
cp out/table.pdf ../out/exp2.pdf
popd

# Table 3
pushd table_3
./kick-tires.sh
cp out/table.pdf ../out/table3.pdf
popd

# Table 5
pushd table_5
./kick-tires.sh
cp out/table.pdf ../out/table5.pdf
popd

popd
