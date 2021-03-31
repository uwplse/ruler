

# TODO replace bool etc with a parameter
# TODO properly parameterize the number of iterations

export RUST_LOG="ruler=info,egg=warn";

#num_runs
# num_iterations
# domain 
while getopts "i:r:d:o:" OPTION
do 
    case "$OPTION" in 
        i) NUM_ITERS="$OPTARG" ;;
        r) NUM_RUNS="$OPTARG" ;;
        d) DOMAIN="$OPTARG" ;;
        o) OUTPUT_DIR="$OPTARG" ;;
        \?) break ;;
    esac
done

if [ -z "${NUM_RUNS:-}" ] ; then
    echo "Running with num_runs = 5 (-r 5)"
    NUM_RUNS=5
    NUM_ITERS=3
fi

if [ -z "${NUM_ITERS:-}" ] ; then
    echo "Running with num_iters = 3 (-i 3)"
    NUM_ITERS=3
fi

if [ -z "${OUTPUT_DIR:-}" ] ; then
    echo "Setting output directory to 'ablation'"
    OUTPUT_DIR="ablation"
fi

if [ -z "${DOMAIN:-}" ]; then
    echo "Please specify a domain with -d"
    exit 1
fi

mkdir -p "$OUTPUT_DIR/mrat";
mkdir -p "$OUTPUT_DIR/orat";
mkdir -p "$OUTPUT_DIR/minimize";
mkdir -p "$OUTPUT_DIR/phase-times";
mkdir -p "$OUTPUT_DIR/no-run-rewrites";

echo "Running orat..."
cp ablation/lib.rs src/lib.rs
for (( i=0; i<$NUM_RUNS; i++ ))
do
  (time cargo $DOMAIN --variables 3 --iters $NUM_ITERS --rules-to-take 1 --minimize false) &> "$OUTPUT_DIR/orat/${DOMAIN}_3-${NUM_ITERS}_$i.log" 
done

echo "Running mrat..."
for r in 5 10 15 25 50 100 
do
    for (( i=0; i<$NUM_RUNS; i++ ))
    do
        (time cargo $DOMAIN --variables 3 --iters $NUM_ITERS --rules-to-take $r --minimize false) &> "$OUTPUT_DIR/mrat/${DOMAIN}_3-${NUM_ITERS}_$r-$i.log" 
    done
done

echo "Running minimize and phase-times..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  (time cargo $DOMAIN --variables 3 --iters $NUM_ITERS --minimize true) &> "$OUTPUT_DIR/minimize/${DOMAIN}_3-${NUM_ITERS}_$i.log" 
  # it's the same but save it in a different location
  (time cargo $DOMAIN --variables 3 --iters $NUM_ITERS --minimize true) &> "$OUTPUT_DIR/phase-times/${DOMAIN}_3-${NUM_ITERS}_$i.log" 
done

echo "Running no run-rewrites..."
cp ablation/lib_no_run_rewrites.rs src/lib.rs
for (( i=0; i<$NUM_RUNS; i++ ))
do
  (time cargo $DOMAIN --variables 3 --iters $NUM_ITERS --minimize true) &> "$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_3-${NUM_ITERS}_$i.log" 
done

#OLD MANUAL SCRIPT

# cp main_mrat.rs ../src/main.rs
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bool_3-3_0.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bool_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bool_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bool_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bool_3-3_4.log

# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bool_3-3_5-0.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bool_3-3_5-1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bool_3-3_5-2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bool_3-3_5-3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bool_3-3_5-4.log

# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bool_3-3_15-0.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bool_3-3_15-1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bool_3-3_15-2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bool_3-3_15-3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bool_3-3_15-4.log

# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bool_3-3_25-0.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bool_3-3_25-1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bool_3-3_25-2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bool_3-3_25-3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bool_3-3_25-4.log

# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bool_3-3_50-0.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bool_3-3_50-1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bool_3-3_50-2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bool_3-3_50-3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bool_3-3_50-4.log

# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bool_3-3_100-0.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bool_3-3_100-1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bool_3-3_100-2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bool_3-3_100-3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bool_3-3_100-4.log

# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4_3-3_0.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4_3-3_4.log

# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/mrat/bv4_3-3_0.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/mrat/bv4_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/mrat/bv4_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/mrat/bv4_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bv4 --variables 3 --iters 3 --rules-to-take 1) &> ablation/mrat/bv4_3-3_4.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4ns_3-3_0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4ns_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4ns_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4ns_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 1) &> ablation/orat/bv4ns_3-3_4.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bv4ns_3-3_5-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bv4ns_3-3_5-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bv4ns_3-3_5-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bv4ns_3-3_5-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 5) &> ablation/mrat/bv4ns_3-3_5-4.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bv4ns_3-3_15-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bv4ns_3-3_15-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bv4ns_3-3_15-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bv4ns_3-3_15-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 15) &> ablation/mrat/bv4ns_3-3_15-4.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bv4ns_3-3_25-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bv4ns_3-3_25-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bv4ns_3-3_25-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bv4ns_3-3_25-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 25) &> ablation/mrat/bv4ns_3-3_25-4.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bv4ns_3-3_50-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bv4ns_3-3_50-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bv4ns_3-3_50-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bv4ns_3-3_50-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 50) &> ablation/mrat/bv4ns_3-3_50-4.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bv4ns_3-3_100-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bv4ns_3-3_100-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bv4ns_3-3_100-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bv4ns_3-3_100-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3 --rules-to-take 100) &> ablation/mrat/bv4ns_3-3_100-4.log


# need to replace mrat() with run()
# cp main_minimize.rs ../src/main.rs
# cp lib.rs ../src/lib.rs
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/minimize/bool_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/minimize/bool_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/minimize/bool_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/minimize/bool_3-3_4.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/minimize/bool_3-3_0.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/minimize/bv4ns_3-3-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/minimize/bv4ns_3-3-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/minimize/bv4ns_3-3-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/minimize/bv4ns_3-3-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/minimize/bv4ns_3-3-4.log


# mkdir -p ablation/phase-times
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/phase-times/bool_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/phase-times/bool_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/phase-times/bool_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/phase-times/bool_3-3_4.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/phase-times/bool_3-3_0.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/phase-times/bv4ns_3-3-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/phase-times/bv4ns_3-3-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/phase-times/bv4ns_3-3-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/phase-times/bv4ns_3-3-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/phase-times/bv4ns_3-3-4.log


# run with minimization, no run_rewrites
# mkdir -p ablation/no-run-rewrites
# cp lib_no_run_rewrites.rs ../src/lib.rs
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/no-run-rewrites/bool_3-3_1.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/no-run-rewrites/bool_3-3_2.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/no-run-rewrites/bool_3-3_3.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/no-run-rewrites/bool_3-3_4.log
# (time cargo run --release --bin ruler -- --domain bool --variables 3 --iters 3) &> ablation/no-run-rewrites/bool_3-3_0.log

# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/no-run-rewrites/bv4ns_3-3-0.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/no-run-rewrites/bv4ns_3-3-1.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/no-run-rewrites/bv4ns_3-3-2.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/no-run-rewrites/bv4ns_3-3-3.log
# (time cargo run --release --bin ruler -- --domain bv4ns --variables 3 --iters 3) &> ablation/no-run-rewrites/bv4ns_3-3-4.log
