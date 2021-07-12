#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

export RUST_LOG="ruler=info,egg=warn";

# num_runs
# num_iterations
# domain
while getopts ":i:v:r:d:o:" OPTION
do
    case "$OPTION" in
        i) NUM_ITERS="$OPTARG" ;;
        v) NUM_VARIABLES="$OPTARG" ;;
        r) NUM_RUNS="$OPTARG" ;;
        d) DOMAIN="$OPTARG" ;;
        o) OUTPUT_DIR="$OPTARG" ;;
        \?) break ;;
    esac
done

# Save the other variables just in case
shift "$((OPTIND-1))"

if [ -z "${NUM_RUNS:-}" ] ; then
    echo "Running with num_runs = 10 (-r 10)"
    NUM_RUNS=10
fi

if [ -z "${NUM_VARIABLES:-}" ] ; then
    echo "Running with num_variables = 3 (-v 3)"
    NUM_VARIABLES=3
fi

if [ -z "${NUM_ITERS:-}" ] ; then
    echo "Running with num_iters = 2 (-i 2)"
    NUM_ITERS=2
fi

if [ -z "${OUTPUT_DIR:-}" ] ; then
    echo "Setting output directory to $MYDIR/output"
    OUTPUT_DIR="$MYDIR/output"
fi

if [ -z "${DOMAIN:-}" ]; then
    echo "Please specify a domain with -d"
    exit 1
fi

mkdir -p "$OUTPUT_DIR/mrat";
mkdir -p "$OUTPUT_DIR/orat";
mkdir -p "$OUTPUT_DIR/default";
mkdir -p "$OUTPUT_DIR/phase-times";
#mkdir -p "$OUTPUT_DIR/no-run-rewrites";

echo "run_ruler.sh [FIGURE 8] [FIGURE 9a]: Running experiment for domain $DOMAIN."

echo "Running orat..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  echo "Run $i."
  (time cargo "$DOMAIN"  \
  --variables "$NUM_VARIABLES" \
  --iters "$NUM_ITERS" \
  --do-final-run $@ \
  --rules-to-take 1) &> "$OUTPUT_DIR/orat/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log"
  # cp out.json "$OUTPUT_DIR/orat/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i-out.json"
done

echo "Running mrat..."
for r in 5 10 15 25 50 100
do
    for (( i=0; i<$NUM_RUNS; i++ ))
    do
        echo "Run $i with mrat_m=$r."
        (time cargo "$DOMAIN" \
        --variables "$NUM_VARIABLES" \
        --iters "$NUM_ITERS" \
        --do-final-run $@ \
        --rules-to-take "$r") &> "$OUTPUT_DIR/mrat/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$r-$i.log"
       # cp out.json "$OUTPUT_DIR/mrat/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$r-$i-out.json"
    done
done

echo "Running phase-times..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  echo "Run $i."
  (time cargo "$DOMAIN" \
  --variables "$NUM_VARIABLES" \
  --iters "$NUM_ITERS" \
  --do-final-run $@ ) &> "$OUTPUT_DIR/phase-times/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log"
  cp "$OUTPUT_DIR/phase-times/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log" \
        "$OUTPUT_DIR/default/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log"
  #cp out.json "$OUTPUT_DIR/default/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i-out.json"
done
