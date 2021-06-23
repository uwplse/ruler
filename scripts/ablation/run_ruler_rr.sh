#!/usr/bin/env bash

# determine physical directory of this script
# h/t Zach
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# TODO replace bool etc with a parameter
# TODO properly parameterize the number of iterations

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
    echo "Running with num_runs = 5 (-r 5)"
    NUM_RUNS=5
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

mkdir -p "$OUTPUT_DIR/orat-default";
mkdir -p "$OUTPUT_DIR/no-run-rewrites";

echo "Running phase-times..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  echo "Running iter $i."
  (time cargo "$DOMAIN" \
  --variables "$NUM_VARIABLES" \
  --iters "$NUM_ITERS" \
  --do-final-run \
  --rules-to-take 1 $@ ) &> "$OUTPUT_DIR/orat-default/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log"
  cp out.json "$OUTPUT_DIR/orat-default/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i-out.json"
done

echo "Running no run-rewrites..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  echo "Running iter $i."
  (time cargo "$DOMAIN" \
  --variables "$NUM_VARIABLES" \
  --iters "$NUM_ITERS" \
  --do-final-run \
  --rules-to-take 1 $@ \
  --no-run-rewrites) &> "$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log"
  cp out.json "$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i-out.json"
done