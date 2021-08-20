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
while getopts ":i:v:r:d:o:t:" OPTION
do
    case "$OPTION" in
        i) NUM_ITERS="$OPTARG" ;;
        v) NUM_VARIABLES="$OPTARG" ;;
        r) NUM_RUNS="$OPTARG" ;;
        d) DOMAIN="$OPTARG" ;;
        o) OUTPUT_DIR="$OPTARG" ;;
        t) TIMEOUT="$OPTARG" ;;
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

if [ -z "${TIMEOUT:-}" ]; then
    echo "Setting timeout to 24 hours (86400 seconds)"
    TIMEOUT="86400s"
fi

mkdir -p "$OUTPUT_DIR/orat-default";
mkdir -p "$OUTPUT_DIR/no-run-rewrites";

echo "run_ruler_rr.sh [FIGURE 9b]: Running experiment for domain $DOMAIN."

echo "Running orat-default..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  echo "Run $i."
  (time cargo "$DOMAIN" \
  --variables "$NUM_VARIABLES" \
  --iters "$NUM_ITERS" \
  --do-final-run \
  --rules-to-take 1 $@ ) &> "$OUTPUT_DIR/orat-default/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log"
  # cp out.json "$OUTPUT_DIR/orat-default/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i-out.json"
done

echo "Running no run-rewrites..."
for (( i=0; i<$NUM_RUNS; i++ ))
do
  echo "Run $i."
  timeout $TIMEOUT \
  bash -c \
  "(time cargo "$DOMAIN" \
  --variables "$NUM_VARIABLES" \
  --iters "$NUM_ITERS" \
  --do-final-run \
  --rules-to-take 1 $@ \
  --no-run-rewrites) &> \"$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log\""
  # did we trigger the timeout?
  TIMEOUT_EXIT="$?"
  if [ $TIMEOUT_EXIT -eq 124 ]; then
    echo "The run timed out."
    mv "$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log" \
    "$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i.log.TIMEOUT"

    # uncomment this if you want to abort on timeout
    # exit $TIMEOUT_EXIT
  fi
  # cp out.json "$OUTPUT_DIR/no-run-rewrites/${DOMAIN}_${NUM_VARIABLES}-${NUM_ITERS}_$i-out.json"
done