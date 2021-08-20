#!/usr/bin/env bash

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# ensure we know where Herbie lives
if [ -z "$HERBIE" ]; then
  echo "ERROR: environment variable HERBIE must point to herbie directory."
  exit 1
fi

# determine number of seeds to sample
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ] || [ -z "$5" ]; then
  echo "Usage: $0 NUM_SEEDS NUM_NODES TIMEOUT CONFIG tstamp"
  exit 1
else
  NSEEDS="$1"
  NUM_NODES="$2"
  TIMEOUT="$3"
  cfg="$4"
  tstamp="$5"
fi

# advise user of execution plan
if [ -z "$PARALLEL_SEEDS" ]; then
  echo "Using Herbie concurrency only."
else
  # support for exporting bash environment to parallel
  source $(which env_parallel.bash)
  env_parallel --record-env

  echo "Using multiple concurrent Herbie runs in parallel."
  echo "Restricting to $PARALLEL_SEEDS parallel concurrent Herbie runs."
fi

#
#   SAMPLE SEEDS
#

# allocate space for output
output="$MYDIR/output/ruler-herbie-eval/results/$tstamp/$cfg"
mkdir -p "$output"

# keep a copy of the rule file used in this experiment
echo "Copying the rule file for backup..."
cp "$HERBIE"/src/syntax/rules.rkt "$output"/"$cfg".rkt

function do_seed {
  seed="$1"

  seed_output="$output/$(printf "%03d" "$seed")"
  mkdir -p "$seed_output"

  $MYDIR/racket/bin/racket "$HERBIE/src/herbie.rkt" report \
    --threads yes \
    --seed "$seed" \
    --num-enodes "$NUM_NODES" \
    --timeout "$TIMEOUT" \
    "$HERBIE/bench/rat-reduced.fpcore" \
    "$seed_output"
}

# sample herbie behavior
if [ -z "$PARALLEL_SEEDS" ]; then
  # by default, do not use parallel
  for seed in $(seq "$NSEEDS"); do
    do_seed "$seed"
  done
else
  # conditionally use parallel
  #
  # Note that Herbie can already use up to # of benchmarks cores,
  # so this probably only makes sense if you have PARALLEL_SEEDS
  # set to something less than # of cores divided by # of benchmarks,
  # i.e., you have a lot of cores. We're not at all careful to get
  # solid timing numbers, but going higher any than that will make
  # any time measurements even less meaningful.
  seq "$NSEEDS" \
    | env_parallel \
        --env _ \
        --jobs "$PARALLEL_SEEDS" \
        --halt now,fail=1 \
        do_seed
fi


#
#   COLLECT OUTPUT
#

pushd "$output"

echo "[" > all.json
echo "[" > errors.json
echo "[" > timeouts.json

first=true
first_error=true
first_timeout=true

for rj in $(find . -name 'results.json' | sort); do
  if $first; then
    first=false
  else
    echo "," >> all.json
  fi

  quote_seed="$(jq '.seed' "$rj")"
  temp="${quote_seed%\"}"
  seed="${temp#\"}"
  
  npts="$(jq '.points' "$rj")"
  herbie_iters="$(jq '.iterations' "$rj")"

  # warn about errors and timeouts that will be filtered out

  errors="$(jq '.tests | map(select(.status == "error"))' "$rj")"
  if [ "$errors" != "[]" ]; then
  	if $first_error; then
  	  first_error=false
  	else
  	  echo "," >> errors.json
  	fi
    echo "WARNING: filtering out errors in $rj on seed $seed"
    echo "$errors"
    echo "{ \"seed\" : $seed ," >> errors.json
    echo " \"errors\" :" >> errors.json
    echo "$errors" >> errors.json
    echo "}" >> errors.json
  fi

  timeouts="$(jq '.tests | map(select(.status == "timeout"))' "$rj")"
  if [ "$timeouts" != "[]" ]; then
  	if $first_timeout; then
  	  first_timeout=false
  	else
  	  echo "," >> timeouts.json
  	fi
    echo "WARNING: filtering out timeouts in $rj on seed $seed"
    echo "$timeouts"
    echo "{ \"seed\" : $seed ," >> timeouts.json
    echo " \"timeouts\" :" >> timeouts.json
    echo "$timeouts" >> timeouts.json
    echo " }" >> timeouts.json
  fi
  cat "$rj" \
    | jq --argjson SEED "$seed" \
         --argjson NPTS "$npts" \
         --argjson HERBIE_ITERS "$herbie_iters" \
         --arg CFG "$cfg" \
      '.tests | map(
         select(.status != "error") |
         select(.status != "timeout") |
         { "test" : .name
         , "input" : .input
         , "output" : .output
         , "output_parens" : (.output | [match("[(]"; "g")] | length)
         , "avg_bits_err_input": .start
         , "avg_bits_err_output": .end
         , "avg_bits_err_improve": (.start - .end)
         , "time": .time
         , "seed": $SEED
         , "npts": $NPTS
         , "herbie_iters": $HERBIE_ITERS
         , "config": $CFG
         })' \
    >> all.json
done
echo "]" >> all.json
echo "]" >> errors.json
echo "]" >> timeouts.json

# flatten array of array of results to an array
jq 'flatten' all.json > all.json.tmp
mv all.json.tmp all.json

jq 'flatten' errors.json > errors.json.tmp
mv errors.json.tmp errors.json

jq 'flatten' timeouts.json > timeouts.json.tmp
mv timeouts.json.tmp timeouts.json

popd
