#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

TIMESTAMP="$(date "+%Y-%m-%d_%H%M")"

"$MYDIR/run_ruler.sh" -d bv4 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/compare" -r 1
"$MYDIR/run_ruler.sh" -d bv32 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/compare" -r 1
"$MYDIR/run_ruler.sh" -d rational -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/compare" -r 1 --use-smt 

"$MYDIR/run_ruler_rr.sh" -d bv4 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/no-rr" -r 1
"$MYDIR/run_ruler_rr.sh" -d bv32 -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/no-rr" -r 1
# -t is a timeout for each run. Given that the below command will likely time out,
# you probably want to avoid multiple long timeouts if you want to increase the number of runs.
# To abort the script after just one timed-out run, uncomment line 100 in run-ruler-rr.sh
"$MYDIR/run_ruler_rr.sh" -d rational -v 3 -i 2 -o "$MYDIR/output/$TIMESTAMP/no-rr" -r 1 -t 3600 --use-smt

node parse.js "$MYDIR/output/$TIMESTAMP/compare/"
# A utility that might make things easier if you change the parsing script
# if [ $? -neq 0 ]; then
#     echo "The run failed to parse. Try re-running with"
#     echo "node parse.js $MYDIR/output/$TIMESTAMP/compare/"
# fi
node parse.js "$MYDIR/output/$TIMESTAMP/no-rr/" yes
# if [ $? -neq 0 ]; then
#     echo "The run failed to parse. Try re-running with"
#     echo "node parse.js $MYDIR/output/$TIMESTAMP/compare/ yes"
# fi

python3 visualize.py "$MYDIR/output/$TIMESTAMP/"
