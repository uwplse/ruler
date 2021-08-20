#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

help() { echo "Run with -r generate-new for new data, or -r use-existing to use existing data."; exit 1; }
# num_runs
# num_iterations
# domain
while getopts ":r:h" OPTION
do
    case "$OPTION" in
        h) help ;;
        r) RUN="$OPTARG" 
            [[ "$RUN" == "generate-new" || "$RUN" == "use-existing" ]] || help 
            ;;
        \?) break ;;
    esac
done

if [ -z "${RUN:-}" ] ; then
    help
fi

if [[ "$RUN" == "use-existing" ]] ; then
    echo "Running with existing data."
    python3 visualize.py 
    exit 0
fi

if [[ "$RUN" == "generate-new" ]] ; then
    # echo "RUNNING FULL!"
    "$MYDIR/run.sh"
fi




