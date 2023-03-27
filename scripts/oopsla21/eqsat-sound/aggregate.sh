#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

pushd "$1"
# process all out.json and populate a top-level all.json file
echo "[ " > all.json
first=true

echo "Generating aggregate json"
for out in $(find . -name 'out.json' | sort); do
    if $first; then
      first=false
    else
      echo "," >> all.json
    fi
    cat "$out" | \
    jq '{"domain": .domain,
         "num_consts": .num_consts,
         "samples": .samples,
         "v_fuzz": .fuzz,
         "v_smt_uknown": .smt_unknown,
         "time": .time,
         "num_rules": .num_rules,
         "post_unsound": .unsound,
         "post_unknown": .unknown,
         "status": .status}' >> all.json
done

echo "]" >> all.json

python3 $MYDIR/tabulate.py all.json
popd

echo "Done."