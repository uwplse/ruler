#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

echo "Usage: derivation.sh [DERIVE_OUTPUT] [ rational | bv32 | bv4 ]"
echo "DERIVE_OUTPUT should be path to directory containing directories that contain out.json"

DIR="$MYDIR/$1/derive"
mkdir -p "$DIR"

is_first_rs=true
prev_rs=""
for rs in $(find $1 -name 'out.json' | sort); do
    if [ "$(jq '.status' "$rs")" = "CRASH" ]; then
        echo "WARNING: SKIPPING CRASHED RULESET!"
        echo "$rs"
        echo
        continue
    fi  
    if $is_first_rs; then
        prev_rs="$rs"
        is_first_rs=false
        continue
    fi
    

    IFS='/ ' read -r -a array1 <<< "$prev_rs"
    IFS='/ ' read -r -a array2 <<< "$rs"

    # indexing like this will break if directory structure changes.
    name1="${array1[-2]}"
    name2="${array2[-2]}"
    echo "$name1"
    echo "$name2"
    IFS='- ' read -r -a dm1 <<< "$name1"
    IFS='- ' read -r -a dm2 <<< "$name2"

    dom1="${dm1[0]}"
    dom2="${dm2[0]}"
    echo "$dom1"
    echo "$dom2"
        
    if [ "$dom1" = "4" ] || [ "$dom1" = "32" ]; then
        d1="bv$dom1"
    else
        d1="$2"
    fi  
    if [ "$dom2" = "4" ] || [ "$dom2" = "32" ]; then
        d2="bv$dom2"
    else
        d2="$2"
    fi  
    if [[ "$d1" = "$2"  &&  "$d2" = "$2" ]]; then
        echo "PREVIOUS = $prev_rs"
        echo "CURRENT  = $rs"
        echo "Deriving $name1/out.json and $name2/out.json"
        
        dout="$DIR/derive-$name1-$name2.json"
        echo "Sending output to '$dout'"
        #cargo run --bin "$2" --release -- derive "$prev_rs" "$rs" "$dout" >&2 "$dout.stderr"
        cargo run --bin "$2" --release -- derive "$prev_rs" "$rs" "$dout"
            
        if [ "$(jq '.forward.not_derivable' "$dout")" != "[]" ]; then
            echo "FORWARD FAILED!!!"
            echo "Not derivable:"
            jq '.forward.not_derivable' "$dout"
            jq '.forward.not_derivable' "$dout" >> "$DIR/derive-$name1-$name2-forward.json"
        fi  
        if [ "$(jq '.reverse.not_derivable' "$dout")" != "[]" ]; then
            echo "REVERSE FAILED!!!"
            jq '.reverse.not_derivable' "$dout"
            jq '.reverse.not_derivable' "$dout" >> "$DIR/derive-$name1-$name2-reverse.json"
        fi
        echo
    fi

    # update for next iter
    prev_rs="$rs"
done



# for out1 in $(find $1 -name 'out.json' | sort); do
#     for out2 in $(find $1 -name 'out.json' | sort); do
#         echo "$out1 $out2"
#         IFS='/ ' read -r -a array1 <<< "$out1"
#         IFS='/ ' read -r -a array2 <<< "$out2"
#         # indexing like this will break if directory structure changes.
#         if [ "$out1" != "$out2" ]; then
#             name1="${array1[2]}"
#             name2="${array2[2]}"
#             IFS='- ' read -r -a dm1 <<< "$name1"
#             IFS='- ' read -r -a dm2 <<< "$name2"
#             dom1="${dm1[0]}"
#             dom2="${dm2[0]}"
#             if [ "$dom1" = "4" ] || [ "$dom1" = "32" ]; then
#                 d1="bv$dom1"
#             else
#                 d1="$2"
#             fi
#             if [ "$dom2" = "4" ] || [ "$dom2" = "32" ]; then
#                 d2="bv$dom2"
#             else
#                 d2="$2"
#             fi
#             if [[ "$d1" = "$2"  &&  "$d2" = "$2" ]]; then
#                 echo "Deriving $name1/out.json and $name2/out.json"
#                 cargo run --bin "$2" --release -- derive "$out1" "$out2" "$DIR/derive-$name1-$name2"
#             else
#                 echo "No compatible pairs."
#             fi
#         fi
#     done
# done


# for out1 in $(find $1 -name 'out.json' | sort); do
#     for out2 in $(find $1 -name 'out.json' | sort); do
#         echo "$out1 $out2"
#         IFS='/ ' read -r -a array1 <<< "$out1"
#         IFS='/ ' read -r -a array2 <<< "$out2"
#         # indexing like this will break if directory structure changes.
#         if [ "$out1" != "$out2" ]; then
#             name1="${array1[2]}"
#             name2="${array2[2]}"
#             IFS='- ' read -r -a dm1 <<< "$name1"
#             IFS='- ' read -r -a dm2 <<< "$name2"
#             dom1="${dm1[0]}"
#             dom2="${dm2[0]}"
#             if [ "$dom1" = "4" ] || [ "$dom1" = "32" ]; then
#                 d1="bv$dom1"
#             else
#                 d1="$2"
#             fi
#             if [ "$dom2" = "4" ] || [ "$dom2" = "32" ]; then
#                 d2="bv$dom2"
#             else
#                 d2="$2"
#             fi
#             if [[ "$d1" = "$2"  &&  "$d2" = "$2" ]]; then
#                 echo "Deriving $name1/out.json and $name2/out.json"
#                 cargo run --bin "$2" --release -- derive "$out1" "$out2" "$DIR/derive-$name1-$name2"
#             else
#                 echo "No compatible pairs."
#             fi
#         fi
#     done
# done