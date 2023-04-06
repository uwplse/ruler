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

# configuration
if [ -z "$BUILD_DIR" ]; then
  BUILD_DIR=herbie/
fi

# check for toolchain
which racket
if [ "$?" != "0" ]; then
  echo "Racket not found"
  exit 1
fi

which cargo
if [ "$?" != "0" ]; then
  echo "Rust not found"
  exit 1
fi

# Install Herbie
echo "Installing Herbie"
mkdir -p $BUILD_DIR
git clone https://github.com/herbie-fp/herbie.git $BUILD_DIR ||    \
  echo "Herbie already checked out"

pushd $BUILD_DIR
# For patches
git checkout "$BUILD_DIR/src/syntax/rules.rkt"
git checkout "$BUILD_DIR/src/core/egg-herbie.rkt"
git checkout "$BUILD_DIR/src/config.rkt"
git checkout "$BUILD_DIR/src/sandbox.rkt"
git checkout "$BUILD_DIR/egg-herbie/src/lib.rs"
# Checkout the branches
git checkout using-ruler-baseline
git checkout using-ruler-nightlies
git checkout ruler-no-fast-forwarding
popd
