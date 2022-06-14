#!/usr/bin/env bash

set -eux

git clone https://github.com/idris-lang/Idris2.git idris2
pushd idris2
make support
cp -v support/c/libidris2_support.dylib ../micropack/
popd
rm -rf idris2
