#!/usr/bin/env bash

set -eux

git clone https://github.com/idris-lang/Idris2.git idris2
pushd idris2
make support
libidris=libidris2_support
if test -F "support/c/${libidris}.so"; then
  cp -v support/c/libidris2_support.so ../micropack/${libidris}.so
else
  cp -v support/c/${libidris}.dylib ../micropack/${libidris}.so
fi
popd
rm -rf idris2
