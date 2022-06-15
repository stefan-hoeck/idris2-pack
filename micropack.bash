#!/usr/bin/env bash

set -eux

git clone https://github.com/idris-lang/Idris2.git idris2
pushd idris2
make support
libidris=libidris2_support
if test -F "support/c/${libidris}.so"; then
	libidris_full=${libidris}.so
else
	libidris_full=${libidris}.dylib
fi
cp -v support/c/${libidris_full} ../micropack/${libidris}.so
popd
rm -rf idris2
