#!/usr/bin/env bash

set -eux

SCHEME=chezscheme

if [ -d ~/.pack ]; then
    echo "There is already a ~/.pack directory."
    echo "Please remove it with 'rm -fr ~/.pack' and rerun this script."
    exit 1
fi

if ! command -v git &> /dev/null; then
    echo "Please install git"
    exit 1
fi

if ! command -v $SCHEME &> /dev/null; then
    echo "Please install $SCHEME"
    exit 1
fi

if ! command -v make &> /dev/null; then
    echo "Please install make"
    exit 1
fi

if ! command -v python3 &> /dev/null; then
    echo "Please install python3"
    exit 1
fi

mkdir ~/.pack
mkdir ~/.pack/clones
mkdir ~/.pack/db

# install toml-cli because we need a way of parsing toml files from bash
python3 -m venv ~/.pack/python/venv
~/.pack/python/venv/bin/pip install toml-cli

git clone https://github.com/stefan-hoeck/idris2-pack-db.git ~/.pack/clones/idris2-pack-db
cp ~/.pack/clones/idris2-pack-db/collections/* ~/.pack/db

# commented out for development so that we can stick to one db
LATEST_DB="$(ls ~/.pack/db/nightly-* | tail -1)"
PACKAGE_COLLECTION="$(basename --suffix .toml $LATEST_DB)"
# TODO how to extract idris commit from toml file in bash?
IDRIS2_COMMIT="$(~/.pack/python/venv/bin/toml get --toml-path ~/.pack/db/$PACKAGE_COLLECTION.toml idris2.commit)"

git clone https://github.com/idris-lang/Idris2.git ~/.pack/clones/Idris2
pushd ~/.pack/clones/Idris2
git checkout $IDRIS2_COMMIT

# NOTE tilde (~) does not work here so use HOME instead
PREFIX_PATH="$HOME/.pack/install/$IDRIS2_COMMIT/idris2"
BOOT_PATH="$HOME/.pack/install/$IDRIS2_COMMIT/idris2/bin/idris2"

make bootstrap PREFIX="$PREFIX_PATH" SCHEME="$SCHEME"
make install PREFIX="$PREFIX_PATH"
make clean
make all IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
make install IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
make install-with-src-libs IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
make install-with-src-api IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
popd

TOML_COMMIT="$(~/.pack/python/venv/bin/toml get --toml-path ~/.pack/db/$PACKAGE_COLLECTION.toml db.toml.commit)"
git clone https://github.com/cuddlefishie/toml-idr ~/.pack/clones/toml-idr
pushd ~/.pack/clones/toml-idr
git checkout $TOML_COMMIT
"$BOOT_PATH" --install toml.ipkg
popd

PACK_COMMIT="$(~/.pack/python/venv/bin/toml get --toml-path ~/.pack/db/$PACKAGE_COLLECTION.toml db.pack.commit)"
git clone https://github.com/stefan-hoeck/idris2-pack.git ~/.pack/clones/idris2-pack
pushd ~/.pack/clones/idris2-pack
git checkout $PACK_COMMIT
"$BOOT_PATH" --build pack.ipkg

mkdir -p ~/.pack/install/$IDRIS2_COMMIT/pack/$PACK_COMMIT/bin
cp -r build/exec/* ~/.pack/install/$IDRIS2_COMMIT/pack/$PACK_COMMIT/bin
popd

mkdir -p ~/.pack/$PACKAGE_COLLECTION/bin
pushd ~/.pack/$PACKAGE_COLLECTION/bin
ln -s ~/.pack/install/$IDRIS2_COMMIT/pack/$PACK_COMMIT/bin/pack pack
popd

ln -s ~/.pack/$PACKAGE_COLLECTION/bin ~/.pack/bin

mkdir ~/.pack/user
cat <<EOF >> ~/.pack/user/pack.toml
# The package collection to use
collection = "$PACKAGE_COLLECTION"

[install]

# Whether to install packages together with their
# sources or not. This is mainly useful for programmers
# who have set their editor up with some *go to definition*
# functionality (for instance by using idris2-lsp with neovim).
with-src   = true

# Whether to prompt the user before building or installing
# packages or applications with custom build hooks in their
# \`.ipkg\` file.
safety-prompt = true

# Must-have libraries. These will be installed automatically
# when using a new package collection.
# libs       = [ "toml", "elab-util" ]

# Must-have applications. These will be installed automatically
# when using a new package collection.
apps       = [ "pack" ]

[idris2]

# Whether to build Idris2 with its bootstrap compiler.
# Bootstrapping takes longer than building with an existing
# Idris2 installation, but it will work even if the existing
# Idris2 compiler is outdated.
bootstrap  = false

# Name or path to the scheme executable to use.
scheme      = "$SCHEME"

# Default code generator to us
codegen     = "chez"

# Set this to \`true\` in order to run REPL sessions from within
# \`rlwrap\`. This will give you additional features such as a
# command history.
repl.rlwrap = false

# Below are some examples for custom packages

# A local package to be available with all
# package collections.
# [custom.all.chem]
# type = "local"
# path = "/data/idris/chem"
# ipkg = "chem.ipkg"

# A package on GitHub to be available with all
# package collections.
# [custom.all.foo]
# type = "github"
# path = "https://github.com/bar/foo"
# ipkg = "foo.ipkg"

# Override library \`toml\` from package collection \`nightly-220503\`
# by using a custom commit hash.
# [custom.nightly-220503.toml]
# type   = "github"
# url    = "https://github.com/cuddlefishie/toml-idr"
# commit = "eb7a146f565276f82ebf30cb6d5502e9f65dcc3c"
# ipkg   = "toml.ipkg"
EOF
