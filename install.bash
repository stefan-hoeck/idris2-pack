#!/usr/bin/env bash

set -eux

# common functions

function check_installed {
	if ! command -v "$1" &>/dev/null; then
		echo "Please install $1"
		exit 1
	fi
}

# end common functions

# Detect Chez executable

PACK_DIR="${PACK_DIR:-$HOME/.pack}"

if command -v chezscheme &>/dev/null; then
	DETECTED_SCHEME=chezscheme
elif command -v scheme &>/dev/null; then
	DETECTED_SCHEME=scheme
elif command -v chez &>/dev/null; then
	DETECTED_SCHEME=chez
else
	DETECTED_SCHEME=''
fi

read -r -p "Enter the name of your chez scheme binary [$DETECTED_SCHEME]: " SCHEME
SCHEME=${SCHEME:-$DETECTED_SCHEME}

# Verify that the necessary programs are installed

if [ -z "$SCHEME" ]; then
	echo 'scheme binary was not set'
	exit 1
fi

if [ -d "$PACK_DIR" ]; then
	echo "There is already a $PACK_DIR directory."
	echo "Please remove it with 'rm -fr $PACK_DIR' and rerun this script."
	exit 1
fi

# Homebrew gmp on M1 macos

if [ -d "/opt/homebrew/include" ]; then
	export CPATH="/opt/homebrew/include"
fi

check_installed git
check_installed "$SCHEME"
check_installed make

# Install package collection

mkdir "$PACK_DIR"
mkdir "$PACK_DIR/clones"
mkdir "$PACK_DIR/db"

git clone https://github.com/stefan-hoeck/idris2-pack-db.git "$PACK_DIR/clones/idris2-pack-db"
cp "$PACK_DIR/clones/idris2-pack-db/collections/"* "$PACK_DIR/db"

LATEST_DB="$(find "$PACK_DIR/db" -name 'nightly-*' | sort | tail -1)"
PACKAGE_COLLECTION="$(basename -s .toml "$LATEST_DB")"
IDRIS2_COMMIT=$(sed -ne '/^\[idris2\]/,/^commit/{/^commit/s/commit *= *"\([a-f0-9]*\)"/\1/p;}' "$PACK_DIR/db/$PACKAGE_COLLECTION.toml")

# Bootstrap the Idris compiler

git clone https://github.com/idris-lang/Idris2.git "$PACK_DIR/clones/Idris2"
pushd "$PACK_DIR/clones/Idris2"
git checkout "$IDRIS2_COMMIT"

PREFIX_PATH="$PACK_DIR/install/$IDRIS2_COMMIT/idris2"
BOOT_PATH="$PACK_DIR/install/$IDRIS2_COMMIT/idris2/bin/idris2"

make bootstrap PREFIX="$PREFIX_PATH" SCHEME="$SCHEME"
make install PREFIX="$PREFIX_PATH"
make clean
make all IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
make install IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
make install-with-src-libs IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
make install-with-src-api IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH"
popd

# Install filepath

FILEPATH_COMMIT=$(sed -ne '/^\[db.filepath\]/,/^commit/{/^commit/s/commit *= *"\([a-f0-9]*\)"/\1/p;}' "$PACK_DIR/db/$PACKAGE_COLLECTION.toml")
git clone https://github.com/stefan-hoeck/idris2-filepath.git "$PACK_DIR/clones/idris2-filepath"
pushd "$PACK_DIR/clones/idris2-filepath"
git checkout "$FILEPATH_COMMIT"
"$BOOT_PATH" --install filepath.ipkg
popd

# Install toml-idr

TOML_COMMIT=$(sed -ne '/^\[db.toml\]/,/^commit/{/^commit/s/commit *= *"\([a-f0-9]*\)"/\1/p;}' "$PACK_DIR/db/$PACKAGE_COLLECTION.toml")
git clone https://github.com/cuddlefishie/toml-idr "$PACK_DIR/clones/toml-idr"
pushd "$PACK_DIR/clones/toml-idr"
git checkout "$TOML_COMMIT"
"$BOOT_PATH" --install toml.ipkg
popd

# Install pack

git clone https://github.com/stefan-hoeck/idris2-pack.git "$PACK_DIR/clones/idris2-pack"
pushd "$PACK_DIR/clones/idris2-pack"
"$BOOT_PATH" --build pack.ipkg
mkdir -p "$PACK_DIR/bin"
cp -r build/exec/* "$PACK_DIR/bin"
popd

pushd "$PACK_DIR/bin"

cat <<EOF >>idris2
#!/bin/sh

APPLICATION="\$($PACK_DIR/bin/pack app-path idris2)"
export IDRIS2_PACKAGE_PATH="\$($PACK_DIR/bin/pack package-path)"
export IDRIS2_LIBS="\$($PACK_DIR/bin/pack libs-path)"
export IDRIS2_DATA="\$($PACK_DIR/bin/pack data-path)"
\$APPLICATION "\$@"
EOF

chmod +x idris2
popd

# Initialize `pack.toml`

mkdir -p "$PACK_DIR/user"
cat <<EOF >>"$PACK_DIR/user/pack.toml"
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
# apps       = [ "lsp" ]

[idris2]

# Whether to build Idris2 with its bootstrap compiler.
# Bootstrapping takes longer than building with an existing
# Idris2 installation, but it will work even if the existing
# Idris2 compiler is outdated.
bootstrap  = false

# Name or path to the scheme executable to use.
scheme      = "$SCHEME"

# Default code generator to us
# codegen     = "chez"

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

# Cleanup

rm -rf "$PACK_DIR/clones"
rm -rf "$PREFIX_PATH/idris2-*/filepath-*"
rm -rf "$PREFIX_PATH/idris2-*/toml-*"

"$PACK_DIR/bin/pack" info
