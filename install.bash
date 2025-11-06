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

# Install directories

CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"

USER_DIR="${PACK_USER_DIR:-$CONFIG_HOME/pack}"
STATE_DIR="${PACK_STATE_DIR:-$STATE_HOME/pack}"
CACHE_DIR="${PACK_CACHE_DIR:-$CACHE_HOME/pack}"
BIN_DIR="${PACK_BIN_DIR:-$HOME/.local/bin}"

DB_DIR="$STATE_DIR/db"
INSTALL_DIR="$STATE_DIR/install"
CLONES_DIR="$CACHE_DIR/clones"

# Detect Chez executable

if command -v chezscheme &>/dev/null; then
	DETECTED_SCHEME=chezscheme
elif command -v scheme &>/dev/null; then
	DETECTED_SCHEME=scheme
elif command -v chez &>/dev/null; then
	DETECTED_SCHEME=chez
elif command -v racket &>/dev/null; then
	DETECTED_SCHEME=racket
else
	DETECTED_SCHEME=''
fi

read -r -p "Enter the name of your chez-scheme or racket binary [$DETECTED_SCHEME]: " SCHEME
SCHEME=${SCHEME:-$DETECTED_SCHEME}

# Verify that the necessary programs are installed

if [ -z "$SCHEME" ]; then
	echo 'scheme binary was not set'
	exit 1
else
	echo "Using $SCHEME for code generation"
fi

# Check and create directories

if [ -d "$STATE_DIR" ]; then
	echo "Directory $STATE_DIR exists."
	echo "Please remove it and rerun this script."
	exit 1
fi

mkdir -p "$USER_DIR"
mkdir -p "$DB_DIR"
mkdir -p "$INSTALL_DIR"
mkdir -p "$CLONES_DIR"
mkdir -p "$BIN_DIR"

# Homebrew gmp on M1 macos

if [ -d "/opt/homebrew/include" ]; then
	export CPATH="/opt/homebrew/include"
fi

check_installed git
check_installed "$SCHEME"
check_installed make

# Install package collection

git clone https://github.com/stefan-hoeck/idris2-pack-db.git "$CLONES_DIR/idris2-pack-db"
cp "$CLONES_DIR/idris2-pack-db/collections/"* "$DB_DIR"

LATEST_DB="$(find "$DB_DIR" -name 'nightly-*' | sort | tail -1)"
PACKAGE_COLLECTION="$(basename -s .toml "$LATEST_DB")"
IDRIS2_COMMIT=$(sed -ne '/^\[idris2\]/,/^commit/{/^commit/s/commit *= *"\([a-f0-9]*\)"/\1/p;}' "$DB_DIR/$PACKAGE_COLLECTION.toml")

# Bootstrap the Idris compiler

git clone https://github.com/idris-lang/Idris2.git "$CLONES_DIR/Idris2"
pushd "$CLONES_DIR/Idris2"
git checkout "$IDRIS2_COMMIT"

PREFIX_PATH="$INSTALL_DIR/$IDRIS2_COMMIT/idris2"
BOOT_PATH="$INSTALL_DIR/$IDRIS2_COMMIT/idris2/bin/idris2"

if [ "$SCHEME" = "racket" ]; then
	CG="racket"
	make bootstrap-racket PREFIX="$PREFIX_PATH"
else
	CG="chez"
	make bootstrap PREFIX="$PREFIX_PATH" SCHEME="$SCHEME"
fi

export IDRIS2_CG="$CG"

make install PREFIX="$PREFIX_PATH" IDRIS2_CG="$CG"
make clean
make all IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH" IDRIS2_CG="$CG"
make install IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH" IDRIS2_CG="$CG"
make install-with-src-libs IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH" IDRIS2_CG="$CG"
make install-with-src-api IDRIS2_BOOT="$BOOT_PATH" PREFIX="$PREFIX_PATH" IDRIS2_CG="$CG"
popd

# Install algebra

git clone https://github.com/stefan-hoeck/idris2-algebra.git "$CLONES_DIR/idris2-algebra"
pushd "$CLONES_DIR/idris2-algebra"
"$BOOT_PATH" --install algebra.ipkg
popd

# Install ref1

git clone https://github.com/stefan-hoeck/idris2-ref1.git "$CLONES_DIR/idris2-ref1"
pushd "$CLONES_DIR/idris2-ref1"
"$BOOT_PATH" --install ref1.ipkg
popd

# Install array

git clone https://github.com/stefan-hoeck/idris2-array.git "$CLONES_DIR/idris2-array"
pushd "$CLONES_DIR/idris2-array"
"$BOOT_PATH" --install array.ipkg
popd

# Install bytestring

git clone https://github.com/stefan-hoeck/idris2-bytestring.git "$CLONES_DIR/idris2-bytestring"
pushd "$CLONES_DIR/idris2-bytestring"
"$BOOT_PATH" --install bytestring.ipkg
popd

# Install getopts

git clone https://github.com/idris-community/idris2-getopts.git "$CLONES_DIR/idris2-getopts"
pushd "$CLONES_DIR/idris2-getopts"
"$BOOT_PATH" --install getopts.ipkg
popd

# Install elab-util

git clone https://github.com/stefan-hoeck/idris2-elab-util.git "$CLONES_DIR/idris2-elab-util"
pushd "$CLONES_DIR/idris2-elab-util"
"$BOOT_PATH" --install elab-util.ipkg
popd

# Install refined

git clone https://github.com/stefan-hoeck/idris2-refined.git "$CLONES_DIR/idris2-refined"
pushd "$CLONES_DIR/idris2-refined"
"$BOOT_PATH" --install refined.ipkg
popd

# Install literal

git clone https://github.com/stefan-hoeck/idris2-literal.git "$CLONES_DIR/idris2-literal"
pushd "$CLONES_DIR/idris2-literal"
"$BOOT_PATH" --install literal.ipkg
popd

# Install ilex-core, ilex, and ilex-toml

git clone https://github.com/stefan-hoeck/idris2-ilex.git "$CLONES_DIR/idris2-ilex"
pushd "$CLONES_DIR/idris2-ilex/core"
"$BOOT_PATH" --install ilex-core.ipkg
popd

pushd "$CLONES_DIR/idris2-ilex"
"$BOOT_PATH" --install ilex.ipkg
popd

pushd "$CLONES_DIR/idris2-ilex/toml"
"$BOOT_PATH" --install ilex-toml.ipkg
popd

# Install filepath

git clone https://github.com/stefan-hoeck/idris2-filepath.git "$CLONES_DIR/idris2-filepath"
pushd "$CLONES_DIR/idris2-filepath"
"$BOOT_PATH" --install filepath.ipkg
popd

# Install pack

git clone https://github.com/stefan-hoeck/idris2-pack.git "$CLONES_DIR/idris2-pack"
pushd "$CLONES_DIR/idris2-pack"
"$BOOT_PATH" --build pack.ipkg
cp -r build/exec/* "$BIN_DIR"
popd

pushd "$BIN_DIR"

cat <<EOF >>idris2
#!/bin/sh

APPLICATION="\$($BIN_DIR/pack app-path idris2)"
export IDRIS2_PACKAGE_PATH="\$($BIN_DIR/pack package-path)"
export IDRIS2_LIBS="\$($BIN_DIR/pack libs-path)"
export IDRIS2_DATA="\$($BIN_DIR/pack data-path)"
export IDRIS2_CG="$CG"
\$APPLICATION "\$@"
EOF

chmod +x idris2
popd

# Initialize `pack.toml`s
#
cat <<EOF >"$STATE_DIR/pack.toml"
# Warning: This file was auto-generated and is maintained by pack.
#          Any changes could be overwritten by pack at any time.
#          Custom settings should go to the global pack.toml file
#          or any pack.toml file local to a project.
collection = "$PACKAGE_COLLECTION"
EOF

if [ ! -f "$USER_DIR/pack.toml" ]; then
	cat <<-EOF >>"$USER_DIR/pack.toml"
		[install]

		# Whether to install packages together with their
		# sources or not. This is mainly useful for programmers
		# who have set their editor up with some *go to definition*
		# functionality (for instance by using idris2-lsp with neovim).
		# with-src   = true

		# Whether to install API docs together with installed
		# libraries.
		# with-docs = false

		# Whether to use katla to generate HTML links to
		# semantically highlighted API sources
		# use-katla = false

		# Whether to prompt the user before building or installing
		# packages or applications with custom build hooks in their
		# '.ipkg' file.
		# safety-prompt = true

		# Whether to prompt the user before running the garbage collector
		# via command 'gc'.
		# gc-prompt = true

		# Whether to remove all outdated libraries and applications during
		# garbage collection (command 'gc').
		# gc-purge = false

		# Whether to issue a warning in presence of a local 'depends' directory
		# which might interfere with the libraries managed by pack
		# warn-depends = true

		# List of packages and apps with custom build hooks we trust to
		# be safe. This gives more fine grained control over package safety
		# than 'safety-prompt'.
		# whitelist = [ "pack", "idris2-lsp" ]

		# Must-have libraries. These will be installed automatically
		# when using a new package collection.
		# Example: 'libs       = [ "toml", "elab-util" ]'
		# libs       = []

		# Must-have applications. These will be installed automatically
		# when using a new package collection.
		# Example: 'apps       = [ "idris2-lsp" ]'
		# apps       = []

		[pack]

		# Override this to use a custom Git repo for pack
		# url = "https://github.com/stefan-hoeck/idris2-pack"

		# Override this to use a custom commit and branch for pack
		# commit = "latest:main"

		[idris2]

		# Whether to build Idris2 with its bootstrap compiler.
		# Bootstrapping takes longer than building with an existing
		# Idris2 installation, but it will work even if the existing
		# Idris2 compiler is outdated.
		# bootstrap  = false

		# Whether to rebuild Idris2 when bootstrapping, using the newly
		# built compiler. Produces a more optimised final executable,
		# but increases build time.
		# bootstrap-stage3 = true

		# Name or path to the scheme executable to use.
		scheme      = "$SCHEME"

		# Default code generator to us
		# codegen     = "chez"

		# Set this to 'true' in order to run REPL sessions from within
		# 'rlwrap'. This will give you additional features such as a
		# command history.
		# Alternatively, you can pass additional command-line arguments
		# to 'rlwrap' by setting this to a string or an array of strings,
		# e.g. to "-pGreen -aN" or ["-pGreen", "--no-children"].
		# repl.rlwrap = false

		# Packages to load automatically when starting a REPL session
		# without an '.ipkg' file in scope. This defaults to "none".
		# Note: Uncomment only one of the following examples:
		# repl.autoload   = "installed"
		# repl.autoload   = "autolibs"
		# repl.autoload   = [ "sop", "toml" ]

		# Override this to use a custom Git repo for the Idris compiler
		# url = "https://github.com/idris-lang/Idris2"

		# Override this to use a custom commit and branch for the Idris compiler
		# commit = "latest:main"

		# Override this to have a .gitignore file and .git directory
		# created inside of a new pack project upon its creation.
		# git = false

		# Additional arguments to be passed to the Idris compiler
		# This can be a single string or a list of strings.
		# extra-args = []

		# Below are some examples for custom packages

		# A local package to be available with all
		# package collections. The path to the package's root
		# directory can be absolute or relative. In the latter
		# case, it will be considered to be relative to the
		# parent directory of the 'pack.toml' file where it is
		# defined.
		#
		# The path to '.ipkg' files must always relative to the
		# given 'path'.
		# [custom.all.chem]
		# type = "local"
		# path = "/data/idris/chem"
		# ipkg = "chem.ipkg"

		# A package as a Git repository to be available with all
		# package collections.
		# [custom.all.foo]
		# type = "git"
		# url  = "https://github.com/bar/foo"
		# ipkg = "foo.ipkg"

		# Override library 'toml' from package collection 'nightly-220503'
		# by using a custom commit hash.
		# [custom.nightly-251023.ilex]
		# type   = "git"
		# url    = "https://github.com/stefan-hoeck/idirs2-ilex"
		# commit = "eb7a146f565276f82ebf30cb6d5502e9f65dcc3c"
		# ipkg   = "ilex.ipkg"

		# Uncomment and adjust the following entries to specify the
		# default log level associated with each pack command.
		# Possible log levels are:
		#   "debug"
		#   "build"
		#   "info"
		#   "cache"
		#   "warning"
		#   "silence"
		[log]

		# build             = "build"
		# install-deps      = "build"
		# typecheck         = "build"
		# clean             = "build"
		# cleanbuild        = "build"
		# repl              = "warning"
		# exec              = "warning"
		# install           = "build"
		# install-app       = "build"
		# remove            = "build"
		# remove-app        = "build"
		# run               = "warning"
		# test              = "warning"
		# new               = "build"
		# update            = "build"
		# fetch             = "build"
		# package-path      = "silence"
		# libs-path         = "silence"
		# data-path         = "silence"
		# app-path          = "silence"
		# switch            = "build"
		# update-db         = "build"
		# gc                = "info"
		# info              = "cache"
		# query             = "cache"
		# fuzzy             = "cache"
		# completion        = "silence"
		# completion-script = "silence"
		# uninstall         = "info"
		# help              = "silence"
	EOF
fi

# Cleanup

rm -rf "$CLONES_DIR"
rm -rf "$PREFIX_PATH/idris2-*/elab-util-*"
rm -rf "$PREFIX_PATH/idris2-*/algebra-*"
rm -rf "$PREFIX_PATH/idris2-*/getopts-*"
rm -rf "$PREFIX_PATH/idris2-*/refined-*"
rm -rf "$PREFIX_PATH/idris2-*/parser-*"
rm -rf "$PREFIX_PATH/idris2-*/filepath-*"
rm -rf "$PREFIX_PATH/idris2-*/ref1-*"
rm -rf "$PREFIX_PATH/idris2-*/array-*"
rm -rf "$PREFIX_PATH/idris2-*/bytestring-*"

"$BIN_DIR/pack" info
