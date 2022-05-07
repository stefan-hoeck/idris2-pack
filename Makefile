export PACK_DIR ?= ${HOME}/.pack
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack

DB ?= nightly-220507

.PHONY: micropack

micropack:
	${SCHEME} --script micropack/micropack.ss ${DB}


.PHONY: toml-idr
toml-idr:
	git clone "https://github.com/cuddlefishie/toml-idr" toml-idr-tmp
	${MAKE} -C toml-idr-tmp install
	rm -rf toml-idr-tmp

.PHONY: test-build
test-build: toml-idr
	idris2 --build pack.ipkg
