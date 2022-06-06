export PACK_DIR ?= ${HOME}/.pack
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack

.PHONY: micropack

micropack:
	bash micropack.bash
	${SCHEME} --script micropack/micropack.ss

.PHONY: install-lib
install-lib:
	idris2 --install pack.ipkg
