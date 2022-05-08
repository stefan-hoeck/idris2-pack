export PACK_DIR ?= ${HOME}/.pack
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack

DB ?= nightly-220507

.PHONY: micropack
micropack:
	${SCHEME} --script micropack/micropack.ss ${DB}

.PHONY: micropack-racket
micropack-racket:
	raco exe micropack/micropack.rkt
	SCHEME=racket micropack/micropack ${DB}
