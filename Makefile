export PACK_DIR ?= ${HOME}/.pack
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack
export DYLD_LIBRARY_PATH = micropack

DOCKER_IMAGE = ghcr.io/stefan-hoeck/idris2-pack
NO_CACHE ?= false

.PHONY: micropack

micropack:
	bash micropack.bash
	${SCHEME} --script micropack/micropack.ss

micropack-racket:
	bash micropack.bash
	SCHEME="racket" racket micropack/micropack.rkt

.PHONY: install-lib
install-lib:
	idris2 --install pack.ipkg

.PHONY: docker-build
docker-build:
	docker build --no-cache=${NO_CACHE} -t ${DOCKER_IMAGE}:latest .

.PHONY: docker-run
docker-run:
	docker run --rm -it ${DOCKER_IMAGE}:latest /bin/bash
