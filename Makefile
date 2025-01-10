export PACK_DIR ?= ${HOME}/.pack
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack
export DYLD_LIBRARY_PATH = micropack

UBUNTU_VERSION ?= 24.04
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
	docker build --build-arg ubuntu_version=${UBUNTU_VERSION} --no-cache=${NO_CACHE} -t ${DOCKER_IMAGE}-${UBUNTU_VERSION}:latest .

.PHONY: docker-run
docker-run:
	docker run --rm -it ${DOCKER_IMAGE}-${UBUNTU_VERSION}:latest /bin/bash
