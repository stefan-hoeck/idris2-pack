export PACK_DIR ?= ${HOME}/.pack
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack

DOCKER_IMAGE = ghcr.io/stefan-hoeck/idris2-pack

.PHONY: micropack

micropack:
	bash micropack.bash
	${SCHEME} --script micropack/micropack.ss

.PHONY: install-lib
install-lib:
	idris2 --install pack.ipkg

.PHONY: docker-build
docker-build:
	docker build --build-arg db=${DB} -t ${DOCKER_IMAGE}:${DB} .
	docker tag ${DOCKER_IMAGE}:${DB} ${DOCKER_IMAGE}:latest

.PHONY: docker-run
docker-run:
	docker run --rm -it ${DOCKER_IMAGE} /bin/bash
