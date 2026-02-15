#export OWNER ?= stefan-hoeck
export OWNER ?= milessabin
export SCHEME ?= scheme
export LD_LIBRARY_PATH = micropack
export DYLD_LIBRARY_PATH = micropack

DOCKER_IMAGE = ghcr.io/${OWNER}/idris2-pack
UBUNTU_RELEASE ?= noble
ALPINE_RELEASE ?= latest
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
	docker build --build-arg ubuntu_release=${UBUNTU_RELEASE} --no-cache=${NO_CACHE} -t ${DOCKER_IMAGE}:${UBUNTU_RELEASE} .

.PHONY: docker-build-alpine
docker-build-alpine:
	docker build -f Dockerfile-alpine --build-arg alpine_release=${ALPINE_RELEASE} --no-cache=${NO_CACHE} -t ${DOCKER_IMAGE}:alpine-${ALPINE_RELEASE} .

.PHONY: docker-run
docker-run:
	docker run --rm -it ${DOCKER_IMAGE}:${UBUNTU_RELEASE} /bin/bash
