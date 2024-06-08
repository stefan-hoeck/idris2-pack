#checkov:skip=CKV_DOCKER_3: we intend to use `root` user

FROM ubuntu:22.04 AS build

SHELL ["/bin/bash", "-c"]

ENV PATH "/root/.pack/bin:/root/.idris2/bin:$PATH"

# hadolint ignore=DL3008,DL3015
RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git gnupg && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/idris2-pack

COPY Makefile .
COPY src src
COPY micropack micropack
COPY micropack.bash .
COPY pack.ipkg .
COPY pack-admin.ipkg .
RUN true

ENV SCHEME=chezscheme

RUN make micropack SCHEME=$SCHEME

FROM ubuntu:22.04

# hadolint ignore=DL3008,DL3015
RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git && rm -rf /var/lib/apt/lists/*

SHELL ["/bin/bash", "-c"]

ENV HOME="/root"
ENV PACK_DIR="$HOME/.pack"

ENV PATH "$PACK_DIR/bin:$PATH"
COPY --from=build $PACK_DIR $PACK_DIR

HEALTHCHECK CMD pack help || exit 1
