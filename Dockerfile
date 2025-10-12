#checkov:skip=CKV_DOCKER_3: we intend to use `root` user

ARG ubuntu_release

FROM ubuntu:$ubuntu_release AS build

SHELL ["/bin/bash", "-c"]

ENV PATH "/root/.local/bin:/root/.idris2/bin:$PATH"

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

FROM ubuntu:$ubuntu_release

# hadolint ignore=DL3008,DL3015
RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git && rm -rf /var/lib/apt/lists/*

SHELL ["/bin/bash", "-c"]

ENV HOME="/root"
ENV PACK_USER_DIR="$HOME/.config/pack"
ENV PACK_STATE_DIR="$HOME/.local/state/pack"
ENV PACK_CACHE_DIR="$HOME/.cache/pack"
ENV PACK_BIN_DIR="$HOME/.local/bin"

ENV PATH "$PACK_BIN_DIR/bin:$PATH"
COPY --from=build $PACK_BIN_DIR $PACK_BIN_DIR
COPY --from=build $PACK_USER_DIR $PACK_USER_DIR
COPY --from=build $PACK_STATE_DIR $PACK_STATE_DIR
COPY --from=build $PACK_CACHE_DIR $PACK_CACHE_DIR

HEALTHCHECK CMD pack help || exit 1
