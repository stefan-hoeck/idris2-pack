FROM ubuntu:22.04 AS build

SHELL ["/bin/bash", "-c"]

ENV PATH "/root/.pack/bin:/root/.idris2/bin:$PATH"

RUN apt-get update && apt-get install --yes gcc make chezscheme libgmp3-dev git gnupg && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/idris2-pack

COPY Makefile .
ADD src src
ADD micropack micropack
COPY pack.ipkg .
COPY pack-admin.ipkg .
RUN true

ENV SCHEME=chezscheme

ARG db
RUN make micropack SCHEME=$SCHEME DB=$db
RUN pack install pack.ipkg

WORKDIR /opt/idris2-pack
