FROM snazzybucket/idris2api AS build

SHELL ["/bin/bash", "-c"]

RUN apt-get update && apt-get install --yes make git && rm -rf /var/lib/apt/lists/*

ENV PATH "/root/.pack/bin:/root/.idris2/bin:$PATH"

WORKDIR /opt/idris2-pack

RUN git clone https://github.com/cuddlefishie/toml-idr.git

WORKDIR /opt/idris2-pack/toml-idr

RUN make install

WORKDIR /opt/idris2-pack

ADD src ./src
COPY pack.ipkg .
COPY pack-admin.ipkg .

RUN idris2 --build pack.ipkg
RUN ./build/exec/pack -p nightly-220604
