FROM haskell:9.8.2-buster AS base

RUN apt-get update
RUN apt-get install -y \
        curl \
        gnupg \
        npm \
        git \
        python3 \
        python3-pip \
        docker.io && \
    rm -rf /var/lib/apt/lists/*


FROM base AS dev

RUN npm install -g gh-pages

RUN cabal update
RUN cabal install hlint
RUN cabal install fourmolu

ENV PATH="/root/.cabal/bin:$PATH"


FROM dev AS ci

WORKDIR /app

COPY . .

RUN cabal update && \
    cabal build all