# -----------------------------------
# Base image layer
# -----------------------------------

FROM haskell:9.8.2-buster AS base

RUN apt-get update
RUN apt-get install -y \
        curl \
        gnupg \
        npm \
        git \
        python3 \
        python3-pip \
    rm -rf /var/lib/apt/lists/*

ENV PATH="/root/.cabal/bin:$PATH"


# -----------------------------------
# Dev image layer
# -----------------------------------

FROM base AS dev

RUN npm install -g gh-pages

RUN cabal update

RUN cabal install hlint

RUN cabal install fourmolu


# -----------------------------------
# Build image layer
# -----------------------------------

FROM dev AS build

WORKDIR /app

COPY . .

RUN cabal build all


# -----------------------------------
# Test image layer
# -----------------------------------

FROM build AS test

WORKDIR /app

CMD ["cabal", "haskell-test"]


# -----------------------------------
# IT image layer
# -----------------------------------

FROM build AS it

WORKDIR /app

CMD ["cabal", "haskell-test-it"]


# -----------------------------------
# Run image layer
# -----------------------------------

FROM debian:bookworm-slim AS prod

RUN apt-get update 
RUN apt-get install -y \
        libgmp-dev && \
    rm -rf /var/lib/apt/lists/*lists/*

COPY --from=build /app/dist-newstyle/build/*/*/haskell-server/x/haskell-server/build/haskell-server /usr/local/bin/haskell-server

EXPOSE 8080

CMD ["haskell-server"]