# -----------------------------------
# Base image layer
# -----------------------------------

FROM ubuntu:20.04 AS base

# Set version arguments for GHC, Cabal, and HLS
ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG HLS_VERSION=1.7.0.0
ARG INDEX_STATE=2025-04-08T10:52:25Z

# Set iohk libsodium and secp256k1 git revisions
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG IOHK_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a

# Set the environment variables for locale and versions
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    LANGUAGE=en_US:en \
    WORKDIR=/app \
    GHC_VERSION=8.10.7 \
    CABAL_VERSION=3.6.2.0 \
    DEBIAN_FRONTEND=noninteractive \
    PATH=${PATH}:${HOME:-/root}/.ghcup/bin

# Install essential system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        automake \
        build-essential \
        ca-certificates \
        curl \
        g++ \
        gcc \
        git \
        jq \
        libffi-dev \
        libgmp-dev \
        libicu-dev \
        libncursesw5 \
        libnuma-dev \
        libpq-dev \
        libreadline-dev \
        libssl-dev \
        libsystemd-dev \
        libtinfo-dev \
        libtool \
        llvm \
        make \
        pkg-config \
        tmux \
        wget \
        xz-utils \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git fetch --all --tags && \
    git checkout ${IOHK_LIBSECP251_GIT_REV} && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install && cd .. && rm -rf ./secp256k1

# install libsodium from sources with prefix '/'
RUN git clone https://github.com/input-output-hk/libsodium.git && \
    cd libsodium && \
    git fetch --all --tags && \
    git checkout ${IOHK_LIBSODIUM_GIT_REV} && \
    ./autogen.sh && \
    ./configure --prefix=/usr && \
    make && \
    make install  && cd .. && rm -rf ./libsodium

# Install ghcup
RUN wget --secure-protocol=TLSv1_2 https://downloads.haskell.org/~ghcup/$(arch)-linux-ghcup && \
    chmod +x $(arch)-linux-ghcup && \
    mkdir -p ${HOME:-/root}/.ghcup/bin && \
    mv $(arch)-linux-ghcup ${HOME:-/root}/.ghcup/bin/ghcup

# install ghc, caball, and hls
RUN ghcup config set downloader Wget && \
    ghcup install ghc ${GHC_VERSION} && \
    ghcup install cabal ${CABAL_VERSION} && \
    ghcup set ghc ${GHC_VERSION} && \
    ghcup install hls ${HLS_VERSION}

# Update cabal
RUN cabal update --index-state='hackage.haskell.org $INDEX_STATE, cardano-haskell-packages $INDEX_STATE'

# Add cabal to PATH
RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc

# Install lint and formatting tools
RUN cabal install hlint fourmolu --installdir=/usr/local/bin

# Set working directory and environment variables
WORKDIR $WORKDIR

# Copy project configuration files
COPY cabal.project haskell.cabal ./

# Install project dependencies
RUN cabal build --only-dependencies --enable-tests --enable-benchmarks && \
    cabal install --only-dependencies --enable-tests --enable-benchmarks

# Set default commands for the image
CMD ["bash"]

# -----------------------------------
# Builder image layer
# -----------------------------------

FROM ghcr.io/lmrco/haskell-base:0.0.1.3 AS builder

# Set the working directory
WORKDIR $WORKDIR

# Copy application source code
COPY . .

# Build and install the project
RUN cabal build && \
    cabal install

# Set default commands for the image
CMD ["bash"]

# -----------------------------------
# Live image layer
# -----------------------------------

FROM debian:bookworm-slim AS live

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        locales \
        libgmp10 \
        libc6 \
        libgcc1 \
        libstdc++6 && \
    rm -rf /var/lib/apt/lists/*

# Set the locale to UTF-8
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8

# Ensure the environment uses the correct locale
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    LANGUAGE=en_US:en

# -----------------------------------
# Server image layer
# -----------------------------------

FROM ghcr.io/lmrco/haskell-live:0.0.1.1 AS server

# Set the working directory
ARG BINARY_PATH

# Copy the built binary from the development stage
COPY $BINARY_PATH /usr/local/bin/haskell-server

# Ensure the binary has executable permissions
RUN chmod +x /usr/local/bin/haskell-server

# Expose the application port and define the default command
EXPOSE 8080

# Set the command to run the Haskell server
CMD ["/usr/local/bin/haskell-server"]
