# -----------------------------------
# Base image layer
# -----------------------------------

FROM ubuntu:22.04 AS base

# Set the environment variables for locale and versions
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    LANGUAGE=en_US:en \
    WORKDIR=/app \
    GHC_VERSION=9.10.1 \
    CABAL_VERSION=3.14.1.1 \
    DEBIAN_FRONTEND=noninteractive \
    PATH=${PATH}:/root/.ghcup/bin

# Install essential system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        ca-certificates \
        chrony \
        curl \
        dpkg-dev \
        g++ \
        gcc \
        git \
        gnupg \
        jq \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        libicu-dev \
        liblzma-dev \
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
        netbase \
        pkg-config \
        procps \
        tmux \
        wget \
        xz-utils \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# install secp2561k library with prefix '/'
ARG IOHK_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git fetch --all --tags && \
    git checkout ${IOHK_LIBSECP251_GIT_REV} && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install && cd .. && rm -rf ./secp256k1

# install libsodium from sources with prefix '/'
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG IOHK_LIBSODIUM_GIT_REV=dbb48cc
RUN git clone https://github.com/input-output-hk/libsodium.git && \
    cd libsodium && \
    git fetch --all --tags && \
    git checkout ${IOHK_LIBSODIUM_GIT_REV} && \
    ./autogen.sh && \
    ./configure --prefix=/usr && \
    make && \
    make install  && cd .. && rm -rf ./libsodium

# Download and install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC, Cabal, and HLS using ghcup
ARG GHC_VERSION=9.10.1
ARG CABAL_VERSION=3.14.1.1
RUN ghcup install ghc ${GHC_VERSION} && \
    ghcup set ghc ${GHC_VERSION} && \
    ghcup install cabal ${CABAL_VERSION} && \
    ghcup set cabal ${CABAL_VERSION}

# Add cabal to PATH
RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc

# Set the working directory
WORKDIR $WORKDIR

# Copy the project files
COPY cabal.project haskell.cabal ./

# Update cabal package list
RUN cabal clean && \
    cabal update && \
    cabal build --only-dependencies --enable-tests --enable-benchmarks

# Install project dependencies
RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc

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
