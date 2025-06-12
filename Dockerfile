# -----------------------------------
# Base image layer
# -----------------------------------

FROM --platform=linux/amd64 haskell:8.10.7 AS base

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    STACK_ROOT=/home/haskell/.stack

# Install essential system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        gcc \
        git \
        gnupg \
        libgmp-dev \
        libgmp10 \
        libtinfo6 \
        npm \
        python3 \
        python3-pip \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Create a non-root user with configurable UID/GID
ARG HOST_UID=1000
ARG HOST_GID=1000

# Set default values for UID and GID if not provided
RUN useradd -m -u $HOST_UID -g $HOST_GID haskell

# Ensure the user has a home directory and set permissions
RUN getent group $HOST_GID && \
    groupmod -n haskell dialout && \
    getent group haskell

# Prepare Stack root directory
RUN mkdir -p $STACK_ROOT && \
    chmod -R 770 $STACK_ROOT && \
    chown -R haskell:haskell $STACK_ROOT

# Set the home directory and PATH for the haskell user
ENV HOME_DIR=/home/haskell \
    PATH="$HOME_DIR/.local/bin:$PATH"

# Create the home directory for the haskell user
RUN mkdir -p $HOME_DIR/.local/bin && \
    chmod -R 770 $HOME_DIR/.local/bin && \
    chown -R haskell:haskell $HOME_DIR/.local/bin

# Create a global project directory for Stack
RUN mkdir -pv $STACK_ROOT/global-project && \
    chmod -R 775 $STACK_ROOT/global-project && \
    chown -R haskell:haskell $STACK_ROOT/global-project && \
    echo "packages: []\nsnapshot: lts-18.28" > $STACK_ROOT/global-project/stack.yaml

# Install stack
RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f

# Switch to the haskell user
USER haskell

# Set up Stack for the haskell user
RUN stack setup --resolver lts-18.28

# Optional tools
RUN stack install hlint fourmolu \
    --resolver lts-18.28 --local-bin-path=/home/haskell/.local/bin

WORKDIR /work

# Copy application source code
#COPY --chown=haskell:haskell . .
COPY --chown=haskell:haskell stack.yaml stack.yaml.lock haskell.cabal ./
#COPY --chown=haskell:haskell src ./src
#COPY --chown=haskell:haskell app ./app
#COPY --chown=haskell:haskell test ./test

# Cache dependencies first (avoids rebuilding on code change)
RUN stack build  \
    --only-dependencies \
    --resolver lts-18.28

# -----------------------------------
# Build image layer
# -----------------------------------

FROM --platform=linux/amd64 ghcr.io/lmrco/haskell-base:0.0.1.3 AS dev

# Switch to the haskell user
USER haskell

# Set the home directory and PATH for the haskell user
ENV HOME_DIR=/home/haskell \
    PATH="$HOME_DIR/.local/bin:$PATH"

# Set the working directory
WORKDIR /work

# Copy application source code
COPY --chown=haskell:haskell . .

# Build and install the project
RUN stack build && \
    stack install --local-bin-path=/home/haskell/.local/bin

# -----------------------------------
# Live image layer
# -----------------------------------

FROM --platform=linux/amd64 debian:bookworm-slim AS live

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
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# -----------------------------------
# Server image layer
# -----------------------------------

FROM --platform=linux/amd64 ghcr.io/lmrco/haskell-live:0.0.1.1 AS server

# Copy the built binary from the development stage
ARG BINARY_PATH
COPY $BINARY_PATH /usr/local/bin/haskell-server

# Ensure the binary has executable permissions
RUN chmod +x /usr/local/bin/haskell-server

# Expose the application port and define the default command
EXPOSE 8080
CMD ["/usr/local/bin/haskell-server"]
