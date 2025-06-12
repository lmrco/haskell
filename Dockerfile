# -----------------------------------
# Base image layer
# -----------------------------------

FROM haskell:9.12.2-bookworm AS base

# Install essential system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        git \
        libgmp10 \
        libtinfo6 \
        python3 \
        python3-pip \
        npm && \
    rm -rf /var/lib/apt/lists/*

# Set environment variables for PATH and working directory
ENV PATH="/root/.local/bin:$PATH"
WORKDIR /app

# -----------------------------------
# Build image layer
# -----------------------------------

FROM ghcr.io/lmrco/haskell-base:0.0.1.2 AS dev

# Copy application source code
COPY . .

# Configure Stack settings
ENV STACK_ROOT=/tmp/stack-root
ENV PATH="/root/.local/bin:$PATH"

# Prepare Stack root directory and set permissions
RUN mkdir -p $STACK_ROOT && \
    chmod -R 777 $STACK_ROOT

# Install GHC, build tools, and dependencies
RUN stack setup --install-ghc --allow-different-user && \
    stack install hlint fourmolu --allow-different-user \
        --local-bin-path=/root/.local/bin && \
    stack build --only-dependencies --allow-different-user

# Build and install the project
RUN stack build --allow-different-user && \
    stack install --allow-different-user --local-bin-path=/root/.local/bin

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
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# -----------------------------------
# Server image layer
# -----------------------------------

FROM ghcr.io/lmrco/haskell-live:0.0.1.2 AS server

# Copy the built binary from the development stage
ARG BINARY_PATH
COPY $BINARY_PATH /usr/local/bin/haskell-server

# Ensure the binary has executable permissions
RUN chmod +x /usr/local/bin/haskell-server

# Expose the application port and define the default command
EXPOSE 8080
CMD ["/usr/local/bin/haskell-server"]
