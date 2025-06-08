# ----------------------------------------------------------------
# Makefile for Haskell project
# ----------------------------------------------------------------

# Include environment variables from `.env` and `.env.local`
-include .env
-include .env.local

# ----------------------------------------------------------------
# Configurable Variables
# ----------------------------------------------------------------
VERSION 		  ?= $(shell cat VERSION 2>/dev/null || echo '0.0.1.1')

BIN_PATH          ?= bin
STACK_WORK        ?= .stack-work

GHC_VERSION       ?= 9.8.2
CABAL_VERSION     ?= 3.10.2.0
PROJECT_NAME      ?= haskell
DOCKER_WORKDIR    ?= /app

# Github Container Registry
GITHUB_REGISTRY   ?= ghcr.io
GITHUB_PROJECT_ID ?= lmrco

# Google Artifact Registry
GOOGLE_REPO       ?= docker
GOOGLE_REGISTRY   ?= europe-west6-docker.pkg.dev
GOOGLE_PROJECT_ID ?= nimble-repeater-462408-j7

# Docker images
BASE_IMAGE        := ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-base:${VERSION}
DEV_IMAGE         := ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-dev:${VERSION}
LIVE_IMAGE        := ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-live:${VERSION}
SERVER_IMAGE      := ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-server:${VERSION}

# ----------------------------------------------------------------
# Platform and architecture detection
# ----------------------------------------------------------------

OS_NAME := $(shell uname -s)
ARCH    := $(shell uname -m)

# Normalize values
ifeq ($(OS_NAME),Darwin)
    PLATFORM_OS := linux
else ifeq ($(OS_NAME),Linux)
    PLATFORM_OS := linux
else
    PLATFORM_OS := unknown
endif

ifeq ($(ARCH),x86_64)
    PLATFORM_ARCH := amd64
else ifeq ($(ARCH),arm64)
    PLATFORM_ARCH := arm64
else
    PLATFORM_ARCH := unknown
endif

PLATFORM := $(PLATFORM_OS)/$(PLATFORM_ARCH)

OTHER_PLATFORM := $(if $(filter $(PLATFORM),linux/amd64),linux/arm64,linux/amd64)

# ----------------------------------------------------------------
# Git metadata (for dynamic tagging)
# ----------------------------------------------------------------

GIT_TAG        ?= $(shell git describe --tags --abbrev=0 2>/dev/null || echo 0.0.0)
GIT_COMMIT     ?= $(shell git rev-parse --short HEAD)

# Default target: Build, test, lint, and format
.PHONY: all
all: build test lint format yamllint

# Setup development environment
.PHONY: setup
setup: stack-setup

# Clean build artifacts
.PHONY: clean
clean: stack-clean
	rm -rf ${STACK_WORK}
	rm -rf ${BIN_PATH}

# Build the project
.PHONY: build
build: stack-build

# Install the project binaries locally
.PHONY: install
install: stack-install

# Run tests (unit + integration)
.PHONY: test
test: stack-test

# Lint the codebase using HLint
.PHONY: lint
lint:
	hlint app/ src/ test/

# Check code formatting using Fourmolu
.PHONY: format-check
format-check:
	find src/ test/ -name '*.hs' | xargs fourmolu --mode check

# Format the codebase using Fourmolu
.PHONY: format
format:
	find src/ test/ -name '*.hs' | xargs fourmolu --mode inplace

# YAML Linting
.PHONY: yamllint
yaml-lint:
	yamllint .

# Format the Cabal file
.PHONY: format-cabal
format-cabal:
	cabal-fmt haskell.cabal > formatted-haskell.cabal && mv formatted-haskell.cabal haskell.cabal

# Lint the Cabal file
.PHONY: lint-cabal
lint-cabal:
	cabal check

# Run the application server
.PHONY: run
run: stack-run

# ----------------------------------------------------------------
# Stack targets
# ----------------------------------------------------------------

# Set up GHC and dependencies
.PHONY: stack-setup
stack-setup:
	stack setup

# Clean build artifacts
.PHONY: stack-clean
stack-clean:
	stack clean

# Build the project
.PHONY: stack-build
stack-build:
	stack build

# Build the project with Docker
.PHONY: stack-build-docker
stack-build-docker:
	docker run --platform=linux/arm64 --rm -it -v /Users/lmarrocco/Workspace/haskell:/app -w /app ${DEV_IMAGE} stack build --allow-different-user
	mkdir -p ${BIN_PATH}
	cp ./.stack-work/dist/aarch64-linux-tinfo6/ghc-9.2.7/build/haskell-server/haskell-server ${BIN_PATH}/haskell-server ${BIN_PATH}/haskell-server

# Install the project binaries to a local directory
.PHONY: stack-install
stack-install:
	stack install --local-bin-path=$(BIN_PATH)

# Run the application server
.PHONY: stack-run
stack-run:
	stack exec ${PROJECT_NAME}-server

# Run all tests (unit + integration)
.PHONY: stack-test
stack-test: stack-unit-test stack-integration-test

# Run unit tests
.PHONY: stack-unit-test
stack-unit-test:
	stack test ${PROJECT_NAME}:${PROJECT_NAME}-unit-tests

# Run integration tests
.PHONY: stack-integration-test
stack-integration-test:
	@echo '🔁 Running server for integration tests...'
	@stack exec ${PROJECT_NAME}-server & \
	SERVER_PID=$$! && \
	sleep 1 && \
	stack test ${PROJECT_NAME}:${PROJECT_NAME}-integration-tests ; \
	EXIT_CODE=$$? ; \
	kill $$SERVER_PID ; \
	wait $$SERVER_PID 2>/dev/null ; \
	pkill ${PROJECT_NAME}-server
	exit $$EXIT_CODE

# ----------------------------------------------------------------
# Login to Docker registries
# ----------------------------------------------------------------

# Log in to GitHub Container Registry
.PHONY: login-github
login-github:
	docker login ${GITHUB_REGISTRY} -u $(GITHUB_ACTOR) -p $(GITHUB_TOKEN)

# Log in to Google Artifact Registry
.PHONY: login-google
login-google:
	gcloud auth configure-docker ${GOOGLE_REGISTRY} --quiet

# ----------------------------------------------------------------
# Docker targets
# ----------------------------------------------------------------

# Lint the codebase inside a Docker container
.PHONY: docker-lint
docker-lint:
	docker run --platform=${PLATFORM} --rm -v $(PWD):$(DOCKER_WORKDIR) -w $(DOCKER_WORKDIR) $(DEV_IMAGE) hlint src/ test/

# Check code formatting inside a Docker container
.PHONY: docker-format-check
docker-format-check:
	docker run --platform=${PLATFORM} --rm -v $(PWD):$(DOCKER_WORKDIR) -w $(DOCKER_WORKDIR) $(DEV_IMAGE) fourmolu --mode check src/ test/

# Format the codebase inside a Docker container
.PHONY: docker-format
docker-format:
	docker run --platform=${PLATFORM} --rm -v $(PWD):$(DOCKER_WORKDIR) -w $(DOCKER_WORKDIR) $(DEV_IMAGE) fourmolu --mode inplace src/ test/

# Build all Docker images
.PHONY: docker-build
docker-build: docker-build-base docker-build-build docker-build-live docker-build-server

# Build the base image
.PHONY: docker-build-base
docker-build-base: login-github
	docker buildx build \
		--push \
		-f Dockerfile \
		--progress=plain \
		--platform ${PLATFORM},${OTHER_PLATFORM} \
		--target base \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-base:${VERSION} \
		.

# Build the development image
.PHONY: docker-build-dev
docker-build-dev: login-github
	docker buildx build \
		--push \
		-f Dockerfile \
		--progress=plain \
		--platform ${PLATFORM},${OTHER_PLATFORM} \
		--target dev \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-dev:${VERSION} \
		.

# Build the live image
.PHONY: docker-build-live
docker-build-live: login-github
	docker buildx build \
		--push \
		-f Dockerfile \
		--progress=plain \
		--platform ${PLATFORM},${OTHER_PLATFORM} \
		--target live \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-live:${VERSION} \
		.

# Build the server image
.PHONY: docker-build-server
docker-build-server: login-github login-google
	docker buildx build \
		--push \
		-f Dockerfile \
		--progress=plain \
		--platform ${PLATFORM},${OTHER_PLATFORM} \
		--target server \
		--build-arg BINARY_PATH=${BIN_PATH}/haskell-server \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-server:${VERSION} \
		-t ${GOOGLE_REGISTRY}/${GOOGLE_PROJECT_ID}/${GOOGLE_REPO}/haskell-server:${VERSION} \
		.

# Remove unused Docker images
.PHONY: docker-clean
docker-clean:
	docker rmi $(BASE_IMAGE) || true
	docker rmi $(DEV_IMAGE) || true
	docker rmi $(LIVE_IMAGE) || true
	docker rmi $(SERVER_IMAGE) || true

# Run the server container locally
.PHONY: docker-run-server
docker-run-server:
	docker run --platform=${PLATFORM} --detach --rm --name haskell-server -p 8080:8080 "${SERVER_IMAGE}"
	# Wait for the server to start
	echo "⏳ Waiting for server to be ready..."
	for i in {1..10}; do
	if curl -sSf http://localhost:8080 > /dev/null; then
	  echo "✅ Server is up!"
	  break
	fi
	echo "Still waiting... (${i}s)"
	sleep 3
	done
	# Check logs if the server fails to start
	if ! curl -sSf http://localhost:8080 > /dev/null; then
	echo "❌ Server failed to start. Checking logs..."
	docker logs haskell-server
	exit 1
	fi
	echo "✅ Server is running"
	docker stop haskell-server

# ----------------------------------------------------------------
# Shell in container
# ----------------------------------------------------------------

.PHONY: shell
shell:
	docker run --platform=${PLATFORM} --rm -it -v $(PWD):$(DOCKER_WORKDIR) -w $(DOCKER_WORKDIR) $(DEV_IMAGE) /bin/bash

# ----------------------------------------------------------------
# Version bumping and changelog
# ----------------------------------------------------------------

.PHONY: bump-setup
bump-setup:
	@echo "Setting up version bumping tools..."
	pip3 install bump2version git-cliff

.PHONY: bump-patch
bump-patch:
	bump2version patch --commit --tag

.PHONY: bump-minor
bump-minor:
	bump2version minor --commit --tag

.PHONY: bump-major
bump-major:
	bump2version major --commit --tag

.PHONY: changelog
changelog:
	git-cliff -o CHANGELOG.md