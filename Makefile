# ----------------------------------------------------------------
# Makefile for Haskell project
# ----------------------------------------------------------------

# Include environment variables from `.env` and `.env.local`
-include .env
-include .env.local

# ----------------------------------------------------------------
# Platform and architecture detection
# ----------------------------------------------------------------

# Detect the operating system
OS_NAME := linux

# Detect the architecture
ARCH_NAME := $(shell uname -m)

# Combine OS and architecture into a platform string
PLATFORM := $(OS_NAME)/$(ARCH_NAME)

# Define a list of platforms including the current one and its alternative
PLATFORMS := $(PLATFORM),$(if $(filter linux/amd64,$(PLATFORM)),linux/arm64,linux/amd64)

# ----------------------------------------------------------------
# Configurable Variables
# ----------------------------------------------------------------
VERSION 		  	?= $(shell cat VERSION 2>/dev/null || echo '0.0.1.3')

# Paths
BIN_PATH          	?= bin

# Haskell toolchain versions
GHC_VERSION       	?= 9.10.1
CABAL_VERSION     	?= 3.14.1.1
PROJECT_NAME      	?= haskell
DOCKER_WORKDIR    	?= /app

# Github Container Registry
GITHUB_REGISTRY   	?= ghcr.io
GITHUB_PROJECT_ID 	?= lmrco

# Google Artifact Registry
GOOGLE_REPO       	?= docker
GOOGLE_REGISTRY   	?= europe-west6-docker.pkg.dev
GOOGLE_PROJECT_ID 	?= nimble-repeater-462408-j7

# Docker image versions
BASE_IMAGE_VERSION 	= 0.0.1.3
LIVE_IMAGE_VERSION 	= 0.0.1.1
SERVER_IMAGE_VERSION = 0.0.1.1

# Docker images
BASE_IMAGE        	:= ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-base:${BASE_IMAGE_VERSION}
LIVE_IMAGE        	:= ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-live:${LIVE_IMAGE_VERSION}
SERVER_IMAGE      	:= ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-server:${SERVER_IMAGE_VERSION}

# ----------------------------------------------------------------
# Git metadata (for dynamic tagging)
# ----------------------------------------------------------------

GIT_TAG        		?= $(shell git describe --tags --abbrev=0 2>/dev/null || echo 0.0.0)
GIT_COMMIT     		?= $(shell git rev-parse --short HEAD)

# Default target: Build, test, lint, and format
.PHONY: all
all: clean build test lint format yamllint

# Setup development environment
.PHONY: setup
setup: cabal-setup

# Clean build artifacts
.PHONY: clean
clean: cabal-clean
	rm -rf ${BIN_PATH}

# Build the project
.PHONY: build
build: cabal-build

# Install the project binaries locally
.PHONY: install
install: cabal-install

# Run tests (unit + integration)
.PHONY: test
test: cabal-test

# Lint the codebase using HLint
.PHONY: lint
lint:
	hlint app/ src/ test/

# Check code formatting using Fourmolu
.PHONY: format-check
format-check:
	find app/ src/ test/ -name '*.hs' | xargs fourmolu --mode check

# Format the codebase using Fourmolu
.PHONY: format
format:
	find app/ src/ test/ -name '*.hs' | xargs fourmolu --mode inplace

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
run: cabal-run

# ----------------------------------------------------------------
# Cabal targets
# ----------------------------------------------------------------

.PHONY: cabal
cabal: ghcup-install ghcup-install-ghc ghcup-install-cabal cabal-update cabal-freeze cabal-build cabal-install

# Download and install ghcup
.PHONY: ghcup-install
ghcup-install:
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- --yes --set-default 9.10.1

# Install GHC using ghcup
.PHONY: ghcup-install-ghc
ghcup-install-ghc:
	ghcup install ghc ${GHC_VERSION}
	ghcup set ghc ${GHC_VERSION}
	ghc --version

# Install Cabal using ghcup
.PHONY: ghcup-install-cabal
ghcup-install-cabal:
	ghcup install cabal ${CABAL_VERSION}
	ghcup set cabal ${CABAL_VERSION}
	cabal --version

# Clean build artifacts
.PHONY: cabal-clean
cabal-clean:
	cabal clean && rm -rf ~/.cabal/packages && rm -fr cabal.project.freeze

# Update package lists
.PHONY: cabal-update
cabal-update:
	cabal update

# Build all components (library, executables, tests)
.PHONY: cabal-freeze
cabal-freeze:
	cabal freeze

# Build all components (library, executables, tests)
.PHONY: cabal-build
cabal-build:
	cabal build all

# Install the project binaries to a local directory
.PHONY: cabal-install
cabal-install:
	cabal install

# Run all test suites
.PHONY: cabal-test
cabal-test: cabal-unit-test cabal-integration-test

# Run unit tests
.PHONY: cabal-unit-test
cabal-unit-test:
	cabal test haskell:haskell-unit-tests

# Run integration tests
.PHONY: cabal-integration-test
cabal-integration-test:
	cabal test haskell:haskell-integration-tests

# Run the main executable
.PHONY: cabal-run
cabal-run:
	cabal run haskell-server

# Open a REPL for the library or executable
.PHONY: cabal-repl
cabal-repl:
	cabal repl

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
		--target base \
		--progress=plain \
		--platform ${PLATFORMS} \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-base:${BASE_IMAGE_VERSION} \
		-f Dockerfile .

# Build the live image
.PHONY: docker-build-live
docker-build-live: login-github
	docker buildx build \
		--push \
		--target live \
		--progress=plain \
		--platform ${PLATFORMS} \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-live:${LIVE_IMAGE_VERSION} \
		-f Dockerfile .

# Build the server image
.PHONY: docker-build-server
docker-build-server: login-github login-google
	docker buildx build \
		--push \
		--target server \
		--progress=plain \
		--platform ${PLATFORMS} \
		-t ${GITHUB_REGISTRY}/${GITHUB_PROJECT_ID}/haskell-server:${VERSION} \
		-t ${GOOGLE_REGISTRY}/${GOOGLE_PROJECT_ID}/${GOOGLE_REPO}/haskell-server:${VERSION} \
		-f Dockerfile .

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
	echo "Setting up version bumping tools..."
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
	git-cliff -o CH
