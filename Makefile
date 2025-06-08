# Haskell CI Makefile

include .env

BRANCH_NAME := $(shell git rev-parse --abbrev-ref HEAD)

.PHONY: all ci setup tools update configure build test lint format haddock docs publish-docs precommit clean

all: ci

ci: tools update configure build test format lint haddock

# --------------------------------------
# 🔧 Tooling Setup
# --------------------------------------

setup:
	@echo "👉 Installing GHC $(GHC_VERSION) and Cabal $(CABAL_VERSION) with ghcup"
	ghcup install ghc $(GHC_VERSION) || true
	ghcup set ghc $(GHC_VERSION)
	ghcup install cabal $(CABAL_VERSION) || true
	ghcup set cabal $(CABAL_VERSION)

	@echo "🎨 Installing formatters and linters"
	brew list hlint >/dev/null 2>&1 || brew install hlint
	brew list fourmolu >/dev/null 2>&1 || brew install fourmolu

	@echo "📦 Installing required CLI tools"
	brew list pre-commit >/dev/null 2>&1 || brew install pre-commit
	brew list gh >/dev/null 2>&1 || brew install gh
	npm list -g gh-pages >/dev/null 2>&1 || npm install -g gh-pages

# --------------------------------------
# 🛠️ Build & Test
# --------------------------------------

update:
	@echo "📦 Updating Cabal package index"
	cabal update

configure:
	@echo "⚙️ Configuring project"
	cabal configure

build:
	@echo "🔨 Building project"
	cabal build all

test:
	@echo "🧪 Running tests (console output only)"
	cabal run haskell-test

test-it:
	@echo "🧪 Running integration tests"
	cabal run haskell-test-it

test-report:
	@echo "🧪 Running tests and generating JUnit XML report"
	mkdir -p test-reports
	TASTY_ANT_XML=$(TEST_REPORT_FILE) cabal run haskell-test -- --xml=$(TEST_REPORT_FILE)

run:
	@echo "🏃 Running server locally"
	cabal run haskell-server

# --------------------------------------
# 🧹 Quality Checks
# --------------------------------------

format:
	@echo "🛠 Formatting code"
	find src/ test/ -name '*.hs' | xargs fourmolu --mode inplace

format-check:
	@echo "🎨 Checking code formatting"
	find src/ test/ -name '*.hs' | xargs fourmolu --mode check

lint:
	@echo "🔍 Running static analysis with HLint"
	hlint src/ test/

precommit:
	@echo "🪝 Setting up pre-commit hooks"
	@which pre-commit >/dev/null 2>&1 || (echo "📦 Installing pre-commit via Homebrew..." && brew install pre-commit && pre-commit install)
	pre-commit run fourmolu-autoformat --all-files

# --------------------------------------
# 📚 Documentation
# --------------------------------------

BRANCH_NAME := $(shell git rev-parse --abbrev-ref HEAD)

haddock:
	@echo "📚 Generating Haddock documentation"
	cabal haddock --haddock-html --haddock-hyperlink-source

docs: haddock
	@echo "📁 Locating and opening Haddock documentation..."
	DOCS_DIR=$$(find dist-newstyle/build -type d -path "*/doc/html/*" -name haskell) && \
	xdg-open $$DOCS_DIR/index.html || open $$DOCS_DIR/index.html || true

publish-docs: haddock
	@echo "🚀 Publishing docs for branch '$(BRANCH_NAME)' to GitHub Pages"
	mkdir -p generated-docs
	DOCS_PATH=$(find dist-newstyle/build -type d -path "*/doc/html/*" -name haskell | head -n 1)
	if [ -z "$$DOCS_PATH" ]; then
	    @echo "❌ Failed to locate generated docs. Run 'make haddock' first."
	    exit 1
	fi
	cp -r $$DOCS_PATH/* generated-docs/

	@if [ "$(BRANCH_NAME)" = "main" ]; then \
	    echo '<meta http-equiv="refresh" content="0; url=main/">' > generated-docs/index.html; \
	fi

	gh auth status >/dev/null 2>&1 || gh auth login
	gh repo set-default
	gh-pages --dir=generated-docs --branch=gh-pages --dest=$(BRANCH_NAME) --message="Update docs for $(BRANCH_NAME)"

# --------------------------------------
# 🧽 Clean
# --------------------------------------

clean:
	@echo "🧹 Cleaning build artifacts"
	cabal clean
	rm -rf dist-newstyle $(TEST_REPORT_DIR) generated-docs

# --------------------------------------
# 🐳 Docker Image Configuration
# --------------------------------------

IMAGE_REPO := ghcr.io/lmrco/haskell
IMAGE_NAME := $(IMAGE_REPO)

PLATFORMS := linux/amd64,linux/arm64

BUILD_IMAGE_TAG := $(shell sha256sum cabal.project.freeze | cut -c1-64)

TAGS := -t $(IMAGE_NAME):$(BUILD_IMAGE_TAG)

# --------------------------------------
# 🐳 Docker Targets
# --------------------------------------

docker-build:
	@echo "🐳 Building haskell image: $(IMAGE_NAME):$(BUILD_IMAGE_TAG)"
	docker buildx create --use --name multiarch || true
	docker buildx build \
		--platform linux/amd64,linux/arm64 \
		--load \
		-t $(IMAGE_NAME):$(BUILD_IMAGE_TAG) \
		-f Dockerfile .

docker-push:
	@echo "🚀 Pushing image: $(IMAGE_NAME):$(BUILD_IMAGE_TAG)"
	docker buildx build \
		--platform linux/amd64,linux/arm64 \
		--push \
		-t $(IMAGE_NAME):$(BUILD_IMAGE_TAG) \
		-f Dockerfile .

docker-tag:
	@echo "$(IMAGE_NAME):$(BUILD_IMAGE_TAG)"

docker-run:
	@echo "🧪 Starting interactive shell in image"
	docker run --rm -it \
		-v "$(PWD)":/app \
		-w /app \
		$(IMAGE_NAME):$(BUILD_IMAGE_TAG) \
		bash

docker-server:
	@echo "🐋 Building and starting server inside Docker..."
	docker build --target prod -t haskell-server . && \
	docker run --rm -p 8080:8080 --name haskell-server haskell-server

docker-test-it:
	@echo "🧪 Running integration tests in Docker..."
	docker build --target test -t haskell-test-it . && \
	docker run --rm --network host haskell-test-it 2>&1 | tee test-output.log