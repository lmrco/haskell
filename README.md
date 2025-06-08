# haskell
=======
# Haskell

[![Haskell CI](https://github.com/<your-username>/<your-repo>/actions/workflows/haskell.yml/badge.svg)](https://github.com/<your-username>/<your-repo>/actions/workflows/haskell.yml)

---

## ✨ Features

- 📦 Written in Haskell using Cabal
- ✅ Unit and property-based tests with Hspec + QuickCheck
- 🧪 JUnit-style test report integration for GitHub Actions
- 🎨 Code formatting check via Fourmolu
- 🔍 Static analysis via HLint
- 🛠️ GitHub Actions CI for every branch, tag, and pull request
- 📚 Auto-generated Haddock documentation, published per branch to GitHub Pages
- 🪝 Pre-commit hooks for formatting, linting, and tests

---


## 🛠️ Getting Started


### 📦 System Requirements

Make sure the following tools are installed on your system:

# Install GitHub CLI
```bash
brew install gh
```

# Install GitHub Pages CLI for deploying docs
```
npm install -g gh-pages
```

# Install pre-commit hooks runner
```
pip install --upgrade pre-commit
```


### Requirements

- [GHC](https://www.haskell.org/ghc/) ≥ 9.8.2

- [Cabal](https://www.haskell.org/cabal/) ≥ 3.10

- [ghcup](https://www.haskell.org/ghcup/) (recommended for installing toolchain)

- GNU Make

To install the Haskell toolchain using ghcup:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

---

## 🛠 Haskell CI Makefile & Docker Tooling

This project includes a powerful `Makefile` and Docker-based CI pipeline to streamline development, testing, and documentation.


### 🔧 Local Development

| Command               | Description                                     |
|----------------------|-------------------------------------------------|
| `make setup`         | Install GHC, Cabal, formatters, and tools       |
| `make ci`            | Run full CI locally (build, test, lint, docs)   |
| `make build`         | Build the Haskell project                       |
| `make test`          | Run tests (console output)                      |
| `make test-report`   | Run tests and generate JUnit XML                |
| `make lint`          | Run static analysis with `hlint`                |
| `make format`        | Auto-format code using `fourmolu`               |
| `make docs`          | Generate and open Haddock documentation         |
| `make publish-docs`  | Deploy docs to GitHub Pages                     |
| `make clean`         | Remove build artifacts and temp files           |


### 🐳 Docker Dev Image

| Command               | Description                                           |
|----------------------|-------------------------------------------------------|
| `make docker-build`  | Build `ghcr.io/lmrco/haskell:<SHA>` locally       |
| `make docker-push`   | Push the image to GHCR                                |
| `make docker-run`    | Start interactive shell inside the image              |

> The tag is computed as `sha256sum` of `cabal.project.freeze`.

---

## 📚 Documentation

Haddock-generated API documentation is published per branch:

- [View docs for `main`](https://<your-username>.github.io/<your-repo>/main/)
