# 🧪 Haskell Project Template

A modern Haskell project with:
- 📦 Reproducible builds via `cabal`
- 🐳 Multi-platform Docker images
- 🧹 Linting and formatting with `hlint` and `fourmolu`
- 🧪 Unit & integration tests
- 🚀 GitHub + Google Artifact Registry support
- 🔖 Git-based versioning and automated changelog

## 🚀 Quick Start

### Clone and Setup
```bash
git clone https://github.com/lmrco/haskell.git
cd haskell
make setup                  # Install dependencies
```

### Build & Run
```bash
make build                  # Build the project using Cabal
make run                    # Run the server
```

## Test, Lint, Format
```bash
make test                   # Run unit & integration tests using Cabal
make lint                   # Run HLint
make format                 # Autoformat using Fourmolu
```

## 🐳 Docker Commands

Build all images
```bash
make docker-build           # Build all Docker images
```

Or selectively:
```bash
make docker-build-base      # Base tools (GHC, Python, Node)
make docker-build-live      # Runtime-only image
make docker-build-server    # Final production image
```

Run the server
```bash
make docker-run-server      # Run the server in a container
```

Open a shell in the dev image
```bash
make shell                  # Open a shell in the dev container
```

## 🧪 Testing

| Command | Description |
| --- | --- |
| `make test` | Run all tests using Cabal |
| `make cabal-test` | Same as above |
| `make cabal-unit-test` | Unit tests only |
| `make cabal-integration-test` | With temp server |

Integration tests launch `cabal run haskell-server` in the background, then kill it after tests complete.

## 🔖 Git Versioning & Releases

This project uses [bump2version](https://github.com/c4urself/bump2version) and [git-cliff](https://github.com/orhun/git-cliff) for version management.

### Initial Setup
```bash
make bump-setup             # Initialize versioning
```

### Version Bump
```bash
make bump-patch             # 0.0.1
make bump-minor             # 0.1.0
make bump-major             # 1.0.0
```

### Generate Changelog
```bash
make changelog              # Generate CHANGELOG.md
```

This uses Git history to generate a clean `CHANGELOG.md`.

## 🛠 Environment

Ensure these tools are available:
- `cabal`
- `docker` and `docker buildx`
- `gcloud` (for pushing to Google Artifact Registry)
- `pip3 install bump2version git-cliff` (for versioning)

Environment Variables
Define these in `.env` or `.env.local`:
```bash
GITHUB_ACTOR=your-github-username
GITHUB_TOKEN=ghp_xxx
GOOGLE_PROJECT_ID=your-google-project-id
GOOGLE_REPO=your-repo
```

## 📁 Project Structure

```
/
├── src/               # Application source
├── test/              # Test suite (unit + integration)
├── Dockerfile         # Multi-stage build
├── Makefile           # All build/dev targets
├── VERSION            # Managed by bump2version
├── CHANGELOG.md       # Auto-generated
└── .bumpversion.cfg   # Versioning config
```

## 🧹 Code Quality

- HLint: Enforces best practices.
- Fourmolu: Ensures consistent formatting.
- Test Reports: Generated in `test-reports/` (if configured in CI).
