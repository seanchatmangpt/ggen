# ggen task runner — single entry point for all dev commands
# Delegates directly to cargo; Makefile.toml is kept as historical reference only.

_default:
    @just --list

# ── Pre-flight ────────────────────────────────────────────────────────────────

# Verify timeout command exists (required for timed recipes)
timeout-check:
    @if command -v timeout >/dev/null 2>&1; then \
        echo "✅ timeout command verified"; \
    else \
        echo "❌ ERROR: timeout not found. Install: brew install coreutils (macOS) or sudo apt install coreutils (Linux)"; \
        exit 1; \
    fi

# ── Compilation ───────────────────────────────────────────────────────────────

# Check all 15 crates without building (fast feedback)
check:
    timeout 60s cargo check --workspace

# Build the ggen CLI binary in debug mode
build:
    cargo build --workspace

# Build release binary
build-release:
    timeout 600s cargo build --release -p ggen-cli --bin ggen

# Remove build artifacts
clean:
    cargo clean

# ── Formatting ────────────────────────────────────────────────────────────────

# Format all code
fmt:
    cargo fmt --all

# Check formatting without modifying (used in pre-commit)
fmt-check:
    cargo fmt --all -- --check

# ── Linting ───────────────────────────────────────────────────────────────────

# Clippy with -D warnings across all targets (90s; first run compiles deps)
lint:
    timeout 90s cargo clippy --all-targets -- -D warnings

# ── Testing ───────────────────────────────────────────────────────────────────

# Integration tests — the primary test gate (<30s hot cache)
test:
    #!/usr/bin/env bash
    set -euo pipefail
    if timeout 30s cargo test --workspace --tests; then exit 0; fi
    status=$?
    [ "$status" -eq 124 ] || exit "$status"
    echo "⚠️  First compile >30s, escalating to 120s..."
    timeout 120s cargo test --workspace --tests

# Unit/lib tests inside each crate
test-lib:
    timeout 30s cargo test --lib --workspace

# Doctests — validates all /// Examples blocks compile and run
test-doc:
    #!/usr/bin/env bash
    set -euo pipefail
    if timeout 60s cargo test --doc --workspace --exclude ggen-core; then exit 0; fi
    status=$?
    [ "$status" -eq 124 ] || exit "$status"
    echo "⚠️  Doc tests >60s, escalating to 180s..."
    timeout 180s cargo test --doc --workspace --exclude ggen-core

# BDD aspirational specs (opt-in; slow)
test-bdd:
    timeout 300s cargo test --test bdd --workspace -- --include-ignored

# Marketplace pack tests — fast gate (Layers 1+2, no network required)
test-marketplace:
    cargo test -p ggen-core --test lsp_max_pack_test
    cargo test -p ggen-core --test all_marketplace_packs_validation_test

# Marketplace full gate including compilation proof (requires crates.io network)
test-marketplace-full:
    cargo test -p ggen-core --test lsp_max_pack_test -- --include-ignored
    cargo test -p ggen-core --test all_marketplace_packs_validation_test

# Mutation testing (≥60% score required)
test-mutation:
    cargo mutants --workspace

# ── Quality gates ─────────────────────────────────────────────────────────────

# Full pre-commit gate: fmt → check → lint → test-lib (in sequence, fail fast)
pre-commit: fmt-check check lint test-lib

# Performance SLO validation
slo-check:
    cargo bench --bench cli_startup_performance -- --test

# Security vulnerability scan
audit:
    cargo audit

# ── Documentation ─────────────────────────────────────────────────────────────

# Build API docs from /// comments (no browser open)
doc:
    cargo doc --workspace --no-deps

# ── Benchmarks ────────────────────────────────────────────────────────────────

bench:
    cargo bench

# ── ggen pipeline ─────────────────────────────────────────────────────────────

# Full μ₁-μ₅ sync with cryptographic receipt
sync:
    ggen sync --audit true

# Preview sync without writing any files
sync-dry:
    ggen sync --dry_run true

# ── cargo-cicd ────────────────────────────────────────────────────────────────

# Full workspace health check
doctor:
    cargo cicd workspace doctor

# Run only tests for crates changed since origin/main
test-changed:
    cargo cicd test changed

# Check git working-tree state
git-status:
    cargo cicd git status

# Check publish readiness
publish-check:
    cargo cicd publish run

# Show target directory size
target-show:
    cargo cicd target show

# Prune stale build artifacts
target-prune:
    cargo cicd target prune

# Run the full manufacturing pipeline (status, target, tests, doctor, publish check, oracle audit)
pipeline:
    cargo cicd pipeline run

# Check pipeline preconditions without running
pipeline-validate:
    cargo cicd pipeline validate

# Invoke ggen sync via cargo-cicd (requires ggen binary on PATH)
workspace-sync:
    cargo cicd workspace sync

# Show pipeline state: evidence files and cicd.toml fields
pipeline-status:
    cargo cicd pipeline status
