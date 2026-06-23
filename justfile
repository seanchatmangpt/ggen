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
    timeout 300s cargo check --workspace

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

# Performance SLO validation — all registered SLO benchmark suites in test mode
# (--test runs Criterion benchmarks as pass/fail assertions, not timed runs)
slo-check:
    cargo bench --bench cli_startup_performance -- --test
    cargo bench --bench ggen_benchmarks -- --test
    cargo bench --bench comprehensive_slo_benchmarks -- --test
    cargo bench --bench schema_layer_slo -- --test
    cargo bench --bench marketplace_performance -- --test

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

# ── lsp-max scaffold ──────────────────────────────────────────────────────────

LSP_MAX_MANIFEST := ".specify/specs/lsp-max/ggen.toml"
LSP_MAX_SCAFFOLD := ".specify/specs/lsp-max/examples/lsp-max-scaffold"

# Generate a new rule-pack LSP server from lsp.ttl (sync + compile check)
lsp-max-new: lsp-max-sync lsp-max-check

# Run the ggen μ-pipeline for lsp-max (30ms)
lsp-max-sync:
    ggen sync --manifest {{LSP_MAX_MANIFEST}}

# Cargo check every generated scaffold crate
lsp-max-check:
    #!/usr/bin/env bash
    set -euo pipefail
    for toml in {{LSP_MAX_SCAFFOLD}}/*/Cargo.toml; do
        name=$(basename "$(dirname "$toml")")
        echo "checking $name..."
        cargo check --manifest-path "$toml"
    done
    echo "all scaffold crates OK"

# Edit lsp.ttl then regenerate and check in one command
lsp-max-edit:
    $EDITOR .specify/specs/lsp-max/lsp.ttl
    just lsp-max-new
