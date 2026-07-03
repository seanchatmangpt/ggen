# ggen task runner — single entry point for all dev commands
# Delegates directly to cargo; Makefile.toml is kept as historical reference only.

GGEN := "cargo run --bin ggen --"

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

# Check the whole workspace without building (fast feedback)
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

# Clippy with -D warnings across all targets (180s; first run / cache invalidation compiles deps)
lint:
    timeout 180s cargo clippy --all-targets -- -D warnings

# ── Testing ───────────────────────────────────────────────────────────────────

# Full test suite — the primary test gate (<30s hot cache)
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

# Niche/slow suites — run directly, no `just` wrapper needed:
#   cargo test --test bdd --workspace -- --include-ignored     (BDD specs)
#   cargo test -p ggen-core --test lsp_max_pack_test -- --include-ignored
#   cargo test -p ggen-core --test all_marketplace_packs_validation_test
#   cargo mutants --workspace                                  (mutation score)
# Phase-2 / coherence / round-trip checks — same commands CI's `phase2` job runs:
#   {{GGEN}} graph validate --schema-file .specify/specs/post-chatman/post_chatman.ttl
#   cargo test -p ggen-core --test ast_extractor_70pct_test
#   cargo test -p ggen-core --test inverse_receipt_chain_test
#   cargo test -p ggen-core --test provenance_envelope_test
#   cargo test -p ggen-graph --test coherence_hash_expectations_test
#   cargo test -p ggen-graph --test post_chatman_coherence_integration

# ── Quality gates ─────────────────────────────────────────────────────────────

# Full pre-commit gate: fmt → check → lint → test-lib (in sequence, fail fast)
pre-commit: fmt-check check lint test-lib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ Pre-commit gate complete (fmt, check, lint, tests)"

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
    {{GGEN}} sync --audit true

# Preview sync without writing any files
sync-dry:
    {{GGEN}} sync --dry_run true

# Fast local health check (rust/cargo/git/marketplace/cache/ggen.toml).
# Pass `all=true` to also run SLO microbenchmarks + observability probes.
doctor all="false":
    {{GGEN}} doctor {{ if all == "true" { "--all" } else { "" } }}

# ── lsp-max scaffold ──────────────────────────────────────────────────────────

LSP_MAX_MANIFEST := ".specify/specs/lsp-max/ggen.toml"
LSP_MAX_SCAFFOLD := ".specify/specs/lsp-max/examples/lsp-max-scaffold"

# Regenerate the lsp-max rule-pack server from lsp.ttl and cargo-check every scaffold crate
lsp-max-new:
    #!/usr/bin/env bash
    set -euo pipefail
    {{GGEN}} sync --manifest {{LSP_MAX_MANIFEST}}
    for toml in {{LSP_MAX_SCAFFOLD}}/*/Cargo.toml; do
        name=$(basename "$(dirname "$toml")")
        echo "checking $name..."
        cargo check --manifest-path "$toml"
    done
    echo "all scaffold crates OK"
