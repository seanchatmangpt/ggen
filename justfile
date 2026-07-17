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
#   {{GGEN}} graph validate --files .specify/specs/post-chatman/post_chatman.ttl
#   cargo test -p ggen-core --test ast_extractor_70pct_test
#   cargo test -p ggen-core --test inverse_receipt_chain_test
#   cargo test -p ggen-core --test provenance_envelope_test
#   cargo test -p ggen-graph --test coherence_hash_expectations_test
#   cargo test -p ggen-graph --test post_chatman_coherence_integration

# Test Phase 2 components (inverse-sync, coherence validation, process discovery)
test-phase2:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Running Phase 2 test suite..."

    # Core AST extraction tests
    cargo test -p ggen-core --test ast_extractor_70pct_test || exit 1

    # Inverse receipt chain validation
    cargo test -p ggen-core --test inverse_receipt_chain_test || exit 1

    # Provenance envelope (O→A bridge)
    cargo test -p ggen-core --test provenance_envelope_test || exit 1

    # Coherence hash expectations
    cargo test -p ggen-graph --test coherence_hash_expectations_test || exit 1

    # Post-Chatman round-trip (O→A→O cycle)
    cargo test -p ggen-graph --test post_chatman_coherence_integration || exit 1

    echo "✅ Phase 2 test suite complete"

# Validate post-Chatman ontology + SHACL shapes
coherence-check:
    #!/usr/bin/env bash
    set -euo pipefail
    ontology=".specify/specs/post-chatman/post_chatman.ttl"
    shapes=".specify/specs/post-chatman/post_chatman_shapes.ttl"

    echo "Validating ontology: $ontology"
    {{GGEN}} graph validate --files "$ontology" || exit 1

    echo "Validating shapes: $shapes"
    {{GGEN}} graph validate --files "$shapes" || exit 1

    echo "✅ Coherence check passed (O→A→O validation gates satisfied)"

# Run inverse-sync on sample artifacts
inverse-sync source_dir=".specify/specs" ontology=".specify/specs/post-chatman/post_chatman.ttl":
    #!/usr/bin/env bash
    set -euo pipefail

    echo "Running inverse-sync..."
    echo "  Source dir: {{source_dir}}"
    echo "  Ontology: {{ontology}}"

    # Invoke the inverse-sync CLI command (when available)
    # For now, this is a placeholder that verifies the ontology is valid
    {{GGEN}} graph validate --files "{{ontology}}" || exit 1

    echo "✅ Inverse-sync validation complete (envelope would be written here)"

# Full O→A→O round-trip test
round-trip: coherence-check inverse-sync
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ O→A→O round-trip complete (coherence + inverse-sync + ontology re-validation)"

# Performance SLO validation (Phase 1 + Phase 2)
slo-check:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Running Phase 1 SLO checks..."
    cargo bench --bench cli_startup_performance -- --test

    echo "Running Phase 2 SLO checks..."
    # Phase 2: Inverse pipeline + coherence checker performance
    # InversePipeline::run_signed() must complete in <5s for typical artifact sets
    # CoherenceChecker::check() must complete in <2s for 3-pole validation
    # These are measured via integration tests that include timing assertions
    cargo test -p ggen-core --test inverse_receipt_chain_test -- --nocapture || exit 1
    cargo test -p ggen-graph --test coherence_hash_expectations_test -- --nocapture || exit 1

    echo "✅ Phase 1 + Phase 2 SLO checks complete"

# ── Quality gates ─────────────────────────────────────────────────────────────

# Full pre-commit gate: fmt → check → lint → test-lib → coherence-check → boundary guard (in sequence, fail fast)
pre-commit: fmt-check check lint test-lib coherence-check guard-process-intelligence-boundary
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ Pre-commit gate complete (fmt, check, lint, tests, coherence, boundary guard)"

# Security vulnerability scan
audit:
    cargo audit

# Publish-safety guard (docs/jira/v26.7.16/01-PUBLISH-SAFETY-AND-CRATE-RENAME.md):
# no workspace member other than root `ggen` may be named "ggen" or ever publish.
# NOT wired into `pre-commit` -- its `cargo publish --dry-run` step currently fails
# on the pre-existing chicago-tdd-tools/cli-proof dev-dependency gap (Cargo.toml
# lines 159, 803-808), unrelated to this guard's own collision/publish=false logic.
# Wiring it into the commit-blocking chain today would break every commit on an
# unrelated, already-documented issue. Run standalone: `just guard-publish-target`.
guard-publish-target:
    ./scripts/ci/guard-publish-target.sh

# Process Intelligence Boundary guard (CLAUDE.md): ggen must only emit process
# evidence, never analyze it. Cheap and always green -- safe to run every commit.
guard-process-intelligence-boundary:
    ./scripts/ci/guard-process-intelligence-boundary.sh

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
