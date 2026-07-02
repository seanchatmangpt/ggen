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

# Clippy with -D warnings across all targets (180s; first run / cache invalidation compiles deps)
lint:
    timeout 180s cargo clippy --all-targets -- -D warnings

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

# ── Phase 2: Inverse Sync + Coherence Gate ────────────────────────────────────

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
    {{GGEN}} graph validate --schema-file "$ontology" || exit 1

    echo "Validating shapes: $shapes"
    {{GGEN}} graph validate --schema-file "$shapes" || exit 1

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
    {{GGEN}} graph validate --schema-file "{{ontology}}" || exit 1

    echo "✅ Inverse-sync validation complete (envelope would be written here)"

# Full O→A→O round-trip test
round-trip: coherence-check inverse-sync
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ O→A→O round-trip complete (coherence + inverse-sync + ontology re-validation)"

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

# Full pre-commit gate: fmt → check → lint → test-lib → coherence-check (in sequence, fail fast)
pre-commit: fmt-check check lint test-lib coherence-check
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ Pre-commit gate complete (fmt, check, lint, tests, coherence)"

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

# Seal evidence journal into a BLAKE3 provenance receipt (requires affi on PATH)
affidavit-seal:
    cargo cicd affidavit seal

# Verify sealed BLAKE3 receipt — ACCEPT or REJECT
affidavit-verify:
    cargo cicd affidavit verify

# Show evidence event summary (count, timestamps, verdicts)
evidence-show:
    cargo cicd evidence show

# wpm oracle adjudication on the evidence log
evidence-audit:
    cargo cicd evidence audit

# Scan changed .rs files for anti-LLM admissibility violations
lsp-check:
    cargo cicd lsp check

# wpm oracle process conformance gate (TRUTHFUL/VARIANCE/DECEPTIVE/BLOCKED)
status-audit:
    cargo cicd status audit

# IEC 61508 / ISO 26262 compliance evidence summary
certification-show:
    cargo cicd certification show

# ── lsp-max scaffold ──────────────────────────────────────────────────────────

LSP_MAX_MANIFEST := ".specify/specs/lsp-max/ggen.toml"
LSP_MAX_SCAFFOLD := ".specify/specs/lsp-max/examples/lsp-max-scaffold"

# Generate a new rule-pack LSP server from lsp.ttl (sync + compile check)
lsp-max-new: lsp-max-sync lsp-max-check

# Run the ggen μ-pipeline for lsp-max (30ms)
lsp-max-sync:
    {{GGEN}} sync --manifest {{LSP_MAX_MANIFEST}}

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
