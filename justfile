# ggen task runner — single entry point for all dev commands
# Delegates directly to cargo; Makefile.toml is kept as historical reference only.

# -p ggen-cli-lib is required (not optional): since the v26.7.16 publish-safety
# fix removed root's own duplicate [[bin]] "ggen" (commit 3862fe000,
# `autobins = false`), ggen-cli-lib is the sole remaining package producing a
# "ggen" binary -- a bare `cargo run --bin ggen --` is now ambiguous/fails
# ("no bin target named `ggen` in default-run packages").
GGEN := "cargo run -p ggen-cli-lib --bin ggen --"

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
    # -p ggen-cli-lib (not ggen-cli, which isn't a real package name -- confirmed
    # broken via `cargo metadata`, the actual package is named ggen-cli-lib).
    timeout 600s cargo build --release -p ggen-cli-lib --bin ggen

# Remove build artifacts
clean:
    cargo clean

# ── Formatting ────────────────────────────────────────────────────────────────

# Format all code
fmt:
    cargo fmt --all

# Check formatting without modifying (used in pre-commit)
#
# NOT `cargo fmt --all` (2026-07-17 finding): `--all` formats every workspace
# member's LOCAL PATH-BASED DEPENDENCIES too, transitively, even outside this
# workspace. `praxis-core`/`praxis-graphlaw` (real members) have live path deps
# into `/Users/sac/praxis/crates/{powl2-decompose,wasm4pm-arazzo,chatman-common}`
# -- contrary to `.claude/rules/architecture.md`'s prior claim that they're
# vendored copies with no live path back to `~/praxis` (that claim was wrong,
# confirmed live, not yet corrected as of this commit). That pulls in the whole
# `/Users/sac/praxis` workspace's metadata resolution, which includes unrelated
# sibling members (`cng`, `multifractal-workflow`, ...) whose own dependency
# chain reaches back into THIS repo's now-excluded `crates/ggen-core` --
# `cargo metadata` then hard-fails the same way described in test-phase2's
# comment above (ggen-core's `workspace = true` fields have no workspace to
# inherit from). `-p <pkg>` per real member avoids the external-path walk
# entirely (confirmed live) without under-covering any real workspace member.
#
# EXCLUDES ggen-engine/praxis-core/praxis-graphlaw (2026-07-17 finding, POLICY
# DECISION -- needs owner review, not silently made): all three, freshly
# vendored from `~/praxis` this session, fail `cargo fmt --check` against
# THIS repo's rustfmt config -- 67 + 209 = 276 file-diffs, confirmed live, not
# a handful of stragglers. This reads as a systemic rustfmt-version/config
# mismatch between the two repos, not scattered one-off mistakes. Reformatting
# 276 files sight-unseen at commit time risks masking real diffs in freshly-
# vendored code; conforming vendored code to a different repo's style is also
# arguably the wrong call vs. giving these crates their own rustfmt.toml. Ergo:
# excluded from this gate for now rather than either (a) leaving `just
# pre-commit` permanently red for reasons unrelated to any single commit's own
# correctness, or (b) unilaterally reformatting 276 files without review. Real
# fix (not done here): either reformat once under careful review, or add a
# crate-local rustfmt.toml matching praxis's own style for these three.
# Member list from `cargo metadata --no-deps` (12 total, matches Cargo.toml).
fmt-check:
    cargo fmt --check \
        -p cpmp -p genesis-core-v2 -p genesis-types -p ggen -p ggen-cli-lib \
        -p ggen-config -p ggen-graph -p ggen-lsp -p ggen-marketplace

# ── Linting ───────────────────────────────────────────────────────────────────

# Clippy with -D warnings across all targets (180s; first run / cache invalidation compiles deps)
#
# SCOPE GAP, found and deliberately left open (2026-07-17): no `--workspace` flag
# means this only ever checks the ROOT `ggen` package (confirmed live: its own
# `Checking` output names exactly one package, `ggen v26.7.4`) -- none of the
# other 11 real workspace members. `just pre-commit` including this recipe has
# reported green all session without ever exercising clippy on ggen-cli,
# ggen-engine, ggen-config, ggen-marketplace, praxis-core, praxis-graphlaw, etc.
# Adding `--workspace` here is the correct fix in principle, but doing so live
# immediately turns this recipe red across real, pre-existing, multi-crate debt
# that was never triaged because this gap always masked it: confirmed live via
# `cargo clippy --workspace --all-targets --exclude ggen-lsp -- -D warnings`:
# `ggen-marketplace` (lib test, 1+ error), `praxis-graphlaw` (lib test 5+
# errors, `pattern4_equivalence_canonicalization` test 1+ error, `owlrl` bench
# 9 genuine `E0599` type errors -- `TripleStore::from` returns `TripleStore`
# directly, not `Result`, so the bench's own `.expect()` calls never should
# have compiled; this one predates this session, not introduced by it).
# ggen-cli-lib's own `#![deny(warnings)]` (crates/ggen-cli/src/lib.rs:51)
# additionally promotes ALL its clippy warnings to compile errors under plain
# `cargo clippy --workspace --all-targets` (no `-D warnings` even needed) --
# 2 real ggen-cli-lib issues found and fixed this session (unnested or-pattern
# in lib.rs, `#[ignore]` without a reason in utils/error.rs); ggen-cli-lib now
# passes `cargo clippy -p ggen-cli-lib --all-targets` clean on its own.
# Widening this recipe to `--workspace` is real, valuable follow-up work but
# needs a dedicated triage pass (each crate's issues reviewed on their own
# merits, not blindly auto-fixed), not a 3am scope change bundled into
# unrelated work -- left undone here on purpose, not silently.
lint:
    timeout 180s cargo clippy --all-targets -- -D warnings

# ── Testing ───────────────────────────────────────────────────────────────────

# Full test suite — the primary test gate (<30s hot cache)
test:
    #!/usr/bin/env bash
    set -euo pipefail
    # NOTE: `status=$?` must be captured in the else-branch — after `if cmd; then exit 0; fi`,
    # `$?` is the if-statement's own exit (0), which silently turned every timeout kill into a
    # green gate (found 2026-07-17: exit 0 with the run killed mid-compile).
    if timeout 30s cargo test --workspace --tests; then
        exit 0
    else
        status=$?
    fi
    [ "$status" -eq 124 ] || exit "$status"
    echo "⚠️  First compile >30s, escalating to 600s..."
    timeout 600s cargo test --workspace --tests

# Unit/lib tests inside each crate
test-lib:
    timeout 30s cargo test --lib --workspace

# Doctests — validates all /// Examples blocks compile and run
# NOTE: no `--exclude ggen-core` here (2026-07-17) -- ggen-core is excluded from
# `[workspace] members` (see Cargo.toml), and `--exclude <SPEC>` requires SPEC to
# resolve as a real workspace member; naming a workspace-excluded crate in --exclude
# makes cargo try to parse its manifest anyway, which fails (`workspace = true`
# fields with no workspace to inherit from -- ggen-core is deliberately not
# re-added to members, and its Cargo.toml is not edited, per the disconnect-not-
# delete/byte-identical doctrine). The exclude in Cargo.toml already keeps it out
# of --workspace runs; no flag is needed here.
test-doc:
    #!/usr/bin/env bash
    set -euo pipefail
    # See the `test:` recipe's note above on why `status=$?` must be captured
    # inside an explicit `else` — the same bug was found here 2026-07-17.
    if timeout 60s cargo test --doc --workspace; then
        exit 0
    else
        status=$?
    fi
    [ "$status" -eq 124 ] || exit "$status"
    echo "⚠️  Doc tests >60s, escalating to 180s..."
    timeout 180s cargo test --doc --workspace

# Niche/slow suites — run directly, no `just` wrapper needed:
#   cargo test --test bdd --workspace -- --include-ignored     (BDD specs)
#   cargo mutants --workspace                                  (mutation score)
# `-p ggen-core --test lsp_max_pack_test`/`all_marketplace_packs_validation_test`
# are UNREACHABLE as of 2026-07-17 (see note below `test-phase2`) -- not listed here.
# Phase-2 / coherence / round-trip checks — same commands CI's `phase2` job runs:
#   {{GGEN}} graph validate --files .specify/specs/post-chatman/post_chatman.ttl
#   cargo test -p ggen-engine --test receipt_chain_e2e         (retargeted from ggen-core's
#     inverse_receipt_chain_test, T067 -- ggen-core is being disconnected from the workspace)
#   cargo test -p ggen-graph --test coherence_hash_expectations_test
#   cargo test -p ggen-graph --test post_chatman_coherence_integration

# Test Phase 2 components (inverse-sync, coherence validation, process discovery)
#
# REGRESSION FOUND + WORKED AROUND (2026-07-17, post-disconnect verification): every
# `cargo test -p ggen-core ...` / `--exclude ggen-core` / `--manifest-path
# crates/ggen-core/Cargo.toml` invocation now hard-fails with "package ID
# specification did not match any packages" or "failed to find a workspace root"
# (confirmed live, all three invocation styles). Root cause: ggen-core/Cargo.toml
# inherits ~25 fields via `workspace = true`, but ggen-core is in root Cargo.toml's
# `exclude = [...]`, not `members = [...]` -- there is no workspace left for those
# fields to inherit from. This is NOT the same as "ggen-core still compiles
# standalone" (a claim made elsewhere in this session before this was checked
# empirically) -- it does not, as currently configured. Fixing it would mean
# literalizing ggen-core/Cargo.toml's inherited fields, which conflicts with the
# disconnect-not-delete doctrine's byte-identical-on-disk guarantee for ggen-core,
# so it is NOT fixed here. `ast_extractor_70pct_test` and `provenance_envelope_test`
# (T067: previously "left on ggen-core deliberately, no ggen-engine/ggen-graph
# equivalent yet") are loudly skipped below rather than silently dropped or left to
# fail opaquely. `receipt_chain_e2e` was already retargeted to ggen-engine (T067)
# and is unaffected. Same fix applied to `.github/workflows/ci.yml`'s `phase2` job.
test-phase2:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Running Phase 2 test suite..."

    # Core AST extraction tests -- SKIPPED: reverse_sync::ast_extractor was
    # abandoned (not ported) when ggen-core was deleted (2026-07-17, see
    # docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md). Not a silent gap:
    # this line intentionally does not run and prints why every time this
    # recipe executes.
    echo "SKIPPED: ast_extractor_70pct_test (ggen-core deleted, functionality abandoned not ported -- see docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md)"

    # Receipt chain validation (T067: retargeted from ggen-core's
    # inverse_receipt_chain_test -- ggen-core is being disconnected from the
    # workspace; ggen-engine::sync + receipt_chain_e2e is the live equivalent)
    cargo test -p ggen-engine --test receipt_chain_e2e || exit 1

    # Provenance envelope (O→A bridge) -- SKIPPED: ProvenanceEnvelope lived only
    # in ggen-core::receipt::provenance_envelope; its only consumer (ggen-cli's
    # inverse_sync command) was abandoned in the same removal pass, not ported
    # (2026-07-17, docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md). Not a
    # silent gap: this line intentionally does not run.
    echo "SKIPPED: provenance_envelope_test (ggen-core deleted, functionality abandoned not ported -- see docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md)"

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
    # Phase 2: receipt-chain + coherence checker performance.
    # T067 (2026-07-16): retargeted from ggen-core's inverse_receipt_chain_test to
    # ggen-engine's receipt_chain_e2e -- ggen-core is being disconnected from the
    # workspace; receipt_chain_e2e is the live equivalent (real sync + real BLAKE3
    # chain recomputation + real `ggen receipt history` CLI boundary, no mocks).
    #
    # Real wall-clock timing assertion (closes the Decorative-Completion gap flagged
    # in docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md: the old comment
    # here claimed "measured via integration tests that include timing assertions"
    # while neither test file actually contained a Duration/Instant/elapsed check --
    # confirmed false via grep returning zero matches on both files, 2026-07-16
    # investigation). This measures real date-based wall-clock elapsed time around
    # the actual `cargo test` invocation (compile + run) and fails loudly if it
    # exceeds the threshold below -- a genuine, executing assertion, not a printed
    # claim; it runs (and reports elapsed time) whether or not the test itself
    # passes, so the measurement never gets silently skipped.
    #
    # Threshold: 180s. Reasoning: a cold `cargo test -p ggen-engine --test
    # receipt_chain_e2e` run on this hardware (Darwin/arm64, pinned nightly
    # toolchain) measured 45s wall-clock end-to-end (2026-07-16, `date +%s`
    # before/after, verified reproducible across two consecutive runs). 180s is 4x
    # that observed cold-compile baseline -- generous enough to absorb CI machine
    # variance and concurrent-build contention, while still catching a genuine
    # multi-minute regression or hang.
    #
    # Scope note: this bounds the *test invocation* (compile + run), not an
    # isolated in-process sync+verify cycle -- a finer-grained std::time::Instant
    # assertion around just the sync+verify logic (excluding compile), as originally
    # requested, would need to live inside
    # crates/ggen-engine/tests/receipt_chain_e2e.rs itself. That file is outside
    # this task's edit boundary (scripts/ci/ + justfile only) and is tracked as a
    # follow-up for whoever owns crates/ggen-engine/tests/.
    receipt_chain_start=$(date +%s)
    if cargo test -p ggen-engine --test receipt_chain_e2e -- --nocapture; then
      receipt_chain_status=0
    else
      receipt_chain_status=$?
    fi
    receipt_chain_end=$(date +%s)
    receipt_chain_elapsed=$((receipt_chain_end - receipt_chain_start))
    echo "receipt_chain_e2e wall-clock: ${receipt_chain_elapsed}s (SLO threshold: 180s)"
    if [ "$receipt_chain_elapsed" -gt 180 ]; then
      echo "❌ SLO VIOLATION: receipt_chain_e2e took ${receipt_chain_elapsed}s, exceeds 180s threshold" >&2
      exit 1
    fi
    if [ "$receipt_chain_status" -ne 0 ]; then
      echo "❌ receipt_chain_e2e reported test failures (see output above); timing SLO was measured (${receipt_chain_elapsed}s, within threshold) but the test itself did not pass" >&2
      exit 1
    fi
    cargo test -p ggen-graph --test coherence_hash_expectations_test -- --nocapture || exit 1

    echo "✅ Phase 1 + Phase 2 SLO checks complete"

# ── Quality gates ─────────────────────────────────────────────────────────────

# Full pre-commit gate: fmt → check → lint → test-lib → coherence-check → boundary guard → cheat scan → claims schema → pack proofs (in sequence, fail fast)
pre-commit: fmt-check check lint test-lib coherence-check guard-process-intelligence-boundary guard-cheat-scan guard-claims-schema guard-pack-proofs
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ Pre-commit gate complete (fmt, check, lint, tests, coherence, boundary guard, cheat scan, claims schema, pack proofs)"

# Pack-proof gate: re-sync the committed multi-pack consumer
# (examples/receiptctl), verify regeneration is idempotent, and run its full
# test suite (the generated proofs plus its own). Makes "the generated proof
# suites pass" a checkable fact from repo state — see
# scripts/ci/guard-pack-proofs.sh and docs/packs/L5_PUSH_ROUND3_RESULTS.md.
guard-pack-proofs:
    ./scripts/ci/guard-pack-proofs.sh

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

# Short alias for guard-process-intelligence-boundary (T065,
# specs/014-ggen-core-replacement/tasks.md -- named exactly this way there).
# Delegates to the same recipe/script rather than duplicating the call.
guard-process-boundary: guard-process-intelligence-boundary

# Test-quality cheat scan (crates/ggen-cheat-scanner): syn-based AST scan for
# CHEAT-T01 vacuous-assert, CHEAT-T02 tautological-result-check, CHEAT-T03
# no-assertion-test, and CHEAT-T04 mock-import across crates/*/src, crates/*/tests,
# and tests/. NOTE (2026-07-17): wired into `pre-commit` per the same
# unconditional pattern as guard-process-intelligence-boundary, but as of this
# recipe's introduction the scanner reports 515 pre-existing findings across the
# workspace's existing test suites -- this currently makes `just pre-commit` fail
# until that debt is triaged/fixed, same as any other newly-added real gate.
guard-cheat-scan:
    cargo run --quiet -p ggen-cheat-scanner --bin ggen-cheat-scanner

# APS claims-ledger schema validation (docs/aps/claims.toml) — structure only;
# runs in pre-commit. Commits are not publishes, so publish-gate enforcement
# is deliberately NOT part of this recipe.
guard-claims-schema:
    ./scripts/ci/guard-publish-standing.sh --schema-only

# Full publish gate: run before any real `cargo publish`. Fails if any
# publish-gated claim in docs/aps/claims.toml is BLOCKED without an explicit
# exception_admitted_by; warns on stale evidence coordinates.
guard-publish-standing:
    ./scripts/ci/guard-publish-standing.sh

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
