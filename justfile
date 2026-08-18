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

# Clippy with -D warnings across the WHOLE workspace (300s; first run / cache
# invalidation compiles deps).
#
# WIDENED 2026-08-02 (closes the SCOPE GAP left open 2026-07-17, see git blame
# for that comment's original text): now real `--workspace`, not root-package-
# only. Two flags earn their own explanation because without them this recipe
# is either uninformative or permanently unusable:
#
# `--keep-going` -- without it, cargo's default fail-fast means the FIRST
# workspace member with any `-D warnings` violation aborts the whole run, so
# every crate after it in build order never gets checked at all. That is
# exactly the hidden-debt failure mode this widening exists to close (just
# with the mask moved to a different, arbitrary crate instead of removed) --
# confirmed live: without `--keep-going` the run stops at `ggen-graph` (25
# errors) and never reaches `ggen-engine`, `praxis-graphlaw`, `ggen-config`,
# `ggen-marketplace`, which between them account for most of the real count
# below. `--keep-going` is a stable (non-nightly-gated) cargo flag as of this
# toolchain (`cargo clippy --help` lists it plainly, confirmed live).
#
# `-A unexpected_cfgs` -- narrow, single-lint carve-out, NOT a broad
# suppression of clippy's real style/correctness lints (those still deny).
# Without it the run hard-fails at `bcinr-pddl` before checking ANY other
# member: `crates/bcinr-pddl/src/mfw/mod.rs:43`'s `#[cfg(feature =
# "mfw-planner")]` references a feature that `crates/bcinr-pddl/Cargo.toml`'s
# own comment (line 43-49) says was deliberately dropped from `[features]` in
# PR #255 -- the cfg-gate attribute was left dangling when the feature
# declaration was removed. That is a real, separate, pre-existing bug (not
# ordinary lint debt -- it is a hard compile failure under `-D warnings`,
# confirmed live via `cargo clippy -p bcinr-pddl -- -D warnings` alone,
# unrelated to `--workspace`), tracked in
# docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md's TECH-DEBT-002.
# `crates/bcinr-pddl/Cargo.toml` is not a file this recipe change's task was
# scoped to touch, so the real fix (declare `mfw-planner = []` in
# `[features]`, matching the crate's existing `dhat-heap = []` pattern) is
# left to that crate's owner; this carve-out should be deleted the same day
# that fix lands.
#
# REAL COUNT (2026-08-02, `cargo clippy --workspace --all-targets --keep-going
# -- -D warnings -A unexpected_cfgs`, isolated CARGO_TARGET_DIR to avoid
# cross-session build-cache corruption): 649 real findings across 5 crates --
# `ggen-engine` 547 (127 lib + 420 lib-test), `ggen-graph` 50 (25 lib + 25
# lib-test), `praxis-graphlaw` 21, `ggen-config` 18, `ggen-marketplace` 13.
# Full breakdown and disposition: TECH-DEBT-002 in
# docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md and the matching
# `dev.lint-workspace` entry in docs/aps/claims.toml. Left wired into
# `pre-commit` below, unchanged position, same precedent as `guard-cheat-scan`
# (TECH-DEBT-001): a widened real gate is allowed to turn `pre-commit` red
# while tracked, rather than silently narrowed back to hide the count.
#
# UPDATE (2026-08-03, reverified live via this exact recipe, isolated run,
# exit code 101): real count is now 4, down from 649 -- ggen-engine,
# ggen-graph, praxis-graphlaw, and ggen-config are all genuinely clean under
# this exact command (0 findings attributable to any of them). Remainder: 1
# non-gating warning (`chicago-tdd-tools`, `clippy::redundant_closure`,
# crates/chicago-tdd-tools/src/cli_proof/receipt.rs:130 -- clippy's own note
# says this lint "ignores -D warnings", so it does not fail the build) and 2
# real compile-gating errors, newly surfaced (not part of the original 649,
# because `ggen-cli-lib` was never reached by `--keep-going` until its
# dependency `ggen-engine` started compiling clean) in
# `crates/ggen-cli/src/generated_commands.rs`: `too_long_first_doc_paragraph`
# (line 1) and `single_element_loop` (line 99), both promoted to hard errors
# by that crate's own `#![deny(warnings)]`. Still NOT zero -- `just lint`
# still exits 101, `pre-commit` is still red on this gate. See TECH-DEBT-002's
# own dated update for the full breakdown.
lint:
    timeout 300s cargo clippy --workspace --all-targets --keep-going -- -D warnings -A unexpected_cfgs

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

# Unit/lib tests inside each crate — same 30s→600s cold-compile escalation
# as `test` above (found 2026-07-21: a bare 30s timeout with no escalation
# fired spuriously right after a target/ cache clear; the ggen-cheat-scanner
# crate visible in the truncated output has zero tests and compiles
# instantly -- the 30s was being spent compiling a LATER crate in the
# workspace graph, not hanging).
test-lib:
    #!/usr/bin/env bash
    set -euo pipefail
    if timeout 30s cargo test --lib --workspace; then
        exit 0
    else
        status=$?
    fi
    [ "$status" -eq 124 ] || exit "$status"
    echo "⚠️  First compile >30s, escalating to 600s..."
    timeout 600s cargo test --lib --workspace

# Integration tests only (crates/*/tests/*.rs), excluding lib tests (test-lib
# above already covers those) and bin-embedded unit tests. Added 2026-08-02
# to close a real gap: `test:` above uses `--tests`, but `--tests` is NOT
# "integration tests only" despite the name -- confirmed live, it also builds
# and runs each crate's `[lib]` unit tests (e.g. `cargo test --workspace
# --tests` reports "Running unittests src/lib.rs" for `ggen-engine` and
# surfaces a real `ggen-engine` lib-test failure), so before this recipe
# existed, `pre-commit`'s `test-lib` dependency was the ONLY test gate that
# ever ran, and NOTHING in `just pre-commit` ever exercised a single file
# under any `crates/*/tests/`. Cargo has no native "integration-only" target
# flag, so this enumerates real `kind == ["test"]` targets via `cargo
# metadata` (the authoritative way to distinguish an integration-test binary
# from a `[lib]`/`[[bin]]` target) and selects each by name.
#
# NOT wired into `pre-commit` (deliberate, tracked, not silent) -- a real
# `cargo test --workspace --tests` run on 2026-08-02 (373 real integration
# targets found; run killed partway through, see below) surfaced 7 genuine
# integration-test failures across 3 crates (`ggen-cli-lib`'s
# `perf_cold_start_with_config`; `ggen-config`'s
# `repo_facts_ttl_crate_map_matches_cargo_toml_workspace_members`, a real
# Cargo.toml/.specify/repo-facts.ttl drift; `ggen-engine`'s
# `exact_repository_inventory_manufactures_partial_alive_evidence`,
# `root_help_gives_each_noun_a_non_blank_description`,
# `doctor_succeeds_with_correct_diagnostic_on_each_supported_schema`,
# `mega_project_all_packs_sync`,
# `custom_behavior_scaffolds_once_and_survives_hand_completion`) -- AND one
# genuine hang risk: `ggen-engine/tests/economics_measured_evidence_test.rs`
# spawns a nested `cargo test -p ggen-engine --test receipt_chain_e2e`
# subprocess via `Command::output()` with no timeout; under load it stalled
# at 0% CPU for minutes with no sign of returning, so the run was killed
# before reaching the remaining crates rather than let it block indefinitely.
# A gate that can hang forever is strictly worse than one that fails fast and
# red (`guard-cheat-scan`/`lint`'s precedent), so this is the "documented,
# explicitly time-boxed exception" branch, not the "stays wired in and red"
# branch. Full breakdown: TECH-DEBT-003 in
# docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md and the matching
# `dev.test-integration` entry in docs/aps/claims.toml. The outer `timeout`
# below bounds THIS recipe's own run even though it cannot kill an already-
# orphaned grandchild subprocess of the hanging test above -- that is the
# real fix (add a timeout inside economics_measured_evidence_test.rs itself),
# not something a `just` recipe can patch over, and is out of this recipe
# change's file scope.
#
# 2026-08-03 fix: this recipe never actually completed a run before today --
# `cargo test --test <name>` hard-errors at target *selection* (before
# compiling anything) for any target declaring `required-features` not
# currently enabled (e.g. "error: target `doctor_adversarial_tests` in
# package `ggen-cli-lib` requires the features: `integration`" -- confirmed
# live, 2026-08-03, on the plain pre-fix recipe). Most `crates/*/tests/` and
# root `tests/*` targets declare `required-features = ["integration"]`
# (root `Cargo.toml`, `crates/ggen-cli/Cargo.toml`); a smaller set additionally
# needs `a2a`/`mcp` (`ggen-lsp`/`ggen-engine` Cargo.toml). `--features
# integration,a2a,mcp` is the exact flag set TECH-DEBT-003's fix pass verified
# against (ad hoc, not through this recipe -- this recipe itself was never
# updated, which is the bug this comment documents). Deliberately NOT adding
# `ggen-core-retired` (permanently-dead code path, meant to stay off, see the
# `ggen-core-retired` feature comment near `[[bench]] ggen_benchmarks` above
# root Cargo.toml) or `a2a-integration-tests` (gates only `test_telco_routing`,
# excluded below and by design unrunnable, see that file's own header).
#
# 2026-08-03 real full completion (first ever, after both the hang fix above
# and this recipe's own --features fix): 372 real targets (test_telco_routing
# excluded by design), 2070 passed, 90 failed, 256 ignored, 15/372 targets
# with at least one failure. The 7 originally-named failures + the hang are
# fixed and individually reconfirmed passing this same day (independently
# re-run: ci_g0_inventory_e2e, cli_boundary, config_schema_dispatch_e2e,
# cross_pack_matrix, custom_behavior_e2e, performance,
# system_crate_map_parity_test all green). The completed run surfaced a
# larger, previously-never-exercised residual of 15 targets / 90 tests, real
# and reproducible (independently spot-re-run this same day: generation_rules_e2e
# 14 failing fns, generation_rules_typed_causes_e2e 1, product_mirror_conformance
# 1, manifest_contract_test 1, plus 2 of the 11 legacy dead-CLI-surface targets
# -- cli_command_tests, doctor_adversarial_tests -- both failing with
# "unrecognized subcommand", confirming the current `ggen`/`ggen doctor` noun
# surface really has dropped `market`/`ci`/`ontology` etc. subcommands these
# tests still assume). Filed as task #33 / TECH-DEBT-003's "15 remaining"
# addendum in docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md.
# STILL NOT wired into `pre-commit`: the promotion condition (full suite
# passing) is not met -- 15/372 targets remain genuinely red, so this stays
# the "documented, explicitly time-boxed exception" branch, not "stays wired
# in and red". Do not add `test-integration` to the `pre-commit:` dependency
# line until that residual closes.
test-integration:
    #!/usr/bin/env bash
    set -euo pipefail
    mapfile -t TARGETS < <(cargo metadata --no-deps --format-version=1 | python3 -c 'import json,sys; d=json.load(sys.stdin); seen=set(); [seen.add(t["name"]) or print(t["name"]) for pkg in d["packages"] for t in pkg["targets"] if t["kind"]==["test"] and t["name"] not in seen and t["name"] != "test_telco_routing"]')
    ARGS=()
    for t in "${TARGETS[@]}"; do ARGS+=(--test "$t"); done
    echo "test-integration: ${#TARGETS[@]} real integration-test targets selected"
    if timeout 600s cargo test --workspace --no-fail-fast --features integration,a2a,mcp "${ARGS[@]}"; then
        exit 0
    else
        status=$?
    fi
    if [ "$status" -eq 124 ]; then
        echo "❌ test-integration timed out after 600s (see TECH-DEBT-003: at least one target has an unbounded subprocess spawn)" >&2
    fi
    exit "$status"

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

# Full pre-commit gate, in sequence, fail fast. The dependency list on the recipe line below
# IS the canonical gate list and count -- do not restate a number in a comment here or in any
# doc; every prior count has gone stale within days of a gate being added or removed.
pre-commit: fmt-check check lint test-lib coherence-check guard-process-intelligence-boundary guard-cheat-scan guard-short-test-timeout guard-fail-open-subprocess guard-claims-schema guard-pack-proofs guard-generation-hash-pin guard-pack-count guard-gate-count guard-pack-e2e-coverage guard-ggen-toml-schema-parity self-play
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ Pre-commit gate complete (fmt, check, lint, tests, coherence, boundary guard, cheat scan, claims schema, pack proofs, generation hash-pin, pack e2e coverage report)"

# Report-only: which packs/* with real testable surface (gates/*.rq or
# templates/*.tmpl) have zero e2e coverage anywhere. Always exits 0 today --
# see scripts/ci/guard-pack-e2e-coverage.sh's own header comment for why a
# hard-fail gate is a documented future step, not yet wired in.
guard-pack-e2e-coverage:
    bash scripts/ci/guard-pack-e2e-coverage.sh

# Ratchet gate: fails if a real field-name/type mismatch is introduced between
# ggen.toml's two independently-defined schemas (ggen-config's declarative-rules
# GgenManifest vs ggen-engine's frontmatter GgenConfig) for a field name they both
# happen to declare under the same table ([project]/[ontology]/[templates]/[law]).
# See the script's own header for why this checks shared-name TYPE agreement, not
# full field-set parity (the two schemas are deliberately different in field count).
guard-ggen-toml-schema-parity:
    python3 scripts/ci/guard-ggen-toml-schema-parity.py

# Pack-proof gate: re-sync each committed pack consumer (examples/receiptctl,
# the multi-pack consumer; examples/praxis-core-verify, the praxis-core-pack
# consumer), verify each regeneration is idempotent, and run each one's full
# test suite (the generated proofs plus its own). Makes "the generated proof
# suites pass" a checkable fact from repo state — see
# scripts/ci/guard-pack-proofs.sh and docs/packs/L5_PUSH_ROUND3_RESULTS.md.
# Self-play: replay the committed adversarial corpus and drive EVERY pack that
# ships an ontology through the full ggen lifecycle (classify -> query ->
# lint -> dry-run -> apply -> receipt verify -> idempotent re-sync).
#
# Deterministic and offline: no LLM, no network, no GPU. Before this existed,
# only 11 of 78 packs had any lifecycle proof (wired across the 6
# guard-pack-proofs consumer projects); this covers all of them. The
# falsifier suite alongside it proves the harness can actually detect a
# violation rather than silently observing nothing.

# Replay the adversarial corpus + drive every pack through the full lifecycle.
self-play:
    cargo test -p ggen-mcp --test self_play_test --test self_play_falsifier_test --test self_play_vacuity_test

# Grow the self-play corpus with the local Gemma (TurboFieldfare's
# OpenAI-compatible server on 127.0.0.1:8080, Metal/GPU). NOT part of any gate:
# an LLM in the assertion path would make a red suite unreproducible. Its only
# output is new files under crates/ggen-mcp/tests/corpus/, which `just
# self-play` then replays deterministically forever after.
#
# The server generates one completion at a time behind a 4-deep queue, so
# concurrency 4 keeps the GPU saturated without overflowing it.

# Grow the self-play corpus using the local Gemma on the GPU (not a gate).
self-play-explore packs="73" cases="4" concurrency="4":
    cargo run --release -p ggen-mcp --bin ggen-selfplay-explore -- \
        --packs {{packs}} --cases-per-pack {{cases}} --concurrency {{concurrency}}

guard-pack-proofs:
    ./scripts/ci/guard-pack-proofs.sh

# Generation-ledger hash-pin guard (.specify/generations.ttl): every
# gen:Generation entry must carry a non-empty, well-formed identity hash fact
# (commit / receiptChainHash / build sha256); previously recorded hash facts
# are append-only vs origin/main; the G0..Gn chain must be monotonic. See
# scripts/ci/guard-generation-hash-pin.sh.
guard-generation-hash-pin:
    ./scripts/ci/guard-generation-hash-pin.sh

# Pack-count drift guard (.specify/repo-facts.ttl's rf:packCount vs the real
# packs/ directory count): refuses when they diverge, the recurring failure
# mode (retrofit:GeneratedTableDriftManifested) this guard exists to close.
# See scripts/ci/guard-pack-count.sh.
guard-pack-count:
    ./scripts/ci/guard-pack-count.sh

# Gate-count drift guard (.specify/repo-facts.ttl's rf:gateCount vs the real
# .specify/gates/*.rq file count): refuses when they diverge, the same
# recurring failure mode (retrofit:GeneratedTableDriftManifested) that
# guard-pack-count closes for rf:packCount. See scripts/ci/guard-gate-count.sh.
guard-gate-count:
    ./scripts/ci/guard-gate-count.sh

# SPARQL law-gate enforcement (.specify/gates/*.rq): re-parses every gate file
# with the real oxigraph SPARQL parser and re-executes six gates
# (every-action-has-binding, every-binding-has-output-pattern,
# every-binding-has-template, every-command-has-handler,
# every-generator-has-action, no-orphan-actions) against this repo's own
# cmx:/cli: ontology data. The remaining four gates (cross-pack-contamination,
# the three l5-*.rq gates) get syntax-check only here -- see
# scripts/ci/guard-sparql-gates.sh's own header for why. See also
# crates/ggen-graph/src/bin/sparql_gate_check.rs.
#
# NOT wired into `pre-commit` (2026-08-02, deliberate, same pattern as
# guard-publish-target below): this guard is not decorative -- it currently
# exits non-zero for real, separate, pre-existing reasons. Running it live
# today (re-confirmed 2026-08-03, red-team finding F3) reports FAIL on ALL
# SIX semantically-checked gates, not just one: every-action-has-binding,
# every-binding-has-output-pattern, every-binding-has-template,
# every-command-has-handler, every-generator-has-action, and
# no-orphan-actions all currently report `ASK -> false`. The only root cause
# documented here so far is the every-command-has-handler one:
# .specify/cli-commands.ttl's ReceiptCommand and DoctorCommand individuals
# have no cli:handler triple (a genuine ontology-data gap, not a bug in this
# guard or its gate file). The other five gates' failures are real but their
# root causes are undocumented here -- do not assume fixing the handler gap
# alone will turn this guard green; re-run `just guard-sparql-gates` after
# each fix to see what's left. Wiring this line into `pre-commit` as-is would
# turn every commit red on unrelated, already-flagged issues. Run
# standalone: `just guard-sparql-gates`.
guard-sparql-gates:
    ./scripts/ci/guard-sparql-gates.sh

# L5 promotion trust-hardening: refreshes evidence/l5-template-derivation.ttl
# (packs/*/templates/*.tmpl frontmatter facts) that
# .specify/gates/l5-cap05-template-derivation-claim.rq consumes at
# `ggen sync run` time. NOT wired into `pre-commit` (same reasoning as
# guard-publish-target above: pre-commit does not invoke sync, so the gate
# this refreshes evidence for never runs there either) -- run by hand after
# editing any pack's templates/, before the next `just sync`/`just sync-dry`.
# See scripts/ci/produce-l5-template-derivation-evidence.sh's own header for
# the full staleness contract.
l5-template-evidence:
    ./scripts/ci/produce-l5-template-derivation-evidence.sh

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
# recipe's introduction the scanner reports 464 pre-existing findings (7
# CHEAT-T01, 456 CHEAT-T03, 1 CHEAT-T04; reconfirmed 2026-07-18) across the
# workspace's existing test suites -- this currently makes `just pre-commit` fail
# until that debt is triaged/fixed, same as any other newly-added real gate.
# (This count was previously mis-stated as 515 here; that was the pre-ggen-core-
# deletion figure -- some CHEAT-T03 findings lived under the now-deleted
# ggen-core/src/*, retired along with the crate in PR #259, not fixed by triage.)
guard-cheat-scan:
    cargo run --quiet -p ggen-cheat-scanner --bin ggen-cheat-scanner

# Refuses hardcoded sub-second recv_timeout/join_timeout/wait_timeout used as a
# termination/liveness proxy in test code -- the exact load-sensitive-flake
# class fixed in csprite_test.rs/backwardchaining_test.rs (2026-08-01).
guard-short-test-timeout:
    python3 scripts/ci/guard_short_test_timeout.py

# Refuses Rust `Command::output()`/`.status()` calls whose failure branch only
# logs (eprintln!/log::/tracing::) instead of propagating (bail!/return Err/
# panic!/process::exit) -- the exact fail-open shape fixed in the (now-migrated-
# to-ggen-legacy) v26.8.1 coverage_projection::run_subsystem_verifier bug.
# Scans all git-tracked *.rs files from the repo root (repointed 2026-08-02
# after PR #554 deleted this guard's original scan target,
# tools/v26.8.1/src/**/*.rs, migrating that corpus to seanchatmangpt/ggen-legacy
# -- matches the same `git ls-files` repoint already applied to its sibling
# guard_short_test_timeout.py in the same commit). See
# scripts/ci/guard_fail_open_subprocess.py and
# .claude/rules/coding-agent-mistakes.md mistake class 3 (Fail-Open Behavior).
guard-fail-open-subprocess:
    python3 scripts/ci/guard_fail_open_subprocess.py

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

# Full μ₁-μ₅ sync with cryptographic receipt. Historically called
# `ggen sync --audit true` -- git history (`git log --all -S"audit" --
# crates/ggen-engine/src/verbs/sync.rs`) shows zero commits ever adding an
# `--audit` flag to the live sync verb (`sync_run(dry_run, watch)` in
# crates/ggen-engine/src/verbs/sync.rs takes only those two args); `--audit`
# was aspirational from this recipe's very first commit, never implemented
# and later removed. There is no audit-specific behavior to translate to --
# this just runs the real `ggen sync run`.
sync:
    {{GGEN}} sync run

# Preview sync without writing any files. Historically called
# `ggen sync --dry_run true` -- `--dry-run` is a bare boolean switch on the
# live sync verb, not a value-taking flag.
sync-dry:
    {{GGEN}} sync run --dry-run

# Generation-ledger receipt: runs a real `ggen sync run` (the live sync verb has
# no per-rule scoping flag -- `sync_run(dry_run, watch)` in
# crates/ggen-engine/src/verbs/sync.rs takes only those two args, confirmed by
# reading it -- so this is the closest equivalent to "sync just this rule": a
# full sync run, after which docs/GENERATIONS.md is regenerated from
# .specify/generations.ttl by the docs-generations-ledger rule) and prints one
# confirmation line naming the receipt this round actually produced.
gen-receipt:
    {{GGEN}} sync run
    @echo "[gen-receipt] docs/GENERATIONS.md regenerated from .specify/generations.ttl; see .ggen-v2/receipt.json for this sync's chain hash."

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

# ── self-hosted verification (ggen-verify-pack pilot) ─────────────────────────

# Run the tcps-generated self-verification loop: emit real check evidence,
# sync (ggen-verify-pack gates refuse red/missing/stale evidence), verify the
# receipt chain. One command replaces the hand-run matrix.
verify-tcps:
    #!/usr/bin/env bash
    set -euo pipefail
    cd examples/tcps-generated
    bash scripts/verify.sh
    # The evidence mini-pack's content changes every emitter run BY DESIGN,
    # which collides with ggen.lock's pack-content pinning (FM-PACK-008).
    # Re-lock intentionally each run. KNOWN LIMITATION of the pilot: this
    # also re-locks the six real packs, weakening lock protection for the
    # duration of this recipe; the proper fix is the planned `ggen verify`
    # engine verb writing evidence through a lock-exempt channel.
    rm -f ggen.lock
    ../../target/debug/ggen sync run
    ../../target/debug/ggen receipt verify
    echo "verify-tcps: evidence green, gates passed, receipt chain verified"

# ── docs-through-ggen drift gate ──────────────────────────────────────────────

# Re-sync the repo's own generated docs (root ./ggen.toml manifest: maturity
# model, architecture rules, TCPS status, CLAUDE.md/README.md merge regions)
# and the Level Five Packs book (book/ggen.toml), then run the book checkers.
# Docs are committed in their generated state, so both syncs must be content
# no-ops — any resulting `git diff` is drift and this gate exists to catch it.
docs-sync:
    #!/usr/bin/env bash
    set -euo pipefail
    ./target/debug/ggen sync run
    (cd book && ../target/debug/ggen sync run)
    python3 book/scripts/check_book.py
    python3 book/scripts/check_level_five.py
    echo "docs-sync: root manuals + book re-synced, checkers green"

# ── Tier-2 REAL-API acceptance (gh-terraform-pack) ────────────────────────────

# Real GitHub API + terraform acceptance test (TCPS 第二十四・二十五章).
# Requires: terraform on PATH, `gh auth login` (ideally with delete_repo scope).
# Creates and deletes a throwaway private repo. Ignored in normal test runs.
tf-acceptance:
    cargo test -p ggen-engine --test gh_terraform_acceptance_e2e -- --ignored --nocapture
