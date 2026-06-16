<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CHECKPOINT-002 — Implementation Receipt](#gall-checkpoint-002--implementation-receipt)
  - [Real state changed](#real-state-changed)
  - [Authoritative path touched](#authoritative-path-touched)
  - [Negative path that now fails correctly](#negative-path-that-now-fails-correctly)
  - [Invariants protecting against drift](#invariants-protecting-against-drift)
  - [Legacy path removed/blocked](#legacy-path-removedblocked)
  - [Proof object](#proof-object)
  - [Changed files (all inside crates/ggen-lsp/ except this receipt)](#changed-files-all-inside-cratesggen-lsp-except-this-receipt)
  - [Proof tails](#proof-tails)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CHECKPOINT-002 — Implementation Receipt

**Mission:** Activate GGEN-HARNESS-001 from Phase-2 metadata-only to a LIVING diagnostic, mirroring the proven GGEN-TPL-001 living loop. ggen LOCAL law. Single-writer integration.

**Self-assessment: ALIVE.**

## Real state changed
- `GGEN-HARNESS-001.detector_active` flipped `false → true` (`crates/ggen-lsp/src/route/diagnostic_species.rs`).
- New living detector: a `Cargo.toml` `[[test]]`/`[[bench]]` explicit-`path` declaration whose file is absent on disk now raises a `GGEN-HARNESS-001` ERROR through both the headless gate and the live `analyze_and_observe` seam, and the full 6-link OCEL chain is written to the EXTERNAL `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl`.

## Authoritative path touched
- Detection: NEW `analyzers/harness_analyzer.rs` (pure) + NEW `harness_index.rs` (I/O boundary), grouped via `analyzers::detect_harness_001`.
- Live seam: `state.rs::analyze_and_observe` — HARNESS branch added; TPL branch tightened to `ggen.toml`-only (basename self-selection; both are `FileType::Toml`).
- Headless gate fold: `check.rs::fold_harness_001` bumps `error_count` (release_blocking).
- Route: seeded `proof-topology.repair` on the previously-UNSEEDED `RepairFamily::AdmissionFailure` (owned exclusively, exactly as TPL owns `DanglingReference`); `family_of_code("GGEN-HARNESS-001")` wired.

## Negative path that now fails correctly
- Invalid fixture (`Cargo.toml` `[[test]] path="tests/proof/nonexistent.rs"`, no file, no `ggen.toml`) → `check_files_in_root` returns `error_count >= 1`, `has_errors()==true`, `exit_code() != 0`. Creating the declared proof file clears it to `error_count == 0`. Proven in `tests/ggen_harness_001_living_loop.rs`.

## Invariants protecting against drift
- Predicate = Cargo.toml explicit-`path` declarations vs. on-disk files only. Makefile.toml shell strings EXCLUDED (no false positive on the clean tree); Makefile.toml stays a repair *surface* in the species table.
- Route is source-law-only: all `NoOp`; co-located + integration tests scan step titles for forbidden markers (`out/`, `output/`, `dist/`, `gen/`, `emitted`, `fabricate`, `make the proof pass`, `pass the test`, `stub`) and require a source-law surface reference.
- Clear is residual-preserving (`harness_clears_for` + `residual_single_file_diags`), NOT a blunt empty publish.
- No-leak barriers: TPL fixture raises 0 HARNESS (`invalid_fixture_emits_only_tpl_001`, unchanged); HARNESS fixture raises 0 TPL (`harness_seam_raises_zero_tpl_001`, NEW symmetric barrier).

## Legacy path removed/blocked
- None existed (Phase-2 species was metadata-only). The metadata-only barrier `harness_001_remains_metadata_only` was FLIPPED to `harness_001_is_active` (asserts `detector_active`), so no later patch can silently revert activation. `registry_contains_exactly_two_species` (==2) and `actuation_boundary_is_inspect_only_for_all_species` UNCHANGED and green.

## Proof object
- 6-link chain (`DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted`, `receipt_requirement=boundary_receipt`) asserted for `Cargo.toml` + `GGEN-HARNESS-001` from the on-disk OCEL log in `analyze_and_observe_records_live_harness_receipt_chain`.

## Changed files (all inside crates/ggen-lsp/ except this receipt)
- NEW `crates/ggen-lsp/src/analyzers/harness_analyzer.rs`
- NEW `crates/ggen-lsp/src/harness_index.rs`
- EDIT `crates/ggen-lsp/src/analyzers/mod.rs`
- EDIT `crates/ggen-lsp/src/lib.rs`
- EDIT `crates/ggen-lsp/src/route/registry.rs`
- EDIT `crates/ggen-lsp/src/route/diagnostic_species.rs`
- EDIT `crates/ggen-lsp/src/state.rs`
- EDIT `crates/ggen-lsp/src/check.rs`
- NEW `crates/ggen-lsp/tests/ggen_harness_001_living_loop.rs`
- NEW fixtures `crates/ggen-lsp/tests/fixtures/ggen_harness_001_living_loop/{invalid_project,valid_project}/**`
- EDIT `crates/ggen-lsp/tests/ggen_tpl_001_regression.rs` (barrier flip)
- NEW `docs/receipts/GALL_CHECKPOINT_002_RECEIPT.md` (this file)

## Proof tails
- `cargo make check`: `Finished dev profile ... Build Done in 4.16 seconds.`
- `cargo test -p ggen-lsp`: `TOTAL passed=221 failed=0` (lib 152/152, harness living-loop 7/7, TPL regression 8/8).
- `cargo clippy -p ggen-lsp --no-deps -- -D warnings`: `Finished` (0 warnings; no `#[allow]`).
- `cargo fmt -p ggen-lsp -- --check`: `fmt_check_exit=0`.
