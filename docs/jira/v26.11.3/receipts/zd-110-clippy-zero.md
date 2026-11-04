# ZD-110 Receipt — ggen-abb-sbb clippy-zero gate

Lane: ZD-110. Date: 2026-11-04. Repo: /Users/sac/ggen (branch v26-11-3-wave).
Subject: integration-test lint posture for `crates/ggen-abb-sbb/tests/` —
drive the CI kernel job's clippy gate to exit 0.

## Deliverable

Four owned test files edited, no other files, no git commands:

- `crates/ggen-abb-sbb/tests/depgraph_hardening_test.rs`
- `crates/ggen-abb-sbb/tests/ea_projection_test.rs`
- `crates/ggen-abb-sbb/tests/kernel_highs_courts.rs`
- `crates/ggen-abb-sbb/tests/refusal_crosswalk_test.rs`

## Lint-posture precedent followed

The crate already establishes the integration-test posture (verified on
disk, 2026-11-04):

- `src/lib.rs:2` — `#![cfg_attr(test, allow(clippy::unwrap_used,
  clippy::expect_used, clippy::panic))]` citing Chicago TDD
  (`.claude/rules/rust/testing.md`).
- Six sibling integration test files carry the exact per-file header
  (`bench_bound.rs:7`, `envelope_courts.rs:22`, `falsifiers.rs:5`,
  `marketplace_fixture.rs:5`, `property_admission_test.rs:16`, and
  `kernel_highs_courts.rs:14` itself):
  `// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.`
  `#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]`

The three files missing the header got that exact header (inserted after
the `//!` doc block, before the first `use`); `kernel_highs_courts.rs`
already had it and needed only real lint fixes.

## Per-file lint census (before -> after)

Primary-span census, `cargo clippy --all-targets` JSON diagnostics:

| File | Before | After |
|---|---|---|
| depgraph_hardening_test.rs | 16 expect_used + 7 unwrap_used + 5 panic = 28 | 0 |
| ea_projection_test.rs | 13 expect_used + 4 panic = 17 | 0 |
| kernel_highs_courts.rs | 1 iter_on_single_items + 2 single_char_pattern = 3 | 0 |
| refusal_crosswalk_test.rs | 8 expect_used | 0 |
| Total | 56 | 0 |

Changes per file:

1. `depgraph_hardening_test.rs`, `ea_projection_test.rs`,
   `refusal_crosswalk_test.rs` — added the crate-standard Chicago-TDD
   allow header. This covers the 16 previous-lane `.expect(...)` sites
   honestly: expect is the sanctioned fail-loud form for test fixtures in
   this crate's posture (lib.rs applies the same allow under cfg(test)).
2. `kernel_highs_courts.rs` — 3 real fixes (allow header alone does not
   cover these lints):
   - `into_iter().collect()` on the 1-element caps array ->
     `std::collections::BTreeMap::from([...])` (caps is a
     `BTreeMap<String, u64>`, src/lib.rs:69).
   - `detail.contains("4") && detail.contains("5")` ->
     `detail.contains('4') && detail.contains('5')` (char form).

## Gates (real commands, real exits, 2026-11-04)

| Gate | Command | Result |
|---|---|---|
| Clippy (kernel-job gate) | `cargo clippy --locked --manifest-path crates/ggen-abb-sbb/Cargo.toml --all-targets -- -D warnings` | **exit 0** |
| Tests | `cargo test --locked --manifest-path crates/ggen-abb-sbb/Cargo.toml --all-targets --no-fail-fast` | **exit 0**, 110 passed / 0 failed across all targets |
| Doc tests | `cargo test --locked --manifest-path crates/ggen-abb-sbb/Cargo.toml --doc` | **exit 0**, 3 passed / 0 failed |

Full kernel job's three steps green. No `git` commands run; files
verified via clippy/test re-execution after each edit.
