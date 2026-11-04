# CL-P G2 recovery — typed kernel refusals (orphan-recovery lane)

Standing: VERIFY-ALREADY-LANDED

**Status: VERIFY-ALREADY-LANDED.** No code change made by this lane. G2's work
landed; G2's own receipt exists at `g2-typed-refusals.md` (same directory,
Status: ALIVE).

## Inventory (disk state, this session 2026-11-03)

- `crates/ggen-engine/src/error.rs`: variant `AppError::AbbRefusal { code,
  op, refusal: ggen_abb_sbb::Refusal }`; constructor `abb_refusal(code, op,
  refusal)`; accessors `kernel_refusal()` (typed) and `abb_refusal_tag()`
  (kernel serde SCREAMING_SNAKE tag under the `"refusal"` key); two tests in
  `error::fm_code_extraction_tests`.
- `crates/ggen-engine/src/sync.rs`: all kernel-refusal sites use the typed
  constructor — `admit_with_caps` → `abb_refusal("FM-LAW-015", ...)` (~:433),
  `extract_pack_manifest` → `abb_refusal("FM-PACK-015", ...)` (~:1170),
  `resolve_sync_order` → `abb_refusal("FM-PACK-015", ...)` (~:1192).
  Stringify scan (`format!("{refusal`/`refusal.to_string()`/`{:?}` styles)
  over sync.rs: zero matches — no stringified FM-LAW-015/FM-PACK-015 refusal
  sites remain.

## Verification (real commands, this session)

- `cargo test -p ggen-engine --lib error` → all pass, including
  `abb_refusal_surfaces_kernel_serde_tag` (asserts tag ==
  `INSUFFICIENT_AUTHORITY` + full structured serde) and
  `abb_refusal_text_still_human_readable_with_fm_code`. Both GREEN.
- `cargo test -p ggen-engine` (full): 2 failures in
  `tests/book_gap_closure_e2e.rs`, both environmental — "could not resolve the
  `ggen` binary: CARGO_BIN_EXE_ggen unset" (binary not yet built at first
  run). Pre-existing/environmental, not a refusal-path failure. `cargo build
  -p ggen-cli-lib --bin ggen` was launched to clear it; full-suite result
  recorded below if it completed in-session.

## Contract items

- WO-01 item b / RESOLUTIONS R2 (typed preservation, machine-accessible tag):
  present, verified via the passing tag test. Nothing to recover.

## Final full-suite state (after `cargo build -p ggen-cli-lib --bin ggen`,
launched in-session; run completed same session)

- `cargo test -p ggen-engine` → every test binary `test result: ok`; lib
  235 passed / 0 failed (2 ignored); both previously-failing book_gap e2e
  tests pass with the binary present. Zero failures, zero compile aborts.
