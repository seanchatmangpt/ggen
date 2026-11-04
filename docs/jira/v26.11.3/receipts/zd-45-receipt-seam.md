# ZD-45: receipt_chain_seam `allow(dead_code)` removal

Date: 2026-11-04 · Lane: CARRY-02 · Repo: /Users/sac/ggen · Branch: v26-11-3-wave

## Disposition chosen

**(a) Keep and un-suppress.** The seam is not dead and no wiring was needed: every
public symbol already has live consumers.

- Lib consumers (pre-existing): `crates/ggen-engine/src/sync.rs` (imports the seam,
  calls `verify_chain_compat` at sync.rs:4046), `crates/ggen-engine/src/verbs/handlers.rs`
  (aliases the seam as `receipt_record`).
- Test consumers: 15 integration-test files under `crates/ggen-engine/tests/` use
  `ggen_engine::receipt_chain_seam::{...}` (receipt_chain_e2e, seam pin, KAT court,
  epoch e2e, differential, own-history replay, scale stress, ...).

The `#![allow(dead_code)]` at the module top (former line 35, comment claimed "seam
is ahead of call-site migration") was stale — the module is `pub mod receipt_chain_seam`
in `crates/ggen-engine/src/lib.rs:55`, so all its public items are publicly reachable
and rustc never fires `dead_code` on them. The suppression was suppressing nothing.

## Change

One line removed from `crates/ggen-engine/src/receipt_chain_seam.rs`:

```
- #![allow(dead_code)] // seam is ahead of call-site migration (documented follow-up)
```

No wrapper, re-export, or call site was touched.

## Verification (ALIVE)

| Gate | Command | Result |
|---|---|---|
| Workspace check | `cargo check --workspace` | exit 0, `Finished dev profile` |
| dead_code warnings in check output | `grep -c "warning:.*dead_code"` | 0 |
| Seam-site grep | `grep -rn allow(dead_code) crates/ggen-engine/src/receipt_chain_seam.rs` | 0 matches (exit 1) |
| Focused tests | `cargo test -p ggen-engine --lib receipt` | 6 passed, 0 failed (2 pre-existing ignored) |
| Seam pin court | `cargo test -p ggen-engine --test receipt_chain_seam_test` | 4 passed, 0 failed |

## Notes

- First `cargo check -p ggen-engine` after the workspace check returned exit 101 due
  to a concurrent build-dir lock (shared checkout, other lanes building); immediate
  clean re-run exit 0. Not a compile failure — no errors in output.
- Deletion (option b) was considered and rejected: consumers exist, ERRC delete
  does not apply to a live surface.
