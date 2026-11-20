# ZD-59 / CARRY-05 — FM-LAW-015 sync_surface_test ledger-row staleness

Standing: ALIVE

Date: 2026-11-04 · Lane: CARRY-05 · Repo: /Users/sac/ggen ·
Branch: v26-11-3-wave ·
Receipt: docs/jira/v26.11.3/receipts/zd-59-carry05.md

## Claim verified

Descope-ledger row "FM-LAW-015 class: 4 pre-existing sync_surface_test
failures surfaced post-census (sx-1 disclosure)" was stale: XD-11 closed the
reds via the e9829bead fixture fix (`git show --stat e9829bead` — commit
message names "sync_surface + perf reds fixed (SX-1/AF-5)") and ZB-9
re-verified. Re-checked under ambient ggen 26.11.3 this session.

## Verification (ALIVE)

| Gate | Command | Result |
|---|---|---|
| Run 1 | `cargo test -p ggen-cli-lib --test sync_surface_test` | 6 passed, 0 failed, exit 0 (14.84s) |
| Run 2 | same | 6 passed, 0 failed, exit 0 (14.33s) |
| Test file (actual location) | `find crates -name "sync_surface*"` | `crates/ggen-cli/tests/sync_surface_test.rs` (compiled under `-p ggen-cli-lib`) |
| Closing receipt exists | `ls docs/jira/v26.11.3/receipts/xd-11-fmlaw015.md` | present |
| Fixture commit | `git log --oneline -1 e9829bead` | e9829bead, 2026-11-03, "sync_surface + perf reds fixed (SX-1/AF-5)" |
| Independent witness | cl-close-2.md:149 | af-5 receipt re-verified 6/0, exit 0 |

Note: the descope-ledger named the crate as `ggen-cli-lib` and XD-11
reporting used the same target; the file physically lives in
`crates/ggen-cli/tests/` but is part of the `ggen-cli-lib` target's test
build — same test binary, same 6 tests.

## Ledger edits (docs/jira/v26.11.3/closure/descope-ledger.md)

1. FM-LAW-015 row → RESOLVED-DISCLOSED, citing xd-11-fmlaw015.md +
   zd-59-carry05.md as witnesses (the row this lane owns).
2. Additional stale rows closed ONLY after reading their landing receipts:
   - receipt_chain_seam allow(dead_code) → CLOSED per zd-45-receipt-seam.md
     (suppression was stale; `#![allow(dead_code)]` removed; workspace check
     0 dead_code warnings; seam pin court 4/0).
   - clap-noun-verb FINDING-1 → CLOSED per zd-46-zero-verb-audit.md
     (4-dir census, explicit `#[verb]` labels, build exit 0, flake
     disclosed as pre-existing).
   - ggen-lsp diagnostic_relay_probe → CLOSED per zd-47-relay-probe.md
     (hard-coded nonexistent child binary root-caused; 2/0 ×3; loud skip
     witnessed; restore byte-verified).
3. Rows deliberately left open (no landed closing receipt found):
   WO-file vs RECEIPTS.md standing drift; sync.rs:431 (pinned-by-law);
   blast_radius/transactional GAP-1; baseline.json debt (PARTIALLY closed,
   already annotated); `--skip-ea-admission`; sjira DS-7-D1/D2/D3.

## Standing

CARRY-05 RESOLVED-DISCLOSED. Test surface ALIVE under ambient ggen 26.11.3.
No git commands run (per lane contract).
