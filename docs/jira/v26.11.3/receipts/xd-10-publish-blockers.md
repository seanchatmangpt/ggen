# XD-10: publish blockers — CHANGELOG entry + ggen-engine version unification

Standing: ALIVE

Lane: XD (PUB-1 rows 6–8). Date: 2026-11-04. Branch `v26-11-3-wave`
(PR #824). No commit performed (other lanes' files dirty in tree, per
contract). Files owned: `CHANGELOG.md`,
`crates/ggen-engine/Cargo.toml` (version line only), this receipt.

## Task 1 — CHANGELOG `## [26.11.3] - 2026-11-03` entry

Added to `/Users/sac/ggen/CHANGELOG.md`, grounded only in on-disk
sources read this session:

- `docs/jira/v26.11.3/closure/v26.11.3-closure.md` — closure seal;
  source of: FM registry 118 codes + parity court, sjira CLI 5 verbs,
  ggen-sjira 211/0 (20 suites), descope-ledger v2, index exit gate
  177/177 (cl-close-2), 71+ receipts.
- `docs/FM_REGISTRY.md:17` — "118 unique codes" (verified verbatim).
- `crates/ggen-cli/src/cmds/sjira.rs` — 5 `#[verb]` fns read directly:
  `admit`, `frontier`, `execute`, `lease_admit`, `epoch_check`.
- `docs/jira/v26.11.3/receipts/gd-2-ggen-dod.md:35` — `211/0` cite
  (sjira-suite-211 MET).
- `docs/jira/v26.11.3/closure/descope-ledger.md:49` — "## v2 update
  (2026-11-03, seal pass)".

**Divergence noted (not edited)**: `docs/CHANGELOG.md` is a stale
divergent copy — its newest entry is 26.10.11 while the canonical root
`CHANGELOG.md` was at 26.10.20 before this lane's edit. Left untouched
per contract; a note pointing readers to the canonical root changelog
was added inside the 26.11.3 entry itself.

## Task 2 — ggen-engine version unification

- Pre-check: root `Cargo.toml` `[workspace.package]` has
  `version = "26.11.3"` (line 2). Confirmed before edit.
- `crates/ggen-engine/Cargo.toml`: `version = "26.10.12"` →
  `version.workspace = true` (line 10). `publish = true` (line 44)
  untouched.
- Verification (real runs, both exit 0):
  - `cargo check -p ggen-engine` → `Finished dev profile ... in 1m 45s`
    (compiled as `ggen-engine v26.11.3`; workspace deps resolve at
    26.11.3).
  - `cargo build -p ggen-engine` → `Finished dev profile ... in 2m 15s`
    (`Compiling ggen-engine v26.11.3`).

## Not touched (lawful dep pins, per contract)

- `ggen-cli` / `pm4pytest` / `bcinr` version pins — documented lawful
  dependency pins, out of this lane's ownership.

## Standing

ALIVE for the three owned files: every number in the CHANGELOG entry
resolves to a file on disk (cites above), version resolution witnessed
by real check+build runs this session, no commit issued.
