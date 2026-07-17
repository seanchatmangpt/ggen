# stpnt — audit (generation-first, 2026-07)

**Purpose:** "Stewards of the Pentecost - Canonical Stewardship Cell Implementation" (`crates/stpnt/Cargo.toml`): four `PartExecutor` cells (consent, followup, steward, welcome) + a signed `StewardshipReceipt`.

## LOC

`tokei crates/stpnt/src --output json` (tokei Rust code lines):

| File | Code LOC |
|---|---|
| cells/consent.rs | 131 |
| cells/followup.rs | 83 |
| cells/steward.rs | 68 |
| cells/welcome.rs | 70 |
| cells/mod.rs | 4 |
| proof/receipt.rs | 245 |
| proof/replay.rs | 19 |
| proof/mod.rs | 2 |
| canon/mod.rs | 10 |
| governance/{mod,slr}.rs | 10 |
| membrane/{mod,github}.rs | 7 |
| projections/{mod,ocel}.rs | 19 |
| lib.rs | 7 |
| **Total** | **675** |

## Verdict: DEAD-DELETE (whole crate, 675 LOC)

- Consolidation analysis (`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §1 line 14, and §4 disagreement note line 74): REMOVE — "grep of all Cargo.toml files found no incoming references; not wired into any binary entry point… Decorative Completion pattern"; the synthesis explicitly overrides the prior narrative-only KEEP.
- Dependent-grep (rerun for this audit):
  `grep -rln 'stpnt' crates/*/Cargo.toml Cargo.toml src crates/*/src | grep -v '^crates/stpnt/'`
  → hits: root `Cargo.toml` (members entry only), `crates/ggen-core/src/lib.rs` / `stewardship.rs` / `stpnt/{mod,github}.rs`. Verified: those ggen-core hits are ggen-core's **own internal `pub mod stpnt`** (`crates/ggen-core/src/lib.rs:196`) plus string literals like "stpnt:ConsentReceived" — not a dependency on this crate. No crate `Cargo.toml` declares `stpnt`.
- Note: the cells' function (steward assignment, consent gating, receipts) is effectively re-implemented inside `ggen-core/src/stewardship.rs` + `ggen-core/src/stpnt/`, reinforcing that this crate is a superseded copy.

## Class breakdown

| Class | LOC (est.) | Basis |
|---|---|---|
| DEAD-DELETE | 675 | entire `src/` — zero workspace dependents, zero binary wiring; DEAD is assigned before generatability per METHODOLOGY |

Had it been live, `cells/*.rs` (~356 LOC of near-identical Input/Output serde structs + `PartExecutor` impls with a shared shape) would be GENERATABLE-WITH-SPEC and `proof/receipt.rs` (BLAKE3 content-hash + keyed-MAC sign/verify) IRREDUCIBLY-CUSTOM — recorded here only so the salvage decision is informed; it does not enter the metrics.
