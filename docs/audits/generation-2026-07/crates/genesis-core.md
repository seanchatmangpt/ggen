# genesis-core — audit (generation-first, 2026-07)

**Purpose:** "Pure mathematical foundation for the Genesis-bearing interchangeable parts architecture (A = μ(O))." (`crates/genesis-core/Cargo.toml` description). `#![no_std]` primitives: `Pair2`, `RelationPage`, `Construct8Packet`, `Receipt`, `ReplayCursor`.

## LOC

`tokei crates/genesis-core/src --output json` (tokei Rust code lines):

| File | Code LOC |
|---|---|
| src/lib.rs | 194 |
| **Total** | **194** |

## Verdict: DEAD-DELETE (whole crate, 194 LOC)

- Consolidation analysis (`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §1, line 13): REMOVE — "0 dependents in active workspace… Only referenced by dormant, non-member crates genesis-wasm-shell and ggen-membrane."
- Dependent-grep (rerun for this audit):
  `grep -rln '"genesis-core"\|genesis-core = \|genesis_core::' crates/*/Cargo.toml crates/*/src Cargo.toml src | grep -v '^crates/genesis-core/'`
  → hits only: root `Cargo.toml` (members + workspace-deps table), `crates/genesis-wasm-shell/*` and `crates/ggen-membrane/*` (both NOT in `members`, dormant), and one comment-level mention in `crates/ggen-core/src/membrane/core.rs`. `crates/ggen-core/Cargo.toml` declares no `genesis-core` dependency.
- The exact same primitives already exist duplicated in `crates/genesis-core-v2/src/primitives.rs` (`Pair2`, `RelationPage`, `Construct8` vs `Construct8Packet`, `Receipt`, `ReplayCursor`), so even the salvage step the consolidation analysis suggests (§3 line 41) is largely done — the code is a live duplicate.

## Class breakdown

| Class | LOC (est.) | Basis |
|---|---|---|
| DEAD-DELETE | 194 | whole `src/lib.rs` — zero active dependents; deletion is the cheapest path to coverage (METHODOLOGY: DEAD is assigned first, never laundered as generatable) |

No matching template/spec is relevant: dead code is not counted toward generatability.
