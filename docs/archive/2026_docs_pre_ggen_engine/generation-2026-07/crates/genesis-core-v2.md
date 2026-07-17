# genesis-core-v2 — audit (generation-first, 2026-07)

**Purpose:** "KNHK V2 core — Pattern trait system, pattern registry, composition, zero-copy execution paths." (`crates/genesis-core-v2/Cargo.toml`).

## LOC

`tokei crates/genesis-core-v2/src --output json` (tokei Rust code lines):

| File | Code LOC | Class |
|---|---|---|
| lib.rs | 252 | GENERATABLE-WITH-SPEC |
| primitives.rs | 314 | IRREDUCIBLY-CUSTOM |
| revelation.rs | 307 | IRREDUCIBLY-CUSTOM |
| split_laws.rs | 188 | IRREDUCIBLY-CUSTOM |
| inventory.rs | 153 | GENERATABLE-WITH-SPEC |
| registry.rs | 39 | GENERATABLE-WITH-SPEC |
| patterns.rs | 0 | DEAD-DELETE |
| **Total** | **1,253** | |

## Consolidation status

`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §2 (line 31) + §3 (line 41): genesis-core-v2 is the **surviving** crate of the genesis-core pair (genesis-core is removed, primitives optionally salvaged here). 1 dependent (`stpnt` — itself a REMOVE candidate; after stpnt removal the only fan-in is via workspace membership, worth re-checking in a later pass). Not dead.

## Class breakdown

| Class | LOC | Rationale |
|---|---|---|
| DEAD-DELETE | 0 (1 empty file) | `patterns.rs` is 0 code LOC — empty placeholder module still declared in lib.rs:17 |
| GENERATABLE-WITH-SPEC | 444 | see per-file below |
| IRREDUCIBLY-CUSTOM | 809 | see per-file below |

## Per-file rationale

- **lib.rs (252, GENERATABLE-WITH-SPEC):** `Pattern` trait + three YAWL pattern impls (`SequencePattern`, `ExclusiveChoicePattern`, `ParallelSplitPattern`) that differ only in metadata + a small validate body, plus `CorePatternRegistry` (Arc/HashMap register/get/list). This is exactly the "43 YAWL patterns from a pattern ontology" shape — each impl is metadata-driven boilerplate; a `yawl-patterns.ttl` spec + impl template covers it.
- **inventory.rs (153, GENERATABLE-WITH-SPEC):** three enums (`ArtifactStatus` 9 variants, `FinishStep`, `ConnectionMechanism`) with per-variant policy tables (`allowed_action`, `deletion_allowed`) — enum-with-attributes, a direct TTL emission target.
- **registry.rs (39, GENERATABLE-WITH-SPEC):** second HashMap registry, register/get/exists/count boilerplate.
- **primitives.rs (314, IRREDUCIBLY-CUSTOM):** no_std zero-alloc `Pair2`/`RelationPage`/`Construct8` with lane-mask push, BLAKE3 `Receipt::generate`, `ReplayCursor::advance` verification, and a compile-time layout assertion (`const _: () = …`). Algorithmic + crypto; candidate for the E (engine core) allowlist as receipt machinery. Note: duplicates the DEAD `genesis-core` crate — after that deletion this is the single copy.
- **revelation.rs (307, IRREDUCIBLY-CUSTOM):** evidence-verification logic (`verify_lamb_authority`, `Plague::from_refusal` mapping, `PlagueRecord::is_genuine` receipt recomputation, `passes_all_gates`) — rule-bearing verification code over the receipt primitives.
- **split_laws.rs (188, IRREDUCIBLY-CUSTOM):** `need9_split` / `need257_split` page-splitting algorithms with `receipt_over_page` hashing — pure algorithm.

## Nearest template/spec

None emitting today. `.specify/` has no yawl/pattern TTL (checked `ls .specify`); genesis-schema-v2's `PatternMetadata` is the in-Rust stand-in for the missing spec. The 444 generatable LOC ratchet on authoring that spec.
