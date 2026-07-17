# genesis-types-v2 — audit (generation-first, 2026-07)

**Purpose:** "KNHK V2 type system — foundational data structures for workflow engine (workflow/pattern defs, execution state, errors, config)." (`crates/genesis-types-v2/Cargo.toml`).

## LOC

`tokei crates/genesis-types-v2/src --output json` (tokei Rust code lines):

| File | Code LOC |
|---|---|
| src/lib.rs | 793 |
| **Total** | **793** |

Note: a large share of the 793 is `#[cfg(test)] mod tests` inside lib.rs (tests start at line 391 of ~780 non-test-relevant region; tokei counts them as src code because they live in src/).

## Consolidation status

KEEP per `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §5 (line 55): 2 dependents (`genesis-core-v2`, `stpnt`), real fan-in. It is also the merge **target** for genesis-schema-v2 (§3 line 39).

## Class breakdown (single-file crate; symbol-level read of lib.rs)

| Class | LOC (est.) | Basis |
|---|---|---|
| GENERATABLE-WITH-SPEC | ~710 (~90%) | dominant content: newtypes (`PatternId`, `StepId`, `ExecutionId`), derive structs (`WorkflowDef`, `PatternDef`, `WorkflowStep`, `ExecutionContext`, `Event`, `PowlNode`, `PowlEdge`, `PowlGraph` fields, `GateResult`, `ProcessAdmissionReport`, `EvidenceTier`, `RepairBand`), a thiserror `Error` enum, and mechanical to_json/from_json/file round-trip methods — plus their state-based tests, which are equally template-shaped |
| IRREDUCIBLY-CUSTOM | ~80 (~10%) | `PowlGraph::validate()` (lines 202–239: root/edge/duplicate-id graph checks), `ProcessAdmissionReport::compute_status` (328–342: refusal-dominance lattice), `compute_receipt_hash` (343–358: canonical hash) — small but genuinely rule-bearing logic a TTL type-spec would not express |

Estimate basis: full symbol sweep of the file (grep of struct/enum/impl/fn declarations), not sampling; the two algorithmic regions were read line-by-line at the cited ranges.

## Nearest template/spec

No emitting template. The POWL/admission vocabulary overlaps `.specify/dod-ontology.ttl` and process-mining specs; a `knhk-types.ttl` + derive-struct template family (same family needed by genesis-schema-v2) would cover the ~90%.

## TSV note

Single file → single dominant-class line (GENERATABLE-WITH-SPEC); the ~80 custom LOC are carried as an estimate here, per METHODOLOGY's one-class-per-file rule.
