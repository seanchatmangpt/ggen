# Conformance Spec — the generator's declared execution model (frozen)

This is the **declared object-centric process model** that
`verify/conformance_check.py` checks a run against (Van der Aalst doctrine,
`.claude/rules/process-mining-chicago-tdd.md`). It is the analogue of the BPMN
model in `conformance_check(event_log, bpmn_file=…)`. Full derivation +
source-citations: `research/05-process-mining-conformance.md`.

> The real OCEL/process-mining engine that this harness mirrors lives in-repo and
> is tested: `ggen_graph::ocel::dfg::discover_dfg`
> (`crates/ggen-graph/src/ocel/dfg.rs:38`) and
> `ggen_graph::ocel::conformance::check_lifecycle_order`
> (`crates/ggen-graph/src/ocel/conformance.rs:23`). `ggen sync` itself emits **no**
> OCEL today (`pipeline.rs` has no event/span emission), so the live-run criteria
> below are honestly unmet until sync is instrumented (research/05 §4.3).

## Object types (8)

| type | identity | count |
|------|----------|-------|
| `generation_run` | run id (the case object) | 1 |
| `ontology` | source TTL set | 4 (source + 3 imports) |
| `inference_rule` | rule name | 2 (`derive-operators` order 1, `derive-pareto-dominance` order 2) |
| `operator` | `tpot:operatorId` | 60 |
| `stage` | `tpot:stageId` | 9 |
| `dominance_edge` | `(opA,opB)` | derived |
| `generation_rule` | rule name | 6 |
| `output_artifact` | `output_file` | 6 |
| `receipt` | receipt id | 1 |

## Activities (9, in lawful order)

`OntologyLoaded` → `InferenceRuleApplied` → `OperatorDerived` → `DominanceDerived`
→ `EliteSelected` → `GenerationRuleQueried` → `ArtifactRendered` → `ArtifactEmitted`
→ `ReceiptEmitted`.

## Lawful transitions (each = a `check_lifecycle_order` over a case object)

1. **No inference before load** — `OntologyLoaded ≺ InferenceRuleApplied` (run).
2. **No derivation before its rule** — `InferenceRuleApplied ≺ OperatorDerived` (run).
3. **No elite without an operator** — `OperatorDerived ≺ EliteSelected` (per operator).
4. **Run-level chain** — `EliteSelected ≺ GenerationRuleQueried ≺ ArtifactRendered ≺ ArtifactEmitted ≺ ReceiptEmitted`.
5. **No render before query** — `GenerationRuleQueried ≺ ArtifactRendered` (per generation_rule).
6. **No output before render** (headline) — `ArtifactRendered ≺ ArtifactEmitted` (per output_artifact).
7. **No duplicate terminal** — an artifact is emitted at most as many times as it is rendered.
8. **Receipt terminal + total** — every event has the `generation_run` case; every emitted artifact has a `GenerationRuleQueried` of its rule; the receipt binds only emitted artifacts; no artifact is both render-failed and emitted without rework.

## Cross-type invariants

- Bijection: exactly 9 `stage`, 60 `operator`, each operator's stage ∈ the 9.
- One elite per stage (9 `EliteSelected`).
- No orphan artifact / no orphan run-case / no phantom receipt binding.

## The 8 impossible logs (must each be REJECTED — anti-cheating)

`receipt-before-emit`, `render-before-query`, `duplicate-terminal-emit`,
`receipt-phantom-artifact`, `elite-before-operator`, `orphan-artifact-no-query`,
`orphan-no-run-case`, `failed-and-released`. A checker that always returns
"conforms" fails all 8; only a real checker passes both the positive and the 8
negatives.

## 8-criteria status (research/05 §3)

- **Met statically here:** event-log completeness, lifecycle soundness,
  object-centric structural consistency (already proven by
  `test_tpot2_autoconfig.py` + `reference_pipeline.json`), OCEL semantic constraints.
- **Requires a real run (honestly unmet — no ggen binary):** temporal lawfulness of
  live stages, conformance fitness ≥ 0.9, observed variant control, deviation-free,
  receipt-causality. These flip to evidence the moment `verify/conformance_check.py`
  finds a real `.ggen/ocel/*.ocel.jsonl` (Mode B).
