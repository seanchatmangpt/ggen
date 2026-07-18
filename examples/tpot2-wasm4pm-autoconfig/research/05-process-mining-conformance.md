# Research Dossier 05 — Conformance-Checking the Generator's OWN Execution

**Agent:** 05 / 10 · **Date:** 2026-06-20 · **Doctrine:** Van der Aalst
process-mining Chicago TDD (`.claude/rules/process-mining-chicago-tdd.md`) +
anti-cheating (`.claude/rules/testing-anti-cheating.md`).

**Research question.** Per the Van der Aalst doctrine — *"if the code says it
worked but the event log cannot prove a lawful process happened, then it did not
work"* — can we treat the ggen generation run **of this project** as a process
whose event log can be mined, and conformance-check it against its declared model
(load → inference → 6 SELECT generation rules)?

**Method.** Read-only source sweep (`Grep`/`Read`/`Glob`/`LSP`-equivalent). No
`cargo`/`ggen`/builds run (CONTRACT §9: no cargo, no ggen binary, no rdflib).
Every capability claim is cited to a real `.rs` file:line. **I did not and cannot
produce a real generation OCEL log here** (no ggen binary), so where the doctrine
requires a real run I say so plainly and do not fabricate event-log evidence.

---

## TL;DR (the honest verdict)

1. **The OCEL/process-mining engine genuinely exists in-repo and is real, not
   stubbed.** `ggen-graph::ocel` ships an OCEL→RDF projector, a SPARQL
   directly-follows-graph discoverer (`discover_dfg`), a SPARQL lifecycle
   conformance checker (`check_lifecycle_order`), and `ggen-lsp::intel` wires them
   into a working **explore→mine→promote** loop with measured success-rates and
   signed promotion receipts. The doctrine's required primitives (event log,
   discovery, conformance, variants) are **all present and tested**.

2. **BUT that engine instruments the LSP *agent-edit* lifecycle, NOT the `ggen
   sync` code-generation pipeline.** The OCEL writer (`IntelLog`) is called from
   the repair/edit surface; `ggen sync`'s pipeline
   (`ggen-core/src/codegen/pipeline.rs`) emits **no OCEL events and no tracing
   spans** for its load/inference/generate stages — it only records per-rule
   `duration_ms` and writes a BLAKE3 receipt. **A real `ggen sync` run of our
   generator does NOT natively emit a process-mineable log of its own stages.**
   This is the load-bearing gap.

3. **Therefore:** a *static / structural* object-centric conformance proof
   (model well-formedness, lawful-transition definition, negative-log rejection)
   can be authored and run **here, today, with Python stdlib** — and I design it
   below. The *dynamic* criteria (real fitness against a real run, real variant
   control, real temporal lawfulness of observed stages) **cannot be met here**
   and are honestly unmet until a ggen binary emits (or a wrapper derives) the
   log. **4 of the doctrine's 8 success criteria are statically satisfiable; 4
   require a real run.**

4. **Proposed artifact (§4):** `verify/conformance_spec.md` (the declared
   object-centric model, frozen) + `verify/conformance_check.py` (a pure-Python
   OCEL conformance harness, mirroring `ggen-graph`'s `check_lifecycle_order`
   semantics) that (a) **today** validates a *labeled-synthetic expected* OCEL log
   and **rejects** the doctrine's 8 impossible logs, and (b) **once a run exists**
   re-runs unchanged against the real `.ggen/ocel/*.jsonl` (or a receipt-derived
   log). The harness is written so faking is harder than a real run.

---

## 1. What OCEL / process-mining capability genuinely exists in-repo

This is real engineering, not aspiration. Citations are file:line.

### 1.1 OCEL data model — two real, coexisting forms

| Form | Type | File | Sections |
|------|------|------|----------|
| OCEL 2.0 **spec JSON** | `cpmp::ocel::OcelEventLog` | `crates/cpmp/src/ocel.rs:123-131` | `objectTypes`, `eventTypes`, `objects`, `events` |
| OCEL **NDJSON / in-memory** | `ggen_graph::ocel::OcelLog` | `crates/ggen-graph/src/ocel/ocel_types.rs:6-55` | `objects`, `events` (streaming, one event/line) |

Both carry the doctrine-required OCEL attributes: `OcelEvent` has `id`,
`activity`, `timestamp: DateTime<Utc>`, object refs, and attributes
(`ocel_types.rs:33-44`); `OcelObjectRef` carries `id`, `r#type`, and an optional
`qualifier` (`ocel_types.rs:48-55`) — i.e. `ocel:activity`, `ocel:timestamp`,
`ocel:object-id`, `ocel:object-type` are all representable.

### 1.2 Directly-follows-graph discovery — **real SPARQL, no external engine**

`ggen_graph::ocel::dfg::discover_dfg` (`crates/ggen-graph/src/ocel/dfg.rs:38-82`)
projects the log to RDF then runs an object-centric directly-follows query: two
events on the **same case object** (reached via the e2o qualifier predicate),
`t1 < t2`, with `FILTER NOT EXISTS` excluding any third event of that case
strictly between them (`dfg.rs:44-59`). Returns `Vec<DfgEdge { source, target,
frequency }>`. This is exactly van der Aalst's per-case DFG, computed in the
triplestore. Tested: `dfg.rs:120-149` proves `A→B→C` over two cases yields edges
`A→B (2)`, `B→C (2)` and **no spurious `A→C`**.

The cpmp reader independently implements the same primitives in Rust:
`directly_follows_graph()` (`cpmp/src/ocel.rs:209-217`),
`variants_per_object_type()` (`cpmp/src/ocel.rs:222-239`), and `stats()` →
`EventLogStats { dfg_edge_count, variant_count, … }` (`cpmp/src/ocel.rs:242-254`).
Tested against the real `p2p.ocel.json` fixture: 3 DFG edges, 3 variants
(`crates/cpmp/tests/ocel_test.rs:152-218`).

### 1.3 Conformance checking — **real SPARQL ASK lifecycle order**

`ggen_graph::ocel::conformance::check_lifecycle_order`
(`crates/ggen-graph/src/ocel/conformance.rs:23-61`) builds a SPARQL `ASK` that
binds each activity to the **same case object** and `FILTER`s strictly increasing
timestamps (`conformance.rs:32-55`). It returns `true` iff the activities occurred
in the given order for one shared object. This is an OCPQ-style temporal
constraint — the doctrine's "lawful order" check, expressed as one ASK.
**Critically, it has a real negative test:** `out_of_order_repair_fails`
(`conformance.rs:122-143`) feeds `GatePassed` *before* `RepairApplied` and asserts
the check returns `false`. There is also a generic guard runner `check_guard`
(`conformance.rs:68-75`) that runs any inline ASK and errors if it is not an ASK.

### 1.4 A working explore→mine→promote loop (the strongest precedent)

`ggen_lsp::intel::mine::mine` (`crates/ggen-lsp/src/intel/mine.rs:211-265`) is a
genuine, end-to-end object-centric mining run over a real OCEL log:

- reads the append-only NDJSON log via `IntelLog::read` (`intel/log.rs:81-105`);
- projects to RDF with `EvidenceProjector::project_ocel`
  (`intel/mine.rs:218`, projector at `ocel/projection.rs:14-167`);
- discovers the DFG over the **episode** case object (`intel/mine.rs:220`);
- filters **failure edges** (`is_failure_target`: `GateFailed`, `RefusalEmitted`,
  rework back-edge `EditApplied` — `intel/mine.rs:45-49`);
- computes **per-family measured success-rate** by replaying each episode through
  `check_lifecycle_order(DiagnosticRaised ≺ GatePassed)` — real conformance, not
  an assertion (`episode_closed`, `intel/mine.rs:124-141`);
- promotes only routes whose measured `success_rate` clears a threshold, and emits
  a **signed promotion receipt** binding source-log-hash → promoted artifact
  (`intel/mine.rs:328-350`).

Tested with real positive/negative behavior: a route that lawfully closes 3/3
episodes is promotable; a route that fails the gate 4/4 (success_rate 0) is **not**
promotable (`intel/mine.rs:388-444`). This is precisely the doctrine's discipline:
*event-log truth gates promotion*, and "the code returned success" is not a
defense — a route only promotes if its **episodes lawfully closed in the log**.

### 1.5 Self-audit log generator — a generated OCEL of the system's own process

`ggen_graph::ocel::generate_self_audit_log`
(`crates/ggen-graph/src/ocel/self_audit.rs:12-632`) hand-builds a 28-object,
18-event OCEL log of ggen-graph's own development/verification process
(`RequirementDeclared → OntologyMapped → FileEmitted → … → TestPassed →
ReceiptEmitted → ReplayVerified → CoverageEvaluated → CheckpointPromoted`), with a
qualifier vocabulary (`--checks-->`, `--produces-->`, `--verifies-->`,
`--satisfied_by-->`, `--decides-->`). It round-trips through RDF
(`self_audit.rs:640-654`) and is emitted to disk by the `emit_audit` binary
(`crates/ggen-graph/src/bin/emit_audit.rs:17-22` →
`crates/ggen-graph/audit/vision2030.self_audit.ocel.json`).

This is the **direct precedent for our research question** — ggen already models
"the system's own process" as a conformance-checkable OCEL log. Note honestly:
this log is *authored*, not *captured from a live run*; it is paired with a
`CoverageMatrix` (`ocel/coverage.rs:30-190`) mapping requirements→source→test→
command, exactly the requirement-evidence structure we want for the generator.

### 1.6 The decisive negative: `ggen sync` does NOT emit a process log

I searched the generation pipeline for any OCEL emission or tracing span:

- `Grep "OcelEvent|IntelLog|EvidenceProjector|self_audit"` over
  `crates/ggen-core` → only `crates/ggen-core/src/membrane/ocel.rs` (an unrelated
  membrane value type) and its re-export. **No `IntelLog`, no
  `EvidenceProjector`, no OCEL writer in the codegen path.**
- `Grep "pipeline\.(load|extract|generate|validate|emit)|#\[instrument|tracing::"`
  over `crates/ggen-core/src/codegen/pipeline.rs` → **no matches.** The
  CLAUDE.md-documented `pipeline.load/extract/generate/validate/emit` spans
  (otel-validation.md) are **aspirational for this pipeline** — they are not in
  the generation code.
- What the sync executor *does* record: per-rule `duration_ms` on each generation
  rule result (`crates/ggen-core/src/codegen/executor.rs:214-215, 753, 880,
  1007, 1189`) and a final BLAKE3 sync receipt. So there **is** structured,
  ordered, timed per-rule data inside `GenerationResult` — but it is never
  projected to OCEL and never written as an event log.

**Conclusion for §1.** The OCEL conformance machinery is real, tested, and even
self-applied (§1.5) — but it lives on the **LSP edit/repair** surface
(`IntelLog` at `.ggen/ocel/agent-edit-events.ocel.jsonl`, `intel::log::default_path`).
The **`ggen sync` generation run emits no such log.** Mining the generator's own
execution therefore requires either (a) instrumenting sync to emit OCEL events, or
(b) a post-hoc adapter that converts the sync receipt + per-rule results into an
OCEL log. Neither exists yet, and neither can be produced here without the binary.

---

## 2. A concrete object-centric conformance model for OUR generator

This is the "declared model" the doctrine demands we conformance-check against.
The generator's authoritative path (Dossier 01 §0, traced to real source) is:

```
load_ontology  →  execute_inference_rules (derive-operators ; derive-pareto-dominance)
               →  execute_generation_rules (6 SELECT rules → 6 output files)  →  receipt
```
(`pipeline.rs:1459-1476`, inference sorted by `order` `pipeline.rs:452-459`,
generation SELECT-only `pipeline.rs:836-858`.)

### 2.1 Object types (object-centric — multiple types must align)

Modeled on the doctrine's "artifact / receipt / proof-gate / benchmark / release"
object families, specialized to our generator and to the **real** counts in this
project (Dossier 01/03, INTEGRATION_REPORT §3):

| Object type | Identity | Count (this project) | Lifecycle (lawful) |
|-------------|----------|----------------------|--------------------|
| `ontology` | source TTL set | 4 files (`tpot-search-space.ttl` + 3 imports, `ggen.toml:36-42`) | `Loaded` (create) → referenced by inference |
| `inference_rule` | rule name | 2 (`derive-operators` order 1, `derive-pareto-dominance` order 2, `ggen.toml:74,112`) | `Scheduled` → `Materialized` (adds triples) |
| `operator` | `tpot:operatorId` (= `pi:algorithmId`) | **60** (one per `pi:` algorithm, `ggen.toml:82-106`) | `Derived` (by inference) → `Ranked` → optionally `EliteSelected` |
| `stage` | `tpot:stageId` | **9** (category↔stage bijection, CONTRACT §4) | `Declared` → `Resolved` (elite bound) |
| `dominance_edge` | `(opA,opB)` | derived pairs (`ggen.toml:120-141`) | `Derived` (by inference) |
| `generation_rule` | rule name | **6** (`ggen.toml:158-229`) | `Queried` → `Rendered` → `Emitted` |
| `output_artifact` | `output_file` | 6 (`ggen.toml`, `tpot_config.py`, `pipeline.json`, `SEARCH_SPACE.md`, `STAGE_PLAN.md`, `objectives.json`) | `Rendered` → `Written` |
| `receipt` | receipt id | 1 sync receipt | `Emitted` (terminal, binds inputs→outputs) |

The **case object** for the run-level DFG is the **generation run** (one
`run_id`), exactly as `ggen-lsp` uses an `episode` case object
(`intel/events.rs:48-51, 92-98`). Per-object cases (e.g. one `operator`, one
`output_artifact`) give the object-centric perspectives the doctrine requires.

### 2.2 Activities (the mineable verbs)

Aligned to the real pipeline stages (`pipeline.rs`), named so they compare
string-equal in SPARQL/mining (the `ggen-lsp` convention, `intel/events.rs:14-35`):

| Activity | Emitting stage (real source) | Touches objects |
|----------|------------------------------|-----------------|
| `OntologyLoaded` | `load_ontology` (`pipeline.rs:1461→408`) | `ontology` |
| `InferenceRuleApplied` | `execute_inference_rule` (`pipeline.rs:490-492`) | `inference_rule`, (`operator`/`dominance_edge` as products) |
| `OperatorDerived` | product of `derive-operators` | `operator`, `stage` |
| `DominanceDerived` | product of `derive-pareto-dominance` | `dominance_edge` |
| `EliteSelected` | result of `extract-pareto-pipeline.rq` argmax | `operator`, `stage` |
| `GenerationRuleQueried` | `execute_generation_rules` SELECT (`pipeline.rs:836`) | `generation_rule` |
| `ArtifactRendered` | Tera render (`pipeline.rs:996-1009`) | `generation_rule`, `output_artifact` |
| `ArtifactEmitted` | `output_dir.join(file)` write (`pipeline.rs:1035`) | `output_artifact` |
| `ReceiptEmitted` | sync receipt (`executor.rs` receipt path) | `receipt` |

### 2.3 Lawful lifecycle transitions (the conformance constraints)

These are the per-case-object ASK constraints, each expressible exactly as
`check_lifecycle_order(graph, qualifier, [A, B, …])`
(`ggen-graph/src/ocel/conformance.rs:23`):

1. **No inference before load.** `OntologyLoaded ≺ InferenceRuleApplied`.
2. **No derivation before its rule.** `InferenceRuleApplied(derive-operators) ≺
   OperatorDerived`; `InferenceRuleApplied(derive-pareto-dominance) ≺
   DominanceDerived`.
3. **Inference order honored.** derive-operators (`order 1`) `≺` the SELECTs;
   generation strictly follows all inference (`pipeline.rs:1464` before `1475`).
4. **No elite without a stage + operators.** `OperatorDerived ≺ EliteSelected`,
   and every `EliteSelected` operator's `stage` is one of the 9.
5. **No render before its query.** per `generation_rule`:
   `GenerationRuleQueried ≺ ArtifactRendered`.
6. **No output before render (the headline rule).** per `output_artifact`:
   `ArtifactRendered ≺ ArtifactEmitted` — *"no output before select/render"*.
7. **No render before selection for pipeline artifacts.** the rules that consume
   `extract-pareto-pipeline.rq` (RULE 1 ggen.toml, RULE 3 pipeline.json,
   `ggen.toml:160,184`) require `EliteSelected ≺ ArtifactRendered`.
8. **Receipt is terminal and total.** `ArtifactEmitted ≺ ReceiptEmitted` for
   **every** output_artifact, and the receipt binds all input hashes (ontology,
   queries, templates) to all output hashes (the doctrine's
   "release only from a complete, validated history").

### 2.4 Object-centric consistency invariants (across types)

- **Bijection:** exactly 9 `stage` objects, exactly 60 `operator` objects, each
  operator's `stage` ∈ the 9 (CONTRACT §4 counts 4+20+6+10+4+8+5+2+1 = 60).
- **One elite per stage:** exactly 9 `EliteSelected` events, partitioning stages.
- **No orphan artifact:** every `ArtifactEmitted` has a preceding
  `GenerationRuleQueried` of the same `generation_rule` (no output without a query
  — the epistemic-bypass guard, mirrors `coding-agent-mistakes.md` §1.2).
- **No release from invalid history:** `ReceiptEmitted` only after all 6
  `ArtifactEmitted` (no `ReceiptEmitted` if a render failed).

---

## 3. The 8 doctrine criteria — met-statically vs requires-run

The doctrine (`process-mining-chicago-tdd.md` §"Success Criteria") lists 8. For
each: can we satisfy it **here** (static/structural, Python stdlib, no run), or
does it **require a real ggen sync OCEL log** (honestly unmet, no binary)?

| # | Criterion | Verdict | Justification |
|---|-----------|---------|---------------|
| 1 | **Event-log completeness** (all required OCEL attrs present) | **MET-STATICALLY** | We define the schema (§2.1-2.2) and the harness validates that every event in a *candidate* log carries `activity`, `timestamp`, `object-id`, `object-type` — exactly the fields `ggen_graph::OcelEvent`/`OcelObjectRef` already guarantee (`ocel_types.rs:33-55`). Validatable on any log, including the labeled-synthetic expected log, **today**. |
| 2 | **Object-lifecycle soundness** (no orphans, no broken workflows) | **MET-STATICALLY** | The lawful transitions (§2.3 rules 4,6 + §2.4 orphan check) are pure graph predicates over a log. The harness asserts them on the expected log and **rejects** an orphan-artifact log. No run needed to validate the *checker*; a run is needed to validate the *real* lifecycle. |
| 3 | **Temporal lawfulness** (all events in lawful order) | **REQUIRES-RUN** | We can *define* and *test the checker* against synthetic in-order / out-of-order logs (mirroring `conformance.rs:100-143`). But proving the **real** generation stages occurred in lawful order needs **real timestamps from a real run**. No ggen binary here → honestly **unmet** for the live process. |
| 4 | **Conformance fitness ≥ 0.9** (discovered model matches declared) | **REQUIRES-RUN** | Fitness is a *replay metric over an observed log*. With no observed log there is nothing to replay. We can publish the declared model (the footprint/transitions of §2) statically, but "fitness 0.9" is meaningless without a captured run. **Unmet here.** |
| 5 | **Variant control** (execution variants within bounds) | **REQUIRES-RUN** | The generator is deterministic (seed 42, `tpot:randomSeed`, CONTRACT §5) so the *expected* variant count is **1** per object type. But counting **observed** variants requires multiple real runs. We can assert the *intended* bound statically; we cannot measure actual variant explosion without runs. **Unmet here** (assertable-as-expectation only). |
| 6 | **Deviation-free** (no skipped stages / hidden loops / shadow paths) | **REQUIRES-RUN** | "No skipped stage" is a statement about what *actually executed*. Statically we know the declared 6 rules + 2 inference rules + load (`ggen.toml`); detecting a *skipped* or *repeated* stage at runtime needs the real log. **Unmet here** for the live process; the *negative-log rejection tests* (which prove the checker catches skips) are met-statically. |
| 7 | **Object-centric consistency** (artifact/receipt/proof/benchmark/release align) | **PARTIALLY MET-STATICALLY** | The cross-type invariants (§2.4: 9 stages, 60 operators, bijection, one-elite-per-stage, receipt-binds-all) are **already proven statically** by the existing reference + validator: `verify/test_tpot2_autoconfig.py` asserts the 9-stage bijection + 60-algo coverage + argmax elites, and `verify/out/reference_pipeline.json` is the materialized 9-elite object set. The *receipt* object's binding to outputs, however, **requires a run** (no receipt is produced here). So: structural alignment MET, receipt-causality REQUIRES-RUN. |
| 8 | **Semantic invariants** (7 laws + 5 OCEL constraints pass) | **PARTIALLY MET-STATICALLY** | The OCEL structural constraints (unique object-ids per type, consistent object-types, lawful create-before-complete, temporal ordering, no duplicate terminal state) are checkable on the expected log **today** by the harness. The 7 process-semantic laws that reference *real execution* (e.g. "no benchmark on an artifact never compiled") reduce here to "no `ArtifactEmitted` without `ArtifactRendered`/`GenerationRuleQueried`" — provable on the expected log statically, but only *evidentially true* of the system once a run exists. |

**Scoreboard.** Met-statically (validatable here, today): **1, 2, 7-structural,
8-OCEL-constraints** → **4 criteria** fully, plus 7/8 partially. Require a real run
(honestly unmet, no binary): **3, 4, 5, 6**, plus the receipt-causality slice of 7
and the execution slice of 8. This matches the doctrine's own coverage note: the
declared-model + structural checks are the "Gate 10 process-conformance /
Gate 5+11 semantic-invariant" surfaces; the run-derived criteria need OCEL from a
real manufacturing run.

This is consistent with the project's standing honesty (INTEGRATION_REPORT §6,
Dossier 01): **`ggen sync` was not executed; no `.ggen/receipts/*.json` exists and
none is fabricated.** The same constraint blocks criteria 3/4/5/6 here.

---

## 4. Proposed artifact — making the proof real once a run exists

Two files under the generator's `verify/` (the only writable tree). Both are
**read-only-honest today** and **become live the moment a real OCEL log appears** —
no edit required to flip from synthetic to real, which is the anti-cheating
property (`testing-anti-cheating.md` §6: faking must be harder than real
execution).

### 4.1 `verify/conformance_spec.md` (the frozen declared model)

A machine-and-human-readable spec containing **§2 verbatim**: the 8 object types,
the 9 activities, the 8 lawful transitions (each as an explicit
`[activity_a ≺ activity_b per <case-object>]` line), and the §2.4 object-centric
invariants with their real source citations. This is the "declared model" the
doctrine demands exist *before* conformance checking — the analogue of the BPMN
model in `conformance_check(event_log, bpmn_file=…)`.

### 4.2 `verify/conformance_check.py` (the OCEL conformance harness)

Pure Python 3.11 stdlib (CONTRACT §9 — `tomllib`/`json` available, no `pm4py`/
`rdflib`). It re-implements the **same lifecycle-order semantics** as
`ggen_graph::ocel::conformance::check_lifecycle_order` (`conformance.rs:23-61`):
for a list of activities and a case-object qualifier, assert each activity exists
on the same case object with strictly increasing timestamps. Structure:

```
load_ocel(path)            # accepts BOTH the cpmp spec-JSON (objectTypes/events,
                           #   cpmp/src/ocel.rs:123) AND the ggen NDJSON form
                           #   (objects/events, ocel_types.rs:6) — auto-detect.
lifecycle_order(log, qualifier_or_type, [A,B,…]) -> bool   # mirrors conformance.rs
directly_follows(log, case_type) -> {(a,b): freq}          # mirrors dfg.rs / cpmp ocel.rs:209
variants(log, object_type) -> {trace: freq}                # mirrors cpmp ocel.rs:222
check_declared_model(log) -> report                        # runs §2.3 rules 1-8 + §2.4 invariants
inject_impossible_logs() -> [8 logs]                       # the doctrine's negative tests
```

**Mode A — runs TODAY (static/structural, criteria 1,2,7,8):**
- Builds a **clearly-labeled-synthetic *expected* OCEL log** straight from the
  already-real `verify/out/reference_pipeline.json` (the 9 elites) + `ggen.toml`
  (6 rules, 2 inference rules, 4 ontologies). Header comment:
  `# SYNTHETIC EXPECTED log — derived from the declared model, NOT a captured ggen run.`
- Asserts the expected log **passes** all §2.3 transitions and §2.4 invariants
  (positive conformance of the *model with itself*).
- **Injects the doctrine's 8 impossible logs** (`process-mining-chicago-tdd.md`
  §7) specialized to our verbs, and **requires each to be REJECTED**:
  1. `ReceiptEmitted` before `ArtifactEmitted` (release before validate),
  2. `ArtifactRendered` before `GenerationRuleQueried` (render before select),
  3. two terminal `ArtifactEmitted` for one artifact without a re-render (dup
     terminal state),
  4. `ReceiptEmitted` referencing an `output_artifact` that was never emitted
     (receipt for non-existent artifact),
  5. `EliteSelected` before any `OperatorDerived` (benchmark before compile),
  6. `ArtifactEmitted` with no causal `GenerationRuleQueried` predecessor (proof
     gate pass with missing predecessor),
  7. an `output_artifact` event with no generation run case (orphan invocation),
  8. an artifact both failed-render and emitted without rework (failed+released).

  This is the anti-cheating core: the harness is only "passing" if it *catches*
  these — a lazy/stub implementation that always returns `True` fails the negative
  suite. Faking the negatives is strictly harder than implementing the real check.

**Mode B — runs ONCE A REAL LOG EXISTS (criteria 3,4,5,6,7-receipt,8-execution):**
- If `.ggen/ocel/agent-edit-events.ocel.jsonl` (or a sync-derived
  `.ggen/ocel/sync-run.ocel.jsonl`, see §4.3) is present, load **the real log**
  and run the identical `check_declared_model` against it — now the verdict is
  *evidence*, not self-consistency. Variant count (criterion 5), temporal
  lawfulness (3), deviation detection (6), and conformance fitness (4) become
  measurable. The script prints `MODE B (real log)` vs `MODE A (synthetic
  expected)` so the evidence class is never ambiguous.
- Exit non-zero on any conformance failure (so it gates CI like
  `validate_artifacts.py` does, INTEGRATION_REPORT §4).

### 4.3 The missing bridge (flag for a future agent / for the ggen team)

Because `ggen sync` emits **no** OCEL today (§1.6), Mode B needs one of:

- **(preferred, deepens authority)** instrument `GenerationPipeline::run`
  (`pipeline.rs:1459-1476`) to append `OntologyLoaded / InferenceRuleApplied /
  GenerationRuleQueried / ArtifactRendered / ArtifactEmitted / ReceiptEmitted`
  events to an `IntelLog`-style writer — reusing the **existing**
  `ggen_graph::ocel::OcelEvent` + `IntelLog::append` (`intel/log.rs:53-76`) so no
  new log format is invented; or
- **(cheaper, reduces drift)** a converter that reads the sync **receipt** + the
  per-rule `duration_ms` results (`executor.rs:214-215,1007,1189`) and synthesizes
  a `sync-run.ocel.jsonl` post-hoc. This is weaker (timestamps are reconstructed,
  not natively emitted) and must be labeled as such — it corroborates, it does not
  capture.

Either makes the doctrine's run-derived criteria genuinely satisfiable. **Neither
can be built or run in this container** (no cargo/ggen, CONTRACT §9); they are the
honest precondition for closing criteria 3/4/5/6.

---

## 5. Direct answers to the four sub-questions

1. **What OCEL/process-mining capability genuinely exists that could derive an
   event log from a real ggen sync run?** The *engine* exists and is real:
   `discover_dfg` (`ggen-graph/src/ocel/dfg.rs:38`), `check_lifecycle_order`
   (`.../conformance.rs:23`), `OcelLog`/`EvidenceProjector`
   (`.../ocel_types.rs`, `.../projection.rs:14`), `IntelLog` NDJSON writer
   (`ggen-lsp/src/intel/log.rs:53`), and a full mine→conformance→promote loop
   (`ggen-lsp/src/intel/mine.rs:211`) — *all tested with real positive/negative
   cases*, plus a generated self-audit OCEL of ggen's own process
   (`.../self_audit.rs:12`, emitted by `bin/emit_audit.rs`). **BUT** none of it is
   wired into `ggen sync`: the codegen pipeline (`ggen-core/src/codegen/pipeline.rs`)
   emits no OCEL and no spans (only per-rule `duration_ms`). So a real sync run
   does **not** currently produce a mineable log — deriving one needs the §4.3
   bridge.

2. **A concrete object-centric conformance proof for our generator?** §2: 8 object
   types (ontology, inference_rule, operator×60, stage×9, dominance_edge,
   generation_rule×6, output_artifact×6, receipt), 9 activities (OntologyLoaded …
   ReceiptEmitted), 8 lawful transitions (no inference before load; no derive
   before its rule; no elite without operators; no render before query; **no
   output before render**; elite-before-render for pipeline artifacts; receipt
   terminal+total), and cross-type invariants (9/60 bijection, one-elite-per-stage,
   no-orphan-artifact, no-release-from-invalid-history). Each transition is exactly
   a `check_lifecycle_order(...)` call.

3. **Which criteria static vs require-run?** §3 table. **Met-statically (here,
   today):** 1 (log completeness), 2 (lifecycle soundness), 7-structural
   (9/60 bijection — already proven by `test_tpot2_autoconfig.py` +
   `reference_pipeline.json`), 8-OCEL-constraints. **Require a real run (honestly
   unmet — no ggen binary):** 3 (temporal lawfulness of live stages), 4
   (conformance fitness ≥ 0.9), 5 (observed variant control), 6 (deviation-free),
   plus the receipt-causality slice of 7 and the execution slice of 8. The
   negative-log *rejection tests* for 3/6/8 are met-statically (they validate the
   checker, not the live process).

4. **Proposed artifact?** §4: `verify/conformance_spec.md` (frozen declared model)
   + `verify/conformance_check.py` (pure-stdlib OCEL conformance harness mirroring
   `ggen-graph`'s `check_lifecycle_order`/`discover_dfg` semantics). It runs in
   **Mode A today** — positively conforms a labeled-synthetic expected log and
   **rejects the doctrine's 8 impossible logs** (anti-cheating: passing requires
   catching the negatives) — and flips to **Mode B unchanged** once a real
   `.ggen/ocel/*.jsonl` exists (criteria 3/4/5/6 become evidence). §4.3 names the
   one-line honest precondition: instrument sync to emit OCEL via the **existing**
   `IntelLog` writer, or convert the sync receipt post-hoc.

---

## 6. Honesty ledger (per anti-cheating doctrine)

- **No event-log evidence is fabricated.** No `.ggen/ocel/*.jsonl` and no
  `.ggen/receipts/*.json` were produced; this container has no ggen binary
  (CONTRACT §9). Any log the proposed harness builds in Mode A is a **labeled
  synthetic expected log derived from the declared model**, never presented as a
  captured run.
- **The run-derived criteria (3,4,5,6) are reported as unmet here, on purpose.**
  Claiming "conformance fitness 0.9" or "no variant explosion" without a captured
  log would be the exact NARRATION/SELF-CERT failure the doctrine forbids.
- **Every capability claim is cited to real source** (`ggen-graph/src/ocel/*`,
  `ggen-lsp/src/intel/*`, `ggen-core/src/codegen/*`, `cpmp/src/ocel.rs`,
  `ggen.toml`, `verify/out/reference_pipeline.json`). The decisive negative
  (sync emits no OCEL) was confirmed by two grep sweeps returning **no matches**
  for OCEL/span emission in the codegen path.
- **The proposed proof is anti-cheating by construction:** its value is in
  *rejecting* the 8 impossible logs; a stub that returns `True` fails the negative
  suite, so faking it is harder than running the real check (the
  `testing-anti-cheating.md` §8 resilience test).
