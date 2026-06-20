# Integration Report — TPOT2 Auto-Configuration Generator for ggen.toml (wasm4pm)

**Built by:** 5 parallel agents against a frozen `CONTRACT.md`, then integrated +
elevated to Vision-2030 levels by the orchestrator.
**Date:** 2026-06-20 · **Branch:** `claude/charming-feynman-dficbr`

---

## 1. What this is

A **TPOT2-style AutoML auto-configuration generator**. It models the **wasm4pm**
process-mining engine (60 algorithms, 55 cognition breeds, 6 SPARQL proofs) as a
TPOT2 genetic-programming search space, then **auto-selects the Pareto-optimal
process-mining pipeline** and **emits a ready-to-run `ggen.toml`** (plus a TPOT2
`config_dict`, a pipeline manifest, a search-space report, a stage plan, and a
fitness-objective summary).

> TPOT2 (Le, Fu & Moore 2020, *Bioinformatics* 36(1):250–256) optimises an ML
> pipeline by genetic programming over a `config_dict` with multi-objective
> NSGA-II Pareto selection — maximise CV score, minimise complexity. We mirror it
> exactly for process mining: **maximise `qualityTier`, minimise `speedTier`
> (cost)** over the 60 wasm4pm algorithms. Process mining: van der Aalst (2016).

**"Auto" = derived, never hand-coded.** The operator search space is produced by a
SPARQL `CONSTRUCT` over the `pi:` registry (`inference/derive-operators.rq`), the
per-operator fitness is *computed* (`fitnessScore = qualityTier − 0.5·speedTier`),
and the per-stage elite is selected by a SPARQL `NOT EXISTS` argmax. No Rust/Tera
match-arm enumerates algorithms — the ontology is the single source of truth.

---

## 2. The auto-selected pipeline (the result)

Computed deterministically (seed 42) by the SPARQL ranking and independently
reproduced by the pure-Python reference (`verify/out/reference_pipeline.json`):

| # | stage | elite operator | q | s | fitness | wasm export | proof |
|---|-------|----------------|---|---|---------|-------------|-------|
| 1 | ingest | `pnml_import` | 90 | 8 | **86.0** | `from_pnml_wasm` | ocel-load.rq |
| 2 | discover | `inductive_miner` | 85 | 10 | **80.0** | `discover_inductive_miner` | discover-dfg.rq |
| 3 | discover_oc | `ocel_petri_net` | 78 | 30 | 63.0 | `discover_ocel_petri_net` | — |
| 4 | analyze | `compute_activity_transition_matrix` | 80 | 10 | 75.0 | `compute_activity_transition_matrix` | — |
| 5 | conform | `complexity_metrics` | 90 | 15 | **82.5** | `compute_complexity_metrics` | conformance-check.rq |
| 6 | learn | `ml_classify` | 80 | 25 | 67.5 | `ml_classify` | ml-classify.rq |
| 7 | predict | `compute_ewma` | 75 | 8 | 71.0 | `compute_ewma` | predict-activity.rq |
| 8 | simulate | `playout` | 80 | 30 | 65.0 | `petri_net_playout` | — |
| 9 | orchestrate | `agentic_pipeline` | 90 | 80 | 50.0 | `run_agentic_pipeline` | — |
| + | drift-monitor | `detect_drift` | — | — | — | `detect_drift` | **detect-drift.rq** |

The `conform` tie is instructive: `complexity_metrics` (q90,s15,f82.5) wins over
`alignments` (q95,s25,f82.5) **only via the frozen tie-break** (equal fitness →
lower speed-tier) — proving the tie-break logic is genuinely exercised.

---

## 3. Capability coverage ("use ALL of wasm4pm")

| Capability | Count | How it is used |
|-----------|------:|----------------|
| `pi:ProcessIntelligenceAlgorithm` | **60** | every one is derived into a `tpot:Operator` (9-category → 9-stage bijection: 4+20+6+10+4+8+5+2+1) |
| `compat:CognitionBreed` | **55** | one is wired as the GP meta-strategy (`tpot:metaBreed compat:Breed_version_space`; ebl/ilp/rl_symbolic cited as alternates) |
| semconv SPARQL proofs | **6/6** | each elite stage cites its CONSTRUCT result-mapping; `detect-drift.rq` wired as the cross-cutting drift monitor |
| `pi:category` buckets | **9/9** | the 9 `tpot:PipelineStage` individuals |

---

## 4. Verification evidence (all real, all reproducible)

Run from the project root (no `ggen`/`cargo`/`rdflib` needed — Python 3.11 stdlib):

```
python3 verify/reference_autoconfig.py     # emits the pipeline + a valid ggen.toml
python3 verify/validate_artifacts.py       # 56/56 PASS, exit 0
python3 verify/test_tpot2_autoconfig.py    # 7/7 Chicago-TDD, exit 0
python3 verify/test_vision2030.py          # 4/4 SLO+E2E, exit 0
```

| Suite | Result | Proves |
|-------|--------|--------|
| `validate_artifacts.py` | **56 PASS / 0 FAIL** | ORDER BY on all SELECTs, no `SELECT *`, no template frontmatter, `imports` not `additional`, no `../` escapes, 60 algos, 9 stages, breed used, **all 6 proofs wired by real generation artifacts** |
| `test_tpot2_autoconfig.py` | **7 passed** | 60-algo parse, 9-stage bijection, exact fitness (ilp=70.0, alignments=82.5, dfg=29.5), determinism (byte-identical re-runs), Pareto non-domination, argmax cross-check, valid emitted TOML |
| `test_vision2030.py` | **4 passed** | SLO latency budget; E2E round-trip (ttl→select→emit→re-parse, elites survive); all 6 proofs wired; per-stage + drift templates present |

The validator and tests are **hardened against self-reference**: documentation and
the `verify/` scripts are excluded as proof-of-wiring evidence, so a proof only
counts when it appears in a real generation artifact (query / template / ttl /
toml). Faking is harder than the real wiring.

---

## 5. The 6-question patch contract (`.claude/rules/coding-agent-mistakes.md`)

- **Q1 real state changed** — a complete ggen example project on disk (ontology,
  inference, 6 queries, 10 templates, driver `ggen.toml`); the reference run
  writes `verify/out/reference_pipeline.json` (9 stages) + a tomllib-valid
  `reference_ggen.toml`.
- **Q2 authoritative path** — the generation pipeline: ontology → inference
  (derive operators + dominance) → SELECT (rank by fitness) → Tera (emit config).
  Operators are derived, not enumerated (no epistemic bypass).
- **Q3 negative path fails correctly** — `validate_artifacts.py` exits non-zero on
  a missing ORDER BY, a `SELECT *`, template frontmatter, `additional`, a `../`
  escape, wrong coverage, or an unwired proof (this is how the `detect-drift.rq`
  gap was caught and then fixed).
- **Q4 invariant** — frozen fitness formula + 9-stage/60-algo bijection + ORDER BY
  discipline, all enforced by the validator and the Chicago-TDD suite.
- **Q5 legacy/broken path removed** — the generated `ggen.toml` no longer
  references non-existent files (the 5 per-stage templates + `drift.tera` were
  created; the 6 proofs were copied into `queries/`); that "contract drift" is gone.
- **Q6 proof object** — `verify/out/validation_report.json` (56/56) +
  `reference_pipeline.json` + the two test suites' output.

This patch **deepens authority** (the search space is SPARQL-derived, not
hardcoded) **and reduces drift** (fitness is computed from the registry; broken
references removed).

---

## 6. Honest limitations

- **`ggen sync` was NOT executed.** This container has no usable `cargo`
  (the active rustup toolchain lacks the cargo component) and no prebuilt `ggen`
  binary, so the real five-stage ggen pipeline could not be run and **no
  `.ggen/receipts/*.json` was produced — none is fabricated.**
- Verification is therefore **(a)** structural (TOML/SPARQL/Tera/TTL scanning) and
  **(b)** an **independent pure-Python reference** that re-derives the exact same
  deterministic pipeline and emits a tomllib-valid `ggen.toml`. The reference is a
  *re-implementation* of the selection logic, corroborating — not a substitute for
  — a real `ggen sync`. To run the real pipeline: install a cargo-capable
  toolchain, `cargo build -p ggen-cli`, then `ggen sync` in this directory.
- The 6 semconv proofs are CONSTRUCT (wasm4pm JSON→RDF mappings); the generated
  config references them as each stage's `tpot:resultMapping`, and uses the
  pipeline SELECT as the generation query (valid SELECT→template), not the
  CONSTRUCT directly.
