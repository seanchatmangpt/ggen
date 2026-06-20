# TPOT2 Auto-Configuration Generator for `ggen.toml` (over wasm4pm)

A [ggen](https://github.com/seanchatmangpt/ggen) example that models the
**wasm4pm** process-mining engine as a **TPOT2 genetic-programming search space**
and uses SPARQL ranking to **auto-select a Pareto-optimal pipeline**, then emits
a generated `ggen.toml`, a TPOT2 `config_dict`, a pipeline manifest, and a
search-space report — entirely from an RDF ontology.

> **"Auto" means derived, not hand-coded.** The optimal operator for each
> pipeline stage is computed by a SPARQL fitness ranking over the ontology, never
> written down by a human. If the algorithm registry grows, the search space —
> and the chosen pipeline — grows with it, with no code changes.

---

## 1. The concept

[TPOT2](https://github.com/EpistasisLab/tpot) (Tree-based Pipeline Optimization
Tool 2) is an AutoML system that searches a space of machine-learning operators
using **genetic programming** with **multi-objective (NSGA-II) Pareto
optimization** — classically: *maximize cross-validation score, minimize
pipeline complexity*.

This example mirrors TPOT2's structure, but the "operators" are the **60
process-mining algorithms** in the wasm4pm registry (the `pi:` namespace), and
the two objectives are read off each algorithm's quality/speed tiers:

| TPOT2 concept | wasm4pm mapping in this project |
|---------------|---------------------------------|
| `config_dict` (operator search space) | 60 `tpot:Operator` individuals, **derived** from `pi:ProcessIntelligenceAlgorithm` |
| Pipeline stage | one of 9 `tpot:PipelineStage` individuals (bijection to the 9 `pi:category` values) |
| Objective 1: maximize CV score | **maximize `qualityTier`** (weight 1.0) |
| Objective 2: minimize complexity | **minimize `speedTier`** as cost (weight 0.5) |
| GP meta-strategy | a `compat:` cognition breed (e.g. `compat:Breed_version_space`) |
| NSGA-II selection | `tpot:dominates` Pareto-dominance edges + per-stage argmax |

### Fitness function (frozen)

Each operator's scalarized fitness is:

```
fitnessScore = qualityTier − (lambdaCost · speedTier),  with  lambdaCost = 0.5
```

computed inside the inference CONSTRUCT via `BIND((?q - (0.5 * ?s)) AS ?fitness)`.
Higher quality is rewarded; higher cost (slower tier) is penalized at half
weight. This is the single knob that turns the 60-algorithm catalog into a
ranked search space.

### Pareto selection (frozen)

Operator **A dominates B** iff
`qualityTier_A ≥ qualityTier_B AND speedTier_A ≤ speedTier_B AND (qualityTier_A > qualityTier_B OR speedTier_A < speedTier_B)`.
The **Pareto front** is the set of globally non-dominated operators. The **chosen
pipeline** picks, per stage, the operator with maximum `fitnessScore` (the
*elite*); ties break by lower `speedTier`, then `operatorId` ascending.

### Determinism

Genetic config is fixed at `populationSize = 100`, `generations = 50`,
`mutationRate = 0.9`, `crossoverRate = 0.1`, `selectionStrategy = "NSGA2"`, and
**`randomSeed = 42`** so the derivation is reproducible. (The SPARQL selection
itself is deterministic; the seed records the GP configuration TPOT2 would use.)

---

## 2. The pipeline: 9 stages ↔ 9 wasm4pm categories ↔ 60 algorithms

The 60 algorithms partition exactly into 9 `pi:category` values. Each category
becomes one ordered pipeline stage. Algorithm counts per stage sum to
4 + 20 + 6 + 10 + 4 + 8 + 5 + 2 + 1 = **60** (verified against
`ontology/algorithms.ttl`).

| Order | Stage (`tpot:stageId`) | wasm4pm category (`pi:category`) | # algos | Proof query |
|-------|------------------------|----------------------------------|---------|-------------|
| 1 | `ingest`      | `import_export`       | 4  | `ocel-load.rq` |
| 2 | `discover`    | `discovery`           | 20 | `discover-dfg.rq` |
| 3 | `discover_oc` | `object_centric`      | 6  | — |
| 4 | `analyze`     | `discovery_analytics` | 10 | — |
| 5 | `conform`     | `conformance`         | 4  | `conformance-check.rq` |
| 6 | `learn`       | `ml_analytics`        | 8  | `ml-classify.rq` |
| 7 | `predict`     | `prediction`          | 5  | `predict-activity.rq` (+ `detect-drift.rq`) |
| 8 | `simulate`    | `simulation`          | 2  | — |
| 9 | `orchestrate` | `agentic`             | 1  | — |

The chosen pipeline is one elite operator per stage — a 9-step plan from raw
event-log ingest through agentic orchestration.

---

## 3. File layout

```
tpot2-wasm4pm-autoconfig/
├── ggen.toml                       # DRIVER manifest (this agent) ── ties everything together
├── README.md                       # this file
├── CONTRACT.md                     # frozen contract shared by all builders
├── ontology/
│   ├── tpot-search-space.ttl       # NEW tpot: vocab: stages, objectives, genetic config, category→stage map
│   ├── algorithms.ttl              # 60 pi:ProcessIntelligenceAlgorithm individuals (registry)
│   ├── breeds.ttl                  # 55 compat: cognition breeds (GP meta-strategy)
│   └── tpot-shapes.ttl             # SHACL shapes for the tpot: vocabulary
├── inference/
│   ├── derive-operators.rq         # CONSTRUCT: pi:Algorithm → tpot:Operator (+ computed fitness)
│   └── derive-pareto-dominance.rq  # CONSTRUCT: tpot:dominates edges
├── queries/
│   ├── extract-operators.rq        # operator catalog (all 60)
│   ├── extract-pipeline-stages.rq  # ordered 9-stage plan
│   ├── extract-pareto-pipeline.rq  # the chosen pipeline (per-stage elite)
│   ├── extract-pareto-front.rq     # globally non-dominated operators
│   ├── extract-hyperparameters.rq  # hyperparameter grid
│   └── extract-fitness-objectives.rq # multi-objective config (2 rows)
├── templates/
│   ├── generated-ggen-toml.tera    # renders the auto-config ggen.toml (a specification)
│   ├── tpot-config-dict.py.tera    # renders a TPOT2 config_dict (Python)
│   ├── pipeline-manifest.json.tera # renders pipeline.json (canonical pipeline form)
│   ├── search-space-report.md.tera # renders SEARCH_SPACE.md
│   ├── stage-plan.md.tera          # renders STAGE_PLAN.md (RULE 5, dedicated)
│   ├── objectives.json.tera        # renders objectives.json (RULE 6, dedicated)
│   ├── {ingest,discover,conform,learn,predict}.tera  # per-stage wasm4pm descriptors
│   └── drift.tera                  # detect-drift monitor descriptor
├── verify/
│   ├── reference_autoconfig.py     # pure-Python TPOT2 selection reference
│   ├── validate_artifacts.py       # structural validators (60 checks)
│   ├── test_tpot2_autoconfig.py    # Chicago-TDD checks (7)
│   ├── test_vision2030.py          # SLO + E2E boundary-crossing (4)
│   ├── conformance_check.py        # process-mining conformance; rejects 8 impossible logs (10)
│   ├── conformance_spec.md         # the frozen declared process model
│   └── slo_check.py                # performance SLO gate
└── research/
    └── 00-INDEX.md … 10-*.md       # 10-agent source-grounded audit dossiers
```

---

## 4. How `ggen sync` drives the generation

`ggen.toml` is the driver. Conceptually, `ggen sync` would:

1. **Load the ontology graph** — `[ontology] source` (`tpot-search-space.ttl`)
   plus `imports` (`algorithms.ttl`, `breeds.ttl`, `tpot-shapes.ttl`). The extra
   ontologies are loaded via **`imports`**, not the legacy field name that
   silently dropped them (WASM4PM BUG-001).

2. **Run inference (the AUTO step)** — the two `[inference]` rules run *before*
   any SELECT, adding **new** triples to the graph:
   - `derive-operators` mints one `tpot:Operator` per `pi:` algorithm, joins it
     to the stage that owns its category, and **computes** `tpot:fitnessScore`.
   - `derive-pareto-dominance` adds `tpot:dominates` edges for every dominating
     operator pair.
   Both are genuine derivations (they materialize triples that did not exist on
   the input), not identity CONSTRUCTs (WASM4PM BUG-009).

3. **Run the `[[generation.rules]]` SELECTs and render templates**, writing
   under `generated/` (which stays inside the project root — WASM4PM YIELD-001):

   | Rule | Query | Template | Output |
   |------|-------|----------|--------|
   | `generate-ggen-toml`          | `extract-pareto-pipeline.rq`   | `generated-ggen-toml.tera`    | `generated/ggen.toml` |
   | `generate-tpot-config`        | `extract-operators.rq`         | `tpot-config-dict.py.tera`    | `generated/tpot_config.py` |
   | `generate-pipeline-manifest`  | `extract-pareto-pipeline.rq`   | `pipeline-manifest.json.tera` | `generated/pipeline.json` |
   | `generate-search-space-report`| `extract-operators.rq`         | `search-space-report.md.tera` | `generated/SEARCH_SPACE.md` |
   | `generate-stage-plan`         | `extract-pipeline-stages.rq`   | `stage-plan.md.tera`          | `generated/STAGE_PLAN.md` |
   | `generate-fitness-objectives` | `extract-fitness-objectives.rq`| `objectives.json.tera`        | `generated/objectives.json` |

   Every rule has a **dedicated** template that reads exactly its query's
   projection — RULES 5 and 6 were originally reused templates that silently
   rendered blank columns (a projection mismatch ggen-lsp's GGEN-TPL-001 cannot
   detect; see `research/08` + `research/10`), now fixed. Every query and template
   path is a direct relative `{ file = "..." }` reference (no pack indirection —
   WASM4PM BUG-008/011). The generated `ggen.toml` is a **rendered specification**
   of the chosen pipeline (it omits the `[inference]`/`imports` needed to self-run —
   `research/08`); the canonical machine-readable form is `generated/pipeline.json`.

### A note on the manifest's inference rules

The ggen manifest schema (`crates/ggen-core/src/manifest/types.rs`, `InferenceRule`
with `#[serde(deny_unknown_fields)]`) accepts inference rules only as inline
`construct = "..."` text — there is **no `file` field** for an inference rule.
So `ggen.toml` inlines the two CONSTRUCT bodies verbatim; the `.rq` files under
`inference/` remain the readable source of record. (Generation rules, by
contrast, *do* accept `query = { file = ... }` and `template = { file = ... }`,
which is why those reference files directly.)

---

## 5. The 6 wasm4pm SPARQL proofs the generated pipeline references

The generated `pipeline.json` annotates each pipeline stage with the matching
**semantic-convention SPARQL proof** from the wasm4pm `semconv/sparql-proofs/`
catalog — the query that demonstrates that capability against an OCEL event log:

| Proof file | Stage it proves |
|------------|-----------------|
| `ocel-load.rq`         | `ingest` — load an OCEL object-centric event log |
| `discover-dfg.rq`      | `discover` — discover a directly-follows graph |
| `conformance-check.rq` | `conform` — replay/conformance against a model |
| `ml-classify.rq`       | `learn` — ML classification over case features |
| `predict-activity.rq`  | `predict` — next-activity prediction |
| `detect-drift.rq`      | `predict` — concept-drift detection |

(The `analyze`, `discover_oc`, `simulate`, and `orchestrate` stages have no
dedicated proof query and carry an empty `proof_query`.) Wiring each chosen
operator to a proof keeps the AutoML selection honest: the pipeline does not just
*name* an algorithm, it points at the query that exercises it.

---

## 6. Running it (and an honest caveat about this container)

**This container has no `cargo` and no `ggen` binary**, so `ggen sync` cannot be
executed here, and **no generation has been run** — there is no `generated/`
directory and no `.ggen/receipts/`. Nothing in this README reports a sync that
happened; the tables above describe what the driver *would* produce.

To make the result observable without the ggen binary, the verification harness
under `verify/` (authored separately) provides a **pure-Python reference
implementation** that reads the same ontology, applies the identical fitness
formula and Pareto/elite selection that the SPARQL encodes, and writes a real
output artifact as evidence. The Python `tomllib` module (available here) also
parses `ggen.toml` to confirm it is valid TOML and schema-shaped.

```bash
# Where the ggen toolchain IS available, the driver would run as:
ggen sync                    # load → infer → select → render into generated/

# In THIS container, use the reference + verification gates instead (Python stdlib):
python3 verify/reference_autoconfig.py     # computes the same elite pipeline, writes an artifact
python3 verify/validate_artifacts.py       # 60 structural checks (ORDER BY, no SELECT *, no frontmatter, coverage)
python3 verify/test_tpot2_autoconfig.py    # 7 Chicago-TDD checks (real files, observable state, zero mocks)
python3 verify/test_vision2030.py          # 4 SLO + E2E boundary-crossing checks
python3 verify/conformance_check.py        # process-mining conformance; rejects the 8 impossible logs
python3 verify/slo_check.py                # performance SLO gate (latency + reproducibility)
```

All gates are reproducible and pass green (60 / 7 / 4 / 10 / SLO). Why a real
`ggen sync` is not run here: no usable `cargo` (the pinned nightly lacks the cargo
component) and the workspace `[patch]` requires two unprovisioned sibling repos
(`../lsp-max`, `../wasm4pm`). Executability is established by **source analysis**
(see `research/01,06,07,08,10`) corroborated by the independent Python reference; no
receipt is fabricated.

## 8. Source-grounded research program

A 10-agent read-only audit of this generator against the real ggen/wasm4pm
codebase is indexed in **`research/00-INDEX.md`** (dossiers `01`–`10`, every claim
cited to `file:line`). It established that the **driver `ggen.toml` is runnable by
`ggen sync`**, that **wasm4pm is metadata-only here** (runtime is a downstream
sibling repo), and drove the hardening recorded in `INTEGRATION_REPORT.md` §7
(RULE 5/6 fix, SHACL-gate activation, the conformance + SLO proofs).

---

## 7. References

- **TPOT2 / AutoML:** Le, T. T., Fu, W., & Moore, J. H. (2020). *Scaling
  tree-based automated machine learning to biomedical big data with a feature set
  selector.* **Bioinformatics, 36(1), 250–256.**
  <https://doi.org/10.1093/bioinformatics/btz470>
- **Process mining:** van der Aalst, W. (2016). *Process Mining: Data Science in
  Action* (2nd ed.). Springer.
- **wasm4pm registry & SPARQL proofs:** `ontology_catalogue/wasm4pm/` in this
  repository (the `pi:` 60-algorithm registry, `compat:` cognition breeds, and
  `semconv/sparql-proofs/` capability proofs).
- **ggen:** <https://github.com/seanchatmangpt/ggen>

---

*This is an illustrative example. The fitness weighting (`lambdaCost = 0.5`) and
the quality/speed tiers come from the wasm4pm ontology; they encode relative
preference, not benchmarked wall-clock performance.*
