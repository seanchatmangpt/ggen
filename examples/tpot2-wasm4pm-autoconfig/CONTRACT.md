# FROZEN CONTRACT — TPOT2 Auto-Configuration Generator for ggen.toml (wasm4pm)

> Single source of truth for all 5 agents building this project. Do NOT deviate
> from frozen names. If something here is wrong, STOP and flag it — do not
> silently invent a different name.

## 0. What we are building

A **TPOT2-style auto-configuration generator**: a ggen project whose ontology
models the **wasm4pm** process-mining engine as a TPOT2 genetic-programming
search space (operators + pipeline stages + fitness objectives), and whose
generation rules **auto-select the Pareto-optimal pipeline** and **emit a
generated `ggen.toml`** (plus a TPOT2 `config_dict`, a pipeline manifest, and a
report). "Auto" = the optimal operator per stage is *derived by SPARQL ranking
over fitness*, never hand-coded.

TPOT2 (Tree-based Pipeline Optimization Tool 2) is an AutoML tool that uses
genetic programming over a `config_dict` (operator search space) with
multi-objective (NSGA-II) Pareto optimization: maximize CV score, minimize
pipeline complexity. We mirror this exactly: **maximize `qualityTier`, minimize
`speedTier` (cost)** over the 60 wasm4pm algorithms.

## 1. Real source files (READ THESE — do not fabricate)

- Algorithm registry (60 individuals, `pi:` ns):
  `ontology_catalogue/wasm4pm/ggen/ontology/algorithms.ttl`
- Cognition breeds (55 individuals, `compat:` ns):
  `ontology_catalogue/wasm4pm/ggen/ontology/breeds.ttl`
- Example extraction query (variable-naming convention to imitate):
  `ontology_catalogue/wasm4pm/ggen/queries-algorithms/extract-algorithms.rq`
- The 6 SPARQL proof queries the GENERATED pipeline will reference:
  `ontology_catalogue/wasm4pm/semconv/sparql-proofs/{ocel-load,discover-dfg,conformance-check,ml-classify,detect-drift,predict-activity}.rq`
- ggen.toml schema (serde structs, `deny_unknown_fields`):
  `crates/ggen-core/src/manifest/types.rs`
- Reference ggen example with inline SPARQL + Tera batch template:
  `examples/config-generator/ggen.toml` and `examples/config-generator/templates/app-config.tera`
- Known ggen footguns to avoid (MANDATORY read):
  `docs/jira/WASM4PM-DISCOVERED-BUGS.md`

## 2. Namespaces (FROZEN)

| prefix | IRI | meaning |
|--------|-----|---------|
| `pi:` | `https://wasm4pm.dev/pi#` | existing 60-algorithm registry (reuse verbatim) |
| `compat:` | `https://wasm4pm.dev/ns#` | existing 55 cognition breeds (reuse verbatim) |
| `tpot:` | `https://wasm4pm.dev/tpot#` | NEW — TPOT2 search space + GP concepts |

## 3. Directory layout (FROZEN) — all under `examples/tpot2-wasm4pm-autoconfig/`

```
ggen.toml                          # DRIVER config (Agent 4)
README.md                          # (Agent 4)
CONTRACT.md                        # this file
ontology/
  algorithms.ttl                   # COPY of the 60-algo registry (Agent 1)
  breeds.ttl                       # COPY of the 55 breeds (Agent 1)
  tpot-search-space.ttl            # stages, fitness objectives, genetic config, category→stage map (Agent 1)
  tpot-shapes.ttl                  # SHACL shapes (Agent 1)
queries/
  extract-operators.rq             # operator catalog per stage (Agent 2)
  extract-pipeline-stages.rq       # ordered stage plan (Agent 2)
  extract-pareto-pipeline.rq       # per-stage elite (the chosen pipeline) (Agent 2)
  extract-pareto-front.rq          # globally non-dominated operators (Agent 2)
  extract-hyperparameters.rq       # hyperparameter grid (Agent 2)
  extract-fitness-objectives.rq    # multi-objective config (Agent 2)
inference/
  derive-operators.rq              # CONSTRUCT: pi:Algorithm → tpot:Operator w/ fitness (Agent 2)
  derive-pareto-dominance.rq       # CONSTRUCT: tpot:dominates edges (Agent 2)
templates/
  generated-ggen-toml.tera         # renders the AUTO-CONFIG ggen.toml (Agent 3)
  tpot-config-dict.py.tera         # renders a TPOT2 config_dict (Agent 3)
  pipeline-manifest.json.tera      # renders pipeline.json (Agent 3)
  search-space-report.md.tera      # renders SEARCH_SPACE.md (Agent 3)
verify/
  reference_autoconfig.py          # pure-python TPOT2 selection reference (Agent 5)
  validate_artifacts.py            # structural validators (Agent 5)
  test_tpot2_autoconfig.py         # Chicago TDD-style checks (Agent 5)
```

## 4. Pipeline stage taxonomy (FROZEN) — bijection to the 9 wasm4pm categories

60 algorithms partition exactly into 9 `pi:category` values. Each becomes one
`tpot:PipelineStage`. (Counts: 4+20+6+10+4+8+5+2+1 = 60.)

| stageOrder | tpot:Stage individual | tpot:stageId | tpot:forCategory (exact pi:category string) | # algos |
|-----------|----------------------|--------------|---------------------------------------------|---------|
| 1 | `tpot:Stage_ingest`      | `ingest`      | `import_export`       | 4  |
| 2 | `tpot:Stage_discover`    | `discover`    | `discovery`           | 20 |
| 3 | `tpot:Stage_discover_oc` | `discover_oc` | `object_centric`      | 6  |
| 4 | `tpot:Stage_analyze`     | `analyze`     | `discovery_analytics` | 10 |
| 5 | `tpot:Stage_conform`     | `conform`     | `conformance`         | 4  |
| 6 | `tpot:Stage_learn`       | `learn`       | `ml_analytics`        | 8  |
| 7 | `tpot:Stage_predict`     | `predict`     | `prediction`          | 5  |
| 8 | `tpot:Stage_simulate`    | `simulate`    | `simulation`          | 2  |
| 9 | `tpot:Stage_orchestrate` | `orchestrate` | `agentic`             | 1  |

## 5. `tpot:` vocabulary (FROZEN predicates)

**Stages** (static, in tpot-search-space.ttl):
`tpot:PipelineStage` rdf:type; `tpot:stageId` (xsd:string); `tpot:stageOrder`
(xsd:integer); `tpot:stageLabel` (xsd:string); `tpot:forCategory`
(xsd:string, equals a `pi:category`).

**Operators** (DERIVED by inference/derive-operators.rq — NOT hand-written):
`tpot:Operator` rdf:type; `tpot:operatorId` (xsd:string, = algorithmId);
`tpot:wrapsAlgorithm` (IRI → pi: individual); `tpot:atStage` (IRI → tpot:Stage);
`tpot:opCategory` (xsd:string); `tpot:opSpeedTier` (xsd:integer, copied from pi:speedTier);
`tpot:opQualityTier` (xsd:integer, copied from pi:qualityTier);
`tpot:fitnessScore` (xsd:decimal); `tpot:wasmExport` (xsd:string).

**Fitness formula (FROZEN):**
`fitnessScore = qualityTier - (lambdaCost * speedTier)`, with `lambdaCost = 0.5`.
Compute in CONSTRUCT via `BIND((?q - (0.5 * ?s)) AS ?fitness)`.

**Pareto dominance (FROZEN):** operator A dominates B iff
`qualityTier_A >= qualityTier_B AND speedTier_A <= speedTier_B AND (qualityTier_A > qualityTier_B OR speedTier_A < speedTier_B)`.
Pareto-front = globally non-dominated operators. Per-stage **elite** (the chosen
pipeline) = the operator in that stage with max `fitnessScore`; tie-break: lower
`opSpeedTier`, then `operatorId` ascending (use `NOT EXISTS` for argmax).

**FitnessObjective** (static): `tpot:FitnessObjective` rdf:type;
`tpot:objectiveId`; `tpot:objectiveName`; `tpot:metric` ("qualityTier"|"speedTier");
`tpot:direction` ("maximize"|"minimize"); `tpot:weight` (xsd:decimal);
`tpot:objectiveOrder` (xsd:integer). Two objectives: quality (maximize, weight 1.0,
order 1), cost (minimize, weight 0.5, order 2).

**GeneticConfig** (static, single individual `tpot:Config_default`):
`tpot:populationSize` (xsd:integer, 100); `tpot:generations` (xsd:integer, 50);
`tpot:mutationRate` (xsd:decimal, 0.9); `tpot:crossoverRate` (xsd:decimal, 0.1);
`tpot:selectionStrategy` (xsd:string, "NSGA2"); `tpot:randomSeed` (xsd:integer, 42 — determinism);
`tpot:lambdaCost` (xsd:decimal, 0.5); `tpot:metaBreed` (IRI → compat: breed used as
GP meta-strategy, e.g. `compat:Breed_version_space` / `compat:Breed_ebl` / `compat:Breed_rl_symbolic`).

**Hyperparameter** (static, illustrative — author a meaningful subset, NOT all 60):
`tpot:Hyperparameter` rdf:type; `tpot:hpOperator` (xsd:string, = operatorId/algorithmId);
`tpot:paramName`; `tpot:paramType` ("int"|"float"|"choice"|"bool");
`tpot:paramDefault`; `tpot:paramMin`; `tpot:paramMax`; `tpot:paramChoices`
(comma-joined string); `tpot:paramOrder` (xsd:integer). Cover at least:
heuristic_miner (dependency_threshold), inductive_miner (noise_threshold),
genetic_algorithm (population_size, generations), ilp, ml_classify, detect_drift,
alignments. Reference real algorithm semantics from algorithms.ttl docs.

## 6. SPARQL SELECT projection variables (FROZEN — templates depend on these EXACT names)

snake_case, mirroring `extract-algorithms.rq`. Every SELECT query MUST end with
`ORDER BY` (ggen BUG-006 + strict-mode E0011/E0013) and MUST use explicit
projections (NEVER `SELECT *` — BUG-007).

- **extract-operators.rq** → one row per operator (all 60):
  `?stage_order ?stage_id ?operator_id ?algorithm_label ?op_category ?output_type ?input_format ?speed_tier ?quality_tier ?fitness_score ?wasm_export ?cli_alias ?is_elite`
  `ORDER BY ?stage_order ?fitness_score ?operator_id` (note: DESC fitness handled in template, or `ORDER BY ?stage_order DESC(?fitness_score) ?operator_id`)
- **extract-pipeline-stages.rq** → one row per stage (9 rows):
  `?stage_order ?stage_id ?stage_label ?for_category ?operator_count` ; `ORDER BY ?stage_order`
- **extract-pareto-pipeline.rq** → the chosen pipeline, one row per stage (9 rows, the elite):
  `?stage_order ?stage_id ?operator_id ?algorithm_label ?wasm_export ?input_format ?output_type ?speed_tier ?quality_tier ?fitness_score ?proof_query` ; `ORDER BY ?stage_order`
  (`?proof_query` = the matching semconv proof file name when one exists, else "")
- **extract-pareto-front.rq** → globally non-dominated operators:
  `?operator_id ?stage_id ?speed_tier ?quality_tier ?fitness_score` ; `ORDER BY DESC(?quality_tier) ?speed_tier ?operator_id`
- **extract-hyperparameters.rq** → one row per hyperparameter:
  `?operator_id ?param_name ?param_type ?param_default ?param_min ?param_max ?param_choices ?param_order` ; `ORDER BY ?operator_id ?param_order`
- **extract-fitness-objectives.rq** → one row per objective (2 rows):
  `?objective_order ?objective_id ?objective_name ?metric ?direction ?weight` ; `ORDER BY ?objective_order`

`?proof_query` mapping (stage → semconv proof file): ingest→`ocel-load.rq`,
discover→`discover-dfg.rq`, conform→`conformance-check.rq`, learn→`ml-classify.rq`,
predict→`predict-activity.rq` (drift handled by detect-drift.rq under predict too);
analyze/discover_oc/simulate/orchestrate → "" (no dedicated proof).

## 7. Tera template rules (FROZEN — match this repo's real engine behavior)

- Batch templates iterate `{% for row in sparql_results %}` and access fields as
  `row["field_name"]` (see `examples/config-generator/templates/app-config.tera`).
- Per-row fan-out templates use BARE `{{ field_name }}` (NOT `{{ row.field }}` — BUG-010).
  This project uses BATCH templates (one output file aggregating all rows), so use
  `sparql_results` + `row["..."]`.
- NO YAML frontmatter (`---`) anywhere in a template — it renders literally (BUG-002).
- Use `| default(value="")` defensively for OPTIONAL columns.

## 8. ggen.toml DRIVER rules (FROZEN — Agent 4)

- Extra ontologies load via `[ontology]` `imports = [...]` — NEVER `additional` (BUG-001).
- `[ontology] source` = the primary ttl; `imports` = the rest.
- Inference rules (`[inference] rules` / `[[generation]]`?) — put CONSTRUCTs that
  derive operators + dominance under `[inference]` with `{ name, file }` or
  `{ name, construct = "..." }`. They MUST add NEW triples (no identity CONSTRUCT — BUG-009).
- Each `[[generation.rules]]`: `name`, `query = { file = "queries/..." }`,
  `template = { file = "templates/..." }`, `output_file = "..."`, `mode = "Overwrite"`.
- Use `{ file = "queries/x.rq" }` direct relative paths — NOT pack indirection (BUG-008/011).
- `output_file` MUST stay inside the project root (no `../` — YIELD-001).
- Add `[ontology.prefixes]` declaring pi, compat, tpot.

## 9. Environment constraints (IMPORTANT — no fabrication)

This container has **NO `cargo`, NO `ggen` binary, NO `rdflib`**. Python 3.11
with built-in `tomllib` IS available. Therefore:
- Do NOT claim `ggen sync` was executed or fabricate a `.ggen/receipts/*.json`.
- Verification (Agent 5) is **structural** (tomllib parse, TTL/SPARQL well-formedness,
  coverage, ORDER BY presence, no SELECT *, no frontmatter) PLUS a **pure-Python
  reference implementation** that reads the ontology and computes the SAME elite
  pipeline the SPARQL would, writing a real output artifact as observable evidence.

## 10. Coverage requirement (the deliverable's "use ALL capabilities")

- All **60** `pi:ProcessIntelligenceAlgorithm` individuals MUST become operators
  (derived) and appear in `extract-operators.rq` output.
- All **9** categories MUST map to the 9 stages.
- At least one **compat: breed** MUST be referenced (GP meta-strategy / orchestrate).
- All **6** semconv SPARQL proofs MUST be referenced by the generated pipeline config.
- The generated `ggen.toml` MUST be valid TOML and itself schema-valid.
