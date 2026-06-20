# verify/ — Independent Verification (Agent 5)

This directory proves the TPOT2 auto-configuration design is **correct and
deterministic** using a real, observable computation — not a fabricated receipt.

## Honest limitation (read first)

This container has **NO `cargo`, NO `ggen` binary, NO `rdflib`** (CONTRACT.md §9).
Therefore **`ggen sync` was NOT executed** and **no `.ggen/receipts/*.json` is
produced or claimed.** What these scripts *do* prove, independently of ggen:

1. The selection logic the SPARQL queries encode (per-stage argmax-fitness elite,
   global Pareto front) computes a real, deterministic 9-stage pipeline from the
   real `algorithms.ttl`.
2. The artifacts the templates/driver are meant to emit are structurally valid
   (the generated `ggen.toml` parses with `tomllib`; queries have `ORDER BY` and
   no `SELECT *`; templates have no frontmatter and use the batch idiom; coverage
   of 60 algorithms / 9 stages / breeds / 6 proofs).

These are **structural + reference-computation** proofs (Chicago TDD: real file
reads, real compute, real files on disk as externalizable evidence). They do NOT
replace an end-to-end `ggen sync` run; they make the design falsifiable without one.

## What each script proves

### `reference_autoconfig.py` — pure-Python TPOT2 reference (observable evidence)
Reads `algorithms.ttl` with stdlib only (regex/line scanning, **no rdflib**),
defaulting to the project copy `ontology/algorithms.ttl` and falling back to the
canonical `ontology_catalogue/wasm4pm/ggen/ontology/algorithms.ttl` (so it runs
even before the project copy lands). It then:

- maps every `pi:category` to the frozen 9-stage taxonomy (CONTRACT.md §4) and
  asserts all 9 categories are present and total algorithms == 60;
- computes `fitness = quality_tier - 0.5 * speed_tier` (FROZEN, §5);
- selects the per-stage **elite** = argmax fitness, tie-break lower `speed_tier`
  then `operator_id` ascending (matches the SPARQL `NOT EXISTS` strict total order);
- computes the **global Pareto front** (non-dominated under: A dominates B iff
  `q_A>=q_B and s_A<=s_B` and at least one strict);
- writes real artifacts to `out/` (deterministic, seed 42, sorted iteration).

It is **importable** (used as a real collaborator by the test module) and runs as
a CLI: `python3 verify/reference_autoconfig.py [path/to/algorithms.ttl] [--out DIR]`.

Output artifacts in `verify/out/`:
| file | content |
|------|---------|
| `reference_pipeline.json` | the 9 elite stages, ordered, with operator_id / wasm_export / io / tiers / fitness + `random_seed: 42` and `proof_query` per stage (CONTRACT.md §6) |
| `reference_ggen.toml` | a valid, `tomllib`-loadable ggen.toml emitting the pipeline (`imports` not `additional`; no `../`; pi/compat/tpot prefixes) |
| `reference_pareto_front.txt` | the globally non-dominated operators, ordered DESC quality, ASC speed, ASC id |

### `validate_artifacts.py` — structural validators (stdlib only)
Given the project dir (default: the parent dir), reports `PASS` / `FAIL` /
`MISSING` for each check and exits non-zero if any `FAIL`. It tolerates
not-yet-created files (reports `MISSING`, never crashes) so it runs now and again
after integration. Checks:

- `ggen.toml` loads via `tomllib`; has `[project] [ontology] [generation]`;
  `[ontology]` uses `imports` not `additional` (BUG-001); no `../` in any
  `output_file` (GGEN-YIELD-001).
- every `queries/*.rq` SELECT has `ORDER BY` (BUG-006) and no `SELECT *` (BUG-007),
  comment-aware.
- every `templates/*.tera` has no `---` frontmatter (BUG-002) and uses
  `sparql_results` (batch idiom, CONTRACT.md §7).
- coverage (CONTRACT.md §10): 60 `a pi:ProcessIntelligenceAlgorithm`; exactly 9
  `tpot:PipelineStage`; a `compat:Breed_` **used** outside the registry (e.g.
  `tpot:metaBreed`); all 6 semconv proof filenames referenced by a **generation
  artifact** (queries/templates/ttl/ggen.toml — docs and `verify/` are excluded
  as evidence so the check cannot be satisfied by self-reference).

Writes a machine-readable summary to `verify/out/validation_report.json`.

### `test_tpot2_autoconfig.py` — Chicago-TDD checks (no mocks)
Each test reads **real files** and/or runs the **real** reference impl, then
asserts **observable state**. No mocks, no test doubles, no fabricated fixtures.
Tests:

- `test_registry_has_60_algorithms` — real `.ttl` parse, 60 unique individuals.
- `test_nine_stage_bijection_covers_all_categories` — 9 categories, counts sum to 60.
- `test_fitness_formula_matches_contract` — hand-computed exact values: `ilp` 70.0,
  `alignments` 82.5, `dfg` 29.5.
- `test_elite_pipeline_is_deterministic` — two runs into ephemeral dirs produce
  byte-identical `reference_pipeline.json` and `reference_ggen.toml`.
- `test_elite_pipeline_matches_argmax_fitness` — every chosen elite is the true
  argmax under the frozen tie-break (corroborates the SPARQL).
- `test_reference_ggen_toml_is_valid_toml` — `tomllib` loads the file off disk and
  it obeys the §8 driver rules.
- `test_pareto_front_nondominated` — no front member dominates another; the max-
  quality operator (`alignments`) is on the front.

## How to run

```bash
cd examples/tpot2-wasm4pm-autoconfig

# 1. Reference computation (writes verify/out/ artifacts + prints the pipeline)
python3 verify/reference_autoconfig.py
#    or against the canonical registry explicitly:
python3 verify/reference_autoconfig.py \
  ../../ontology_catalogue/wasm4pm/ggen/ontology/algorithms.ttl

# 2. Structural validators (exit non-zero on any FAIL)
python3 verify/validate_artifacts.py

# 3. Chicago-TDD checks (plain python3 — pytest not required)
python3 verify/test_tpot2_autoconfig.py
#    pytest also works if installed:
python3 -m pytest -q verify/test_tpot2_autoconfig.py
```

`verify/out/` is regenerated output; it is safe to delete and recompute.
