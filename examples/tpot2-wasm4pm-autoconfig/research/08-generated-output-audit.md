# Dossier 08 — Generated-Output Audit: Path Fix + Projection-Key Correctness

**Agent:** 08 / 10 · **Date:** 2026-06-20 · **Scope:** (A) design the fix for the
generated-config path-resolution bug found by agent 01; (B) audit every other
generated output for projection-key correctness (template column reads vs. the
driving query's actual SELECT projection).

> Method note: **source-trace only**, per CONTRACT.md §9 (no `cargo`/`ggen` in this
> container). Every claim is cited to a real `.rs` / `.rq` / `.tera` / `.toml`
> line. Nothing was executed. Where I infer runtime behavior I cite the engine
> source that produces it.

---

## TL;DR

| Finding | Verdict |
|---------|---------|
| **(A) Path fix for generated `ggen.toml`** | Recommend **option (iii): relabel as a non-runnable rendered specification** with a clear header — because even with paths fixed (option i is *mechanically* safe), the generated config still cannot reproduce the pipeline (it omits `[inference]` + the algorithms/breeds `imports`, so `?op a tpot:Operator` matches nothing). Option (i) is the right *secondary* fix if/when the generated config is made self-contained. Exact diff for both below. |
| **`../` is safe for input paths** | **CONFIRMED at source.** The E0006 `../` guard (`pipeline.rs:1558-1566`) fires **only** on OUTPUT paths (`validate_generated_output`, called at `pipeline.rs:1066,1218`). Input `query` / `template` / `ontology.source` / `imports` paths resolve via a plain `base_path.join(file)` + `.exists()` with **no traversal guard and no canonicalization** (`validation.rs:130-164`, `dependency_validation.rs:96-142`, `pipeline.rs:412-434`). So `../templates/...` resolves fine. This corroborates agent 01 §6 and extends it: the guard agent 01 cited is output-only. |
| **(B) Projection audit — DRIVER's 6 rules** | **2 real MISMATCHES** (RULE 5 + RULE 6, the reused-template rules), both **non-failing** (every divergent read is wrapped in `\| default`, so Tera emits blanks, not an error) → **silent wrong output**, not a crash. RULES 1–4 are **clean**. The 6 per-stage/drift templates emitted *inside* the generated config are clean against their nominal driver but are mooted by the (A) defect. Tables below. |

---

## PART A — The generated-config path fix

### A.1 What the generated config actually emits (re-derived from the template)

`templates/generated-ggen-toml.tera` renders, per elite row with a non-empty
`proof_query`, a `[[generation.rules]]` whose **input paths are root-relative
literals**:

- `query = { file = "queries/extract-pareto-pipeline.rq" }` (`generated-ggen-toml.tera:59,73`)
- `template = { file = "templates/{{ stage_id }}.tera" }` (`generated-ggen-toml.tera:60`) → `templates/ingest.tera`, `templates/discover.tera`, `templates/conform.tera`, `templates/learn.tera`, `templates/predict.tera`
- `template = { file = "templates/drift.tera" }` (`generated-ggen-toml.tera:74`)
- and the ontology block: `source = "ontology/tpot-search-space.ttl"` (`generated-ggen-toml.tera:33`)

Only **5** per-stage rules are emitted, not 9: the `{% if proof_query != "" %}`
gate (`generated-ggen-toml.tera:52`) drops `analyze`, `discover_oc`, `simulate`,
`orchestrate` (their `?proof_query` is `""` per `extract-pareto-pipeline.rq:79-84`).
So the emitted config wires: ingest, discover, conform, learn, predict + drift.

### A.2 Why it is not runnable in place (confirming + sharpening agent 01 §6)

When `ggen sync` runs against `generated/ggen.toml`, `base_path` = the manifest's
parent = `generated/` (executor sets base to manifest parent; agent 01 cites
`executor.rs:357-362`). Then:

- `ManifestValidator::validate_generation_rules` resolves each template as
  `base_path.join(file)` (`validation.rs:156`) = `generated/templates/ingest.tera`
  and each query as `generated/queries/extract-pareto-pipeline.rq`
  (`validation.rs:131`). It checks `.exists()` (`validation.rs:132,157`).
- Those files live at the **project root** (`templates/`, `queries/`), not under
  `generated/`. And `generated/` does not even exist yet (Glob of
  `examples/tpot2-wasm4pm-autoconfig/**` shows no `generated/` tree).
- → `Template file not found for rule '...': .../generated/templates/ingest.tera`
  (`validation.rs:158-162`) → surfaced as `error[E0001]` by the executor. Hard stop
  before generation. `DependencyValidator` independently flags the same
  (`dependency_validation.rs:96-142`).

**This is a contract-drift-class defect** (`.claude/rules/coding-agent-mistakes.md`
§1.5): a generated proof object references inputs that are not where it says.

### A.3 NEW finding — a *second*, deeper defect: the generated config can't re-derive operators

Even if every path were fixed, the generated `ggen.toml` **still cannot reproduce
the pipeline**, because:

- Its rules' SELECT is `queries/extract-pareto-pipeline.rq`, which matches
  `?op a tpot:Operator ; tpot:operatorId ... ; tpot:atStage ...`
  (`extract-pareto-pipeline.rq:45-52`).
- `tpot:Operator` individuals are **not in any TTL** — they are DERIVED by the
  DRIVER's `[inference]` CONSTRUCT `derive-operators` (DRIVER `ggen.toml:74-107`).
- The generated config has **no `[inference]` block at all** (the template emits
  only `[project]`, `[ontology]`, `[generation]`, the per-stage rules, the drift
  rule, `[sync]`, `[rdf]` — see `generated-ggen-toml.tera:21-88`) and its
  `[ontology]` has **no `imports`** (no `algorithms.ttl`/`breeds.ttl`), only
  `source = "ontology/tpot-search-space.ttl"` (`generated-ggen-toml.tera:28-33`).
- → At sync time the graph contains the static `tpot:` vocab only; `?op a
  tpot:Operator` matches **0 rows** → every per-stage rule's SELECT returns empty →
  `validate_generated_output` raises `error[E0004]: Generated content is empty`
  (`pipeline.rs:1538-1543`) for any stage template that produces no output (the
  per-stage templates emit nothing when their `{% if row["stage_id"] == "..." %}`
  never matches — `ingest.tera:15`, etc.).

So the generated config is **doubly non-runnable**: (1) wrong input paths, and (2)
missing the inference + imports that create the operators its SELECT queries. Fix
(1) alone does not make it run. This strengthens the case for **option (iii)** over
option (i): a one-line `../` path prefix produces a config that *passes validation
but then fails generation with E0004* — arguably worse (it looks runnable, then
dies deeper in the pipeline). To make option (i)/(ii) actually runnable you must
ALSO emit `[inference]` + `imports`, which is a much larger template change.

### A.4 `../` safety verdict (the explicit sub-question)

**Is `../` safe for ontology source / query / template inputs?** **Yes — confirmed
at source, with no guard and no canonicalization in the way.**

- The only `../` rejection in the whole generation path is `validate_generated_output`
  Check 3 (`pipeline.rs:1558-1566`, error `E0006`). Its **only two call sites**
  (`pipeline.rs:1066`, `1218`) both pass the rendered **output** path. Grep for
  `validate_generated_output` returns exactly those two non-test call sites.
- Input paths are never passed through it. They go through:
  - queries: `base_path.join(file)` + `.exists()` (`validation.rs:131-132`;
    `dependency_validation.rs:123-124`)
  - templates: `base_path.join(file)` + `.exists()` (`validation.rs:156-157`;
    `dependency_validation.rs:99-100`)
  - ontology `source`: `resolved_sources(&self.base_path)` (a plain join) then
    `read_to_string` (`pipeline.rs:412-413`)
  - ontology `imports`: `self.base_path.join(import)` then `read_to_string`
    (`pipeline.rs:424-426`)
- **No `canonicalize` call exists in `pipeline.rs`** (grep: zero hits) and none in
  the validators on these paths, so `generated/../templates/ingest.tera` is handed
  to the OS as-is; `Path::exists()` resolves the `..` and returns true when
  `<project>/templates/ingest.tera` exists. Input `../` therefore **works and is not
  blocked**.

This is the precise extension of agent 01's finding (agent 01 §4: "the only escape
guard (`../` string check)… is a substring check on the rendered path"). Agent 01
correctly scoped that to `output_file`; I confirm it does **not** touch input paths,
so option (i) is mechanically valid for inputs — it just isn't *sufficient* (see A.3).

### A.5 RECOMMENDATION (pick ONE) — option (iii), relabel as a rendered specification

Given A.3 (paths alone don't make it run; it also lacks inference+imports),
**option (iii)** is the honest, minimal, drift-removing fix for the *demo*: stamp
the generated `ggen.toml` with a header that says it is a **rendered specification
of the chosen pipeline, not a runnable manifest**, and that the runnable driver is
the project-root `ggen.toml`. This removes the false "DO NOT EDIT — re-run ggen
sync" runnability claim that currently sits at `generated-ggen-toml.tera:4-5`.

> Why not (i): it makes the config *pass validation* then fail at generation with
> E0004 (A.3) — a worse failure mode (looks fixed, isn't). Why not (ii) as the
> primary: a self-contained bundle requires emitting `[inference]`, `[ontology]
> imports`, copying 6 templates + the SELECT + 4 TTLs into `generated/` — a large
> change for a demonstration artifact. Keep (ii) as the documented "to make it truly
> runnable" path; ship (iii) now.

#### Exact edit to `templates/generated-ggen-toml.tera`

Replace the header block (current lines 1–6) so the artifact is self-describing as a
specification, not a runnable manifest:

```diff
--- a/templates/generated-ggen-toml.tera
+++ b/templates/generated-ggen-toml.tera
@@ -1,6 +1,16 @@
-# ===========================================================================
-# AUTO-CONFIGURED ggen.toml — wasm4pm Pareto-optimal process-mining pipeline
-# ===========================================================================
-# Generated by ggen sync from the TPOT2 auto-configuration generator.
-# DO NOT EDIT BY HAND — re-run `ggen sync` to regenerate.
-#
+# ===========================================================================
+# RENDERED PIPELINE SPECIFICATION (NOT a runnable manifest)
+# wasm4pm Pareto-optimal process-mining pipeline
+# ===========================================================================
+# Generated by `ggen sync` from the TPOT2 auto-configuration generator, as a
+# human-readable RECORD of the auto-selected pipeline. DO NOT EDIT BY HAND —
+# re-run `ggen sync` (against the project-root ggen.toml) to regenerate.
+#
+# This file is a SPECIFICATION, not a re-runnable driver:
+#   * its query/template/ontology paths are PROJECT-ROOT-relative, but its own
+#     base_path would be `generated/`, so they will not resolve in place; and
+#   * it intentionally omits the [inference] rules and [ontology] imports that
+#     DERIVE the tpot:Operator search space, so its SELECTs would match nothing.
+# The runnable driver is ../ggen.toml (the generator). To make THIS file
+# runnable you would also need to emit [inference] + imports and copy the
+# templates/queries beside it (see research/01 §6 option 2).
+#
```

That is the whole fix for option (iii): no behavior change to the DRIVER, the
generated artifact stops lying about being runnable, drift removed.

#### (If/when you choose option (i) instead — the exact path-prefix diff)

For completeness, the *mechanically-valid* path fix (only correct AFTER you also add
inference+imports per option ii; otherwise it converts E0001→E0004). Prefix every
input path with `../`:

```diff
@@ generated-ggen-toml.tera:33
-source = "ontology/tpot-search-space.ttl"
+source = "../ontology/tpot-search-space.ttl"
@@ generated-ggen-toml.tera:59
-query = { file = "queries/extract-pareto-pipeline.rq" }
+query = { file = "../queries/extract-pareto-pipeline.rq" }
@@ generated-ggen-toml.tera:60
-template = { file = "templates/{{ stage_id }}.tera" }
+template = { file = "../templates/{{ stage_id }}.tera" }
@@ generated-ggen-toml.tera:73
-query = { file = "queries/extract-pareto-pipeline.rq" }
+query = { file = "../queries/extract-pareto-pipeline.rq" }
@@ generated-ggen-toml.tera:74
-template = { file = "templates/drift.tera" }
+template = { file = "../templates/drift.tera" }
```

`output_file` values (`{{ stage_id }}-result.ttl`, `drift-monitor.ttl`) stay
**unprefixed** — they must remain inside `generated/` (E0006 would reject a `../`
there). The asymmetry is the whole point: `../` is legal on inputs, illegal on
outputs.

---

## PART B — Per-template projection-key audit

For each DRIVER `[[generation.rules]]` I list the columns the template reads
(`row["..."]` and the bare `{{ ... }}` / `{% set %}` forms) and compare to the
driving query's actual `SELECT` projection. A **MISMATCH** = a template reads a
column the query does not project → at runtime the engine builds rows keyed only by
projected vars (`pipeline.rs:843-851`), so the missing key is absent → Tera's
`row["missing"]` is undefined → if wrapped in `| default(value="...")` it renders
the default (silent blank); if NOT wrapped, Tera errors and generation fails
(`E0004`/template error).

### Ground-truth projections (verbatim from the `.rq` SELECT lines)

| Query | Projected vars (bare, snake_case) | Source line |
|-------|-----------------------------------|-------------|
| `extract-pareto-pipeline.rq` | `stage_order, stage_id, operator_id, algorithm_label, wasm_export, input_format, output_type, speed_tier, quality_tier, fitness_score, proof_query` | `:40-42` |
| `extract-operators.rq` | `stage_order, stage_id, operator_id, algorithm_label, op_category, output_type, input_format, speed_tier, quality_tier, fitness_score, wasm_export, cli_alias, is_elite` | `:32-34` |
| `extract-pipeline-stages.rq` | `stage_order, stage_id, stage_label, for_category, operator_count` | `:20` |
| `extract-fitness-objectives.rq` | `objective_order, objective_id, objective_name, metric, direction, weight` | `:20` |

(Each matches CONTRACT.md §6 exactly. `extract-pareto-pipeline.rq` does **not**
project `is_elite` — all 9 rows are already elite — and does **not** project
`op_category`/`cli_alias`. `extract-operators.rq` does **not** project `proof_query`.)

### RULE-by-RULE audit (DRIVER `ggen.toml`)

| Rule (ggen.toml) | Template | Driving query | Keys the template consumes | Provided by query? | MISMATCH? |
|---|---|---|---|---|---|
| **RULE 1** `generate-ggen-toml` (`:158-163`) | `generated-ggen-toml.tera` | `extract-pareto-pipeline.rq` | `stage_order, stage_id, operator_id, wasm_export, speed_tier, quality_tier, fitness_score, proof_query` (`:13-16,43-51`) | all ✓ | **No** — clean |
| **RULE 2** `generate-tpot-config` (`:170-175`) | `tpot-config-dict.py.tera` | `extract-operators.rq` | `stage_id, operator_id, wasm_export, speed_tier, quality_tier, fitness_score, input_format, output_type, cli_alias, is_elite` (`:42-50,70-73`) | all ✓ (`cli_alias` `:34`, `is_elite` `:34`) | **No** — clean |
| **RULE 3** `generate-pipeline-manifest` (`:182-187`) | `pipeline-manifest.json.tera` | `extract-pareto-pipeline.rq` | `stage_order, stage_id, operator_id, wasm_export, input_format, output_type, speed_tier, quality_tier, fitness_score, proof_query` (`:24-33`) | all ✓ | **No** — clean |
| **RULE 4** `generate-search-space-report` (`:194-199`) | `search-space-report.md.tera` | `extract-operators.rq` | `stage_id, operator_id, wasm_export, speed_tier, quality_tier, fitness_score, is_elite` (`:18-24`) | all ✓ | **No** — clean |
| **RULE 5** `generate-stage-plan` (`:210-215`) | `search-space-report.md.tera` (REUSED) | `extract-pipeline-stages.rq` | `stage_id`✓, `operator_id`✗, `wasm_export`✗, `speed_tier`✗, `quality_tier`✗, `fitness_score`✗, `is_elite`✗ (`:18-24`) | only `stage_id`; **6 missing** | **YES** — see B.1 |
| **RULE 6** `generate-fitness-objectives` (`:224-229`) | `pipeline-manifest.json.tera` (REUSED) | `extract-fitness-objectives.rq` | `stage_order`✗, `stage_id`✗, `operator_id`✗, `wasm_export`✗, `input_format`✗, `output_type`✗, `speed_tier`✗, `quality_tier`✗, `fitness_score`✗, `proof_query`✗ (`:24-33`) | **none** of the 10 | **YES** — see B.2 |

### B.1 RULE 5 mismatch (stage plan via operator-report template) — SILENT WRONG OUTPUT

`search-space-report.md.tera` is built for `extract-operators.rq` rows. RULE 5 feeds
it `extract-pipeline-stages.rq` rows (`stage_order, stage_id, stage_label,
for_category, operator_count`). The template's per-row body
(`search-space-report.md.tera:18-25`) reads `operator_id, wasm_export, speed_tier,
quality_tier, fitness_score, is_elite` — **none of which the stage query projects**.

- **Does it crash?** **No.** Every read is `row["..."] | default(value="...")`
  (`:18-24`), and the `is_elite` test compares to the default `"false"` (`:24-25`).
  Undefined keys → defaults → table row renders as
  `` | `<stage_id>` | `` (empty op) ` | ` | ` | ` | no |``. Output is non-empty (the
  header table + 9 mostly-blank rows + the trailer line `:30-31`), so
  `validate_generated_output` passes (non-empty, `pipeline.rs:1538`).
- **Result:** `generated/STAGE_PLAN.md` is a table whose **operator/wasm/speed/
  quality/fitness/elite columns are all blank**, and whose only populated column is
  `stage_id`. The genuinely useful stage-plan columns the query *does* provide —
  `stage_label`, `for_category`, `operator_count` — are **never read**, so they are
  dropped. The artifact is misleading (it looks like a per-operator report with
  missing data), not a stage plan. **Silent wrong output. Confirmed.** (Agent 01 §5
  ⚠️Q1 reached the same conclusion; this dossier pins the exact 6 missing keys and
  the 3 dropped useful keys.)

**Fix (recommended):** give RULE 5 its own `stage-plan.md.tera` that reads
`stage_order, stage_id, stage_label, for_category, operator_count` — the columns the
query actually projects. Minimal new template:

```tera
# wasm4pm Pipeline Stage Plan
> Auto-generated from `queries/extract-pipeline-stages.rq`. Do not edit by hand.

| # | stage_id | label | category | # operators |
|---|----------|-------|----------|-------------|
{%- for row in sparql_results %}
| {{ row["stage_order"] | default(value="") }} | `{{ row["stage_id"] | default(value="") }}` | {{ row["stage_label"] | default(value="") }} | `{{ row["for_category"] | default(value="") }}` | {{ row["operator_count"] | default(value="0") }} |
{%- endfor %}
```
(NO `---` frontmatter — BUG-002; uses `sparql_results` + `row["..."]` — §7.) Then
point RULE 5 at `templates/stage-plan.md.tera`.

### B.2 RULE 6 mismatch (objectives via pipeline-manifest template) — DOUBLY WRONG

`pipeline-manifest.json.tera` is built for `extract-pareto-pipeline.rq` rows. RULE 6
feeds it `extract-fitness-objectives.rq` rows (`objective_order, objective_id,
objective_name, metric, direction, weight`). **Not one** of the 6 objective columns
is read by the template, and **all 10** stage columns the template reads
(`:24-33`) are absent from the objective rows.

- **Does it crash?** **No.** All 10 reads are `| default` (`:24-33`); numeric ones
  default to `"0"`, strings to `""`. The template's `objectives` array (`:6-21`) is
  **hardcoded JSON literal text** — it does not come from the rows at all.
- **Result:** `generated/objectives.json` is valid JSON containing (a) the
  **hardcoded** 2-objective block (correct by luck, because it's a literal in the
  template, not query-driven) and (b) a `stages` array of **2 bogus stage objects**
  (one per objective row) with `stage_order:0, stage_id:"", operator_id:"", ... ,
  proof_query:""`. So the file's *useful* content (objectives) is **not derived from
  the objectives query** — the query rows only manufacture 2 empty `stages` entries.
  The objective columns (`metric`, `direction`, `weight`, `objective_id`, …) are
  **silently dropped**. **Doubly wrong:** the right data is ignored; the wrong array
  is populated with blanks. **Confirmed** (agent 01 §5 ⚠️Q2 noted the shape; this
  dossier confirms zero of the 6 objective keys are consumed and the objectives
  block is a hardcoded literal, so the rule's query is effectively decorative).

**Fix (recommended):** give RULE 6 its own `objectives.json.tera` that reads the 6
projected columns. Minimal new template:

```tera
{
  "selection_strategy": "NSGA2",
  "random_seed": 42,
  "objectives": [
{%- for row in sparql_results %}
    {
      "objective_order": {{ row["objective_order"] | default(value="0") }},
      "objective_id": "{{ row["objective_id"] | default(value="") }}",
      "name": "{{ row["objective_name"] | default(value="") }}",
      "metric": "{{ row["metric"] | default(value="") }}",
      "direction": "{{ row["direction"] | default(value="") }}",
      "weight": {{ row["weight"] | default(value="0") }}
    }{% if not loop.last %},{% endif %}
{%- endfor %}
  ]
}
```
Then point RULE 6 at `templates/objectives.json.tera`. This makes the objectives
genuinely *query-derived* (deepens authority: the NSGA-II config comes from the
ontology via SPARQL, not a Tera literal).

### B.3 The 6 templates emitted *inside* the generated config (per-stage + drift)

These are not DRIVER rules; they are the templates the generated config wires. Their
nominal driver is `extract-pareto-pipeline.rq` (`generated-ggen-toml.tera:59,73`).
For completeness, their projection correctness **against that query**:

| Template | Keys consumed | In `extract-pareto-pipeline.rq`? | MISMATCH? |
|---|---|---|---|
| `ingest.tera` | `stage_id, stage_order, operator_id, wasm_export, input_format, output_type, speed_tier, quality_tier, fitness_score, proof_query` (`:15-27`) | all ✓ | **No** |
| `discover.tera` | same set (`:15-27`) | all ✓ | **No** |
| `conform.tera` | same set (`:15-27`) | all ✓ | **No** |
| `learn.tera` | same set (`:15-27`) | all ✓ | **No** |
| `predict.tera` | same set (`:15-27`) | all ✓ | **No** |
| `drift.tera` | `stage_id, operator_id` (`:20-23`); everything else is literal text | both ✓ | **No** |

So the per-stage/drift templates are **projection-clean** against
`extract-pareto-pipeline.rq`. Their problem is **not** key mismatch — it is the
PART-A defects: (1) the generated config's paths don't resolve, and (2) without
`[inference]`+`imports` the SELECT returns 0 rows so each `{% if row["stage_id"] ==
"ingest" %}` (`ingest.tera:15`) never fires → empty output → `E0004`. Fixing keys
here is unnecessary; fixing the generated config (A) is what matters.

> One latent caveat in the per-stage templates worth recording: they emit
> `tpot:eliteOperator pi:Algo_{{ row["operator_id"] }}` (`ingest.tera:19`), i.e.
> they assume the pi: individual IRI is `pi:Algo_<operatorId>`. That is an
> assumption about `algorithms.ttl`'s individual-naming, not a projection issue —
> out of this dossier's scope (agent 01/others cover ontology shape), flagged only
> so it isn't missed.

---

## PART C — Consolidated fix checklist

**(A) Generated-config drift — pick ONE, recommend the first:**
- [ ] **A-iii (recommended, ship now):** relabel `generated/ggen.toml` as a rendered
      *specification* via the header diff in §A.5. Removes the false runnability
      claim; zero risk; no DRIVER change.
- [ ] **A-ii (to make it genuinely runnable, larger):** emit `[inference]` (the two
      CONSTRUCTs) + `[ontology] imports` into the generated config AND copy the 6
      templates + `extract-pareto-pipeline.rq` + the 4 TTLs into `generated/`, then
      keep input paths root-local (no `../`). Removes both A.2 and A.3 defects.
- [ ] **A-i (mechanically valid but insufficient alone):** prefix inputs with `../`
      (diff in §A.5). **Only correct if combined with A-ii's inference+imports**;
      on its own it converts E0001 (path) into E0004 (empty SELECT) — do not ship
      standalone.

**(B) Projection mismatches in the DRIVER (silent wrong output, non-failing):**
- [ ] **B-5:** RULE 5 → new `templates/stage-plan.md.tera` reading
      `stage_order, stage_id, stage_label, for_category, operator_count` (§B.1).
      Repoint `ggen.toml:213`.
- [ ] **B-6:** RULE 6 → new `templates/objectives.json.tera` reading
      `objective_order, objective_id, objective_name, metric, direction, weight`
      (§B.2). Repoint `ggen.toml:227`.

**Clean (no action):** RULES 1–4 (projection-correct); the 5 per-stage templates +
`drift.tera` (projection-correct against `extract-pareto-pipeline.rq`; their issues
are the PART-A generated-config defects, not keys).

---

## Evidence index (every cited file:line)

- Driver wiring: `ggen.toml:158-163` (R1), `170-175` (R2), `182-187` (R3),
  `194-199` (R4), `210-215` (R5, reused template `:213`), `224-229` (R6, reused
  template `:227`).
- Query projections: `extract-pareto-pipeline.rq:40-42`; `extract-operators.rq:32-34`
  (`cli_alias`/`is_elite` `:34`); `extract-pipeline-stages.rq:20`;
  `extract-fitness-objectives.rq:20`.
- Templates: `generated-ggen-toml.tera:1-6` (header), `28-33` (no imports/inference),
  `52` (proof_query gate), `59-60,73-74` (input paths); `tpot-config-dict.py.tera:42-50,70-73`;
  `pipeline-manifest.json.tera:6-21` (hardcoded objectives), `24-33`;
  `search-space-report.md.tera:18-25`; `ingest.tera:15-27`,
  `discover/conform/learn/predict.tera:15-27`; `drift.tera:20-23`.
- Engine path resolution (input, no guard): `validation.rs:130-164`
  (query `:131-132`, template `:156-157`); `dependency_validation.rs:96-142`
  (template `:99-100`, query `:123-124`); `pipeline.rs:412-413` (source),
  `424-426` (imports).
- Engine `../` guard (output only): `pipeline.rs:1536-1568` (`E0006` at `:1558-1566`),
  callers `pipeline.rs:1066,1218`; empty-output guard `E0004` `pipeline.rs:1538-1543`.
- Row build / `sparql_results` shape: `pipeline.rs:843-851`, `996-1009` (via agent 01 §2).
- base_path = manifest parent: `executor.rs:357-362` (via agent 01 §0/§6).
- Sibling corroboration: `research/01-ggen-sync-executability.md` §4 (output `../`
  guard), §5 ⚠️Q1/Q2 (RULE 5/6 reuse), §6 (generated-config path drift).
