# Dossier 01 — Is `ggen sync` Actually Able to Run Our Project?

**Agent:** 01 / 10 · **Date:** 2026-06-20 · **Scope:** trace the real `ggen sync`
execution path in source and decide, with file:line evidence, whether (A) our
DRIVER `ggen.toml` and (B) the GENERATED `ggen.toml` it emits are runnable.

> Method note: this dossier is **source-trace only**. Per CONTRACT.md §9 this
> container has no `cargo`/`ggen` binary, so nothing here was executed. Every
> claim is cited to a real `.rs` / `.rq` / `.tera` / `.ttl` line. Where I could
> not verify a thing from source, I say so.

---

## VERDICT (two artifacts, judged separately)

| Artifact | Verdict | One-line reason |
|----------|---------|-----------------|
| **DRIVER `ggen.toml`** (the generator we ship) | **RUNNABLE** | Every gate the real `SyncExecutor` runs is satisfied; SELECT-routing, `sparql_results` shape, inference ordering, and in-root output all check out against source. |
| **GENERATED `generated/ggen.toml`** (the emitted artifact) | **RUNNABLE-WITH-FIX** | It is valid TOML and schema-valid, but its `template = { file = "templates/<stage>.tera" }` / `query = { file = "queries/..." }` paths resolve **relative to `generated/`**, where those files do not exist → it hard-fails `ManifestValidator`/`DependencyValidator` if you `cd generated && ggen sync`. Fix = emit absolute-from-root paths (`../templates/...`, blocked by `..` guard) → so instead emit it at project root, or have it copy/point templates into `generated/`. Details + minimal fix below. |

The DRIVER is the thing the task asks about first ("does `ggen sync` actually run
our project?") — **yes, it is expected to succeed.** The generated config is a
*demonstration artifact*; it parses and is schema-valid, but is not itself
re-runnable in place without the one-line path fix in §6.

---

## 0. The real execution path (who calls what)

`ggen sync` (manifest mode, no `--queries`) flows:

```
cmds/sync.rs::sync()                              [crates/ggen-cli/src/cmds/sync.rs:338]
  └─ run_manifest_pipeline()                       [sync.rs:400]
       └─ SyncExecutor::new(options).execute()     [sync.rs:437-439]
            └─ executor.rs::execute()              [crates/ggen-core/src/codegen/executor.rs:307]
                 ├─ ManifestParser::parse()        [executor.rs:348]  → toml::from_str (deny_unknown_fields)
                 ├─ ManifestValidator::validate()  [executor.rs:363-370]
                 ├─ DependencyValidator::validate_manifest() [executor.rs:373]
                 ├─ QualityGateRunner::run_all()    [executor.rs:396-402]
                 ├─ MarketplaceValidator::pre_flight_check() [executor.rs:405-411]
                 └─ execute_full_sync()             [executor.rs:456 → 659]
                      └─ GenerationPipeline::run()   [executor.rs:722]
                           ├─ load_ontology()        [pipeline.rs:1461 → 408]
                           ├─ execute_inference_rules() [pipeline.rs:1464 → 441]
                           ├─ execute_shacl_validation() [pipeline.rs:1467 → 1383]
                           ├─ execute_validation_rules() [pipeline.rs:1472 → 1306]
                           └─ execute_generation_rules() [pipeline.rs:1475 → 726]
  └─ emit_sync_receipt()                            [sync.rs:465 / 541]  (non-dry-run only)
```

So the **manifest pipeline does NOT bypass `GenerationPipeline`** — the executor
constructs it (`executor.rs:708`) and calls `.run()` (`executor.rs:722`), which
runs load → inference → SHACL → validation → generation in that fixed order
(`pipeline.rs:1459-1476`). This is the authoritative path; our driver rides it.

---

## 1. Does a generation rule's `query` accept SELECT only, or also CONSTRUCT/ASK?

**SELECT only. CONSTRUCT and ASK are rejected at runtime with `error[E0003]`.**

Evidence — `execute_generation_rules` matches the oxigraph result type and errors
on anything that is not `QueryResults::Solutions`:

`crates/ggen-core/src/codegen/pipeline.rs:836-858`
```rust
let rows = match results {
    QueryResults::Solutions(solutions) => { /* … build rows … */ }
    _ => {
        return Err(Error::new(&format!(
            "error[E0003]: Generation rules require SELECT queries (not CONSTRUCT/ASK)\n  --> rule: '{}'\n  |\n  = help: Change SPARQL query to SELECT …",
            rule.name
        )));
    }
};
```

- A **CONSTRUCT** in a generation `query` yields `QueryResults::Graph(...)` → hits
  the `_ =>` arm → **E0003, generation aborts.** So putting a CONSTRUCT there
  *would* misbehave (hard error).
- An **ASK** yields `QueryResults::Boolean(...)` → same `_ =>` arm → E0003.

**Our routing is correct.** The 6 semconv proofs are CONSTRUCT (wasm-JSON→RDF) and
are **never** used as a generation `query`. The driver routes generation through
`queries/extract-pareto-pipeline.rq`, a **SELECT** (verified:
`queries/extract-pareto-pipeline.rq:40` `SELECT ?stage_order …`), and the proofs
are only *referenced as strings* via `?proof_query`
(`extract-pareto-pipeline.rq:78-84`) and rendered into output descriptors
(`templates/ingest.tera:27` `tpot:resultMapping "queries/{{ row["proof_query"] }}"`).
No proof CONSTRUCT is ever executed by ggen — they are documentation payload.

Note for completeness: the *inference* CONSTRUCTs ARE executed, but through a
different code path (`ConstructExecutor::execute_and_materialize`,
`pipeline.rs:490-492`) that expects a CONSTRUCT — see §3.

---

## 2. How is `sparql_results` constructed, and what is the row shape?

**`sparql_results` is a JSON array of objects; each row is keyed by the bare
projection variable name (no leading `?`), value = cleaned term string. `row["col"]`
is correct.** Our batch templates are right.

Evidence — row build (`pipeline.rs:836-851`):
```rust
QueryResults::Solutions(solutions) => {
    let mut rows = Vec::new();
    for solution in solutions {
        let solution = solution.map_err(…)?;
        let mut row = BTreeMap::new();
        for (var, term) in solution.iter() {
            let key = var.to_string();                                   // e.g. "?stage_id"
            let clean_key = key.strip_prefix('?').unwrap_or(&key).to_string(); // "stage_id"
            row.insert(clean_key, clean_sparql_term(&term.to_string())); // values cleaned
        }
        rows.push(row);
    }
    rows
}
```

Exposure into Tera, **static-output path** (our case — every driver `output_file`
is a literal, no `{{`), `pipeline.rs:996-1009`:
```rust
let is_static_output = !rule.output_file.contains("{{");
if is_static_output && !rows.is_empty() {
    let results_json = serde_json::json!(rows);
    let mut context = tera::Context::new();
    context.insert("results", &results_json);
    context.insert("sparql_results", &results_json);   // ← our templates iterate this
    context.insert("entities", &results_json);
    for (key, value) in &rows[0] {                      // first-row scalars also exposed bare
        let clean_key = key.strip_prefix('?').unwrap_or(key);
        context.insert(clean_key, value.as_str());
    }
    …
}
```

So inside `{% for row in sparql_results %}`, **`row["stage_id"]` resolves** because
each row object's keys are the bare snake_case projection names. This matches:
- `templates/generated-ggen-toml.tera:13-17,43-65` — `row["stage_order"]`, `row["operator_id"]`, …
- `templates/tpot-config-dict.py.tera:41-77` — `row["stage_id"]`, `row["is_elite"]`, …
- `templates/pipeline-manifest.json.tera:23-46`, `search-space-report.md.tera:17-26`,
  `templates/ingest.tera:15-29`, etc.

**Term cleaning** (`clean_sparql_term`, `pipeline.rs:298-311` + `unescape_turtle_literal`
`316-366`): IRIs lose angle brackets; literals lose quotes and the `^^<dtype>` /
`@lang` suffix. Consequences our templates rely on:
- `?wasm_export "discover_inductive_miner"` → `discover_inductive_miner` ✓
- `?speed_tier "10"^^xsd:integer` → `10` (numeric-looking string) → emitted bare into
  TOML/JSON as `speed_tier = {{ speed_tier }}` ✓
- `?fitness_score` from `BIND((?q - (0.5*?s)) AS ?fitness)` → oxigraph decimal,
  e.g. `80` or `80.0`, cleaned to `"80"`/`"80.0"` → numeric in output ✓
- **`?is_elite`** boolean: oxigraph renders `xsd:boolean` true as
  `"true"^^<…#boolean>` → cleaned to `"true"`. The config-dict / report templates
  test `is_elite == "true"` (`tpot-config-dict.py.tera:71`,
  `search-space-report.md.tera:25`). This is the **correct** comparison string —
  **verified consistent**. (If oxigraph ever emitted bare `true`,
  `clean_sparql_term` leaves it as `"true"` too — same result.)

**Verdict for Q2: batch templates are correct** — `sparql_results` + `row["…"]` is
exactly the contract the engine provides.

---

## 3. How/when do inference rules run, and are derived `tpot:Operator` triples
visible to the SELECTs?

**Inference runs BEFORE generation, sorted by `order` ascending; materialized
triples persist in the same graph the SELECTs query. Our `derive-operators`
(order 1) → `derive-pareto-dominance` (order 2) ordering works, and the derived
`tpot:Operator` triples ARE visible to the generation SELECTs.**

Evidence:
- **Order in pipeline:** `run()` calls `execute_inference_rules()` (`pipeline.rs:1464`)
  strictly before `execute_generation_rules()` (`pipeline.rs:1475`).
- **Sort by `order`:** `execute_inference_rules` clones and sorts:
  `pipeline.rs:452-459`
  ```rust
  let mut rules: Vec<_> = self.manifest.inference.rules.clone();
  rules.sort_by_key(|r| r.order);
  for rule in rules { let result = self.execute_inference_rule(&rule)?; … }
  ```
  Our driver sets `order = 1` and `order = 2` (`ggen.toml:74,112`) → deterministic
  derive-operators-then-dominance.
- **Materialization into the live graph:** each rule runs
  `ConstructExecutor::new(graph).execute_and_materialize(&rule.construct)`
  (`pipeline.rs:490-492`). The executor is constructed over `self.ontology_graph`
  (`pipeline.rs:470-473`), the *same* `Graph` later handed to generation
  (`execute_generation_rules` reads `self.ontology_graph` at `pipeline.rs:731-734`,
  clones it into `graph_arc` at `pipeline.rs:754`). So CONSTRUCT output is inserted
  into the graph the SELECTs see. **Inference is additive and visible.**
- **Ordering between our two CONSTRUCTs is not load-bearing but is honored:**
  `derive-pareto-dominance` reads only `pi:` tiers (`ggen.toml:124-141`), so it does
  not depend on `derive-operators`; `derive-operators` reads `pi:` algos + the
  static `tpot:PipelineStage` triples (`ggen.toml:93-106`). Both inputs exist at
  load time. The SELECTs that need operators (`extract-pareto-pipeline.rq:45-52`,
  `extract-operators.rq:37-45`, `extract-pipeline-stages.rq:31-34`) all match
  `?op a tpot:Operator …`, which only exists post-`derive-operators`. Because
  inference fully precedes generation, **the operators exist before any SELECT
  runs.**
- **The CONSTRUCT WHERE clauses will actually match** (so triples are really added,
  not zero): `ontology/algorithms.ttl` carries `pi:algorithmId`, `pi:category`,
  `pi:speedTier`, `pi:qualityTier`, `pi:wasmExport` on all 60 individuals
  (e.g. `algorithms.ttl:14-26`; grep count = 540 predicate hits across the 60), and
  `ontology/tpot-search-space.ttl` declares 9 `tpot:PipelineStage` with
  `tpot:forCategory` (`tpot-search-space.ttl:60-121`). The join
  `?stage tpot:forCategory ?category` (`ggen.toml:101-102`) binds each algorithm's
  `pi:category` to its stage. **60 operators will be produced.**

**Zero-triple guard is not a risk:** `execute_inference_rule` only errors on
0-added-triples when `strict_mode` is on (`pipeline.rs:499-508`); our driver has no
`[validation]` block so `strict_mode = false` (see §5) → at worst a warning, and we
add many triples anyway.

> Caveat I can state but not execute-verify: I am reading the CONSTRUCT/SELECT
> patterns and the TTL data and concluding the join binds. I did not run oxigraph,
> so I cannot 100% rule out a datatype-comparison subtlety in the `FILTER(?qA >= ?qB)`
> dominance arithmetic or the `NOT EXISTS` argmax. The patterns are well-formed and
> the tiers are typed `xsd:integer`, so I assess the risk as low.

---

## 4. Does `output_dir` + `output_file` keep us in-root? Any GGEN-YIELD-001 risk?

**No YIELD-001 risk for the DRIVER. Output stays in-root, and the only escape guard
(`../` string check) is never tripped.**

Resolution logic (`execute_generation_rules`):
- `output_dir` is joined onto the manifest's base path:
  `pipeline.rs:739-743`
  ```rust
  let output_dir = if let Some(ref override_dir) = self.output_dir_override {
      self.base_path.join(override_dir)
  } else {
      self.base_path.join(&self.manifest.generation.output_dir)
  };
  ```
  Driver `output_dir = "generated/"` (`ggen.toml:151`) → `<project>/generated/`.
- Static-output final path: `output_dir_arc.join(effective_output_file_static)`
  (`pipeline.rs:1035`). All six driver `output_file`s are plain names
  (`ggen.toml:162,174,186,198,214,228`: `ggen.toml`, `tpot_config.py`,
  `pipeline.json`, `SEARCH_SPACE.md`, `STAGE_PLAN.md`, `objectives.json`) → all land
  in `<project>/generated/`.
- **The escape guard:** `validate_generated_output` rejects any path containing
  `../` or `..\\` with `error[E0006]` (`pipeline.rs:1558-1566`):
  ```rust
  if path_str.contains("../") || path_str.contains("..\\") {
      return Err(Error::new(&format!(
          "error[E0006]: Directory traversal pattern detected in output path …", …)));
  }
  ```
  None of our outputs contain `..`. **Guard passes.**

Honest limitation of that guard (relevant to the generated config in §6): it is a
**substring check on the rendered path**, not a canonicalized "is this under root"
containment test. A symlink or an absolute path would not be caught. We do not use
either, so the DRIVER is clean; I flag it only because the doc calls this surface
"GGEN-YIELD-001". The marketing name maps here to E0006, not a separate check.

---

## 5. Driver gate-by-gate: will each pre-generation gate pass?

The driver has **no `[validation]` section**, so `ValidationConfig::default()`
applies (`types.rs:397-419`): `shacl = []`, `strict_mode = false`,
`no_unsafe = false`, `rules = []`. (Grep for `shacl|strict_mode|[validation]` in the
driver returns **no matches** — confirmed.)

| Gate (call site) | What it checks | Driver outcome |
|---|---|---|
| `ManifestParser::parse` (`executor.rs:348`) | `toml::from_str` into `deny_unknown_fields` structs | **PASS.** Every `[ontology]`/`[inference]`/`[generation]`/rule key is a real field. `[sync]/[rdf]/[templates]/[output]` are captured as opaque `Option<toml::Value>` (`types.rs:121-127`) so their inner keys are not validated. The driver's comments at `ggen.toml:14-17,231-235` are accurate. |
| `ManifestValidator::validate` (`executor.rs:363`) | project non-empty; ontology source + imports exist; per-rule query/template files exist; `output_file` non-empty; ORDER-BY/VALUES checks | **PASS.** project name/version set (`ggen.toml:20-21`); `source` + 3 imports exist (`ontology/*.ttl` all present); all 6 query files and 4 template files exist at project root (verified by Glob); ORDER BY present on every SELECT; no `VALUES` in `.rq` files. See note ⚠️ below on inference ORDER BY. |
| `DependencyValidator::validate_manifest` (`executor.rs:373`) | ontology+imports exist, inference cycle check (via `when`), template files exist, query files exist (`dependency_validation.rs:34-142`) | **PASS.** Same files; no `when` clauses → no cycles. |
| `QualityGateRunner::run_all` (`executor.rs:396`) | (not read in full this dossier) | **Assessed PASS, not source-verified here.** Flagged as residual risk — see Checklist. |
| `MarketplaceValidator::pre_flight_check` (`executor.rs:405`) | FMEA over packs; driver declares **no** `[[packs]]` | **PASS** (nothing to check). |
| `execute_shacl_validation` (`pipeline.rs:1467`) | runs only if `manifest.validation.shacl` non-empty (`pipeline.rs:1386-1388`) | **SKIPPED** — driver lists `tpot-shapes.ttl` under `[ontology] imports` (loaded as *data*), **not** under `[validation] shacl`. So the SHACL gate is dormant and the derived-operator shape is never enforced. (Good for runnability; means SHACL is documentation-only here.) |
| `execute_validation_rules` (`pipeline.rs:1472`) | runs only if `validation.rules` non-empty (`pipeline.rs:1313-1316`) | **SKIPPED** (none declared). |

⚠️ **Inference ORDER BY:** both driver CONSTRUCTs lack `ORDER BY`
(`ggen.toml:82-106`, `120-141`). `validate_inference_rules` only *errors* on this
when `strict_mode` is true (`validation.rs:94-105`); with `strict_mode = false` it is
a **warning**, non-blocking. **Not a blocker as configured.** If a future maintainer
turns on `strict_mode`, these two CONSTRUCTs would raise `error[E0011]` and must gain
`ORDER BY`. (CONTRACT.md §6 only mandates ORDER BY on SELECTs, so this is consistent.)

**Net for the DRIVER: every gate the executor runs is satisfied → generation
proceeds → 6 rules emit 6 files under `generated/`. RUNNABLE.**

What gets produced (static-output path, one render each):
- RULE 1 `generated/ggen.toml` (9 elite rows) — the headline artifact.
- RULE 2 `generated/tpot_config.py` (60 operator rows).
- RULE 3 `generated/pipeline.json` (9 elite rows).
- RULE 4 `generated/SEARCH_SPACE.md` (60 rows).
- RULE 5 `generated/STAGE_PLAN.md` (9 stage rows, via the operator report template — see ⚠️Q below).
- RULE 6 `generated/objectives.json` (2 objective rows, via the pipeline-manifest template — see ⚠️Q below).

⚠️ **Two non-blocking semantic mismatches (template reuse), NOT failures:**
1. **RULE 5** feeds `extract-pipeline-stages.rq` rows
   (`?stage_order ?stage_id ?stage_label ?for_category ?operator_count`) into
   `search-space-report.md.tera`, which reads `row["operator_id"]`,
   `row["wasm_export"]`, `row["fitness_score"]`, `row["is_elite"]`. Those columns
   are absent in stage rows, but the template wraps **every** access in
   `| default(value="…")` (`search-space-report.md.tera:18-25`), so Tera does **not**
   error — it renders a table with blank operator/fitness cells. Output is
   non-empty → passes `validate_generated_output` (`pipeline.rs:1536-1544`).
   Cosmetic, not blocking.
2. **RULE 6** feeds `extract-fitness-objectives.rq` rows
   (`?objective_order ?objective_id ?objective_name ?metric ?direction ?weight`)
   into `pipeline-manifest.json.tera`, whose `{% for row in sparql_results %}` block
   reads stage fields (`row["stage_id"]`, `row["wasm_export"]`, …) — all guarded by
   `| default` (`pipeline-manifest.json.tera:24-33`). It renders a JSON `stages` array
   of 2 empty-ish stage objects plus the **hardcoded** `objectives` array already in
   the template (`pipeline-manifest.json.tera:6-21`). So `objectives.json` is valid
   JSON, just structurally odd (the objective rows don't drive the objectives block;
   they drive the stages block as blanks). Non-blocking; flagged for quality.

Neither (1) nor (2) trips an error path; both produce non-empty output and pass
validation. If you want them clean, give RULE 5 and RULE 6 their own templates
(see Checklist, optional).

---

## 6. The GENERATED `generated/ggen.toml`: why RUNNABLE-WITH-FIX

The emitted config (rendered from `templates/generated-ggen-toml.tera`) is **valid
TOML and schema-valid** — its keys all map to real fields, `mode = "Overwrite"` is
set on every emitted rule (`generated-ggen-toml.tera:62,76`), and it stays in-root
on its own terms (`output_dir = "generated/"`, output files like
`{{ stage_id }}-result.ttl`, no `..`). So as a *document* it is fine, and the
pure-Python reference's `reference_ggen.toml` parses (INTEGRATION_REPORT §4).

**But it is not re-runnable *in place*.** The emitted rules reference:
- `template = { file = "templates/{{ stage_id }}.tera" }` (`generated-ggen-toml.tera:60`)
  → e.g. `templates/ingest.tera`, `templates/discover.tera`, …
- `query = { file = "queries/extract-pareto-pipeline.rq" }` (`generated-ggen-toml.tera:59,73`)
- `template = { file = "templates/drift.tera" }` (`generated-ggen-toml.tera:74`)

If the generated file lives at `generated/ggen.toml`, then `ggen sync` run against
it sets `base_path = generated/` (the manifest's parent), and
`ManifestValidator::validate_generation_rules` resolves each template as
`base_path.join(file)` = `generated/templates/ingest.tera`
(`manifest/validation.rs:155-164`) and each query as
`generated/queries/extract-pareto-pipeline.rq` (`validation.rs:130-138`). **Those
paths do not exist** — the templates/queries live at the *project root*
(`templates/`, `queries/`), and there is currently no `generated/` directory at all
(Glob of `examples/tpot2-wasm4pm-autoconfig/**/*` shows no `generated/…`). Result:

> `Template file not found for rule 'stage-ingest-…': …/generated/templates/ingest.tera`
> (`manifest/validation.rs:158-162`) → `error[E0001]: Manifest validation failed`
> (`executor.rs:364-370`). Hard stop, **before** any generation.

`DependencyValidator` would independently flag the same missing files
(`dependency_validation.rs:96-142`, surfaced as `error[E0002]` at
`executor.rs:387-393`).

This is a **legacy/contract-drift-class issue per `.claude/rules/coding-agent-mistakes.md`
§1.5** — the generated proof object references inputs that aren't where it says they
are. It does **not** affect the DRIVER (the DRIVER's own `templates/`/`queries/` are
correct relative to the project root). It only bites if someone treats the emitted
config as independently runnable.

### Minimal fix options (pick one)

1. **Cheapest / honest-scope:** Document the generated `ggen.toml` as a *rendered
   sample to read*, not a runnable manifest, OR emit it at the **project root** path
   so its `templates/...`/`queries/...` resolve correctly. (Emitting to the parent of
   `generated/` is blocked by the `..` guard if done via `output_file`; instead set
   that one rule's `output_dir`/path to the project root, or post-copy it. The
   simplest is: state in the header that it must be run from the project root, since
   its relative paths are root-relative.)
2. **Make it truly self-contained:** have the generator also emit the referenced
   per-stage templates and the proof queries **into `generated/templates/` and
   `generated/queries/`** (extra generation rules / a copy step), so a `cd generated
   && ggen sync` resolves everything locally.
3. **Keep paths root-relative and run from root:** ship the generated config at
   `generated/ggen.toml` but always invoke it as
   `ggen sync --manifest generated/ggen.toml` *from the project root* — wait: that
   still sets `base_path = generated/`. So option 3 does **not** work as stated; the
   base path is always the manifest's parent (`executor.rs:357-362`). Discard.

→ **Recommended:** option 1 (re-label as a sample + require project-root context) for
the demo, or option 2 if you want the generated pipeline to be a genuinely runnable
second-stage. Either satisfies "deepen authority / reduce drift": option 2 removes the
drift entirely.

> I verified the path-resolution claim directly from `ManifestValidator`
> (`validation.rs:130-164`, base = manifest parent via `executor.rs:357-362`) and from
> the absence of any `generated/` tree on disk. I did **not** run it.

---

## 7. Quick answers to the five questions

1. **SELECT only.** CONSTRUCT/ASK in a generation `query` → `error[E0003]`
   (`pipeline.rs:852-857`). Our SELECT-routing is correct; the 6 CONSTRUCT proofs are
   only string-referenced, never executed by ggen.
2. **`sparql_results` = JSON array of objects keyed by bare var name**
   (`pipeline.rs:843-851`, `1000-1009`); `row["col"]` is right. Values are
   term-cleaned (`pipeline.rs:298-311`). Batch templates correct, including the
   `is_elite == "true"` boolean test.
3. **Inference runs first, sorted by `order`** (`pipeline.rs:452-459`, `1464` before
   `1475`), **materialized into the shared graph** (`pipeline.rs:490-492`, graph reused
   at `731-754`). derive-operators(1)→dominance(2) works; the 60 derived
   `tpot:Operator` triples are visible to every generation SELECT.
4. **In-root.** `output_dir.join(output_file)` under `<project>/generated/`
   (`pipeline.rs:739-743`, `1035`); `../` guard at `pipeline.rs:1558-1566` never trips.
   No YIELD-001/E0006 risk for the driver.
5. **VERDICT: DRIVER `ggen sync` is expected to SUCCEED (RUNNABLE).** The emitted
   `generated/ggen.toml` is RUNNABLE-WITH-FIX (path-base mismatch, §6).

---

## 8. Required-fixes checklist

**Blocking for the DRIVER:** *(none found)* — driver is RUNNABLE as written.

**Blocking only if you claim the GENERATED config is independently runnable:**
- [ ] **G1 (drift, §6):** The emitted `generated/ggen.toml` references
      `templates/<stage>.tera`, `templates/drift.tera`, `queries/extract-pareto-pipeline.rq`
      relative to `generated/`, where they don't exist → `error[E0001]`
      (`manifest/validation.rs:158-162`, `executor.rs:364`). **Fix:** either (a) emit
      the referenced templates+queries into `generated/templates/` and
      `generated/queries/` (self-contained), or (b) re-label the artifact as a
      rendered *sample* and document that its relative paths are project-root-relative.

**Non-blocking quality items (optional, no failure today):**
- [ ] **Q1 (§5):** RULE 5 reuses the operator report template for stage rows → blank
      operator/fitness cells in `STAGE_PLAN.md`. Give RULE 5 a stage-specific template
      if you want a clean stage plan.
- [ ] **Q2 (§5):** RULE 6 reuses the pipeline-manifest template for objective rows →
      `objectives.json` carries the hardcoded objectives block plus a `stages` array of
      2 empty objects. Give RULE 6 an objectives-specific template for clean output.
- [ ] **Q3 (§5, latent):** Both inference CONSTRUCTs lack `ORDER BY`. Harmless while
      `strict_mode = false`, but would raise `error[E0011]`
      (`manifest/validation.rs:94-99`) if strict mode is ever enabled. Add `ORDER BY`
      to future-proof.

**Residual unverified (could not execute; flagged honestly):**
- [ ] **R1:** `QualityGateRunner::run_all` (`executor.rs:396`) was not read in this
      dossier; I assess PASS (no packs, well-formed manifest) but did not confirm what
      it enforces. Worth a 5-minute read before declaring "definitely green".
- [ ] **R2:** The SPARQL `NOT EXISTS` argmax and `FILTER` dominance arithmetic are
      well-formed and operate on `xsd:integer` tiers, but were not executed against
      oxigraph here. The pure-Python reference (INTEGRATION_REPORT §4) independently
      reproduces the same 9 elites, which corroborates correctness — but that is a
      re-implementation, not the real ggen SPARQL engine.

---

### Bottom line

The DRIVER `ggen.toml` rides the real `SyncExecutor → GenerationPipeline::run`
path, satisfies every gate that path runs (parse, manifest-validate, dependency,
SHACL-skipped, validation-skipped), routes generation through a SELECT, hands the
inference-materialized `tpot:Operator` graph to that SELECT, and writes six files
in-root. **`ggen sync` is expected to succeed on our driver.** The *emitted*
`ggen.toml` is a valid, schema-correct document but is not re-runnable in place
because its relative template/query paths are root-relative while its base path
would be `generated/` — a one-fix drift item, not a flaw in the generator itself.
