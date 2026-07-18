# Research Dossier 02 — Is wasm4pm a REAL invocable runtime?

**Agent:** 02 of 10 · **Date:** 2026-06-20 · **Mode:** read-only investigation, no builds run
**Question:** Could the auto-configured TPOT2 pipeline actually *execute* against wasm4pm, or
is wasm4pm represented only as ontology/semconv metadata?

---

## VERDICT (read this first)

**wasm4pm is a METADATA-ONLY SPECIFICATION in this repository. There is NO executable
runtime present.** The auto-configured pipeline **cannot execute** against wasm4pm from
within `/home/user/ggen` as it stands.

Specifically:

- **Zero `.wasm` modules** exist anywhere in the repo (`Glob **/*.wasm` → no files).
- The entire `ontology_catalogue/wasm4pm/` tree contains **only `.ttl` and `.rq` files** —
  no `.rs`, no `.js/.ts`, no `Cargo.toml`, no `package.json`, no CLI, no MCP tool that
  invokes the exports. (`find ontology_catalogue/wasm4pm -type f ! -name '*.ttl' ! -name '*.rq'` → empty.)
- The 60 `pi:wasmExport` names (`from_pnml_wasm`, `discover_inductive_miner`,
  `run_agentic_pipeline`, …) appear in **zero executable source files** repo-wide
  (`Grep` over `*.{rs,js,ts,py,wat,wit}` → "No files found"). They exist only as RDF string literals.
- The real `wasm4pm` crate is referenced by the **root `Cargo.toml` as an out-of-tree path
  dependency** (`wasm4pm = { path = "../wasm4pm/wasm4pm" }`) — and **that path does not exist**
  on this machine (`ls ../wasm4pm` → "No such file or directory"). No workspace member even uses it.
- The 6 semconv "proof" CONSTRUCTs that supposedly map wasm4pm JSON → RDF are wired to
  **hardcoded `BIND(...)` placeholder literals**, with **zero `SERVICE`/`VALUES`/parameter
  inputs** — they emit fixed demo RDF irrespective of any wasm4pm execution.

This is consistent with — and corroborated by — `INTEGRATION_REPORT.md §6 ("Honest
limitations")` and `CONTRACT.md §9`, which already state no `ggen sync` / wasm runtime is
available in this container and that verification is structural + pure-Python reference only.
This dossier extends that: **even with a working `ggen` + `cargo`, there is no wasm4pm
runtime in-repo to invoke** — the dependency points outside the repository and is not vendored.

---

## Q1 — Does a real wasm4pm binary/module exist in-repo?

**No. wasm4pm is ontology + semconv metadata only.** Evidence by path:

| Probe | Command / path | Result |
|-------|----------------|--------|
| Any compiled WASM | `Glob **/*.wasm` | **No files found** (repo-wide) |
| wasm4pm tree contents | `find ontology_catalogue/wasm4pm -type f ! -name '*.ttl' ! -name '*.rq'` | **empty** — only `.ttl`/`.rq` |
| wasm4pm tree top dirs | `ontology_catalogue/wasm4pm/{crates,ggen,ocel,ontology,semconv}` | all contain only RDF/SPARQL |
| The `crates/prolog8/` dir | `ontology_catalogue/wasm4pm/crates/prolog8/ontology/prolog8.ttl` | **a `.ttl`, not a Rust crate** (no `src/`, no `Cargo.toml`) |
| Rust/JS impl of exports | `Grep from_pnml_wasm\|discover_inductive_miner\|… --glob *.{rs,js,ts,py,wat,wit}` | **No files found** |
| The real crate, vendored? | root `Cargo.toml:798` `wasm4pm = { path = "../wasm4pm/wasm4pm" }` | path **`../wasm4pm` does not exist** here |
| `wasm4pm-compat` crate | root `Cargo.toml:106,797` `path = "../wasm4pm-compat"` | path **does not exist** here; `ontology_catalogue/wasm4pm-compat/` is **ontology-only** (TTL/RQ) |
| Used by any member crate? | `grep -rl wasm4pm crates/*/Cargo.toml` | **none** — declared in `[workspace.dependencies]`/`[patch]` but no member depends on it |

**The strongest single tell is in the open-ontologies catalogue itself**:
`ontology_catalogue/open-ontologies/ontology/wasm4pm-stubs.ttl` is titled
*"wasm4pm **Stub** Module Specification Ontology"* and declares itself the
*"SOURCE OF TRUTH"* for the **module specification** — i.e. it specifies APIs, not code.
Its function individuals carry comments such as:

- `w4pm:dispatch_breed_test` → *"Production: dispatches to real breed engines; **stub: returns
  Err('wasm4pm_cognition not available')**."*
- `w4pm:check_conformance_alignment` → *"Production: replay trace …; **stub: returns
  Err('wasm4pm_algos not available')**."*
- `w4pm:discover_alpha` → *"Production: returns Petri net …; **stub: returns
  Err('wasm4pm_algos not available')**."*
- `w4pm:parse_powl_model_string` → *"Production version parses real POWL syntax; **stub
  returns PowlModel { name: 'stub_model' }**."*

So the *spec itself* describes a stub that errors out, and a "production" version that lives
elsewhere. Note also that the stub spec's API surface (`discover_alpha`,
`check_conformance_alignment`, `dispatch_breed_test`, `parse_powl_model_string`) is a **tiny,
different set** from the 60 `pi:wasmExport` names used by the auto-config pipeline — they do
not even line up one-to-one. (`ontostar-wasm4pm-integration.ttl` is likewise pure bridge
ontology: `powl:satisfiesLiveRule` linkages, SHACL shapes — no code.)

---

## Q2 — Are the 9 elite operators' `wasm_export` symbols defined anywhere executable?

**No. Every one is DECLARED in the registry (a string literal) and DEFINED nowhere executable.**

`pi:wasmExport` is an `xsd:string` data-property in
`ontology_catalogue/wasm4pm/ggen/ontology/algorithms.ttl`, e.g. line 23:
`pi:wasmExport "discover_a_star"`. It names an intended export; it does not bind to a symbol.
The auto-config example copies these verbatim into
`examples/tpot2-wasm4pm-autoconfig/ontology/algorithms.ttl` and surfaces them via the
`?wasm_export` SELECT column (`queries/extract-pareto-pipeline.rq`) and the per-stage `.tera`
templates — but nothing downstream resolves a symbol.

### Mapping table — auto-selected pipeline operator → wasm_export → artifact found?

(Operators/exports taken from `INTEGRATION_REPORT.md §2`; "declared in" = the TTL string
literal; "executable definition" = a real `.rs/.wasm/.js` symbol.)

| # | stage | elite operator | `wasm_export` (declared) | declared in registry? | executable definition found? |
|---|-------|----------------|--------------------------|:---------------------:|:----------------------------:|
| 1 | ingest | `pnml_import` | `from_pnml_wasm` | ✅ algorithms.ttl | ❌ none in repo |
| 2 | discover | `inductive_miner` | `discover_inductive_miner` | ✅ | ❌ |
| 3 | discover_oc | `ocel_petri_net` | `discover_ocel_petri_net` | ✅ | ❌ |
| 4 | analyze | `compute_activity_transition_matrix` | `compute_activity_transition_matrix` | ✅ | ❌ |
| 5 | conform | `complexity_metrics` | `compute_complexity_metrics` | ✅ | ❌ |
| 6 | learn | `ml_classify` | `ml_classify` | ✅ | ❌ |
| 7 | predict | `compute_ewma` | `compute_ewma` | ✅ | ❌ |
| 8 | simulate | `playout` | `petri_net_playout` | ✅ | ❌ |
| 9 | orchestrate | `agentic_pipeline` | `run_agentic_pipeline` | ✅ | ❌ |
| + | drift-monitor | `detect_drift` | `detect_drift` | ✅ | ❌ (see note) |

**Note on `detect_drift`:** a repo grep for `detect_drift` *does* hit
`ontology_catalogue/yawl/ontology/godspeed/inverse-godspeed-mcp-tools.ttl`, but that is an
**unrelated** MCP tool — `mcp:psi_inv_detect_drift` "Ψ⁻¹ Detect Drift" for *fact-state vs
codebase divergence*, not the wasm4pm process-mining drift export. No MCP tool in the repo
exposes any wasm4pm export.

**Net:** 10/10 export symbols are registry-declared; 0/10 are executably defined in this repo.

---

## Q3 — The intended runtime chain: wasm export → JSON → proof CONSTRUCT → RDF

**Intended chain (per the proof-file header comments):**

```
[wasm4pm export]  →  [JSON result]  →  [semconv proof CONSTRUCT]  →  [RDF triples]
  e.g. ocel_load        {object_types,        ocel-load.rq             pm:ObjectCentricEventLog,
  detect_concept_drift   event_types,         detect-drift.rq          pm:ConceptDrift, …
  ml_classify            counts, …}            ml-classify.rq
```

The proof files at `ontology_catalogue/wasm4pm/semconv/sparql-proofs/` literally document this
intent. `ocel-load.rq:1-4`: *"Maps wasm4pm OCEL load result into pm:ObjectCentricEventLog …
Input: JSON with object_types[], event_types[], counts …"*. `detect-drift.rq:1-3`: *"Maps
wasm4pm **detect_concept_drift MCP tool** result into pm:ConceptDrift triples. Input: JSON
with driftDetected, changePoint, …"*.

**Which links of the chain are present in-repo?**

| Chain link | Present in repo? | Evidence |
|-----------|:----------------:|----------|
| 1. wasm export (the runtime) | ❌ **absent** | no `.wasm`, no impl (Q1/Q2) |
| 2. JSON result of an export | ❌ **absent** | no sample/fixture JSON produced by a wasm call; the `WHERE` clauses don't read any |
| 3. proof CONSTRUCT (the mapper) | ✅ **present** (×6) | the six `.rq` files exist and are syntactically real CONSTRUCTs |
| 4. RDF output | ⚠️ **synthetic only** | produced from hardcoded literals, not from link 2 |

**Critical detail — link 3 is not actually wired to link 2.** All six CONSTRUCTs source their
data from **inline `BIND(...)` placeholder literals**, not from an input variable bound to
wasm JSON. Measured:

| proof file | `BIND` count | `SERVICE` | `VALUES` | data source |
|-----------|:-----------:|:---------:|:--------:|-------------|
| `ocel-load.rq` | 46 | 0 | 0 | hardcoded literals (e.g. `BIND(45000 AS ?event_count)`) |
| `discover-dfg.rq` | 4 | 0 | 0 | hardcoded literals |
| `conformance-check.rq` | 5 | 0 | 0 | hardcoded literals |
| `ml-classify.rq` | 50 | 0 | 0 | hardcoded literals |
| `detect-drift.rq` | 32 | 0 | 0 | hardcoded literals |
| `predict-activity.rq` | 35 | 0 | 0 | hardcoded literals |

`ocel-load.rq:80-83` even labels the WHERE block *"Source bindings (from application layer JSON
processing)"* — but no such application layer exists in-repo; the values are stubbed in place
(e.g. `BIND(IRI("http://wasm4pm.org/log/ocel-2026-04.jsonocel") AS ?source_file)`,
`BIND(0.92 AS ?strength)`). With **zero `SERVICE`** federation and **zero `VALUES`** injection
slots, these CONSTRUCTs are self-contained example generators. Run against an empty graph they
emit the same fixed demo RDF whether or not wasm4pm ever ran.

**Conclusion for Q3:** the chain's *mapping stage* (link 3) is present as 6 real CONSTRUCT
queries; the *runtime* (link 1) and its *JSON output* (link 2) are absent, and the mapping
stage is not even plumbed to accept link-2 input — it is fed mock literals. So end-to-end
execution of the documented chain is **not possible in-repo**.

---

## Q4 — VERDICT: real execution status of wasm4pm here

> **METADATA-ONLY SPECIFICATION. No executable runtime present in this repository.**

What genuinely exists here is a rich, internally-consistent **semantic description** of a
process-mining engine:

- a 60-individual algorithm registry with `pi:wasmExport` *names*, quality/speed tiers,
  citations, I/O formats (`algorithms.ttl`);
- a 55-breed cognition vocabulary (`breeds.ttl`);
- an OWL **stub-module spec** that openly documents stub functions returning
  *"… not available"* errors (`open-ontologies/ontology/wasm4pm-stubs.ttl`);
- a bridge/integration ontology (`ontostar-wasm4pm-integration.ttl`);
- 6 semconv **CONSTRUCT mappers** that *would* turn wasm JSON into RDF but are currently fed
  hardcoded example values;
- projected "evidence" TTL (`ocel/reports/pi_evidence.ttl`) whose own header says it was
  produced by `scripts/project_pi_evidence.py` from **fixtures** ("…-fixture") — and **that
  script does not exist in-repo** (`ls scripts/project_pi_evidence.py` → not found).

What does **not** exist here: any `.wasm` module, any Rust/JS/CLI/MCP implementation of the
60 exports, a vendored copy of the `wasm4pm`/`wasm4pm-compat` crates (the `Cargo.toml` path
deps `../wasm4pm/wasm4pm` and `../wasm4pm-compat` dangle off the repo into the author's
machine), or any wiring that would let a wasm result reach the proof CONSTRUCTs.

**Therefore the auto-configured TPOT2 pipeline is a *configuration/specification* artifact, not
an *executable* one.** It correctly, deterministically selects a Pareto-optimal *plan* of
operators and emits a valid `ggen.toml` + config_dict + manifest naming those operators and
their intended `wasm_export`s and proof queries. But "executing the pipeline against wasm4pm"
is **not achievable from this repo**: the named exports resolve to nothing runnable, and the
JSON→proof→RDF chain has no live source. The honest framing already in `INTEGRATION_REPORT.md`
(structural + pure-Python reference verification; no `ggen sync`; no fabricated receipts) is
**accurate and, if anything, understated** — the gap is not merely "no `cargo` in this
container" but "no wasm4pm runtime is vendored here at all."

To actually run it one would need to obtain the external `wasm4pm`/`wasm4pm-compat` crates (or a
compiled `*.wasm` + a host that resolves these exports), produce real JSON outputs, and add the
missing application layer that binds that JSON into the 6 proof CONSTRUCTs (replacing the
placeholder `BIND`s with real input variables or a `SERVICE`/`VALUES` feed). None of those three
pieces is present.

---

## Path index (all cited paths, absolute)

Ontology / semconv (metadata — present):
- `/home/user/ggen/ontology_catalogue/wasm4pm/ggen/ontology/algorithms.ttl` (60 algos, `pi:wasmExport` literals; e.g. line 23 `pi:wasmExport "discover_a_star"`)
- `/home/user/ggen/ontology_catalogue/wasm4pm/ggen/ontology/breeds.ttl`
- `/home/user/ggen/ontology_catalogue/wasm4pm/ocel/reports/pi_evidence.ttl` (header cites `scripts/project_pi_evidence.py` + "*-fixture" provenance)
- `/home/user/ggen/ontology_catalogue/wasm4pm/semconv/wasm4pm-ontology.ttl`
- `/home/user/ggen/ontology_catalogue/wasm4pm/semconv/sparql-proofs/{ocel-load,discover-dfg,conformance-check,ml-classify,detect-drift,predict-activity}.rq` (6 CONSTRUCTs, all `BIND`-only WHERE)
- `/home/user/ggen/ontology_catalogue/open-ontologies/ontology/wasm4pm-stubs.ttl` ("Stub Module Specification"; stub fns return "… not available")
- `/home/user/ggen/ontology_catalogue/open-ontologies/ontology/ontostar-wasm4pm-integration.ttl` (bridge ontology)
- `/home/user/ggen/ontology_catalogue/wasm4pm-compat/` (ontology-only; TTL/RQ)

Runtime (absent / dangling):
- `/home/user/ggen/Cargo.toml:106,797,798` — `wasm4pm-compat`/`wasm4pm` path deps to `../wasm4pm-compat`, `../wasm4pm/wasm4pm` (**paths do not exist on this machine**)
- `../wasm4pm`, `../wasm4pm-compat` — **not found**
- `/home/user/ggen/scripts/project_pi_evidence.py` — **not found**
- (no `**/*.wasm`; no `*.rs/.js/.ts` defining any of the 60 exports)

Context corroboration:
- `/home/user/ggen/examples/tpot2-wasm4pm-autoconfig/INTEGRATION_REPORT.md` §2 (pipeline table), §6 (honest limits: no `ggen sync`, no receipts)
- `/home/user/ggen/examples/tpot2-wasm4pm-autoconfig/CONTRACT.md` §9 (env constraints), §6 (`?wasm_export` column)
- `/home/user/ggen/docs/jira/WASM4PM-DISCOVERED-BUGS.md` (history: wasm4pm exercised via `ggen sync`/codegen, never via wasm execution)
- Unrelated `detect_drift` namesake: `/home/user/ggen/ontology_catalogue/yawl/ontology/godspeed/inverse-godspeed-mcp-tools.ttl` (`mcp:psi_inv_detect_drift`)
