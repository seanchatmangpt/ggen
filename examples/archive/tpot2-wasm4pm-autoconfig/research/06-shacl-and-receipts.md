# Research Dossier 06 — SHACL Shapes & ggen Receipts

**Agent:** research 06/10 · **Date:** 2026-06-20 · **Mode:** read-only on the generator project
**Scope:** (A) Are `ontology/tpot-shapes.ttl` real, valid, and aligned with the data they'd validate?
(B) What would a real `ggen sync` receipt for this project bind?

All claims below cite real file:line in the ggen codebase or the generator project. No `cargo`/`ggen`
was run (none available — INTEGRATION_REPORT §6). Findings are evidence-first from source.

---

## TL;DR (the four answers)

1. **Shapes are syntactically valid SHACL and semantically correct against the data** — every constraint
   *would* pass if enforced. But **half of the OperatorShape constraints can never fire on ggen's engine**
   because ggen's SHACL implementation only honors a whitelist (`minCount/maxCount/datatype/pattern/minLength/maxLength/message/severity`) and **silently drops `sh:nodeKind` and `sh:class`** (`crates/ggen-core/src/validation/shacl.rs:168`, `:270-303`). The load-bearing datatype/cardinality checks are fine; the IRI/class checks are decorative on ggen.
2. **Enabling `strict_mode=true` is SAFE but does NOT enable SHACL.** `strict_mode` only elevates determinism warnings (missing `ORDER BY`) to errors (`crates/ggen-core/src/manifest/types.rs:412-414`, `crates/ggen-core/src/codegen/pipeline.rs:500`). SHACL enforcement is gated by a *different* field — `[validation].shacl` (a file list) being non-empty (`pipeline.rs:1386-1389`). The driver `ggen.toml` has **no `[validation]` section at all**, so `validation.shacl` is empty → `execute_shacl_validation()` returns `Ok(())` immediately and the shapes (loaded only as `[ontology].imports` data) are **never enforced**. Turning on `strict_mode` would not change that. The queries already satisfy the ORDER BY discipline strict_mode checks, so it is also safe to enable.
3. **A real receipt binds an 18-entry input closure and a 6-entry output set** (enumerated in §5). Hashing is **SHA-256, not BLAKE3** (the CLAUDE.md/coding-agent-mistakes "BLAKE3" claim is wrong for this code path — `crates/ggen-core/src/receipt/receipt_impl.rs:180-184`). Signature is Ed25519 (correct).
4. **Recommended tightenings** (§7): wire the shapes into `[validation].shacl`; add `sh:minCount` floors that ggen *does* enforce on `opSpeedTier/opQualityTier/atStage` etc.; add `sh:in`/`sh:pattern` where ggen honors them; and replace the ggen-unsupported `sh:class`/`sh:nodeKind` checks with enforceable equivalents.

---

## Part A — Are the SHACL shapes real and correct?

### A.0 What ggen's SHACL engine actually enforces (the constraint whitelist)

This is the decisive context for the whole shape analysis. ggen does **not** run a full SHACL processor.
It loads shapes by SPARQL and supports only an explicit field set:

- `ShapeLoader.load` property scan filters to exactly these predicates
  (`crates/ggen-core/src/validation/shacl.rs:168`):
  `sh:minCount, sh:maxCount, sh:datatype, sh:pattern, sh:minLength, sh:maxLength, sh:message, sh:severity`
  — plus `sh:in` handled by a separate query (`shacl.rs:202-235`).
- `apply_constraint_field` only has arms for those same predicates (`shacl.rs:270-303`).
- **`sh:nodeKind`, `sh:class`, `sh:node`, `sh:hasValue`, `sh:or`, `sh:minInclusive/maxInclusive` are NOT parsed.**
  Any such constraint in a shape is **silently ignored** (no error, no enforcement).
- The validator (`crates/ggen-core/src/validation/validator.rs:74-211`) only emits violations for the
  whitelisted constraint kinds.

**Datatype check is strict-inequality** (`validator.rs:273-304`):
`FILTER (?actualType != <expected> && ?actualType != rdf:langString)`. So a `sh:datatype xsd:decimal`
constraint flags **every** value whose `datatype()` is not exactly `xsd:decimal`.

### A.1 Shape-by-shape validity table

| Shape | Property constraint (file:line) | Syntactically valid SHACL? | Enforced by ggen? | Would it PASS the data? | Verdict |
|-------|--------------------------------|:--:|:--:|:--:|--------|
| **PipelineStageShape** (`tpot-shapes.ttl:26`) `tpot:stageId` datatype=string min/max=1 (`:29-35`) | yes | **yes** (datatype+card) | PASS — all 9 stages have one `tpot:stageId "..."` plain string (`tpot-search-space.ttl:62,69,…`) | ✅ valid & fires |
| | `tpot:stageOrder` datatype=integer min/max=1 (`:36-42`) | yes | **yes** | PASS — `"1"^^xsd:integer … "9"^^xsd:integer` (`tpot-search-space.ttl:63,70,…`) | ✅ valid & fires |
| | `tpot:forCategory` datatype=string min/max=1 (`:43-49`) | yes | **yes** | PASS — `"import_export"`…`"agentic"` plain strings (`tpot-search-space.ttl:65,72,…`) | ✅ valid & fires |
| **OperatorShape** (`tpot-shapes.ttl:57`) `tpot:operatorId` datatype=string min/max=1 (`:60-66`) | yes | **yes** | PASS — derive-operators emits `tpot:operatorId ?algorithm_id` and `pi:algorithmId` are plain strings (`algorithms.ttl:15` `"a_star"`) | ✅ valid & fires |
| | `tpot:wrapsAlgorithm` **nodeKind=IRI + class=pi:ProcessIntelligenceAlgorithm** min/max=1 (`:67-74`) | yes (valid SHACL) | **NO — `sh:nodeKind` & `sh:class` dropped**; only `min/maxCount` would fire | min/maxCount=1 PASS (one `tpot:wrapsAlgorithm ?a`); the IRI+class assertion is **never checked** | ⚠️ valid but **mostly inert on ggen** |
| | `tpot:atStage` **nodeKind=IRI + class=tpot:PipelineStage** min/max=1 (`:75-82`) | yes | **NO** (same as above — only card fires) | card PASS; class/IRI never checked | ⚠️ valid but **mostly inert on ggen** |
| | `tpot:fitnessScore` **datatype=xsd:decimal** min/max=1 (`:83-89`) | yes | **yes** (datatype+card) | **PASS** — `BIND((?q - (0.5*?s)))` yields `xsd:decimal` (see §A.2) | ✅ valid & fires — **and the datatype matches** |
| **FitnessObjectiveShape** (`tpot-shapes.ttl:95`) `tpot:metric` datatype=string min/max=1 + `sh:in (qualityTier speedTier)` (`:98-105`) | yes | **yes** (datatype+card+in) | PASS — `"qualityTier"` / `"speedTier"` (`tpot-search-space.ttl:137,146`) | ✅ valid & fires |
| | `tpot:direction` datatype=string min/max=1 + `sh:in (maximize minimize)` (`:106-113`) | yes | **yes** | PASS — `"maximize"` / `"minimize"` (`tpot-search-space.ttl:138,147`) | ✅ valid & fires |

**No constraint would FAIL the data.** The shapes are honest and aligned. The only defects are *inert*
constraints (`sh:nodeKind`/`sh:class` on the two IRI-valued Operator properties), which ggen ignores —
they neither pass nor fail; they simply do nothing. On a full SHACL processor (e.g. pySHACL) they would
also pass *provided* `pi:ProcessIntelligenceAlgorithm` is treated as a class (see §A.3 caveat).

### A.2 Datatype-alignment check — shape vs BIND output (the CONTRACT flagged this)

**Question:** CONTRACT §5 declares `tpot:fitnessScore (xsd:decimal)`. The shape requires `sh:datatype xsd:decimal`.
But the value is computed: `BIND((?q - (0.5 * ?s)) AS ?fitness)` (`inference/derive-operators.rq:61`,
also inlined verbatim in `ggen.toml:104`). Does the computed literal actually carry `xsd:decimal`?

**Operand datatypes (verified):**

- `?q` = `pi:qualityTier`, `?s` = `pi:speedTier`. **All 120 tier literals (60 algos × 2) are `^^xsd:integer`**;
  zero plain literals (`algorithms.ttl:21-22` etc.; `grep` count: 120× `^^xsd:integer`, 0× bare). 
- `0.5` written as a bare decimal in SPARQL is parsed as an **`xsd:decimal`** literal (SPARQL 1.1 grammar
  rule `[148] DECIMAL`).

**SPARQL 1.1 arithmetic typing (XPath 1.0 numeric promotion; Oxigraph 0.5.8 implements this —
`Cargo.toml: oxigraph = "0.5.8"`, evaluated via `oxigraph::sparql::SparqlEvaluator`,
`crates/ggen-core/src/graph/core.rs:17`):**

| Step | Operation | Operand types | Result type |
|------|-----------|---------------|-------------|
| 1 | `0.5 * ?s` | `xsd:decimal` × `xsd:integer` | **`xsd:decimal`** (`op:numeric-multiply`, integer promoted to decimal) |
| 2 | `?q - (…)` | `xsd:integer` − `xsd:decimal` | **`xsd:decimal`** (`op:numeric-subtract`, integer promoted to decimal) |

**Conclusion:** `datatype(?fitness) = xsd:decimal`. The shape's `sh:datatype xsd:decimal` is **correctly aligned**
— *if* the shape were enforced it would PASS. There is **no datatype mismatch**.

> Why this is worth stating precisely: had the formula been integer-only (e.g. `?q - ?s`), the result would
> be `xsd:integer` and the `xsd:decimal` shape would flag **all 60 operators** under ggen's strict-inequality
> datatype check (`validator.rs:282`). The presence of the `0.5` decimal literal is exactly what promotes the
> whole expression to decimal. This is robust, not accidental — but it hinges on `0.5` staying a decimal
> literal. If anyone "simplifies" `0.5` to `1/2` or casts tiers, the alignment breaks. (Note also Oxigraph
> would produce `xsd:double` only if an operand were a `xsd:double`/`e`-notation literal — none are present,
> so decimal stands.)

**Corroboration:** the equality `?other_fit = ?fitness_score` in the elite argmax
(`queries/extract-pareto-pipeline.rq:71-73`) is a *value* comparison and works on decimals; the pure-Python
reference computes the identical `quality_tier - (0.5 * speed_tier)` as a float
(`verify/reference_autoconfig.py:108-110`), and the tie-break case (`complexity_metrics` f82.5 vs
`alignments` f82.5, INTEGRATION_REPORT §2) requires fractional fitness to even be representable — consistent
with decimal.

### A.3 Caveat: `pi:ProcessIntelligenceAlgorithm` is never declared as a class

The 60 individuals are typed `a pi:ProcessIntelligenceAlgorithm` (`algorithms.ttl:14` etc.), but the IRI
`pi:ProcessIntelligenceAlgorithm` has **no `owl:Class`/`rdfs:Class` declaration** anywhere in `algorithms.ttl`
(grep for a class-declaration triple returns nothing). On ggen this is irrelevant (`sh:class` is dropped).
On a real SHACL processor, `sh:class X` is satisfied by `?value rdf:type/rdfs:subClassOf* X`, so the
*instances* still satisfy it regardless of whether `X` is declared a class — so even a strict processor would
PASS. But it is a latent modeling gap worth noting: nothing in the ontology formally says
`pi:ProcessIntelligenceAlgorithm a owl:Class`.

---

## Part B — strict_mode safety verdict

### B.1 The two independent knobs (commonly conflated)

`ValidationConfig` (`crates/ggen-core/src/manifest/types.rs:397-419`):

```rust
pub struct ValidationConfig {
    #[serde(default)] pub shacl: Vec<PathBuf>,   // line 402 — SHACL files to ENFORCE
    #[serde(default)] pub strict_mode: bool,     // line 414 — elevate determinism warnings to errors
    // … syntax flag, custom rules …
}
```

- **`shacl`** gates SHACL: `execute_shacl_validation()` does `if shacl_paths.is_empty() { return Ok(()); }`
  (`crates/ggen-core/src/codegen/pipeline.rs:1386-1389`). It reads each listed file into a *separate* shapes
  graph (`pipeline.rs:1397-1408`) and runs `SparqlValidator` (`pipeline.rs:1411-1414`).
- **`strict_mode`** is consumed in exactly one place in the generation pipeline: rejecting a 0-triple
  (identity/no-match) inference CONSTRUCT — `if self.manifest.validation.strict_mode { return Err(GGEN-INFER-001) }`
  (`pipeline.rs:500-505`). It also drives the E0011/E0013 ORDER-BY-as-error behavior described in CLAUDE.md.
  **`strict_mode` does not touch `execute_shacl_validation` at all.**

### B.2 Current driver state

`ggen.toml` (the driver) declares `[ontology]`, `[ontology.prefixes]`, `[inference]`, `[generation]`,
`[sync]`, `[rdf]`, `[templates]`, `[output]` — and **no `[validation]` section**. `ValidationConfig` derives
`Default` (`types.rs:397`), so `validation.shacl = []` and `validation.strict_mode = false`. The shapes file
`ontology/tpot-shapes.ttl` is referenced **only** as an `[ontology].imports` entry (`ggen.toml:41`), which
loads its triples into the *ontology* graph as data — never into the *shapes* graph that
`execute_shacl_validation` builds. **Net: shapes are inert today.**

### B.3 Verdict: is enabling `strict_mode=true` safe?

**SAFE — but it is the wrong lever if the goal is to enforce SHACL.**

| Scenario | Effect | Risk |
|----------|--------|------|
| Add `[validation] strict_mode = true` only | Elevates missing-ORDER-BY (E0011/E0013) and 0-triple inference CONSTRUCTs to hard errors. | **Low/none.** All 6 SELECT queries already end in explicit `ORDER BY` (e.g. `extract-pareto-pipeline.rq:86`, CONTRACT §6 mandates it). Both inference CONSTRUCTs add new triples (derive-operators: 60 operators; derive-pareto-dominance: dominance edges) — neither is identity/0-triple (verified non-empty WHERE patterns, `ggen.toml:82-141`). So strict_mode would **pass**. |
| Add `[validation] shacl = ["ontology/tpot-shapes.ttl"]` | Actually runs the shapes. | **Low** — per §A.1 every enforced constraint passes the data; the inert `sh:nodeKind`/`sh:class` ones can't fail. Safe to enable; this is the change that gives the shapes teeth. |

**Concrete recommendation:**

1. To make SHACL real (deepen authority), add a `[validation]` section that lists the shapes under `shacl`,
   **not** just under `[ontology].imports`:
   ```toml
   [validation]
   shacl = ["ontology/tpot-shapes.ttl"]
   strict_mode = true
   ```
   This is safe (all constraints pass) and converts the shapes from documentation into an enforced gate that
   aborts generation before writing files if a future ontology edit breaks an invariant
   (`pipeline.rs:1443-1449`).
2. `strict_mode = true` is independently safe and worth enabling for the ORDER-BY / no-op-CONSTRUCT
   guarantees — it does not risk the run.
3. Keep `tpot-shapes.ttl` in `[ontology].imports` too **only if** you want the `sh:*` triples queryable; it
   is harmless but redundant for enforcement. (Loading shapes as ontology data does not cause false
   violations because no shape targets `sh:NodeShape` etc.)

> Caveat for whoever flips these on: ggen builds the shapes graph from the **file path** listed in
> `validation.shacl` (`pipeline.rs:1399`), relative to the manifest's `base_path`. The path
> `ontology/tpot-shapes.ttl` resolves correctly from the project root. Run once with `--validate-only` first
> to confirm zero violations before a full sync.

---

## Part C — What a real receipt would bind

### C.1 Where receipts come from (correct the CLAUDE.md reference)

There is **no `ggen-receipt` crate** (the CLAUDE.md "Coding-Agent Mistakes" path
`crates/ggen-receipt/src/lib.rs` does not exist). The real implementation:

- **Receipt type / sign / verify / hash:** `crates/ggen-core/src/receipt/receipt_impl.rs`.
- **Sync receipt emission:** `crates/ggen-cli/src/cmds/sync.rs::emit_sync_receipt` (`sync.rs:541-652`).
- **Verify command + key mgmt:** `crates/ggen-cli/src/receipt_manager.rs`.

`Receipt` fields (`receipt_impl.rs:10-29`): `operation_id` (UUIDv4, `sync.rs:631`), `timestamp` (RFC-3339
`DateTime<Utc>`), `input_hashes: Vec<String>`, `output_hashes: Vec<String>`, `signature` (hex Ed25519),
`previous_receipt_hash: Option<String>` (chaining).

**Crypto (important correction):**
- `hash_data` and `Receipt::hash` use **SHA-256** (`receipt_impl.rs:119-124`, `:180-184`) — *not* BLAKE3.
  Each hash is 64 hex chars.
- `signature` is **Ed25519** over the JSON-serialized receipt with the signature field blanked
  (`receipt_impl.rs:72-77`, `:145-154`). Keys live at `.ggen/keys/signing.key` + `.ggen/keys/verifying.key`
  (hex), generated once and never overwritten (`sync.rs:551-566`).
- Empty signature ⇒ `verify()` returns `Err` (sabotage-tested, `receipt_impl.rs:94-96`, `:316-336`).

### C.2 When the receipt is written

`emit_sync_receipt` is called after `SyncExecutor::execute()` on every **non-dry-run** manifest sync
(`sync.rs:461-469`). A `--dry-run` writes **no** receipt (preview ≠ actuation, `sync.rs:461-462`). Writes two
files: `.ggen/receipts/sync-<YYYYMMDD-HHMMSS>.json` (archive) and `.ggen/receipts/latest.json`
(`sync.rs:645-649`).

> Note: today `generated/` does not exist (no `ggen sync` has run — confirmed: `ls generated/` absent; only
> `verify/out/` python-reference artifacts exist). The receipt below is what the **first real** `ggen sync`
> in this directory would bind, derived from the manifest closure that `emit_sync_receipt` walks.

### C.3 Input-hash closure (exactly what `emit_sync_receipt` binds)

`emit_sync_receipt` builds `input_hashes` as the full O\* closure (`sync.rs:582-618`):

1. **Actuator identity** (1 entry, not a hash): `actuator:ggen-sync@<CARGO_PKG_VERSION>` (`sync.rs:584`).
2. **Manifest:** `ggen.toml:<sha256>` (`sync.rs:585-589`). This binds all **inlined** inference CONSTRUCTs,
   since both `derive-operators` and `derive-pareto-dominance` live *inside* `ggen.toml` as
   `construct = """…"""` (`ggen.toml:74-142`) — they are covered by the manifest hash, **not** separately.
   ⇒ The standalone `inference/derive-operators.rq` and `inference/derive-pareto-dominance.rq` files are
   **NOT** in the receipt closure (they're human-readable copies, per `ggen.toml:60-66`).
3. **Ontology source + imports** (`sync.rs:594-595`): `manifest.ontology.source` then each
   `manifest.ontology.imports`.
4. **Per-rule external query + template files** (`sync.rs:596-603`): for each `[[generation.rules]]`, the
   `query.file` and `template.file` (deduplicated by the closure walk only if identical paths repeat — note
   the code does **not** dedupe, so a file reused across rules is hashed once per appearance; see below).
5. **Installed packs** (`sync.rs:616-618`): `pack:<id>@<version>` per entry in `.ggen/packs.lock`. This
   project ships no `packs.lock` ⇒ **0 pack entries**.

A file that cannot be read is recorded as `<path>:MISSING` (honest gap, `sync.rs:611`) rather than dropped.

**Enumerated input closure for THIS project** (manifest order; all files verified PRESENT):

| # | Input entry | Source (manifest ref) |
|---|-------------|------------------------|
| 1 | `actuator:ggen-sync@<version>` | `sync.rs:584` |
| 2 | `ggen.toml:<sha256>` | manifest itself (covers inlined inference) |
| 3 | `ontology/tpot-search-space.ttl:<sha256>` | `[ontology].source` (`ggen.toml:37`) |
| 4 | `ontology/algorithms.ttl:<sha256>` | `[ontology].imports[0]` (`ggen.toml:39`) |
| 5 | `ontology/breeds.ttl:<sha256>` | `[ontology].imports[1]` (`ggen.toml:40`) |
| 6 | `ontology/tpot-shapes.ttl:<sha256>` | `[ontology].imports[2]` (`ggen.toml:41`) |
| 7 | `queries/extract-pareto-pipeline.rq:<sha256>` | RULE 1 query (`ggen.toml:160`) |
| 8 | `templates/generated-ggen-toml.tera:<sha256>` | RULE 1 template (`ggen.toml:161`) |
| 9 | `queries/extract-operators.rq:<sha256>` | RULE 2 query (`ggen.toml:172`) |
| 10 | `templates/tpot-config-dict.py.tera:<sha256>` | RULE 2 template (`ggen.toml:173`) |
| 11 | `queries/extract-pareto-pipeline.rq:<sha256>` (again) | RULE 3 query (`ggen.toml:184`) — **duplicate of #7** |
| 12 | `templates/pipeline-manifest.json.tera:<sha256>` | RULE 3 template (`ggen.toml:185`) |
| 13 | `queries/extract-operators.rq:<sha256>` (again) | RULE 4 query (`ggen.toml:196`) — **duplicate of #9** |
| 14 | `templates/search-space-report.md.tera:<sha256>` | RULE 4 template (`ggen.toml:197`) |
| 15 | `queries/extract-pipeline-stages.rq:<sha256>` | RULE 5 query (`ggen.toml:212`) |
| 16 | `templates/search-space-report.md.tera:<sha256>` (again) | RULE 5 template (`ggen.toml:213`) — **duplicate of #14** |
| 17 | `queries/extract-fitness-objectives.rq:<sha256>` | RULE 6 query (`ggen.toml:225`) |
| 18 | `templates/pipeline-manifest.json.tera:<sha256>` (again) | RULE 6 template (`ggen.toml:227`) — **duplicate of #12** |

**= 18 input entries** (1 actuator string + 1 manifest + 4 ontology + 12 query/template, of which 4 are
exact-path duplicates the code does not dedupe — `sync.rs:604-613` iterates the closure vec as built).
0 pack entries. **NOT bound:** `inference/*.rq` standalone files, `verify/*`, the 6 semconv proof `.rq`
copies under `queries/` (`ocel-load.rq` etc.) — because those proof files are referenced as *strings* inside
templates/config, never as a `[[generation.rules]]` `query.file`, so the closure walk doesn't reach them.

> Drift flag (minor, honest): the 4 duplicate query/template hashes are harmless (same path ⇒ same hash) but
> mean the receipt's `input_hashes.len()` over-counts distinct inputs. A verifier comparing two runs is
> unaffected (deterministic). If you want a clean closure, dedupe paths before hashing — but it is not a
> correctness defect.

### C.4 Output-hash set

`output_hashes` = SHA-256 of each generated file actually written (`sync.rs:620-628`), keyed by the path the
executor reports. With `[generation].output_dir = "generated/"` (`ggen.toml:151`) and the 6 rules
(`ggen.toml:158-229`), the output set is:

| # | Output entry | Rule (ggen.toml) |
|---|--------------|------------------|
| 1 | `generated/ggen.toml:<sha256>` | RULE 1 `generate-ggen-toml` (`:159-163`) |
| 2 | `generated/tpot_config.py:<sha256>` | RULE 2 `generate-tpot-config` (`:171-175`) |
| 3 | `generated/pipeline.json:<sha256>` | RULE 3 `generate-pipeline-manifest` (`:183-187`) |
| 4 | `generated/SEARCH_SPACE.md:<sha256>` | RULE 4 `generate-search-space-report` (`:195-199`) |
| 5 | `generated/STAGE_PLAN.md:<sha256>` | RULE 5 `generate-stage-plan` (`:211-215`) |
| 6 | `generated/objectives.json:<sha256>` | RULE 6 `generate-fitness-objectives` (`:225-229`) |

**= 6 output entries.** (Exact path prefix depends on how the executor reports `output_file` relative to
`output_dir`; `emit_sync_receipt` hashes whatever path strings `sync_result.files[].path` carries —
`sync.rs:452,621-627`. A file that can't be re-read is skipped via `filter_map`, `sync.rs:623`.)

### C.5 Receipt chaining + verification

- If `.ggen/receipts/latest.json` exists, the new receipt's `previous_receipt_hash` = SHA-256 of the prior
  receipt JSON (`sync.rs:571-576`, `:635-637`; `Receipt::chain` `receipt_impl.rs:139-142`). First run ⇒
  genesis (`previous_receipt_hash = null`).
- `ggen receipt verify` re-checks the Ed25519 signature against `.ggen/keys/public.pem`
  (`receipt_manager.rs:219-234`) — note: sync writes `verifying.key` (`sync.rs:564`) while
  `ReceiptManager` reads `public.pem` (`receipt_manager.rs:221`). **Key-filename divergence** between the
  two code paths is a real footgun worth flagging to the integration agents (sync's verify path in
  `cmds/receipt.rs` may differ from `receipt_manager.rs`; the golden-path `latest.json` is written by sync
  using `verifying.key`).

---

## Part D — Concrete recommendations (deepen validation authority)

Ordered by leverage. All are safe against the current data (every proposed constraint passes §A.1's data).

### D.1 Wire the shapes into enforcement (the single highest-value change)
Add to `ggen.toml`:
```toml
[validation]
shacl = ["ontology/tpot-shapes.ttl"]
strict_mode = true
```
Without this, the shapes are inert (`pipeline.rs:1387`). This converts them into a hard gate that aborts
before writing files on any Error-severity violation (`pipeline.rs:1443-1449`). **Deepens authority** —
makes the operator contract un-bypassable.

### D.2 Replace ggen-inert constraints with enforceable equivalents
`sh:nodeKind sh:IRI` and `sh:class …` on `tpot:wrapsAlgorithm`/`tpot:atStage` (`tpot-shapes.ttl:68-70,76-78`)
do nothing on ggen. Until ggen supports them, add constraints ggen *does* honor:
- Keep `sh:minCount 1 / sh:maxCount 1` (already enforced — good).
- Add an explicit datatype/cardinality floor on the **other derived tiers** the shape currently omits:
  `tpot:opSpeedTier` and `tpot:opQualityTier` (both `xsd:integer`, copied from `pi:` — derive-operators
  emits `?s`/`?q` which are `xsd:integer`). A `sh:datatype xsd:integer ; sh:minCount 1` on each catches a
  future formula change that drops or retypes them.
- Add `sh:minCount 1` on `tpot:opCategory` and `tpot:wasmExport` (both emitted by derive-operators,
  `ggen.toml:87,91`; both plain `xsd:string` in source — `algorithms.ttl:23`). These give the OperatorShape
  full coverage of the 8 derived predicates instead of the current 4.

### D.3 Tighten FitnessObjective with the constraints ggen supports
The `sh:in` enumerations on `tpot:metric`/`tpot:direction` already fire (good). Add:
- `tpot:weight`: `sh:datatype xsd:decimal ; sh:minCount 1 ; sh:maxCount 1` (data: `"1.0"^^xsd:decimal`,
  `"0.5"^^xsd:decimal`, `tpot-search-space.ttl:139,148`).
- `tpot:objectiveOrder`: `sh:datatype xsd:integer ; sh:minCount 1`.

### D.4 Add a PipelineStage `sh:pattern` / `sh:in` guard on `forCategory`
ggen honors `sh:in`. Pin `tpot:forCategory` to the 9 frozen category strings:
```
sh:in ( "import_export" "discovery" "object_centric" "discovery_analytics"
        "conformance" "ml_analytics" "prediction" "simulation" "agentic" ) ;
```
This makes the 9-category↔9-stage bijection (CONTRACT §4) a *checked* invariant: a typo'd `forCategory`
would (a) silently drop operators in derive-operators' category JOIN (`ggen.toml:101-102`) **and** (b) now
fail SHACL loudly. **Reduces drift** — the category map can't rot unnoticed.

### D.5 Protect the fitness datatype explicitly (defend §A.2)
The decimal alignment hinges on `0.5` staying a decimal literal. Keep `sh:datatype xsd:decimal` on
`tpot:fitnessScore` (already present, `tpot-shapes.ttl:86`) **and** enable enforcement (D.1) so that if anyone
ever rewrites the formula to integer arithmetic, all 60 operators fail SHACL immediately instead of producing
a silently-retyped graph.

### D.6 (Optional, modeling hygiene) Declare the class
Add `pi:ProcessIntelligenceAlgorithm a owl:Class .` to `algorithms.ttl` so a real SHACL processor's
`sh:class` check (and any OWL reasoner) has a formal class node. Harmless on ggen; correct for portability.

---

## Evidence index (key file:line)

- Shapes: `examples/tpot2-wasm4pm-autoconfig/ontology/tpot-shapes.ttl:26,57,95` (3 NodeShapes); fitnessScore
  decimal `:83-89`; inert nodeKind/class `:67-82`.
- Tier datatypes: `examples/tpot2-wasm4pm-autoconfig/ontology/algorithms.ttl:21-22` (`^^xsd:integer`); 120×
  integer / 0× bare (grep).
- BIND formula: `inference/derive-operators.rq:61`; inlined `ggen.toml:104`.
- ggen SHACL whitelist: `crates/ggen-core/src/validation/shacl.rs:168` (field filter), `:270-303`
  (apply arms — no class/nodeKind).
- Datatype strict-inequality: `crates/ggen-core/src/validation/validator.rs:273-304` (`:282` FILTER).
- SHACL gate on `validation.shacl` non-empty: `crates/ggen-core/src/codegen/pipeline.rs:1386-1389`; runs at
  `pipeline.rs:1467`; aborts at `:1443-1449`.
- `ValidationConfig`: `crates/ggen-core/src/manifest/types.rs:397-419` (`shacl` `:402`, `strict_mode` `:414`).
- strict_mode use: `crates/ggen-core/src/codegen/pipeline.rs:500-505` (0-triple CONSTRUCT only).
- Driver has no `[validation]`: `examples/tpot2-wasm4pm-autoconfig/ggen.toml` (sections at `:19,36,45,68,150,
  237,243,248,252`; shapes only in imports `:41`).
- Receipt impl (SHA-256 + Ed25519): `crates/ggen-core/src/receipt/receipt_impl.rs:10-29,72-77,119-124,
  180-184`.
- Receipt emission + closure: `crates/ggen-cli/src/cmds/sync.rs:541-652` (closure `:582-618`, outputs
  `:620-628`, dry-run skip `:461-469`).
- Oxigraph 0.5.8: `Cargo.toml` (`oxigraph = { version = "0.5.8" }`); evaluator
  `crates/ggen-core/src/graph/core.rs:17`.
- Generated dir absent / verify outputs present: `ls generated/` (absent), `verify/out/` (4 files).
