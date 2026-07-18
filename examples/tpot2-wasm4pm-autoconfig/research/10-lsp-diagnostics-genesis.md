# Dossier 10 — `ggen lsp check` Trip Analysis + Genesis YAWL Pattern Mapping

**Agent:** 10 / 10 · **Date:** 2026-06-20 · **Scope:** (A) Would this project pass
`ggen lsp check`? Per-diagnostic trip table grounded in the real ggen-lsp detector
source. (B) Map the 9-stage linear pipeline to the genesis YAWL (van der Aalst WCP)
pattern system.

> Method: **source-trace only** (CONTRACT.md §9 — no `cargo`/`ggen` in this
> container). Every verdict is cited to a real `.rs` detector line in `ggen-lsp` /
> `ggen-core` and to the real project file it would (or would not) fire on. Nothing
> was executed. Where the documentation (CLAUDE.md, WASM4PM bugs) disagrees with the
> source, **the source wins** and the drift is flagged.

---

## TL;DR

| Question | Verdict |
|----------|---------|
| **Would `ggen lsp check` exit clean (exit 0)?** | **YES — exit 0, gate PASSES.** Zero ERROR-severity diagnostics fire on the as-written project. (`error_count == 0` ⇒ `exit_code() == 0`, `check.rs:90-98`.) |
| **Any WARNINGs?** | **One latent GGEN-TPL-001 ERROR is *possible* on RULE 5** (`sparql_results`), and the E0011/E0013 ORDER-BY checks are **WARNING-only and don't even apply** (the `.rq` files all have ORDER BY; the 2 ORDER-BY-less CONSTRUCTs are inline in `ggen.toml`, never seen by the SPARQL analyzer). See the table. |
| **Does LSP catch agent 08's RULE 5/6 projection mismatch?** | **Mostly NO — and this is the headline finding.** The GGEN-TPL-001 detector is **defeated by the `{% set X = row["X"] %}` aliasing idiom** these templates use: `bracket_keys` adds `X`, then `local_vars` subtracts `X` (same name), so the net "consumed" set collapses. RULE 6 → TPL-001 fires **0 times** (silently wrong, LSP-clean). RULE 5 → fires **at most once**, on `sparql_results`, **not** on the 6 data columns agent 08 flagged. **LSP under-reports vs. agent 08's manual audit.** |
| **43 YAWL patterns?** | **Aspirational.** Only **3** are implemented in `genesis-core-v2` (Sequence WCP-1, Parallel Split WCP-2, "Exclusive Choice" labelled WCP-3). `patterns.rs` is a roadmap stub; Synchronization is a Phase-2 comment. One **WCP-numbering bug** in the crate is flagged below. |

---

# PART A — `ggen lsp check` per-diagnostic trip table

## A.0 How the gate decides pass/fail (the exit-code contract)

`ggen lsp check` runs `check_files_in_root` (`check.rs:322-432`), which runs every
single-file analyzer plus the cross-surface "fold" detectors, then:

- `CheckReport::has_errors()` ⇔ `error_count > 0` (`check.rs:89-91`).
- `exit_code()` = `1` iff `has_errors()`, else `0` (`check.rs:95-98`).
- **Only ERROR-severity diagnostics increment `error_count`** (`check.rs:342-348`,
  and each `fold_*` counts only `severity == ERROR`, `check.rs:483`). **WARNINGs are
  reported but never fail the gate** (`fold_query_002`/`fold_pack_001` add to
  `warning_count`, not `error_count` — `check.rs:648-687`).

So the verdict reduces to: **does any detector emit an ERROR on this project's
files?** Answer below: **no** (with one conditional caveat on RULE 5).

## A.1 The trip table

Legend: **TRIP?** = does the detector emit ≥1 diagnostic on *this* project. **Fails
gate?** = is that diagnostic ERROR-severity (the only kind that flips exit code).

| Code | Detector source (ggen-lsp) | Severity | TRIP on this project? | Fails gate? | Evidence (detector `:line` + our file) |
|------|----------------------------|----------|-----------------------|-------------|----------------------------------------|
| **GGEN-TPL-001** (unbound projection) | `tera_analyzer.rs:536-555` via `detect_tpl_001` `analyzers/mod.rs:51-65` | ERROR | **CONDITIONAL — RULE 5 only, on `sparql_results`**; RULES 1–4,6 clean | **RULE 5 possibly** | See **A.2** — defeated by `{% set %}` for the 6 real columns; only `sparql_results` survives (`search-space-report.md.tera:30`) vs `extract-pipeline-stages.rq:20` |
| **GGEN-OUT-001** (unbound output path) | `tera_analyzer.rs:578-597` via `detect_out_001` `analyzers/mod.rs:69-83` | ERROR | **NO** | No | Every `output_file` is a STATIC literal (`ggen.toml:162,174,…229` = `"ggen.toml"`, `"tpot_config.py"`, …). `consumed_vars` finds no `{{ }}` ⇒ silent by construction (`tera_analyzer.rs:964-972`). |
| **GGEN-RULE-001** (missing query/template file) | `tera_analyzer.rs:613-627` via `detect_rule_001` `analyzers/mod.rs:87-98` | ERROR | **NO** | No | All 6 DRIVER rules point at files that exist: `queries/extract-pareto-pipeline.rq`, `extract-operators.rq`, `extract-pipeline-stages.rq`, `extract-fitness-objectives.rq`; templates `generated-ggen-toml.tera`, `tpot-config-dict.py.tera`, `pipeline-manifest.json.tera`, `search-space-report.md.tera` — all present (Glob of `queries/` + `templates/`). Fires only on `"query file missing:"` / `"template file missing:"` issue prefixes (`tera_analyzer.rs:615`). |
| **GGEN-YIELD-001** (output escapes root) | `tera_analyzer.rs:671-694` via `detect_yield_001` `analyzers/mod.rs:164-175` | ERROR | **NO** | No | No `../` in any `output_file`; all are bare filenames resolved under `output_dir = "generated/"` (`ggen.toml:151`). Lexical skeleton stays inside root (`yield_001_diagnostics` `:679`). NOTE: this detector **does exist and is wired** (`check.rs:401`) — contradicting WASM4PM **BUG-003** (see **A.4**). |
| **GGEN-YIELD-003** (orphaned output, e.g. bare `.rs`) | `tera_analyzer.rs:701-716` via `detect_yield_003` `analyzers/mod.rs:213-224` | ERROR | **NO** | No | Every `output_file` has a static base name (`ggen.toml`, `tpot_config.py`, `pipeline.json`, `SEARCH_SPACE.md`, `STAGE_PLAN.md`, `objectives.json`). None is a bare extension. |
| **GGEN-YIELD-004** (competing authority — two rules, one output) | `tera_analyzer.rs:742-760` via `detect_yield_004` `analyzers/mod.rs:179-209` | ERROR | **NO** | No | All 6 DRIVER `output_file` values are **distinct** (`ggen.toml:162,174,187,198,214,229`). The template-emitted per-stage rules collide only *inside the generated config*, which `ggen lsp check` would only flag if you ran it on `generated/ggen.toml` (which doesn't exist yet — agent 08 §A.2). |
| **GGEN-YIELD-005** (output is a URL) | `tera_analyzer.rs:722-736` via `detect_yield_005` `analyzers/mod.rs:228-239` | ERROR | **NO** | No | No `output_file` starts with `http://`/`https://`. |
| **GGEN-QUERY-002** (`SELECT *` blindspot) | `tera_analyzer.rs:806-822` via `detect_query_002` `analyzers/mod.rs:248-263` | **WARNING** | **NO** | No (warning only) | **Zero `SELECT *`** anywhere — every `.rq` uses explicit projections (CONTRACT §6, BUG-007). `is_select_star` (`tera_analyzer.rs:826-842`) returns false for all 6 queries + 2 inline CONSTRUCTs. |
| **GGEN-PACK-001** (pack source disables checks) | `tera_analyzer.rs:639-657` via `detect_pack_001` `analyzers/mod.rs:273-284` | **WARNING** | **NO** | No (warning only) | No rule uses `{ pack = … }` — all are direct `{ file = … }` (BUG-008/011 avoided). |
| **GGEN-SRC-001** (output in source-caste dir) | `analyzers/mod.rs:102-126` (`source_caste_path_violation`) | ERROR | **NO** (very likely) | No | Outputs land in `generated/` (`ggen.toml:151`), not a flagged source-caste dir. (Detector keys on specific dir names via `source_caste_path_violation`, `toml_analyzer.rs`; `generated/` is not source-caste.) |
| **GGEN-SRC-002/003** (DO-NOT-EDIT banner / source-caste comment in emitted `.rs`) | `source_law_analyzer.rs` via `detect_src_002_003_in_dir` `analyzers/mod.rs:130-155` | ERROR | **NO** | No | Scans `.rs` files in each rule's output dir (`check.rs:698-731`). This project emits **no `.rs`** outputs (outputs are `.toml/.py/.json/.md`); `tpot_config.py` is Python, not scanned. No emitted Rust ⇒ nothing to flag. |
| **GGEN-HARNESS-001** (Cargo `[[test]]`/`[[bench]]` path missing) | `harness_analyzer.rs:harness_mismatch_diagnostics` via `detect_harness_001` `analyzers/mod.rs:35-47` | ERROR | **NO** | No | No `Cargo.toml` in this example dir (it is a ggen example, not a crate). `HarnessIndex::from_root` returns Err ⇒ fold is a no-op (`check.rs:526-537`). |
| **E0010** (`VALUES` in external `.rq`) | `sparql_analyzer.rs:74-85` | ERROR | **NO** | No | No `.rq` contains `VALUES` (`query_contains_values` false for all). |
| **E0011** (CONSTRUCT without ORDER BY) | `sparql_analyzer.rs:88-96` | **WARNING (always)** | **NO** | No | **The two ORDER-BY-less CONSTRUCTs are INLINE in `ggen.toml` `[inference]`** (`ggen.toml:74-142`), **not `.rq` files** — the SPARQL analyzer only runs on `.rq`/`.sparql` law surfaces (`analyzers/mod.rs:364`), so it never sees them. The two `inference/*.rq` files are not even referenced by the manifest. **See A.3.** |
| **E0013** (SELECT without ORDER BY) | `sparql_analyzer.rs:99-107` | **WARNING (always)** | **NO** | No | All 6 `queries/*.rq` end with explicit `ORDER BY` (verified: `extract-pareto-pipeline.rq:86`, `extract-operators.rq:75`, `extract-pipeline-stages.rq:40`, `extract-fitness-objectives.rq:30`, + the two not wired). `query_has_order_by` true ⇒ no E0013. |
| **E0015** (identity CONSTRUCT) | `sparql_analyzer.rs:110-120` | **WARNING** | **NO** | No | Both inline CONSTRUCTs are **non-identity** (head `?operator a tpot:Operator…` ≠ where body; `derive-pareto-dominance` head `?opA tpot:dominates ?opB` ≠ body) — `is_identity_construct` (`:248-283`) false. And again, inline ⇒ analyzer never sees them. |
| **E0024** (Tera syntax error) | `tera_analyzer.rs:142-159` | ERROR | **NO** | No | All 4 DRIVER templates parse (valid `{% for %}`/`{% set %}`/`{% if %}` balance; no `---` frontmatter, BUG-002 avoided). |
| **GGEN-SRC-001..003, GGEN-YIELD-001..005** detector existence | — | — | **All exist & wired** | — | `check.rs:401-422` calls `fold_yield_001/003/004/005` + `fold_src_001` + `fold_src_002_003`. **This refutes WASM4PM BUG-003** (A.4). |

### Net verdict

**`ggen lsp check` exits 0 (clean gate) on this project as written** — **with one
conditional asterisk:** the GGEN-TPL-001 detector *may* emit a single ERROR on
RULE 5's `sparql_results` reference (A.2). If it does, the gate would flip to exit 1.
This is **detector-version-dependent** and is itself a *false positive of a different
kind* than agent 08's *true* mismatch. Both point to the same fix (give RULES 5/6
their own templates), so the recommendation is unaffected.

---

## A.2 GGEN-TPL-001 vs. agent 08's RULE 5/6 finding — the `{% set %}` masking effect (HEADLINE)

Agent 08 (dossier 08 §B) proved by manual audit that **RULE 5 and RULE 6 reuse a
template whose `row["…"]` reads do not match the driving query's SELECT projection**
(RULE 5 reads 6 operator columns the stage query doesn't project; RULE 6 reads 10
stage columns the objective query doesn't project). The task asks: **would LSP catch
that?** I traced the actual detector. **Answer: almost entirely NO**, for a precise,
source-grounded reason.

### The detector's "consumed" computation (`tera_analyzer.rs:432-456`)

`detect_tpl_001` (`analyzers/mod.rs:51-65`) compares `consumed_vars(template)` minus
`entry.selected_vars` (the rule's own SELECT vars, `rule_index.rs:150-151`).
`consumed_vars` is:

1. **`bracket_keys(template)`** — every `row["KEY"]` → `KEY` (`:459-479`).
2. **`{{ … }}` interpolation heads** — `{{ NAME }}`, `{{ row.NAME }}` (`:438-449`).
3. **MINUS `local_vars(template)`** — names bound by `{% for X … %}` and
   `{% set X = … %}` (`:394-420`, subtracted at `:451-455`).

### The masking bug, traced on `search-space-report.md.tera` (RULE 5's template)

The template (`search-space-report.md.tera:17-26`) is built entirely on the
`{% set X = row["X"] %}` idiom:

```
{%- for row in sparql_results %}
{%- set stage_id     = row["stage_id"]     | default(value="") -%}
{%- set operator_id  = row["operator_id"]  | default(value="") -%}
{%- set wasm_export  = row["wasm_export"]  | default(value="") -%}
{%- set speed_tier   = row["speed_tier"]   | default(value="") -%}
{%- set quality_tier = row["quality_tier"] | default(value="") -%}
{%- set fitness_score= row["fitness_score"]| default(value="") -%}
{%- set is_elite     = row["is_elite"]     | default(value="false") -%}
| `{{ stage_id }}` | `{{ operator_id }}` | … | {% if is_elite == "true" %}…{% endif %} |
{%- endfor %}
…
Total operators in search space: {{ sparql_results | length }} …
```

Trace of `consumed_vars`:

- **bracket_keys** = `{stage_id, operator_id, wasm_export, speed_tier, quality_tier,
  fitness_score, is_elite}` (the 7 RHS `row["…"]`).
- **interpolation heads** = the same 7 (line 25 bare `{{ … }}`) **+ `sparql_results`**
  (line 30 `{{ sparql_results | length }}`; the head before `|` is `sparql_results`).
- **local_vars** = `{row}` (the `for`) **+ the 7 `set` LHS names** = exactly those same
  7. These are **subtracted**.
- **Net consumed = `{sparql_results}`** — the 7 data columns are *cancelled out*
  because each appears as both a `row["X"]` bracket key **and** a `set X` local with the
  identical name.

RULE 5's query `extract-pipeline-stages.rq:20` projects
`{stage_order, stage_id, stage_label, for_category, operator_count}`. `sparql_results`
is not among them ⇒ **GGEN-TPL-001 fires exactly once, on `sparql_results`** — a
**spurious** hit (`sparql_results` is the always-present batch collection, not a SELECT
var; the detector has no concept of it). It does **NOT** fire on `operator_id`,
`wasm_export`, `speed_tier`, `quality_tier`, `fitness_score`, `is_elite` — **the six
columns agent 08 correctly flagged as the real defect are invisible to the detector.**

> (Whether even the `sparql_results` hit lands depends on the detector treating
> `sparql_results` as a consumable identifier; `leading_var` only excludes `loop`,
> not `sparql_results` — `:492,507` — so on the current source it **would** be
> reported. Either way the 6 real columns are masked.)

### RULE 6's template (`pipeline-manifest.json.tera`) — TPL-001 fires ZERO times

Same idiom (`pipeline-manifest.json.tera:23-46`): `{% for row in sparql_results %}`
then 10 `{% set X = row["X"] %}` then `{{ X }}`. **Crucially there is no bare
`{{ sparql_results }}`** (it appears only inside `{% for … %}`, a `{%` block, which
`consumed_vars` does not scan for interpolation). So:

- bracket_keys = the 10 stage columns; interpolation heads = the same 10 (+ `loop`
  via `{% if not loop.last %}`, excluded by `leading_var` `:492`).
- local_vars subtracts all 10 `set` LHS + `row`.
- **Net consumed = ∅** ⇒ **`unbound_projection_diagnostics` returns empty ⇒ no
  GGEN-TPL-001 at all** for RULE 6, even though *every single one* of its template's
  reads is unprojected by `extract-fitness-objectives.rq`. **Silent, LSP-clean,
  wrong** — exactly agent 08's "doubly wrong" output, completely undetected.

### Why this matters

The `{% set local = row["local"] %}` pattern — used pervasively in this project's
batch templates and recommended by CONTRACT.md §7 — is a **blind spot in the
GGEN-TPL-001 detector**: it cancels the very `row["…"]` evidence the detector relies
on. The detector catches the *naive* `{{ row["title"] }}` form (its test fixtures,
`check.rs:886`, `tera_analyzer.rs:873`) but **not** the aliased form these templates
use. So `ggen lsp check` gives a **false sense of safety** here: it would pass RULE 6
clean and (at most) misfire on RULE 5's `sparql_results`. **LSP is not a substitute
for agent 08's projection audit; agent 08 caught what LSP cannot.**

---

## A.3 The two inference CONSTRUCTs lack ORDER BY — does it matter? (E0011)

The task flags that the 2 inference CONSTRUCTs (`derive-operators`,
`derive-pareto-dominance`) lack ORDER BY. Two independent facts make this a
**non-issue for `ggen lsp check`**, and a **non-issue for `ggen sync` too** (given
this manifest):

1. **`ggen lsp check` never sees them.** They are **inlined** in `ggen.toml`
   `[inference]` as `construct = """…"""` (`ggen.toml:74-142`), not `.rq` law-surface
   files. The SPARQL analyzer (E0011 source) only runs on `FileType::Sparql`
   (`analyzers/mod.rs:364` → `.rq`/`.sparql`). The headless gate has **no cross-surface
   fold for inline inference CONSTRUCTs** (no `fold_e0011` exists — `check.rs:369-422`
   lists every fold; none reads `manifest.inference.rules`). So E0011 cannot fire on
   them via `ggen lsp check`. *(The orphan `inference/derive-*.rq` files on disk, if
   they exist and have no ORDER BY, *would* be scanned as standalone `.rq` surfaces and
   would each emit an **E0011 WARNING** — but WARNING ≠ gate failure, `check.rs:342-348`,
   and per CONTRACT §8 the inline copies are authoritative.)*

2. **Even at `ggen sync`, strict_mode is OFF.** E0011/E0013 are only escalated to a
   hard `Err` when `manifest.validation.strict_mode == true`
   (`ggen-core/manifest/validation.rs:94-100` for CONSTRUCT, `:196-201` for SELECT).
   This project's `ggen.toml` has **no `[validation]` section**, and `strict_mode`
   defaults to `false` (`types.rs:412-414`, `#[serde(default)]` bool). So the missing
   ORDER BY on the inline CONSTRUCTs would produce only a `log::warn!`
   (`validation.rs:101-104`), never an error.

**Determinism note (real, but orthogonal):** a CONSTRUCT without ORDER BY *is*
non-deterministic in principle. Here it is harmless because (a) CONSTRUCT output is an
unordered triple *set* fed back into the graph (not row-ordered output), and (b) the
downstream SELECTs that *do* order (`extract-*.rq … ORDER BY`) impose the determinism.
The INTEGRATION_REPORT's "byte-identical re-runs" claim (`test_tpot2_autoconfig.py`)
is consistent with this.

---

## A.4 SPEC↔IMPL drift discovered: WASM4PM **BUG-003 is STALE**

`docs/jira/WASM4PM-DISCOVERED-BUGS.md:31-36` (BUG-003) asserts: *"GGEN-YIELD-001..005
codes have no detector in ggen-lsp … No YIELD family detector exists … authors
checking `ggen lsp check` see no YIELD violations."* **This is no longer true in the
current source.** The YIELD family is fully implemented and wired into the headless
gate:

- Detectors: `yield_001_diagnostics` (`tera_analyzer.rs:671`), `yield_003`
  (`:701`), `yield_004` (`:742`), `yield_005` (`:722`).
- Cross-surface wiring: `detect_yield_001/003/004/005` (`analyzers/mod.rs:164-239`),
  folded into the gate at `check.rs:401-407` (all ERROR severity, all increment
  `error_count`).
- Living-loop tests exist: `tests/ggen_yield_001_living_loop.rs`.

**Implication for *this* project:** the claim that a YIELD-001 back-write "passes lsp
check silently" does **not** apply — if this project's `output_file` ever escaped root,
`ggen lsp check` *would* now flag it ERROR and fail. (It doesn't escape, so it's moot,
but the safety net is real.) **Recommend updating BUG-003 to RESOLVED/stale.**

---

# PART B — Genesis YAWL (van der Aalst WCP) pattern mapping

## B.0 Ground truth: what is actually implemented (not 43)

The CLAUDE.md / crate docs advertise "43 YAWL patterns," but the **implemented**
surface is **3 control-flow patterns**, all in `genesis-core-v2/src/lib.rs`:

| Impl struct | `name` | `yawl_pattern_id` | `id` | Source |
|-------------|--------|-------------------|------|--------|
| `SequencePattern` | "Sequence" | `WCP-1` | 1 | `genesis-core-v2/src/lib.rs:72-116` |
| `ParallelSplitPattern` | "Parallel Split" | `WCP-2` | 3 | `genesis-core-v2/src/lib.rs:177-218` |
| `ExclusiveChoicePattern` | "Exclusive Choice" | `WCP-3` | 2 | `genesis-core-v2/src/lib.rs:119-174` |

- `genesis-core-v2/src/patterns.rs` is a **roadmap stub** (comments only: "Phase 1: 3
  core patterns … Phase 2: Synchronization, Deferred choice …" `:1-23`). **No
  Synchronization, no Simple Merge implemented.**
- `genesis-schema-v2/src/lib.rs` provides the *type system* (`PatternMetadata` with
  `yawl_pattern_id: String`, `is_control_flow: bool`, `:160-169`; `PatternRegistry`
  `:171-207`) but registers only a single "Sequence"/WCP-1 in a **test** (`:234-247`).
  There is **no shipped RDF/TTL ontology enumerating the 43 patterns** in this crate
  (`to_turtle` `:151-157` builds TTL at runtime from hand-added triples; no static
  43-pattern catalogue file exists — Glob of `genesis-schema-v2/**` returns only
  `lib.rs` + `Cargo.toml`).

> **WCP-numbering bug flagged (genesis-core-v2):** the crate labels **"Exclusive
> Choice" as `WCP-3`** (`lib.rs:131`). In van der Aalst's canonical Workflow Control-flow
> Patterns, **WCP-3 is *Synchronization*** and **Exclusive Choice is WCP-4** (WCP-1
> Sequence, WCP-2 Parallel Split, WCP-3 Synchronization, WCP-4 Exclusive Choice, WCP-5
> Simple Merge). So the crate's `ParallelSplit=WCP-2` and `Sequence=WCP-1` are correct,
> but `ExclusiveChoice=WCP-3` is **mis-numbered** (should be WCP-4), and the genuine
> WCP-3 (Synchronization) is **absent**. This is a real defect in the genesis crate,
> not in our generator — flagged for completeness since the mapping below uses the
> *correct* canonical numbers.)

## B.1 Mapping the 9-stage pipeline to WCP control-flow patterns

Our pipeline (CONTRACT §4) is a strict linear chain of 9 stages, each producing exactly
one elite operator (`extract-pareto-pipeline.rq`, 9 rows, one per `stage_order`). Mapped
to the WCP canon (and to the 3 genesis-implemented patterns where one exists):

| Pipeline relation | Canonical WCP | genesis impl? | Justification |
|-------------------|---------------|---------------|---------------|
| ingest → discover → discover_oc → analyze → conform → learn → predict → simulate → orchestrate (the whole chain, ordered by `tpot:stageOrder` 1..9) | **WCP-1 Sequence** | ✅ `SequencePattern` (`genesis-core-v2/lib.rs:72`, `yawl_pattern_id="WCP-1"`) | Each stage's output is the next stage's input; total order enforced by `ORDER BY ?stage_order` (`extract-pareto-pipeline.rq:86`). This is the textbook Sequence pattern: "an activity is enabled after the completion of the preceding activity." |
| per-stage **operator selection** (argmax fitness within a stage; CONTRACT §5) | **WCP-4 Exclusive Choice** (the genesis crate's mis-numbered "WCP-3") | ✅ `ExclusiveChoicePattern` (`genesis-core-v2/lib.rs:119`) | Within each stage, exactly **one** of N candidate operators is chosen (the elite), by a condition over fitness. `extract-pareto-pipeline.rq:63-75` `FILTER NOT EXISTS` selects the single winner — an XOR-split over the operator candidates. The drift-monitor branch (predict + `detect-drift.rq`) is also a conditional fork. |
| the **drift-monitor** sibling rule (predict stage + `detect-drift.rq`, emitted alongside predict; INTEGRATION_REPORT row "+") | **WCP-2 Parallel Split** (AND-split) — *aspirationally* | ✅ `ParallelSplitPattern` (`genesis-core-v2/lib.rs:177`) | The drift monitor runs *concurrently with / alongside* the predict stage rather than replacing it (`generated-ggen-toml.tera:66-76`, "Emitted as a sibling of the predict-stage rule"). That sibling-emission is the closest thing to an AND-split this otherwise-linear pipeline has. **But see B.2 — our pipeline does NOT actually run these in parallel.** |

## B.2 Could discover_oc / analyze be parallel branches (WCP-2/WCP-3)? — NO, as built

The task asks whether `discover_oc` and `analyze` could be parallel branches
(WCP-2 Parallel Split + WCP-3 Synchronization). **As authored, they are strictly
sequential, not parallel:**

- They carry consecutive `stageOrder` (discover_oc = 3, analyze = 4; CONTRACT §4),
  and the chosen pipeline emits them in that order (`extract-pareto-pipeline.rq …
  ORDER BY ?stage_order`). There is **no fork/join construct** in the ontology or the
  queries — no `tpot:parallelWith`, no synchronization barrier predicate exists in the
  frozen `tpot:` vocabulary (CONTRACT §5 lists only `stageOrder`/`atStage`/etc.).
- **WCP-2 (Parallel Split / AND-split)** would require a single point spawning multiple
  concurrent threads. **WCP-3 (Synchronization / AND-join)** would require a barrier
  merging them. Neither appears: the pipeline is a single thread of control (one
  operator per stage, total order).

**Where parallelism is *latent* and could be modelled (recommendation, not current
state):** discover_oc (object-centric discovery) and analyze (discovery_analytics)
both consume the *same* discovered model from the `discover` stage and produce
independent artifacts — a natural **WCP-2 Parallel Split** after `discover`, joined by
a **WCP-3 Synchronization** before `conform`. Likewise the drift monitor is genuinely
concurrent with predict (WCP-2). To *model* this lawfully you would need (a) a
`tpot:Synchronization` / `tpot:parallelBranch` vocabulary, and (b) the genesis crate to
*actually implement* WCP-3 (it doesn't — `patterns.rs:12` lists Synchronization as an
unbuilt Phase-2 item). **Today the pipeline is honestly WCP-1 Sequence with a per-stage
WCP-4 Exclusive Choice and one quasi-concurrent drift sibling.**

---

# PART C — Recommendations

## C.1 On `ggen lsp check` (Part A)

1. **The gate passes today (exit 0).** No action is *required* to clear `ggen lsp
   check`. But "passes lsp check" is **not** evidence of projection correctness here —
   see C.2.

2. **Fix RULES 5 & 6 anyway (corroborates agent 08).** The GGEN-TPL-001 detector is
   **defeated by the `{% set X = row["X"] %}` aliasing** (A.2), so it does **not**
   catch agent 08's true mismatch (RULE 6: 0 hits; RULE 5: at most a spurious
   `sparql_results` hit). Agent 08's recommended fix is therefore **independently
   necessary** and not redundant with LSP:
   - **RULE 5** → give it `templates/stage-plan.md.tera` reading
     `stage_order, stage_id, stage_label, for_category, operator_count` (agent 08 §B.1).
   - **RULE 6** → give it `templates/objectives.json.tera` reading
     `objective_order, objective_id, objective_name, metric, direction, weight`
     (agent 08 §B.2).
   This *deepens authority* (objectives become query-derived, not Tera literals) and
   *reduces drift* (templates stop reading columns their query never projects).

3. **Mind the possible RULE 5 `sparql_results` false-positive.** If you run
   `ggen lsp check` and it *does* flag GGEN-TPL-001 on `search-space-report.md.tera`
   for `sparql_results`, that is the detector mis-treating the batch collection as a
   SELECT var. Fixing RULE 5 per C.2 removes the spurious hit (the dedicated
   stage-plan template would not have a bare `{{ sparql_results }}` count line over a
   stage query, or it would be acknowledged). Do **not** "fix" it by deleting the
   `{{ sparql_results | length }}` line from RULE 4's legitimate use.

4. **ORDER BY on the inline CONSTRUCTs is optional but cheap.** `ggen lsp check` will
   never flag it (inline ⇒ unseen, A.3) and `ggen sync` won't either (strict_mode off).
   **If you ever set `[validation] strict_mode = true`** (Vision-2030 determinism
   posture), the inline CONSTRUCTs would then hard-fail at sync with E0011
   (`validation.rs:94-100`). To be strict-mode-clean *pre-emptively*, append a
   deterministic `ORDER BY` to both inline CONSTRUCTs' WHERE results (e.g. ORDER BY the
   bound `?algorithm_id` / `?idA ?idB`). Low cost, future-proofs strict mode.

5. **Documentation drift to fix (not code):**
   - **WASM4PM BUG-003 is stale** (A.4): YIELD-001..005 detectors now exist and are
     wired (`check.rs:401-407`). Mark it RESOLVED so future authors don't assume YIELD
     violations pass silently.

## C.2 On the genesis YAWL mapping (Part B)

6. **State the mapping honestly in docs:** our pipeline is **WCP-1 Sequence** (the
   9-stage chain) + **WCP-4 Exclusive Choice** (per-stage elite argmax) + one
   quasi-concurrent drift sibling. It is **not** a parallel/synchronized workflow.
   Avoid claiming WCP-2/WCP-3 usage unless the `tpot:` vocabulary and the genesis
   crate gain real fork/join support.

7. **If parallelism is wanted:** model `discover_oc ∥ analyze` (and `predict ∥ drift`)
   as an explicit **WCP-2 split / WCP-3 join** — which requires (a) new `tpot:`
   predicates (`tpot:parallelBranch`, `tpot:synchronizesAt`) and (b) the genesis crate
   to implement WCP-3 Synchronization (currently an unbuilt stub, `patterns.rs:12`).

8. **Report the genesis WCP-numbering bug upstream:** `ExclusiveChoicePattern` is
   labelled `WCP-3` (`genesis-core-v2/lib.rs:131`) but canonically Exclusive Choice is
   **WCP-4**; WCP-3 is Synchronization (absent). This is a defect in genesis-core-v2,
   independent of our generator, but it means any code keying off `yawl_pattern_id ==
   "WCP-3"` to mean "synchronization" would silently bind to Exclusive Choice. Flag it.

---

## Evidence index (every cited source:line)

**ggen-lsp gate & detectors:**
- Exit-code contract: `check.rs:89-98` (`has_errors`/`exit_code`), `:342-348`
  (error_count counts only ERROR), `:483` (fold counts only ERROR), `:648-687`
  (QUERY-002/PACK-001 → warning_count).
- Fold wiring (all detectors run by the gate): `check.rs:369-422`.
- GGEN-TPL-001: `tera_analyzer.rs:432-456` (`consumed_vars`), `:459-479`
  (`bracket_keys`), `:394-420` (`local_vars`), `:536-555`
  (`unbound_projection_diagnostics`); `analyzers/mod.rs:51-65` (`detect_tpl_001`);
  `rule_index.rs:150-151` (`selected_vars`); `analyzers/mod.rs:311-355`
  (`select_projection_vars`).
- GGEN-OUT-001: `tera_analyzer.rs:578-597`, `:964-972` (static-path silent);
  `analyzers/mod.rs:69-83`.
- GGEN-RULE-001: `tera_analyzer.rs:613-627`; `analyzers/mod.rs:87-98`.
- GGEN-YIELD-001/003/004/005: `tera_analyzer.rs:671-694/701-716/742-760/722-736`;
  `analyzers/mod.rs:164-239`; gate wiring `check.rs:401-407`.
- GGEN-QUERY-002: `tera_analyzer.rs:806-842`; `analyzers/mod.rs:248-263`;
  `check.rs:648-664`.
- GGEN-PACK-001: `tera_analyzer.rs:639-657`; `analyzers/mod.rs:273-284`.
- GGEN-SRC-001/002/003: `analyzers/mod.rs:102-126/130-155`; `check.rs:689-731`.
- E0010/E0011/E0013/E0015: `sparql_analyzer.rs:74-85/88-96/99-107/110-120` (all
  WARNING for ORDER-BY/identity), `:248-283` (`is_identity_construct`).
- E0024 (Tera syntax): `tera_analyzer.rs:142-159`.
- Single-file analyzer dispatch (only `.rq`/`.sparql`/`.tera`/`.ttl`/`ggen.toml`):
  `analyzers/mod.rs:357-375`; `check.rs:786-813` (law-surface discovery).

**ggen-core strict-mode escalation:**
- E0011 CONSTRUCT → Err only if strict: `manifest/validation.rs:94-100` (warn at
  `:101-104`).
- E0013 SELECT → Err only if strict: `manifest/validation.rs:184-201`.
- `strict_mode` default false: `manifest/types.rs:412-414` (`#[serde(default)]` bool),
  `:396-399` (`ValidationConfig` Default).

**Our project files:**
- DRIVER: `ggen.toml:36-52` (ontology/prefixes), `:68-143` (inline CONSTRUCTs, no
  ORDER BY), `:150-151` (`output_dir="generated/"`), `:158-229` (6 rules, RULE 5
  reuse `:213`, RULE 6 reuse `:227`), no `[validation]` section.
- Queries (all ORDER BY, all explicit projection): `extract-pareto-pipeline.rq:40-42,86`;
  `extract-operators.rq:32-34,75`; `extract-pipeline-stages.rq:20,40`;
  `extract-fitness-objectives.rq:20,30`.
- Templates (the `{% set %}` masking idiom): `search-space-report.md.tera:17-30`
  (RULE 5 template, bare `{{ sparql_results | length }}` at `:30`);
  `pipeline-manifest.json.tera:23-46` (RULE 6 template, no bare `{{ sparql_results }}`).
- `generated-ggen-toml.tera:66-76` (drift sibling emission).

**genesis crates:**
- 3 implemented patterns: `genesis-core-v2/src/lib.rs:72-116` (Sequence/WCP-1),
  `:177-218` (Parallel Split/WCP-2), `:119-174` (Exclusive Choice mis-labelled WCP-3).
- Stub roadmap (Synchronization unbuilt): `genesis-core-v2/src/patterns.rs:1-23`.
- Pattern type system + registry: `genesis-schema-v2/src/lib.rs:160-207`; single
  test pattern `:234-247`; no static 43-pattern TTL catalogue (Glob: only `lib.rs`).

**Documentation drift:**
- WASM4PM BUG-003 (stale — YIELD detectors now exist):
  `docs/jira/WASM4PM-DISCOVERED-BUGS.md:31-36`.

**Sibling corroboration:**
- `research/08-generated-output-audit.md` §B (RULE 5/6 projection mismatch — the
  ground truth this dossier shows LSP cannot catch).
