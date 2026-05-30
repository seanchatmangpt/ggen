# GGEN-RULE-001 Pre-Implementation Inventory

**Species:** GGEN-RULE-001 — `unbound_rule_file` (dangling rule binding)
**Date:** 2026-05-30
**Repo:** ggen @ /Users/sac/ggen, branch `main`, HEAD `53a4d1c2`
**Role:** Species/Route Architect (READ-ONLY survey; this doc is the only write).
**Mandate cite:** CONSOLIDATE-002 (species-driven publish loop), coding-agent-mistakes.md §1.3 (fail-open), §6 (deepen authority / reduce drift).

---

## 1. Discovery Gate Verdict: READY (real gap)

**Question (mandated FIRST):** Is `RuleIndexEntry.issues` (or any existing path)
already published as a live LSP diagnostic / through `observe_diagnostics` for a
missing rule file?

**Answer: NO. It is collected-but-not-published — a silent author-time failure.**

### Evidence (mechanically verified)

1. **`issues` is populated for missing files.** `crates/ggen-lsp/src/rule_index.rs`
   `RuleIndexEntry::from_rule_with_overlay`:
   - Line 88-95: `QuerySource::File` read error → `issues.push("query file missing: {path} ({err})")`, `query_content = ""`.
   - Line 107-110: `TemplateSource::File` read error → `issues.push("template file missing: {path}")`, `template_content = None`.
   The read goes through `read_overlay_or_disk` (line 156-161): overlay hit, else `std::fs::read_to_string` — so existence is overlay-aware.

2. **`issues` is NEVER read in production.** `grep -rn '\.issues' crates/ggen-lsp/src/`
   outside `rule_index.rs` returns only `crates/ggen-lsp/src/project_index.rs`
   lines 195, 197, 235, 237, 295, 299 — **all inside `#[cfg(test)]`** (the
   `ProjectIndex` unit tests at lines 183-299). There is no conversion of `issues`
   to a `tower_lsp::Diagnostic`, no path into `check.rs`, `state.rs`, or
   `analyzers/mod.rs`.

3. **The two existing ProjectIndex species deliberately SKIP missing-file rules.**
   - `detect_tpl_001` (`analyzers/mod.rs:62-64`): `let Some(template) = entry.template_content.as_deref() else { continue; };` — a missing template (`template_content == None`) is skipped. The doc comment (mod.rs:51-52): *"a missing template is an index-level issue (Agent 1's `issues`), not GGEN-TPL-001."*
   - `detect_out_001` (`analyzers/mod.rs:96-98`): `if entry.selected_vars.is_empty() { continue; }` — a missing **query** file yields empty `query_content` → empty `selected_vars` → skipped. Comment (mod.rs:97): *"SELECT * / missing query → no lawful comparison."*
   - `check.rs:411-418` `fold_tpl_001` reaffirms (doc 408-410): *"A missing template ... stays a `RuleIndexEntry::issues` index problem, never GGEN-TPL-001."* The headless test `missing_template_is_not_reclassified_as_tpl_001` (check.rs:761-800) **asserts** this skip.

**Conclusion:** A `[[generation.rules]]` entry whose `query={file=...}` or
`template={file=...}` points at a non-existent file is a **fail-open** defect
(coding-agent-mistakes.md §1.3): the LSP continues, the existing species step
around it by design, the only record is a non-fatal `issues` string that no
channel surfaces. This is the FOUNDATIONAL binding-integrity check TPL/OUT
presuppose (they assume the bound files exist). **READY** — not redundant.

---

## 2. Canonical Values (fixed by this inventory; MUST NOT vary)

| Field | Value |
|-------|-------|
| `code` | `GGEN-RULE-001` |
| `failure_class` | `unbound_rule_file` |
| `surfaces` | `["ggen.toml"]` |
| `severity_policy` | `error` |
| `route` (species slug) | `source_law_repair` |
| `origin` | `ark-covenant / living-lsp RULE-001` |
| `actuation_boundary` | `inspect_only` |
| `receipt_requirement` | `diagnostic_receipt` |
| `detector_active` | `true` |
| Route id (registry) | `source-law.bind-rule-file` |
| `RepairFamily` | `RuleFileMissing` (NEW exclusive variant) |
| Diagnostic anchor | the rule's `ggen.toml` (`entry.manifest_path`) |

### Why a NEW `RepairFamily` variant (`RuleFileMissing`)

`select_for_diagnostic` keys ONLY on family, so each living species must own its
family EXCLUSIVELY to avoid cross-code route contamination (registry.rs:127-128).
The currently-unseeded families are `PublicVocabViolation` and `ShaclFailure` —
both semantically wrong for "a rule-bound file is missing" and both reserved by
their doc comments for RDF/SHACL concerns. Reusing `LoadFailure` would STEAL the
GGEN-OUT-001 route; `DanglingReference` would steal TPL-001; `TemplateFailure`
is multi-seeded. Therefore RULE-001 introduces `RepairFamily::RuleFileMissing`
(model.rs) and owns it exclusively — the same discipline TPL/HARNESS/OUT each
followed.

---

## 3. The Defect (precise)

`ggen sync` resolves each `[[generation.rules]]` query/template `{file=...}`
relative to the manifest dir (`ggen_core::manifest` + `rule_index.rs:85,104`).
A missing file means μ-pipeline load (`pipeline.load`/`pipeline.extract`) has no
SPARQL to run or no Tera to render — the rule cannot manufacture. At author time
this is silent today. RULE-001 makes it a live, routed, receipted refusal.

**Detector scope (exact):** RULE-001 fires ONLY for the two file-missing issue
classes, identified by stable prefix on `entry.issues`:
- `"query file missing:"` (rule_index.rs:90)
- `"template file missing:"` (rule_index.rs:108)

It MUST NOT fire for `"unsupported template source for MVP:"` (Git/Package,
rule_index.rs:114,122) or the `"SELECT *"` info issue — those are not
missing-file defects. Matching by prefix keeps the detector coupled to the
authoritative `issues` channel (deepens authority: the silent channel now has
exactly one lawful consumer that publishes it).

---

## 4. Living-Loop Wiring (the consolidated loop; cite CONSOLIDATE-002)

RULE-001 is a `ProjectIndex`-derived species like TPL/OUT. Adding it is, per
CONSOLIDATE-002, **a registry entry + a detector + one descriptor-array entry +
one fold + one route** — NO bespoke publish branch.

| Surface | Change | Anchor |
|---------|--------|--------|
| `route/diagnostic_species.rs` | 4th `DiagnosticSpecies` entry (active); count test 3→4 | — |
| `route/model.rs` | `RepairFamily::RuleFileMissing` variant | — |
| `route/registry.rs` | `family_of_code("GGEN-RULE-001") => RuleFileMissing`; seed route `source-law.bind-rule-file` | — |
| `analyzers/tera_analyzer.rs` (or new `rule_analyzer.rs`) | `pub const GGEN_RULE_001` + pure `unbound_rule_file_diagnostics(issues, …)` | — |
| `analyzers/mod.rs` | `detect_rule_001(project) -> Vec<(PathBuf, Vec<Diagnostic>)>` | `entry.manifest_path` (ggen.toml) |
| `check.rs` | `fold_rule_001(root, files, registry)` + one call in `check_files_in_root`, appended LAST | ggen.toml |
| `state.rs` | `detect_rule_001_for`; 4th `Species` in the descriptor array, appended LAST; 4th close-doc reconcile (rides `tpl_is_trigger`) | ggen.toml |

**ORDERING CONSTRAINT (critical, behavior-preserving):** RULE-001 MUST be the
LAST species in both the `analyze_and_observe` descriptor array (state.rs:560-579)
and the `check_files_in_root` fold sequence (check.rs:368-385). The
CONSOLIDATE-002 sequence-equivalence golden
(`tests/consolidate_002_sequence_equivalence.rs`) asserts the exact OCEL event
order for the `consolidate_002_multispecies` fixture, which has NO missing rule
files (so RULE-001 emits zero events there). Appending RULE-001 after OUT keeps
TPL→HARNESS→OUT byte-identical and adds nothing to that fixture's sequence — the
golden stays green WITHOUT editing it.

**Trigger gate:** RULE-001 shares `tpl_is_trigger` (`.tera` / `.rq` / `ggen.toml`),
because the defect lives in the `ggen.toml` binding and is cleared by creating a
`.tera`/`.rq` file — both are TPL-trigger surfaces. Disjoint from HARNESS
(`Cargo.toml`/`Makefile.toml` basenames).

---

## 5. The Route (source-law; never emitted output)

`RouteId("source-law.bind-rule-file")`, family `RuleFileMissing`, all
`EditTemplate::NoOp` (advisory; `inspect_only`). Two source-law steps:

1. **`create-missing-rule-file`** — *"Create the missing query/template file at
   the path the ggen.toml rule binds (source law) — never fabricate generated
   output."*
2. **`fix-rule-file-path`** — *"Or correct the `query`/`template` `{file=...}`
   path in the ggen.toml rule to point at an existing source-law file."*

NO step references an emitted-output path (`out/`, `output/`, `dist/`, `gen/`,
`emitted`). The fix is: create the source file, or fix the path. Mirrors the
TPL/OUT/HARNESS source-law doctrine exactly.

---

## 6. RED Living-Loop Proof (Chicago TDD; real fixtures, real log)

New integration test `tests/ggen_rule_001_living_loop.rs`, mirroring
`ggen_out_001_living_loop.rs` (anchors on ggen.toml, identical OCEL chain shape).
Fixture root `tests/fixtures/ggen_rule_001_living_loop/{invalid_project,valid_project}`.
The existing `tests/fixtures/ggen_tpl_001/missing_template_file/` is a ready-made
invalid case (rule binds `templates/missing.tera`, which does not exist) and is
reused/copied as the invalid fixture seed.

1. **LIVE RAISE** — invalid fixture (rule binds a missing `.tera`/`.rq`) →
   `check_files_in_root` surfaces ≥1 `GGEN-RULE-001` ERROR anchored on `ggen.toml`,
   `error_count >= 1`, `has_errors()`.
2. **INDEPENDENCE (no leak)** — invalid RULE fixture raises ZERO GGEN-TPL-001,
   ZERO GGEN-OUT-001, ZERO GGEN-HARNESS-001 (the missing file makes TPL/OUT skip
   by design; HARNESS basenames disjoint). Sanity: RULE-001 count ≥1 (non-vacuous).
3. **LIVE CLEAR** — copy fixture to TempDir, write the missing file → re-check →
   ZERO GGEN-RULE-001 (the `issues` entry disappears). And TPL/OUT do NOT
   spuriously appear (the now-present file's vars are still lawful).
4. **ROUTE METADATA** — `select_for_diagnostic(rule_001_diag)` → `source-law.bind-rule-file`,
   `Provenance::Seeded`, every step title references a source-law surface
   (ggen.toml / query / template), NONE references a forbidden emitted-output marker.
5. **NO ARTIFACT** — gate over invalid fixture never materializes the rule's
   `output_file` dir (read-only).
6. **OCEL CHAIN (6-link)** — `analyze_and_observe` on the `ggen.toml` URI (a
   `tpl_is_trigger`) RAISES GGEN-RULE-001; create the missing file + re-analyze
   CLEARS it through the SAME `observe_diagnostics`; read the EXTERNAL
   `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` and assert all of
   `DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied →
   GatePassed → ReceiptEmitted` appear on a line naming the `ggen.toml` + `GGEN-RULE-001`.
7. **VALID FIXTURE** — both files present → ZERO GGEN-RULE-001, gate passes.
8. **CROSS-SPECIES BARRIERS** — the existing TPL/HARNESS/OUT living-loop tests are
   NOT edited; add an assertion in the RULE test that a TPL/OUT/HARNESS invalid
   fixture raises ZERO GGEN-RULE-001 (their bound files exist), proving symmetry.

---

## 7. ALIVE / FAKE-LIVE / BLOCKED definitions

**ALIVE (acceptance):**
- A rule with a missing `query`/`template` file raises GGEN-RULE-001 live
  (`analyze_and_observe`, anchored on ggen.toml) AND headless (`check.rs` fold,
  `error_count++`).
- The repair route is source-law (create the file / fix the path) — NEVER
  fabricates emitted output, NEVER auto-creates a file (NoOp/advisory).
- Creating the missing file clears it through `observe_diagnostics` with the full
  6-link OCEL chain on the EXTERNAL log; clears are residual-preserving.
- NO regression to TPL/HARNESS/OUT (their assertions untouched; sequence golden green).
- NO cross-species leakage: a RULE fixture raises 0 TPL/OUT/HARNESS and vice-versa.
- Species registry now 4; count test updated 3→4; `clippy -p ggen-lsp --no-deps -- -D warnings` = 0; `fmt` clean.

**FAKE-LIVE (reject):**
- Detector relies on the silent `issues` channel WITHOUT publishing it as a
  Diagnostic (i.e. just reading `issues` into a log, not into the published set).
- No real existence check (e.g. hardcoding "file X missing" rather than driving
  the overlay-aware `read_overlay_or_disk` result via `issues`).
- OCEL chain incomplete (any of the 6 links absent) or chain forged.
- Blunt empty clear (clearing republishes `[]` instead of residual single-file diags).
- RULE-001 inserted mid-array, reordering TPL→HARNESS→OUT and breaking the golden.

**BLOCKED-redundant (would have halted; did NOT apply):**
- Missing rule files already surface as a living diagnostic. **Refuted in §1** —
  `issues` is `#[cfg(test)]`-only consumed; no published path exists.

---

## 8. Self-Check (coding-agent-mistakes.md §6)

- **Deepens authority:** the authoritative binding-integrity check that TPL/OUT
  presuppose now exists and fails LOUD (was fail-open §1.3). The previously-dead
  `RuleIndexEntry.issues` channel gains exactly one lawful publishing consumer.
- **Reduces drift:** a `ggen.toml` rule whose bound file vanished now produces a
  receipted refusal instead of a silent `issues` string the proof object never saw.
- **Q3 negative path:** delete `templates/missing.tera`'s peer (or bind a
  nonexistent `.rq`) → `check_files_in_root` exits with `error_count >= 1`,
  `has_errors() == true`. Create the file → clears.

**END OF PRE-INVENTORY — verdict READY.**
