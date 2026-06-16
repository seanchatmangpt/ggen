<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [BUG-001 — OntologyConfig silently ignores unknown fields (SILENT-DATA-LOSS)](#bug-001--ontologyconfig-silently-ignores-unknown-fields-silent-data-loss)
- [BUG-002 — Frontmatter machinery unreachable from `ggen sync` (SPEC-IMPL-DRIFT)](#bug-002--frontmatter-machinery-unreachable-from-ggen-sync-spec-impl-drift)
- [BUG-003 — GGEN-YIELD-001..005 codes have no detector in ggen-lsp (SPEC-IMPL-DRIFT)](#bug-003--ggen-yield-001005-codes-have-no-detector-in-ggen-lsp-spec-impl-drift)
- [BUG-004 — mode="Create" doc says "fail if exists" but implementation skips (FOOTGUN)](#bug-004--modecreate-doc-says-fail-if-exists-but-implementation-skips-footgun)
- [BUG-005 — No `ggen verify` CLI despite verify library code existing (UX-GAP)](#bug-005--no-ggen-verify-cli-despite-verify-library-code-existing-ux-gap)
- [BUG-006 — SELECT row order nondeterministic without ORDER BY; no warning emitted (FOOTGUN)](#bug-006--select-row-order-nondeterministic-without-order-by-no-warning-emitted-footgun)
- [BUG-007 — `SELECT *` silently disables GGEN-TPL-001/OUT-001 provision checks (FOOTGUN)](#bug-007--select--silently-disables-ggen-tpl-001out-001-provision-checks-footgun)
- [BUG-008 — Pack `[pack.outputs]` indirection not resolved in pipeline (FOOTGUN)](#bug-008--pack-packoutputs-indirection-not-resolved-in-pipeline-footgun)
- [BUG-009 — Identity CONSTRUCT `alive-gate` accepted silently; no no-op detection (UX-GAP)](#bug-009--identity-construct-alive-gate-accepted-silently-no-no-op-detection-ux-gap)
- [BUG-010 — LSP completion/hover docs say `row.VAR` but pipeline uses direct `{{ VAR }}` (SPEC-IMPL-DRIFT)](#bug-010--lsp-completionhover-docs-say-rowvar-but-pipeline-uses-direct--var--spec-impl-drift)
- [BUG-011 — `{ pack=, output=, file= }` query/template source silently produces no output (SILENT-DATA-LOSS)](#bug-011---pack-output-file--querytemplate-source-silently-produces-no-output-silent-data-loss)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

---
# WASM4PM-DISCOVERED-BUGS: ggen Defects Found During Breed Scaffold Research

**Discovered:** 2026-06-11  
**Discovered by:** wasm4pm breed scaffold ggen-pack planning (4 source-level research agents)  
**ggen version under analysis:** 26.5.21 (source-verified against crates/ggen-core, codegen/pipeline.rs, manifest/types.rs, ggen-lsp/src)  
**Severity legend:** SILENT-DATA-LOSS | SPEC-IMPL-DRIFT | FOOTGUN | UX-GAP

---

## BUG-001 — OntologyConfig silently ignores unknown fields (SILENT-DATA-LOSS)

**Source evidence:** `manifest/types.rs:93-95` — `OntologyConfig` struct has no `#[serde(deny_unknown_fields)]`.  
**Manifestation:** wasm4pm's `ggen.toml` uses `additional = [...]` (4 domain TTLs). The correct field is `imports`. Because the struct silently drops unknown fields, all 4 ontologies were NEVER loaded. The sync-state graph size of 75,080 bytes (= wasm4pm-compat.ttl alone) proves this empirically.  
**Impact:** Any SPARQL query depending on those ontologies returns empty results with no error. Consumer believes ontology is loaded; it is not.  
**Fix:** Add `#[serde(deny_unknown_fields)]` to `OntologyConfig` AND emit a hard error if an `imports` field references a non-existent file.  
**Workaround (consumer):** Rename `additional` → `imports` in ggen.toml.

---

## BUG-002 — Frontmatter machinery unreachable from `ggen sync` (SPEC-IMPL-DRIFT)

**Source evidence:** `codegen/pipeline.rs:835` loads templates raw (`fs::read_to_string`); `template_types.rs` has `TemplateMetadata` and `TemplateWithMeta` structs fully defined.  
**Manifestation:** A `---\n...\n---` frontmatter block in any `.tera` file consumed by `ggen sync` renders literally into the output — the YAML delimiters and body appear verbatim in the generated source.  
**Additional evidence:** `docs/templates/hello.tmpl` uses a removed `vars:` key — misleading legacy artifact.  
**Impact:** Authors following the CLAUDE.md frontmatter guidance will silently corrupt their generated output.  
**Fix:** Wire `template_types::parse_template_with_meta` into the `ggen sync` pipeline before Tera rendering. Strip the frontmatter block before passing the template string to the Tera engine.

---

## BUG-003 — GGEN-YIELD-001..005 codes have no detector in ggen-lsp (SPEC-IMPL-DRIFT)

**Source evidence:** ggen-lsp source contains detectors for GGEN-TPL-001, GGEN-OUT-001, GGEN-RULE-001, GGEN-SRC-001/002/003, GGEN-HARNESS-001, and E00xx codes. No YIELD family detector exists.  
**Manifestation:** CLAUDE.md documents YIELD-001 (back-write to pack root), YIELD-003 (orphan use-site), YIELD-004 (competing authority), YIELD-005 (remote fetch). The receipt schema even cites YIELD-005. Authors checking `ggen lsp check` see no YIELD violations even when the back-write pattern is active.  
**Impact:** wasm4pm's `witness-markers` rule writes to `../wasm4pm-compat/src/witnesses.rs` — a YIELD-001 violation — and passes lsp check silently.  
**Fix:** Implement YIELD-001 detector: flag any `output_file` that resolves outside the consumer project root or into a pack directory.

---

## BUG-004 — mode="Create" doc says "fail if exists" but implementation skips (FOOTGUN)

**Source evidence:** `manifest/types.rs:284` doc-comment: "fail if the output file already exists". `codegen/pipeline.rs:1017-1021` implementation: `if output_path.exists() { continue; }` (silently skips).  
**Manifestation:** Authors reading the doc expect an error when a Create-mode file exists; they get silent skip. The per-row fan-out use case (generate stubs for each breed) relies on the skip behavior — but it is undocumented, and the doc actively misleads.  
**Fix:** Update doc-comment to say "skip if output file already exists (idempotent stub generation)" OR add a `--force` flag that enables the fail-if-exists behavior.

---

## BUG-005 — No `ggen verify` CLI despite verify library code existing (UX-GAP)

**Source evidence:** `receipt_impl.rs` contains `Receipt::verify()` and `ReceiptChain::verify()` (cryptographic chain verification). No `ggen verify` CLI subcommand exposes this.  
**Manifestation:** Stale `latest.json` (output path no longer in manifest) goes undetected. The only CI gate is `ggen sync && git diff --exit-code` — which will overwrite with new correct content rather than flagging drift.  
**Additional:** Current wasm4pm `.ggen/receipts/latest.json` references a path the manifest no longer targets; no warning is emitted.  
**Fix:** Add `ggen verify` subcommand: reads `.ggen/receipts/latest.json`, verifies Ed25519 chain, checks that all `output_hashes` match current files on disk (SHA-256).

---

## BUG-006 — SELECT row order nondeterministic without ORDER BY; no warning emitted (FOOTGUN)

**Source evidence:** `codegen/pipeline.rs:686-700` iterates oxigraph query results in insertion order (no deterministic sort).  
**Manifestation:** Without `ORDER BY` in a query, per-row fan-out generates files in arbitrary order across platforms/runs. `Overwrite`-mode multi-section files (e.g. registration.rs) have nondeterministic section order → flapping diffs and flapping receipts.  
**Fix:** Emit a GGEN-QUERY-001 warning when a SELECT used by a generation rule has no ORDER BY clause. Parse the SPARQL AST for an `OrderClause`.

---

## BUG-007 — `SELECT *` silently disables GGEN-TPL-001/OUT-001 provision checks (FOOTGUN)

**Source evidence:** ggen-lsp provision check logic: when the SELECT projection is empty (SELECT *), the variable set is empty, so no template variable can be found missing — checks pass vacuously.  
**Manifestation:** A query using `SELECT *` paired with a template using `{{ row.foo }}` gets no GGEN-TPL-001 diagnostic even if `foo` is not in the graph.  
**Fix:** Detect `SELECT *` in ggen-lsp; emit GGEN-QUERY-002 advisory: "SELECT * disables provision checks — use explicit projections".

---

## BUG-008 — Pack `[pack.outputs]` indirection not resolved in pipeline (FOOTGUN)

**Source evidence:** `codegen/pipeline.rs:660-663` treats `[pack.outputs]` key names as literal directory paths relative to the pack root, with no mapping step.  
**Manifestation:** If a pack declares `[pack.outputs] queries = "queries/"` but the rule references `output = "queries"` (no trailing slash), the path resolution differs from what the author expects. The key is not an alias — it IS the directory name fragment.  
**Fix:** Document this explicitly in the pack authoring guide: "The key in `[pack.outputs]` must equal the directory name exactly as it appears on disk." Optionally add a GGEN-PACK-001 diagnostic when a referenced output key has no matching directory.

---

## BUG-009 — Identity CONSTRUCT `alive-gate` accepted silently; no no-op detection (UX-GAP)

**Source evidence:** wasm4pm `ggen.toml` inference rule: `CONSTRUCT { ?s a ?class } WHERE { ?s a ?class }` — a mathematical identity that materializes zero new triples.  
**Manifestation:** The consumer intended to gate breed admission via inference. ggen accepted the rule, ran it, added its hash to the receipt, and produced zero benefit. No warning that the CONSTRUCT produces no new triples.  
**Fix:** After CONSTRUCT materialization (`pipeline.rs:383-447`), count newly-added triples. If count == 0, emit GGEN-INFER-001 advisory: "Inference rule added 0 new triples — check CONSTRUCT clause".

---

---

## BUG-010 — LSP completion/hover docs say `row.VAR` but pipeline uses direct `{{ VAR }}` (SPEC-IMPL-DRIFT)

**Discovered:** 2026-06-11 during ggen sync of wasm4pm templates.  
**Source evidence:** `crates/ggen-core/src/codegen/pipeline.rs:978-983` — per-row context inserts variables with bare keys (e.g. `context.insert(clean_key, value)` where `clean_key` is `name`, not `row.name`). `sparql_results` (the full row list) is inserted separately for batch templates.  
**Manifestation:** The ggen-lsp completion list offers `row.name` as a suggestion and hover text says "Field `name` in the SPARQL result row". Authors who follow this doc write `{{ row.name }}` — which renders to an empty string because `row` is not a variable in the per-row Tera context (unless the template accesses `sparql_results[0].name`).  
**Impact:** Silent wrong output. Template renders without error; `row.name` is treated as an undefined variable by Tera (which substitutes empty string by default).  
**Fix:** Update ggen-lsp hover/completion text to reflect the actual context shape: bare `{{ name }}` for per-row variables, `sparql_results` for the full result list. The `consumed_vars()` function correctly handles both `{{ name }}` and `{{ row.name }}` for GGEN-TPL-001 purposes, but the UX suggests the wrong form.  
**Status:** Hover/completion text corrected in ggen-lsp v26.6.9 (this fix session). The `consumed_vars()` logic retained both forms for backwards compatibility.

---

## BUG-011 — `{ pack=, output=, file= }` query/template source silently produces no output (SILENT-DATA-LOSS)

**Discovered:** 2026-06-11 during wasm4pm sync test with pack-sourced queries.  
**Source evidence:** `crates/ggen-core/src/manifest/types.rs:262-274` — `QuerySource::Pack { pack, output, file }` variant exists and parses correctly. `codegen/pipeline.rs:660-663` (pack query resolution) and the equivalent template site treat `output` as a literal directory suffix with no `package.toml` mapping step. Additionally, `crates/ggen-lsp/src/rule_index.rs:98-106` records pack queries as issues ("pack query resolved at generation time") and returns `(false, String::new())`, meaning the query_content is empty and no SELECT projection is parsed.  
**Manifestation:** A ggen.toml rule using `query = { pack = "wasm4pm-compat", output = "queries", file = "pm-rust-bridge.rq" }` silently resolves to no query content and no file output. No error is emitted; the rule is skipped as if `skip_empty = true`.  
**Workaround:** Use direct relative paths (e.g. `query = { file = "../packs/wasm4pm-compat/queries/pm-rust-bridge.rq" }`). Confirmed working.  
**Fix:** This is BUG-008 (already in backlog). Additionally add GGEN-PACK-001 diagnostic in ggen-lsp when a `QuerySource::Pack` or `TemplateSource::Pack` cannot be resolved: surface the existing `issues` string as an ERROR rather than silently producing empty content.

---

*Filed against ggen 26.5.21 source as analyzed 2026-06-11. BUG-010 and BUG-011 added 2026-06-11 during fix session. All line references are from the source-verified codebase at `~/ggen`.*
