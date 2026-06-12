# GGEN-RULE-001 Verifier Receipt — `unbound_rule_file`

**Species:** GGEN-RULE-001 — `unbound_rule_file` (dangling rule binding: a `ggen.toml`
`[[generation.rules]]` whose `query={file=...}` or `template={file=...}` points at a
file that does not exist on disk, overlay-aware).
**Date:** 2026-05-30
**Repo:** ggen @ /Users/sac/ggen, branch `main`, HEAD `53a4d1c2`
**Role:** VERIFIER + RECEIPT (independent re-verification; trust evidence only).
**Verdict:** **ALIVE** — the 4th living species, wired through the consolidated loop.

---

## 1. Discovery Gate Verdict (decided FIRST): READY (real gap)

**Question (mandated):** Is `RuleIndexEntry.issues` already published as a live LSP
diagnostic / through `observe_diagnostics` for a missing rule file?

**Answer: NO — collected-but-not-published (silent author-time failure). READY.**

Independently re-verified, not trusted from the pre-inventory:

- `crates/ggen-lsp/src/rule_index.rs` `from_rule_with_overlay`:
  - L88-95: `QuerySource::File` read error → `issues.push("query file missing: {path} ({err})")`, `query_content = ""`.
  - L107-110: `TemplateSource::File` read error → `issues.push("template file missing: {path}")`, `template_content = None`.
  - Read goes through `read_overlay_or_disk` (L156-161) → overlay hit else `std::fs::read_to_string` → existence is overlay-aware.
- The two existing ProjectIndex species SKIP a missing-file rule **by design** (re-read at HEAD):
  - `analyzers/mod.rs:62` `detect_tpl_001`: `let Some(template) = entry.template_content.as_deref() else { continue; };` (comment L51: "a missing template is an index-level issue … not GGEN-TPL-001").
  - `analyzers/mod.rs:96` `detect_out_001`: `if entry.selected_vars.is_empty() { continue; }` (missing query → empty `selected_vars`).
- `project_index.rs` consumes `entry.issues` ONLY under `#[cfg(test)]` — no path to `check.rs` / `state.rs` / published Diagnostics at HEAD.

⇒ A dangling rule binding was a **fail-open** defect (`coding-agent-mistakes.md §1.3`):
the LSP continued, both live species stepped around it, the only trace was a
non-fatal `issues` string no channel surfaced. The gap is real. **Not BLOCKED-redundant.**

---

## 2. The Consolidated Loop carries it (cite CONSOLIDATE-002) — no bespoke branch

Adding GGEN-RULE-001 = **registry entry + detector + one descriptor-array entry + one fold + one route.** Verified each surface:

| Surface | Change (verified) | Anchor |
|---------|-------------------|--------|
| `route/diagnostic_species.rs` | 4th `DiagnosticSpecies` (active); count test 3→4 | — |
| `route/model.rs` | `RepairFamily::RuleFileMissing` (NEW, exclusive) | — |
| `route/registry.rs` | `family_of_code("GGEN-RULE-001") => RuleFileMissing`; seed route `source-law.bind-rule-file` (2 NoOp steps) | — |
| `analyzers/tera_analyzer.rs` | `pub const GGEN_RULE_001` + pure `unbound_rule_file_diagnostics(issues)` (prefix-filters `"query file missing:"` / `"template file missing:"` only) | — |
| `analyzers/mod.rs` | `detect_rule_001(project)` over `entry.issues`, anchored on `entry.manifest_path` | ggen.toml |
| `check.rs` | `fold_rule_001(root, files, registry)` + one call, appended LAST | ggen.toml |
| `state.rs` | `detect_rule_001_for`; 4th `Species` in the descriptor array appended LAST; 4th close-doc reconcile riding `tpl_is_trigger` with residual-preserving clear | ggen.toml |
| `intel/mine.rs` | 1 line: `RepairFamily::RuleFileMissing => "rule-file-missing"` in `family_slug` | — |

**On `intel/mine.rs` (hard rule "do not touch"):** the single arm is a
COMPILE-FORCED exhaustive-match consequence of the new `RepairFamily` variant
(`family_slug` matches all variants). No mining logic changed; without it the crate
does not compile. Treated as the minimal mechanical cost of a new family, not a
content edit to the miner.

**Ordering (behavior-preserving):** RULE-001 is LAST in both the `analyze_and_observe`
descriptor array and the `check_files_in_root` fold. The CONSOLIDATE-002
sequence-equivalence golden fixture has no missing rule files → RULE-001 emits zero
groups there → TPL→HARNESS→OUT emit sequence stays byte-identical. Golden green,
unedited.

---

## 3. FAKE-LIVE guard — REAL (external on-disk OCEL 6-link chain)

`tests/ggen_rule_001_living_loop.rs` test #5
`analyze_and_observe_records_live_rule_001_receipt_chain`:
- Copies the invalid fixture into a hermetic TempDir (real fs).
- Drives the REAL `ServerState::analyze_and_observe` → RAISE on `ggen.toml` URI.
- Performs a REAL `std::fs::write(templates/missing.tera, …)` source-law repair, re-analyzes → CLEAR through the SAME `observe_diagnostics`.
- Reads the **EXTERNAL** log `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` via `read_log_lines` (not an in-process bool) and asserts ALL SIX links appear on a line naming both `ggen.toml` and `GGEN-RULE-001`:
  `DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted`.
- Confirmed the write path is real: `observe_diagnostics` (state.rs:183) → append-only NDJSON at `.ggen/ocel/agent-edit-events.ocel.jsonl` (intel/log.rs:1); RULE clear rides this same path (state.rs reconcile lines).
- Test passes (1/1, `--exact`). Not vacuous: the clear test (#2) and raise test (#1) assert ≥1 RULE-001 pre-repair before asserting 0 post-repair.

⇒ Not FAKE-LIVE: real existence check (driven by the overlay-aware `issues`),
complete chain, residual-preserving clear, no mid-array reorder.

---

## 4. Cross-species: NO leakage (both directions)

- Test #1b `missing_rule_file_raises_zero_other_species`: RULE invalid fixture raises
  0 TPL / 0 OUT / 0 HARNESS, AND ≥1 RULE-001 (non-vacuous). Fixture designed so
  TPL skips (template body absent), OUT skips (`output_file = "out.txt"` static, no `{{`),
  HARNESS absent (no Cargo.toml).
- Test #7 `other_species_fixtures_raise_zero_rule_001`: OUT-001 valid+invalid fixtures
  (bound files exist) raise 0 RULE-001.
- Unit `ggen_rule_001_does_not_contaminate_other_species`: TPL/OUT/HARNESS still select
  their own routes; `ggen_rule_001_maps_to_its_own_family` → `RuleFileMissing` exclusively.

---

## 5. Evidence (re-run independently)

| Gate | Command | Result |
|------|---------|--------|
| Compile | `cargo make check` | Finished, 0 errors |
| Clippy | `cargo clippy -p ggen-lsp --no-deps -- -D warnings` | Finished, **0 warnings** |
| Fmt | `cargo fmt -p ggen-lsp -- --check` | exit 0 (clean) |
| Full suite | `cargo test -p ggen-lsp` | all binaries 0 failed (lib: 174 passed) |
| New species | `--test ggen_rule_001_living_loop` | **8 passed; 0 failed** |
| OCEL chain (exact) | `…receipt_chain --exact` | 1 passed; 0 failed |
| Species count | `registry_contains_exactly_four_species` | ok (3→4) |

**Existing species/regression UNCHANGED, all green:**
- `ggen_tpl_001_living_loop` 5/0 · `ggen_out_001_living_loop` 7/0 · `ggen_harness_001_living_loop` 7/0
- `ggen_tpl_001_regression` 9/0 · `ggen_tpl_001_stale_clear` 3/0 · `ggen_tpl_001_did_close_clear` 2/0
- `ggen_live_buffer_001` 1/0 · `consolidate_002_sequence_equivalence` 2/0 (1 ignored) · `ggen_tpl_001` 6/0

**No-test-edit guard:** `git diff --name-only HEAD -- crates/ggen-lsp/tests/` excluding
`ggen_rule_001_living_loop` → NONE. Existing test files are byte-untouched (the new
test + fixtures are `??` untracked, not modifications).

**Scope guard:** all changed/new paths under `crates/ggen-lsp/` plus `docs/receipts/`
only. `project_index.rs`, `rule_index.rs`, and the other analyzers (harness/sparql/rdf/toml/diag)
are unchanged.

---

## 6. ALIVE acceptance — all satisfied

- [x] Missing query/template file raises GGEN-RULE-001 live (`analyze_and_observe`, anchored on ggen.toml) AND headless (`check.rs` fold, `error_count++`).
- [x] Repair route source-law only (`source-law.bind-rule-file`, all `EditTemplate::NoOp`); never fabricates / auto-creates output.
- [x] Creating the file clears it via `observe_diagnostics` with the full 6-link OCEL chain on the EXTERNAL log; clears residual-preserving.
- [x] No regression to TPL/HARNESS/OUT (assertions untouched; sequence golden green).
- [x] No cross-species leakage either direction.
- [x] Species registry now 4; count test 3→4; clippy 0; fmt clean.

**Did any existing ALIVE checkpoint regress?** NO. GGEN-TPL-001, GGEN-HARNESS-001,
GGEN-OUT-001 living-loop tests + the CONSOLIDATE-002 sequence-equivalence golden all
pass unedited.

**END OF RECEIPT — verdict ALIVE.**
