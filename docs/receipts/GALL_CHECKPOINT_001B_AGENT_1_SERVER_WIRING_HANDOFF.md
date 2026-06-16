<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CHECKPOINT-001B — Agent 1 Handoff: server.rs live wiring](#gall-checkpoint-001b--agent-1-handoff-serverrs-live-wiring)
  - [Files changed](#files-changed)
  - [Doctrine compliance](#doctrine-compliance)
  - [Exact live trigger points touched](#exact-live-trigger-points-touched)
  - [How the project root is found](#how-the-project-root-is-found)
  - [How diagnostics are published (merge-once-per-URI detail)](#how-diagnostics-are-published-merge-once-per-uri-detail)
  - [How observe_diagnostics stays in the path](#how-observe_diagnostics-stays-in-the-path)
  - [Cross-file template URIs: routed through observe_diagnostics (NOT best-effort)](#cross-file-template-uris-routed-through-observe_diagnostics-not-best-effort)
  - [Tests run + results](#tests-run--results)
  - [Known gaps / notes](#known-gaps--notes)
  - [Acceptance mapping](#acceptance-mapping)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CHECKPOINT-001B — Agent 1 Handoff: server.rs live wiring

**Mission:** GALL-CHECKPOINT-001B — live wiring for the already-landed GGEN-TPL-001 nerve.
**Branch:** `feat/ggen-tpl-001-living-lsp`
**Agent:** 1 (server wiring)
**Date:** 2026-05-29
**Owned file (only file written):** `crates/ggen-lsp/src/server.rs`

## Files changed

- `crates/ggen-lsp/src/server.rs` — ONLY file edited. No other files touched.
  No edits to `state.rs`, `check.rs`, `project_index.rs`, `rule_index.rs`,
  `analyzers/`, `route/`, any tests, Cargo.toml, or lib.rs.

## Doctrine compliance

- ggen-lsp stays READ-ONLY: the new path only builds a `ProjectIndex` (which reads
  files) and runs the pure `detect_tpl_001`. No file writes, no `ggen sync`, no
  materialization of `output_file`. (`ggen_tpl_001.rs::analysis_never_materializes_output_file`
  continues to pass.)
- No new nerve. Reuses the existing `ProjectIndex::from_root` + `analyzers::detect_tpl_001`
  path. No second detector, no duplicated variable extraction.
- Detector behavior unchanged (no edits to analyzers).
- No `unwrap()`/`expect()` in the wiring (all `Result`/`Option` handled with `match`/`?`/`let-else`).

## Exact live trigger points touched

The single seam is `GgenLanguageServer::refresh_analyzer(&self, uri, content)`,
called from both `did_open` (server.rs:~210 post-edit) and `did_change`. Those two
handlers were NOT restructured — they still call `refresh_analyzer`; the cross-file
flow lives entirely inside `refresh_analyzer`, so both didOpen and didChange get the
live GGEN-TPL-001 flow for free.

`refresh_analyzer` now:
1. Computes the edited file's single-file diagnostics (`build_analyzer(...).diagnostics()`)
   exactly as before, and stores the analyzer.
2. If the edited file's `FileType` is `Tera | Sparql | Toml` (a rule-referenced
   surface), resolves the project root, builds `ProjectIndex::from_root(root)`, and
   runs `crate::analyzers::detect_tpl_001(&proj)` → `Vec<(PathBuf, Vec<Diagnostic>)>`.
   For all other file types (RDF, Unknown) the original single-file flow is used
   unchanged.
3. Publishes per the merge strategy below.

This satisfies tasks 1 (`.tera`), 2 (`.rq`/`.sparql`), and 3 (`ggen.toml`): any of
the three rule-referenced surfaces rebuilds the index and re-publishes affected
template diagnostics. Task 4 (single-file flow intact for non-rule files and for the
edited file itself) is preserved — see "publish strategy".

## How the project root is found

`project_root_for(uri)`:
- If `uri.to_file_path()` succeeds, walk up parent directories from the file,
  returning the first directory that contains a `ggen.toml` (bounded by the
  filesystem root).
- Fallback: `self.state.root` (the `ServerState.root: PathBuf`, default cwd /
  injectable in tests) — but only if it actually holds a `ggen.toml`.
- If neither yields a manifest, returns `None` and `detect_tpl_001_for` returns an
  empty vec (best-effort; the single-file flow still publishes normally).

This means a `.tera`/`.rq`/`ggen.toml` opened outside any ggen project degrades
gracefully to the original single-file behavior with zero cross-file noise.

## How diagnostics are published (merge-once-per-URI detail)

LSP diagnostics are replace-semantics per URI: publishing twice for the same URI
clobbers. To avoid clobbering the edited template's own E0024 diagnostics with its
GGEN-TPL-001 diagnostics (or vice-versa), the wiring MERGES before publishing:

- The edited document's path is round-tripped to a `Url` (`url_from_path_str`,
  which percent-decodes `uri.path()` then `Url::from_file_path`).
- Each `(template_path, tpl_diags)` group from the detector is converted to a `Url`
  via `Url::from_file_path` (`url_from_path`).
- **If a group's template URI equals the edited document's URI:** the edited file's
  own single-file diagnostics and that group's GGEN-TPL-001 diagnostics are merged
  into ONE `Vec` and published exactly once for that URI. (`published_self = true`.)
- **If a group's template URI is a DIFFERENT (cross-file) template** (e.g. the user
  edited the `.rq` or `ggen.toml`, not the template): the group's diagnostics are
  published for that template's URI directly.
- **If the edited file was not itself a TPL-001-affected template:** its own
  single-file diagnostics are published for its own URI (this is the path taken by
  `.rq` and `ggen.toml` for their own URIs, and by `.tera` files that have no
  unbound-var problem). This preserves the original single-file flow (task 4).

A small `percent_decode_path` helper handles the `%XX` sequences `Url::path()`
emits (e.g. spaces in paths) so the round-trip comparison is reliable without adding
a dependency.

## How observe_diagnostics stays in the path

Every publish — edited-file merged, edited-file-only, AND cross-file template URIs —
goes through the new `publish_observed(uri, diags)` helper, which calls
`self.state.observe_diagnostics(uri, &diags).await` BEFORE
`self.client.publish_diagnostics(uri, diags, None).await`. There is no direct
`publish_diagnostics` call left in `refresh_analyzer`. The living loop
(DiagnosticRaised → RouteSelected → RepairSuggested → … → ReceiptEmitted) therefore
records GGEN-TPL-001 raises and their rework-closures identically to single-file
diagnostics.

`did_close` still publishes an empty diagnostic set directly via `self.client`
(unchanged existing behavior) — it is an LSP-conformant clear, not part of the
raise/repair loop, so it intentionally does not route through `observe_diagnostics`.

## Cross-file template URIs: routed through observe_diagnostics (NOT best-effort)

I did NOT need the escape hatch. `observe_diagnostics` is `pub` and takes `&uri`, so
I CALL it for the cross-file template URIs without editing `state.rs`. All affected
template URIs — edited and non-edited — flow through `observe_diagnostics`. state.rs
was not touched.

## Tests run + results

`cargo check -p ggen-lsp` → clean (Finished, no warnings/errors in my file).

Targeted server-exercising tests:
- `editor_apply_test` → 2 passed
- `ggen_tpl_001` → 5 passed, 1 ignored (GGEN-OUT-001 next phase)
- `lsp_protocol_test` → 2 passed (incl. `lsp_full_lifecycle_over_stdio`)
- `session_attribution_test` → 1 passed

Full suite `cargo test -p ggen-lsp` → **0 failed, 0 FAILED, 0 `error[`** across the
136-test lib suite + all integration test binaries. No regressions.

## Known gaps / notes

1. **Perf (uncached index build):** `ProjectIndex::from_root` is rebuilt on every
   didOpen/didChange of a `.tera`/`.rq`/`ggen.toml`. This re-reads the manifest and
   every referenced query/template file each keystroke (FULL sync). Acceptable for
   the MVP per the checkpoint brief; a future caching layer keyed on manifest mtime
   would remove the per-change I/O. No caching added this checkpoint (scope lock).

2. **`.rq`/`.sparql` → template mapping is coarse.** `RuleIndexEntry` stores
   `query_inline: bool` and `query_content: String` but NOT the resolved query file
   path. So when a `.rq` file changes I cannot pinpoint which template(s) it feeds;
   instead `detect_tpl_001` runs over the WHOLE index and re-publishes diagnostics
   for ALL affected templates. This is correct (no missed diagnostics) but broader
   than strictly necessary. Tightening this would require a `query_path` field on
   `RuleIndexEntry` (Agent 1's rule_index.rs — not in scope this checkpoint).

3. **Stale cross-file diagnostics on close.** If a `.rq`/`ggen.toml` that produced
   GGEN-TPL-001 diagnostics for an unopened template is closed, the template's
   squiggles are not proactively cleared (the template URI may never have been
   opened). They refresh on the next edit of any rule-referenced surface. Minor;
   acceptable for MVP.

4. **Cross-file template URI shape.** Cross-file template diagnostics are published
   for `file://` URIs derived from `Url::from_file_path(template_path)`. If a client
   has the same file open under a differently-normalized URI, replace-semantics may
   not perfectly coincide. In practice editors use canonical file URIs, and the
   edited-file merge path uses the document's own URI, so the common case is exact.

## Acceptance mapping

- "Editing a template to consume an unbound var produces GGEN-TPL-001 through the
  live server path" → `refresh_analyzer` on a `.tera` edit builds the index, runs
  `detect_tpl_001`, merges + publishes via `observe_diagnostics`. Detector proven by
  `ggen_tpl_001::unbound_template_var_emits_tpl_001_error`; live-loop tests are
  Agent 3's.
- "Repairing the template clears it through the live path" → on the next
  didChange the index is rebuilt, the unbound var is gone, the merged publish for
  that URI no longer contains GGEN-TPL-001; `observe_diagnostics` sees the
  disappearance and emits RepairApplied → GatePassed → ReceiptEmitted (same closure
  proven for E0024 by `editor_apply_test`).
- "Route stays source-law.bind-projection; emitted output untouched; no files
  written" → detector returns source-law diagnostics only; the wiring never writes
  (`analysis_never_materializes_output_file` passes).
