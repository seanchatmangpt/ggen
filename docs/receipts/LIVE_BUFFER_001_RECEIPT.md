# LIVE-BUFFER-001 — Verification Receipt

**Checkpoint:** LIVE-BUFFER-001 — make the cross-surface living loop LIVE ON THE OPEN BUFFER, not just disk.
**Repo:** ggen @ /Users/sac/ggen · **Base:** main @ 8f3d11ef (CLEAN; species GGEN-TPL-001, GGEN-HARNESS-001, GGEN-OUT-001 ALIVE).
**Verifier role:** independent re-verification — trust evidence, not reports. No commit. SINGLE-WRITER honored.
**Date:** 2026-05-30

## VERDICT: ALIVE

The cross-surface living loop now runs over an open-buffer overlay. A SPARQL query repair held
ONLY in the editor buffer (never written to disk) clears the consumer template's GGEN-TPL-001
through the same `observe_diagnostics` editor path, with the full 6-link OCEL chain on the external
on-disk log — while the disk `.rq` is asserted STILL broken. Every existing test passes UNCHANGED;
the disk path is byte-identical to before (empty overlay = `read_to_string`).

---

## The gap closed (real files)

`ServerState::analyze_and_observe(uri, content)` (crates/ggen-lsp/src/state.rs) ran the cross-surface
detectors over a project graph built from **disk**: `detect_tpl_001_for`/`detect_out_001_for` →
`ProjectIndex::from_root` → `RuleIndexEntry::from_rule` (`std::fs::read_to_string` on the `.rq`/`.tera`),
and `detect_harness_001_for` → `HarnessIndex::from_root` (`std::fs::read_to_string` on `Cargo.toml`).
So an unsaved in-buffer `.rq` edit updated the edited file's OWN single-file diagnostics (which use the
passed `content`) but NOT the consumer template's cross-surface GGEN-TPL-001 — the index re-read the
stale on-disk query. The fix threads an open-buffer overlay (snapshot of `self.documents` + the edited
`(uri, content)` pair) through the index constructors, overlay-first then disk-fallback.

## Implementation (as verified in the tree)

| File | Change |
|---|---|
| `crates/ggen-lsp/src/project_index.rs` | Adds `pub type BufferOverlay = HashMap<PathBuf,String>`; `from_root` becomes a thin empty-overlay delegate to new `from_root_with_overlay`. Manifest (`ggen.toml`) still read from disk (recommended scope — no `ggen_core` change); each rule's query/template read via overlay. |
| `crates/ggen-lsp/src/rule_index.rs` | `from_rule` delegates to new `from_rule_with_overlay`; the two `read_to_string` sites become `read_overlay_or_disk(overlay, &resolved)` (overlay hit → buffer; miss → exact `std::fs::read_to_string` io::Error semantics). |
| `crates/ggen-lsp/src/harness_index.rs` | `from_root` delegates to new `from_root_with_overlay`; overlay shadows ONLY the `Cargo.toml` text read. Existence guard is `!manifest.is_file() && !overlay.contains_key(&manifest)`. `enumerate_proof_files` stays a disk fact (file existence is not changed by an unsaved buffer). |
| `crates/ggen-lsp/src/state.rs` | New `buffer_overlay(&self)` snapshots `self.documents` as `PathBuf→String` (keys via `Url::to_file_path()`, non-canonicalized to match the index's join-based resolution; non-file URLs skipped). `analyze_and_observe` snapshots the overlay AND splices the edited `(uri, content)` pair (handles the `did_change`-insert-vs-call ordering). `close_document` snapshots AFTER `remove_document` (closed doc falls back to disk — correct). `detect_{tpl,out,harness}_001_for` each gain an `&BufferOverlay` param forwarded to `*_from_root_with_overlay`. |
| `crates/ggen-lsp/tests/ggen_live_buffer_001.rs` (NEW) | The buffer-liveness proof (see below). |

## Behavior-extending guarantee (the disk path is preserved)

With **no open buffers** the overlay is empty and every index read falls through to
`std::fs::read_to_string` — byte-identical to the pre-overlay code. `from_root`/`from_rule` are kept as
empty-overlay delegates (non-deletion doctrine: the disk path is the documented fallback the overlay
extends, not a removed bypass). Confirmed by the UNCHANGED existing unit tests in `project_index.rs`,
`rule_index.rs`, `harness_index.rs` and the disk-driven living-loop integration tests.

## The buffer-liveness proof (ggen_live_buffer_001.rs)

`buffer_only_query_repair_clears_template_tpl_001_with_disk_still_broken`:
1. Writes an invalid project to a TempDir: query SELECTs `?name`; template wants `row["title"]`.
2. **Act 1 (RAISE):** `analyze_and_observe(tera_uri, tera_src)` raises GGEN-TPL-001 on the template.
3. **Act 2 (BUFFER-ONLY repair):** `analyze_and_observe(rq_uri, repaired_rq)` where `repaired_rq`
   projects `?name ?title` — and the disk `.rq` is LEFT UNCHANGED. The overlay spliced inside
   `analyze_and_observe` makes the repaired query visible to the cross-surface `ProjectIndex`.
4. **Clear:** the returned publish set re-publishes `tera_uri` with NO GGEN-TPL-001.
5. **FAKE-LIVE guard (two-sided):** asserts `disk_rq == broken_query` and `!disk_rq.contains("title")`
   — the disk `.rq` is STILL broken, so the clear can ONLY have come from the buffer. If a future change
   read disk instead of the overlay, TPL-001 would not clear (assertion (4) fails). If the repair leaked
   to disk, assertion (5) fails.
6. **6-link chain:** the external on-disk OCEL log
   (`<root>/.ggen/ocel/agent-edit-events.ocel.jsonl`) shows all of `DiagnosticRaised → RouteSelected →
   RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted` for `item.tera` + `GGEN-TPL-001`.
7. **No artifact:** asserts `out.txt` (the rule's `output_file`) is absent.

Chicago TDD: real on-disk project tree, real `ServerState` orchestration, real `RouteRegistry`, real
`IntelLog` read back from its external JSONL surface. No mocks, no test doubles, no fabricated events.

## Independent verification (re-run by the verifier)

| Gate | Command | Result |
|---|---|---|
| Compile | `cargo make check` | PASS — Finished, 0 errors/warnings (full workspace) |
| Full LSP suite | `cargo test -p ggen-lsp` | PASS — every test binary `0 failed` (163 lib unit + all integration; one pre-existing `1 ignored`) |
| New proof | `cargo test -p ggen-lsp --test ggen_live_buffer_001` | PASS — 1 passed |
| TPL living-loop | `--test ggen_tpl_001_living_loop` | PASS — 5 passed (UNCHANGED) |
| TPL stale-clear | `--test ggen_tpl_001_stale_clear` | PASS — 3 passed (UNCHANGED) |
| TPL did_close | `--test ggen_tpl_001_did_close_clear` | PASS — 2 passed (UNCHANGED) |
| TPL regression | `--test ggen_tpl_001_regression` | PASS — 9 passed (UNCHANGED) |
| HARNESS living-loop | `--test ggen_harness_001_living_loop` | PASS — 7 passed (UNCHANGED) |
| OUT living-loop | `--test ggen_out_001_living_loop` | PASS — 7 passed (UNCHANGED) |
| Clippy | `cargo clippy -p ggen-lsp --no-deps -- -D warnings` | PASS — 0 warnings |
| Format | `cargo fmt -p ggen-lsp -- --check` | PASS — exit 0 |

## Guard checks

- **No-test-edit guard:** `git diff HEAD -- <tracked tests>` is EMPTY. The only new test is an entirely
  new file `tests/ggen_live_buffer_001.rs`. No existing test was edited to pass.
- **No new `#[allow]`:** `git diff HEAD -- crates/ggen-lsp/src/` contains zero added `#[allow`.
- **mine.rs untouched:** `git diff HEAD -- crates/ggen-lsp/src/intel/mine.rs` is EMPTY.
- **Scope fence:** changed paths are only `crates/ggen-lsp/{src,tests}` + `docs/receipts/`.
- **SINGLE-WRITER:** the documents lock is released as the overlay snapshot returns; no lock held across
  the synchronous index build.

## No ALIVE checkpoint regressed

All three species living-loops (TPL/HARNESS/OUT), the stale-clear, the did_close honest-deviation clear,
and the regression suite pass with byte-for-byte UNCHANGED counts. CONSOLIDATE-001's generic
`clears_for` + keyed `flagged` HashMap is untouched and still drives all clears.

## Patch contract (coding-agent-mistakes §3)

- **Q1 real state:** the in-memory `ProjectIndex.rule_entries[*].query_content`/`selected_vars` now
  reflect the open buffer; the consumer template's published diagnostics and the on-disk OCEL log change
  live on a buffer-only edit. No artifact written.
- **Q2 authoritative stage:** the cross-surface GGEN-TPL-001/OUT-001/HARNESS-001 detection stage
  (`analyze_and_observe` → `ProjectIndex`/`HarnessIndex` → `detect_*`).
- **Q3 negative path:** disk `.rq` still broken ⇒ MUST clear from buffer (proven); empty overlay ⇒
  byte-identical disk behavior (all existing tests pass); the "disk still broken" assertion fails loudly
  if a future change reintroduces a disk write or ignores the overlay.
- **Q4 invariant:** `read_overlay_or_disk` preserves exact `read_to_string` io::Error semantics on a
  miss (same `issues` strings), so detection law is unchanged when nothing is open.
- **Q5 legacy path:** none removed; `from_root`/`from_rule` PRESERVED as empty-overlay delegates
  (non-deletion doctrine). The disk path is the documented fallback, not a bypass.
- **Q6 proof object:** `tests/ggen_live_buffer_001.rs` reads the external OCEL log (6-link chain) and
  asserts the disk `.rq` unchanged; clippy `-D warnings` = 0.

**Deepens authority:** the living loop now reflects the editor's true (unsaved) state, closing the
disk-staleness bypass the did_close honest-deviation exposed.

## Changed-file list

```
 M crates/ggen-lsp/src/harness_index.rs
 M crates/ggen-lsp/src/project_index.rs
 M crates/ggen-lsp/src/rule_index.rs
 M crates/ggen-lsp/src/state.rs
?? crates/ggen-lsp/tests/ggen_live_buffer_001.rs
?? docs/receipts/LIVE_BUFFER_001_PRE_INVENTORY.md
?? docs/receipts/LIVE_BUFFER_001_RECEIPT.md
```

## Next frontier

- **Manifest-overlay (`ggen.toml` itself):** the recommended scope keeps `ggen.toml` disk-read. An
  unsaved `ggen.toml` edit (e.g. changing a rule's `output_file` pattern for OUT-001, or rebinding a
  rule's query/template `file`) is NOT yet buffer-live. Requires a `ManifestParser::parse_str(&str)` in
  `ggen_core::manifest` (OUTSIDE the ggen-lsp write fence) — a separate checkpoint.
- **Live wiring at the LSP transport:** confirm the real `did_change` handler in `server.rs` calls
  `analyze_and_observe` with the in-flight buffer content so the overlay liveness surfaces end-to-end to
  a real editor client (this receipt proves the Client-free core; the transport wiring is the next link).
