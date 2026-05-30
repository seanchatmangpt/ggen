# LIVE-BUFFER-001 — Pre-Inventory (Live-Buffer Architect)

**Checkpoint:** LIVE-BUFFER-001 — make the cross-surface living loop LIVE ON THE OPEN BUFFER, not just disk.
**Repo:** ggen @ /Users/sac/ggen · **O\*:** main @ 8f3d11ef (CLEAN; species GGEN-TPL-001, GGEN-HARNESS-001, GGEN-OUT-001 ALIVE).
**Author role:** READ-ONLY architect. This doc is the only write. No `src/` mutation in this turn.

**VERDICT: READY** — buffer-overlay is behavior-EXTENDING and natural (no invasive rewrite). Verbatim implementation diff plan below.

---

## 1. The gap (confirmed against real files)

`ServerState::analyze_and_observe(uri, content)` (crates/ggen-lsp/src/state.rs:442) and
`ServerState::close_document(uri)` (state.rs:335) run the cross-surface detectors:

| Detector (state.rs) | Index constructor | Disk reads |
|---|---|---|
| `detect_tpl_001_for` (state.rs:644) | `ProjectIndex::from_root` (project_index.rs:70) | `ggen.toml` + each rule's `.rq`/`.tera` via `RuleIndexEntry::from_rule` |
| `detect_out_001_for` (state.rs:660) | `ProjectIndex::from_root` (project_index.rs:70) | same as above |
| `detect_harness_001_for` (state.rs:697) | `HarnessIndex::from_root` (harness_index.rs:97) | `Cargo.toml` + `tests/**`,`benches/**` enumeration |

`RuleIndexEntry::from_rule` (rule_index.rs:56) reads the SPARQL query and the Tera template
**off disk** at rule_index.rs:69 and rule_index.rs:88:
```rust
let resolved = manifest_dir.join(file);
match std::fs::read_to_string(&resolved) { ... }   // query  (line 69)
match std::fs::read_to_string(&resolved) { ... }   // template (line 88)
```
`HarnessIndex::from_root` reads the manifest off disk at harness_index.rs:109
(`std::fs::read_to_string(&manifest_path)`).

So when an editor edits a `.rq` **in-buffer (unsaved)** and calls `analyze_and_observe(rq_uri, new_query)`:
- the edited file's OWN single-file diagnostics use the passed `content` (state.rs:450 `build_analyzer(uri.path(), content)`), BUT
- the cross-surface `ProjectIndex::from_root` re-reads the **stale on-disk** `.rq`, so the consumer template's GGEN-TPL-001 (and the manifest's GGEN-OUT-001) do **not** update until the `.rq` is saved.

`ServerState` already holds open buffers: `documents: Arc<Mutex<HashMap<Url,String>>>` (state.rs:64),
accessor `get_document` (state.rs:292). They are simply **not consulted** by the index path.

**Honest deviation that surfaced this:** the existing living-loop proof
(`tests/ggen_tpl_001_living_loop.rs:499`) writes the repaired query to **disk**
(`std::fs::write(&rq_path, repaired_rq)`) before re-analyzing. That on-disk write masks
whether the loop is buffer-live. With buffer liveness, the same clear must hold with the
disk `.rq` STILL broken.

---

## 2. The relation / invariant

> The project graph the cross-surface detectors run over MUST reflect OPEN-BUFFER content for
> any file currently open (an overlay of the buffer over disk), falling back to disk for any
> file not open.

**Behavior-extending:** with **no open buffers** (the existing disk-fixture unit tests in
`project_index.rs`, `harness_index.rs`, `rule_index.rs`, and the disk-driven living-loop tests),
the overlay map is empty and every read falls through to `std::fs::read_to_string` — **byte-identical**
to today. The new behavior only ever ADDS: an open buffer shadows its disk file.

---

## 3. The async/sync boundary (the one real design constraint)

`detect_*_for` are **synchronous** `fn`s (state.rs:644/660/697); `self.documents` is a **tokio**
`Mutex` requiring `.lock().await`. A sync `fn` cannot `.await`. Two clean options; this design takes (A):

- **(A) Snapshot-and-thread (CHOSEN).** The async callers `analyze_and_observe` (state.rs:442)
  and `close_document` (state.rs:335) ALREADY run in async context. They snapshot
  `self.documents` once into a plain `HashMap<PathBuf,String>` (Url→disk-path keyed), then pass
  `&overlay` down through `detect_*_for` → `*_from_root_with_overlay` → `RuleIndexEntry::from_rule_with_overlay`.
  The detectors stay sync; no lock held across the index build. The overlay is keyed by **disk PathBuf**
  (not Url) so the index read sites (`resolved`, `manifest_path`) look up directly with the path they
  already compute — no Url reconstruction inside the index.

- (B) Make `detect_*_for` async and `.await` the lock inside. Rejected: wider blast radius
  (every detector + `close_document` already calls them sync), holds the documents lock across
  the whole index build (I/O under lock), and changes more signatures than (A).

**Overlay key choice — disk PathBuf, canonicalized-by-construction.** Buffers are keyed by `Url`.
The index resolves files by `manifest_dir.join(file)` / `root.join("Cargo.toml")` — plain (un-canonicalized)
joins. To match, the snapshot converts each open `Url` to a path via `uri.to_file_path()` and stores it
**without canonicalization**, mirroring exactly how the index builds its lookup path (both are
`root`/`manifest_dir` joins of the same components on the same OS). This keeps the comparison consistent —
the same discipline `harness_index.rs:90` already documents ("no canonicalization required").

---

## 4. The buffer-overlay design (signatures)

A tiny type alias keeps the surface uniform and self-documenting:

```rust
// crates/ggen-lsp/src/project_index.rs  (top, shared via a small module or re-export)
/// Open-buffer overlay: disk path → in-buffer (possibly unsaved) content.
/// An empty overlay makes every read fall back to disk (byte-identical to no-overlay).
pub type BufferOverlay = std::collections::HashMap<std::path::PathBuf, String>;
```

### 4.1 `ProjectIndex` (project_index.rs)

Keep `from_root` as the disk-only entry (delegates to the overlay form with an empty map, so the
existing 5 unit tests at project_index.rs:109-267 pass UNCHANGED). Add the overlay form:

```rust
impl ProjectIndex {
    /// Disk-only (unchanged public API). Delegates to the overlay form with an
    /// empty overlay → byte-identical to today.
    pub fn from_root(root: &Path) -> Result<ProjectIndex, IndexError> {
        Self::from_root_with_overlay(root, &BufferOverlay::new())
    }

    /// Overlay form: any file present in `overlay` (keyed by its on-disk path) is
    /// read from the buffer instead of disk; absent files fall back to disk.
    /// The manifest itself is overlay-aware (an unsaved `ggen.toml` edit is honored),
    /// and each rule's query/template read is overlay-aware via `from_rule_with_overlay`.
    pub fn from_root_with_overlay(
        root: &Path, overlay: &BufferOverlay,
    ) -> Result<ProjectIndex, IndexError> {
        let manifest_path = root.join("ggen.toml");
        // Manifest existence still gates on disk (an unsaved-but-never-saved ggen.toml
        // has no path to resolve rule files against); parse from overlay if open.
        if !manifest_path.is_file() {
            return Err(IndexError::ManifestNotFound { path: manifest_path });
        }
        let manifest = match overlay.get(&manifest_path) {
            Some(buf) => ManifestParser::parse_str(buf)            // see §4.4 note
                .map_err(|err| IndexError::ManifestParse {
                    path: manifest_path.clone(), message: err.to_string(),
                })?,
            None => ManifestParser::parse(&manifest_path)
                .map_err(|err| IndexError::ManifestParse {
                    path: manifest_path.clone(), message: err.to_string(),
                })?,
        };
        let rule_entries = manifest.generation.rules.iter()
            .map(|rule| RuleIndexEntry::from_rule_with_overlay(rule, &manifest_path, overlay))
            .collect();
        Ok(ProjectIndex { root: root.to_path_buf(), rule_entries })
    }
}
```

### 4.2 `RuleIndexEntry` (rule_index.rs)

Keep `from_rule` (delegates with empty overlay → existing 7 unit tests at rule_index.rs:155-350
pass UNCHANGED). Add the overlay form; the ONLY change is the two `read_to_string` sites become
"overlay-then-disk":

```rust
impl RuleIndexEntry {
    pub fn from_rule(rule: &GenerationRule, manifest_path: &Path) -> RuleIndexEntry {
        Self::from_rule_with_overlay(rule, manifest_path, &BufferOverlay::new())
    }

    pub fn from_rule_with_overlay(
        rule: &GenerationRule, manifest_path: &Path, overlay: &BufferOverlay,
    ) -> RuleIndexEntry {
        // ... manifest_dir + issues unchanged ...
        // QUERY (was rule_index.rs:69):
        let (query_inline, query_content) = match &rule.query {
            QuerySource::Inline { inline } => (true, inline.clone()),
            QuerySource::File { file } => {
                let resolved = manifest_dir.join(file);
                match read_overlay_or_disk(overlay, &resolved) {       // <- only change
                    Ok(text) => (false, text),
                    Err(err) => { issues.push(format!("query file missing: {} ({})", resolved.display(), err)); (false, String::new()) }
                }
            }
        };
        // TEMPLATE (was rule_index.rs:88): identical overlay-or-disk swap.
        // ... rest unchanged (select_projection_vars, struct build) ...
    }
}

/// Overlay-then-disk read. Empty/absent overlay key → exact `std::fs::read_to_string`
/// semantics (same `io::Error` on miss), so disk-only behavior is byte-identical.
fn read_overlay_or_disk(
    overlay: &BufferOverlay, path: &Path,
) -> std::io::Result<String> {
    if let Some(buf) = overlay.get(path) {
        return Ok(buf.clone());
    }
    std::fs::read_to_string(path)
}
```

### 4.3 `HarnessIndex` (harness_index.rs)

Mirror exactly. `from_root` delegates with empty overlay (existing 7 unit tests at
harness_index.rs:218-333 pass UNCHANGED). The overlay touches ONLY the `Cargo.toml` text read
(harness_index.rs:109). The `enumerate_proof_files` WalkDir enumeration (harness_index.rs:184)
stays disk-based: a proof file's *existence* is a disk fact (an unsaved-but-never-created file is
not on disk and the editor would not hold a buffer for a nonexistent path under our key scheme); the
overlay only shadows the **manifest text**, which is what the predicate compares against.

```rust
impl HarnessIndex {
    pub fn from_root(root: &Path) -> Result<HarnessIndex, HarnessIndexError> {
        Self::from_root_with_overlay(root, &BufferOverlay::new())
    }
    pub fn from_root_with_overlay(
        root: &Path, overlay: &BufferOverlay,
    ) -> Result<HarnessIndex, HarnessIndexError> {
        let manifest_path = root.join("Cargo.toml");
        let existing_files = enumerate_proof_files(root);   // disk fact, unchanged
        if !manifest_path.is_file() && !overlay.contains_key(&manifest_path) {
            return Ok(HarnessIndex { root: root.to_path_buf(), targets: Vec::new(), existing_files });
        }
        let raw = match overlay.get(&manifest_path) {
            Some(buf) => buf.clone(),
            None => std::fs::read_to_string(&manifest_path)
                .map_err(|e| HarnessIndexError::ManifestRead { path: manifest_path.clone(), message: e.to_string() })?,
        };
        // ... toml::from_str(&raw) + collect_targets unchanged ...
    }
}
```

### 4.4 Manifest-from-string note (only if `ggen.toml` itself is to be overlay-aware)

`ManifestParser::parse(&Path)` is the current API (project_index.rs:79). If the orchestrator wants an
unsaved `ggen.toml` honored too, a sibling `ManifestParser::parse_str(&str)` (in `ggen_core::manifest`)
is required. **This is OUTSIDE `crates/ggen-lsp/`** and therefore outside this checkpoint's write fence.

**Scoping decision (recommended):** ship the overlay for **`.rq`/`.tera` rule files first** (the
GGEN-TPL-001/OUT-001 producer surfaces — exactly the gap the honest deviation found), and read the
**`ggen.toml` manifest from disk** in `from_root_with_overlay` (i.e. drop the `overlay.get(&manifest_path)`
branch in §4.1, keep `ManifestParser::parse(&manifest_path)`). This keeps the entire patch INSIDE
`crates/ggen-lsp/` (no `ggen_core` change), still closes the stated gap (the living-loop proof edits a
`.rq`, not the manifest), and is the minimal behavior-extending move. The manifest-overlay branch can be a
follow-up once `ManifestParser::parse_str` lands. **The diff plan in §6 assumes this recommended scope**
(`.rq`/`.tera` + `Cargo.toml` text overlay; `ggen.toml` stays disk-read).

### 4.5 Detector wiring (state.rs)

`detect_*_for` gain an `overlay: &BufferOverlay` parameter and forward it:

```rust
fn detect_tpl_001_for(&self, uri: &Url, overlay: &BufferOverlay) -> Vec<(PathBuf, Vec<Diagnostic>)> {
    let Some(root) = self.project_root_for(uri) else { return Vec::new(); };
    match crate::project_index::ProjectIndex::from_root_with_overlay(&root, overlay) {
        Ok(project) => crate::analyzers::detect_tpl_001(&project),
        Err(_) => Vec::new(),
    }
}
// detect_out_001_for, detect_harness_001_for: identical overlay forward.
```

The async callers snapshot once and thread it:

```rust
// In analyze_and_observe (state.rs:442) and close_document (state.rs:335), BEFORE the detect_* calls:
let overlay = self.buffer_overlay().await;   // snapshot of self.documents as PathBuf->String
// ...then every self.detect_*_for(uri) becomes self.detect_*_for(uri, &overlay).
```

New helper on `ServerState`:

```rust
/// Snapshot every open buffer as a disk-path→content overlay. Keys are
/// `Url::to_file_path()` (non-canonicalized, matching the index's join-based
/// resolution). Non-file URLs are skipped. Empty when nothing is open →
/// every index read falls back to disk.
async fn buffer_overlay(&self) -> BufferOverlay {
    let docs = self.documents.lock().await;
    docs.iter()
        .filter_map(|(u, c)| u.to_file_path().ok().map(|p| (p, c.clone())))
        .collect()
}
```

**Critical liveness detail for `analyze_and_observe`:** the edited buffer's content arrives as the
`content` argument, which may NOT yet be in `self.documents` (the server's `did_change` updates
`self.documents` and calls `refresh_analyzer`; order matters). To guarantee the edited file's own
new content participates in the overlay regardless of insertion order, the snapshot must **also insert
the edited (uri, content) pair**:

```rust
let mut overlay = self.buffer_overlay().await;
if let Ok(p) = uri.to_file_path() { overlay.insert(p, content.to_string()); }
```

This makes the producer edit (the `.rq`) visible to the consumer (`.tera`) within the SAME
`analyze_and_observe` call — the exact liveness the proof requires. `close_document` does NOT insert
(the closed doc was already removed from `self.documents` at state.rs:338, and its disk content is the
correct fallback).

---

## 5. ALIVE / FAKE-LIVE / BLOCKED (concrete)

### ALIVE (acceptance)
1. New living-loop proof (`tests/live_buffer_001_living_loop.rs`, §6.4):
   - writes an **invalid** project to a TempDir (query SELECTs `?name`; template wants `row["title"]`);
   - `state.set_document(rq_uri, broken_on_disk_query)` is NOT used — instead the repaired query is
     applied **buffer-only**: `state.analyze_and_observe(&rq_uri, repaired_query)` is called with the
     **disk `.rq` file LEFT UNCHANGED (still broken)**;
   - the template's GGEN-TPL-001 **clears** through `observe_diagnostics` (the returned publish set
     re-publishes `tera_uri` with residual, i.e. no GGEN-TPL-001);
   - the EXTERNAL on-disk OCEL log (`<root>/.ggen/ocel/agent-edit-events.ocel.jsonl`) shows the full
     **6-link chain** for the template: `DiagnosticRaised → RouteSelected → RepairSuggested →
     RepairApplied → GatePassed → ReceiptEmitted`;
   - an explicit assertion reads the disk `.rq` back and confirms it STILL contains the broken query
     (`SELECT ?name` only) — proving the clear came from the BUFFER, not disk;
   - `out.txt` (the rule's `output_file`) is asserted absent (analysis writes no artifact).
2. EVERY existing test passes UNCHANGED — the 5 `project_index.rs` + 7 `rule_index.rs` +
   7 `harness_index.rs` unit tests (empty overlay = disk-identical) and the 3 disk-driven living-loop
   integration tests (`ggen_tpl_001_living_loop.rs`, `ggen_out_001_living_loop.rs`,
   `ggen_harness_001_living_loop.rs`) — NONE edited.
3. `detector_active` unchanged; `clippy -p ggen-lsp --no-deps -- -D warnings` stays 0; no `#[allow]`.

### FAKE-LIVE (rejection)
- The detector still reads disk for the producer surface (overlay ignored / not threaded).
- The proof writes the repair to disk (`std::fs::write(&rq_path, ...)`) — then it is NOT buffer-only and
  is indistinguishable from the existing disk test.
- Any existing test had to be EDITED to keep passing (means the disk path was not preserved).
- The 6-link chain is incomplete (any of the 6 activities missing for the template).
- The "disk still broken" assertion is absent (cannot distinguish buffer-live from disk-live).

### BLOCKED (only if the mapper finds it invasive — it does NOT here)
- Would trigger ONLY if `ProjectIndex`/`RuleIndexEntry`/`HarnessIndex` could not accept an overlay
  without rewriting their construction shape (e.g. if rule files were read eagerly inside an
  un-parameterizable closure, or if the manifest API forced a path-only read with no string entry AND
  manifest-overlay were in scope). Neither holds: the reads are two plain `read_to_string` call sites
  (rule_index.rs:69, :88) and one (harness_index.rs:109), each trivially swappable for `read_overlay_or_disk`.
  The recommended scope (§4.4) keeps `ggen.toml` disk-read, so no `ggen_core` change is forced.
  **Therefore: NOT BLOCKED.**

---

## 6. Verbatim implementation diff plan (recommended scope: §4.4 — `.rq`/`.tera`/`Cargo.toml` overlay, `ggen.toml` disk-read)

All edits inside `crates/ggen-lsp/` except the new test (also inside). SINGLE-WRITER. No `#[allow]`.

### 6.1 `crates/ggen-lsp/src/project_index.rs`
- Add at top (after imports): `pub type BufferOverlay = std::collections::HashMap<std::path::PathBuf, String>;`
- Replace the body of `from_root` (lines 70-96) with a one-line delegate:
  `pub fn from_root(root: &Path) -> Result<ProjectIndex, IndexError> { Self::from_root_with_overlay(root, &BufferOverlay::new()) }`
- Add `from_root_with_overlay` per §4.1 BUT with the manifest read kept as
  `ManifestParser::parse(&manifest_path)` (disk; recommended scope), and rule mapping calling
  `RuleIndexEntry::from_rule_with_overlay(rule, &manifest_path, overlay)`.
- Existing tests (lines 99-268): UNCHANGED.

### 6.2 `crates/ggen-lsp/src/rule_index.rs`
- Import `BufferOverlay` (`use crate::project_index::BufferOverlay;`).
- Replace `from_rule` (lines 56-129) body with delegate:
  `Self::from_rule_with_overlay(rule, manifest_path, &BufferOverlay::new())`.
- Add `from_rule_with_overlay` = the current body verbatim EXCEPT the two `std::fs::read_to_string(&resolved)`
  sites (lines 69, 88) become `read_overlay_or_disk(overlay, &resolved)`.
- Add free fn `read_overlay_or_disk` per §4.2.
- Existing tests (lines 132-351): UNCHANGED.

### 6.3 `crates/ggen-lsp/src/harness_index.rs`
- Import `BufferOverlay`.
- Replace `from_root` (lines 97-132) body with delegate:
  `Self::from_root_with_overlay(root, &BufferOverlay::new())`.
- Add `from_root_with_overlay` per §4.3 (overlay shadows the `Cargo.toml` text read at line 109;
  `enumerate_proof_files` unchanged; the existence guard becomes `!is_file() && !overlay.contains_key`).
- Existing tests (lines 205-334): UNCHANGED.

### 6.4 `crates/ggen-lsp/src/state.rs`
- Add `use crate::project_index::BufferOverlay;` (line 1-8 area).
- Add `buffer_overlay(&self)` helper per §4.5 (snapshot of `self.documents` as `PathBuf→String`).
- `detect_tpl_001_for` / `detect_out_001_for` / `detect_harness_001_for` (lines 644/660/697):
  add `overlay: &BufferOverlay` param; call `*_from_root_with_overlay(&root, overlay)`.
- `analyze_and_observe` (line 442): after computing `file_type`, add
  `let mut overlay = self.buffer_overlay().await; if let Ok(p) = uri.to_file_path() { overlay.insert(p, content.to_string()); }`
  Pass `&overlay` to all three `self.detect_*_for(uri, ...)` calls (lines 465, 475, 486).
  `residual_single_file_diags` (line 627) already prefers `get_document` then disk — it is buffer-aware
  for the CLEARED surface's OWN single-file diags; no change needed (the cross-surface clear it serves is
  now driven by the overlay-aware detector).
- `close_document` (line 335): add `let overlay = self.buffer_overlay().await;` after the
  `remove_document` call (line 338); pass `&overlay` to the three `self.detect_*_for(uri, ...)` calls
  (lines 353, 369, 388). (No edited-pair insertion — the closed doc is gone; disk is correct fallback.)

### 6.5 `crates/ggen-lsp/tests/live_buffer_001_living_loop.rs` (NEW)
- Chicago TDD, mirrors `ggen_tpl_001_living_loop.rs` test #5 structure (helpers `write_project`,
  `read_log_lines`, `has_template_event`, `is_tpl_001` reused/copied), with the CRITICAL difference:
  the repair is applied **buffer-only** via `analyze_and_observe(&rq_uri, repaired_rq)` while the disk
  `.rq` is LEFT BROKEN. Asserts: (a) raise on template; (b) clear of `tera_uri` in the returned set
  with no GGEN-TPL-001; (c) 6-link chain in the external OCEL log; (d) disk `.rq` still reads the broken
  `SELECT ?name`-only query (the buffer-only proof); (e) `out.txt` absent.

---

## 7. Patch contract self-check (coding-agent-mistakes §3)

- **Q1 real state changed:** the in-memory `ProjectIndex.rule_entries[*].query_content` / `selected_vars`
  now reflect the open buffer; the consumer template's published diagnostic set (and the on-disk OCEL log
  under `<root>/.ggen/ocel/`) changes live on a buffer-only edit. No artifact written.
- **Q2 authoritative path:** the GGEN-TPL-001/OUT-001/HARNESS-001 cross-surface detection stage
  (`analyze_and_observe` → `ProjectIndex`/`HarnessIndex` → `detect_*`).
- **Q3 negative path:** empty overlay ⇒ byte-identical disk behavior (all existing disk tests still pass);
  a buffer-only repair with the disk file still broken MUST clear the consumer (new proof), and the
  "disk still broken" assertion fails loudly if a future change reintroduces a disk write.
- **Q4 invariant:** the overlay-or-disk read preserves exact `read_to_string` error semantics on miss
  (same `io::Error` → same `issues` strings), so detection law is unchanged when nothing is open.
- **Q5 legacy path removed:** no legacy path removed; `from_root`/`from_rule` are PRESERVED as
  empty-overlay delegates (non-deletion doctrine). The disk path is not a bypass — it is the documented
  fallback the overlay extends.
- **Q6 proof object:** new `live_buffer_001_living_loop.rs` reads the external OCEL log (6-link chain) +
  asserts the disk `.rq` unchanged; `clippy -p ggen-lsp --no-deps -- -D warnings` = 0.

**Deepens authority:** the living loop now reflects the editor's true (unsaved) state, closing the
disk-staleness bypass the did_close deviation exposed.

---

**RETURN: READY**
