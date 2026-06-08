# Handoff Report — Explorer 3

## 1. Observation

### Defect 1: Concurrency Race Condition in Bidirectional Symbol Lookup
*   **File Path**: `crates/genesis-construct8/src/models.rs`
*   **Line Numbers**: 136–151 (`SymbolTable::insert_custom`)
*   **Code Quote**:
    ```rust
    self.str_to_id.insert(s.to_string(), id);
    self.id_to_str.insert(id, s.to_string());
    ```
*   **Verification**: The forward mapping (`str_to_id`) is inserted first. Concurrent callers can query and retrieve the ID, but their subsequent lookup using that ID in `id_to_str` will fail if it runs before the next line completes.

---

### Defect 2: Broken Git Commit Chain History in Receipt Storage
*   **File Path**: `crates/genesis-lockchain/src/storage.rs`
*   **Line Numbers**: 149
*   **Code Quote**:
    ```rust
    let commit_id = repo
        .commit(Some("HEAD"), &sig, &sig, &msg, &tree, &[])
        .map_err(|e| StorageError::GitError(format!("Failed to create commit: {}", e)))?;
    ```
*   **Verification**: Passing `&[]` as parents when calling `repo.commit` leaves the parents of the new commit empty, making it a root commit. Every call to `append_to_git` produces an independent root commit, which orphans the previous commits.

---

### Defect 3: Silent Mapping Range Overwrites on File Collision
*   **File Path**: `crates/ggen-projection/src/mapping.rs`
*   **Line Numbers**: 20–22, 54
*   **Code Quote**:
    ```rust
    pub struct ProjectionMap {
        pub mappings: HashMap<PathBuf, ProjectionMapping>,
    }
    // ...
    self.mappings.insert(target_path, mapping);
    ```
*   **Verification**: Because `mappings` is a `HashMap` mapping `PathBuf` to a single `ProjectionMapping`, any insert to a previously mapped path will overwrite the previous entry.

---

### Defect 4: Non-Deterministic Indexes Caused by Random UUIDs
*   **File Path**: `crates/ggen-projection/src/receipt.rs`
*   **Line Numbers**: 42, 58
*   **Code Quote**:
    ```rust
    receipt_id: uuid::Uuid::new_v4().to_string(),
    // ...
    combined.extend_from_slice(r.receipt_id.as_bytes());
    ```
*   **Verification**: The `receipt_id` uses `new_v4()`, generating random values. This ID is hashed inside `update_index_hash` to compute the final `index_hash`.

---

### Defect 5: Pre-Existing Workspace Compile Blocker in `ggen-core` Test Targets
*   **File Path**: Multiple test/watch targets in `crates/ggen-core/` (e.g. `conditional_execution_tests.rs`, `llm_generation_test.rs`, `pipeline_edge_cases_test.rs`, `values_inline_enforcement_test.rs`, `lean_six_sigma.rs`, `watch.rs`, `watch_cache_integration.rs`).
*   **Git Diff**: Running `git diff` confirms that local uncommitted changes added `packs: vec![]` to struct initializers to fix compiler errors.
*   **Code Quote**:
    ```rust
    let manifest = GgenManifest {
        project: ...,
        ontology: ...,
        inference: ...,
        generation: ...,
        validation: ...,
        // packs: vec![], <-- Missing packs blocker
    };
    ```

---

## 2. Logic Chain

1.  **Defect 1**: Writing a forward lookup key (`str_to_id`) before its reverse lookup value is present in `id_to_str` exposes a transient invalid state to concurrent readers. For a table claiming concurrency safety and bidirectionality, the reverse mapping must exist before the forward lookup becomes queryable.
2.  **Defect 2**: The git commit graph forms history via pointers from commits to their parent commits. An empty parent commit slice in `repo.commit` results in git producing a root commit. Repeating this on every commit creates a chain of orphaned, independent root commits, breaking the audit history.
3.  **Defect 3**: A `HashMap` key maps to a single value. When `target_path` is the key, inserting a new mapping replaces the old mapping. The code logic loops to check for overlapping ranges, showing an intent to allow multiple non-overlapping mappings on the same path. However, due to `HashMap` key uniqueness, the second non-overlapping mapping silently overwrites the first.
4.  **Defect 4**: Random UUID v4 is generated via a pseudo-random number generator (PRNG) at runtime, leading to different UUID strings on every call. Since the combined bytes of the index hash include `receipt_id`, the index hash inherits this non-determinism, preventing reproducible/deterministic receipt generation.
5.  **Defect 5**: Adding a new non-optional field `packs` to `GgenManifest` struct definition requires all struct literal initializers to specify `packs`. Since many tests in `ggen-core` constructed `GgenManifest` directly via literal initializers and did not implement or use `Default`, adding `packs` caused compilation failures.

---

## 3. Caveats

*   **Defect 3**: Assumed that the system intends to support multiple non-overlapping ranges on the same path (Option A) based on the presence of range overlap validation. If the system only permits a single mapping per file, then Option B (returning a collision error) is the correct design. Both implementations are presented in `analysis.md`.
*   **Defect 4**: While making the `receipt_id` and `index_hash` deterministic ensures reproducible receipt indexes, the serialization of the index itself also contains timestamps (`verified_at`, `last_updated`). If exact file-level byte-for-byte serialization reproducibility is needed, the timestamps must also be frozen (e.g. Unix Epoch or sourced from `SOURCE_DATE_EPOCH`).

---

## 4. Conclusion

All five defect locations have been successfully investigated:
1.  **Defect 1**: Concurrency race condition is resolved by reversing the insertion order in `SymbolTable::insert_custom`.
2.  **Defect 2**: Broken git history is resolved by peeling `HEAD` and passing it as the parent commit in `append_to_git`.
3.  **Defect 3**: Silent mapping range overwrites are resolved by changing `mappings` type to `HashMap<PathBuf, Vec<ProjectionMapping>>` or returning a path collision error in `add_mapping`.
4.  **Defect 4**: Non-deterministic indexes are resolved by using deterministic name-based UUIDs (UUID v5 or manually constructed RFC 4122 v4 bytes from a BLAKE3 hash).
5.  **Defect 5**: The compilation blocker is resolved by adding `packs: vec![]` to struct initializers, and can be future-proofed by implementing `Default` or introducing a helper builder.

---

## 5. Verification Method

*   **Defect 1**: Verification can be done by inspecting `crates/genesis-construct8/src/models.rs` at line 136, confirming that the insertion order is `id_to_str` first, then `str_to_id`.
*   **Defect 2**: Run `cargo test -p genesis-lockchain` to run lockchain storage tests.
*   **Defect 3**: Run `cargo test -p ggen-projection` to verify that projection map tests pass. Add a test case that inserts two non-overlapping mappings on the same path and verifies that both exist (for Option A) or that the second returns an error (for Option B).
*   **Defect 4**: Run `cargo test -p ggen-projection` and verify that calling `add_receipt` twice on the same file path and content yields identical `index_hash` results.
*   **Defect 5**: Execute `cargo check --tests` in the workspace root. The command compiles cleanly, proving that the uncommitted fixes successfully resolved the `packs` compilation blocker.
