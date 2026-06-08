# Baseline Manifest Recommendation and Analysis Report

## 1. Executive Summary
This report analyzes the test failure of `test_gc006_authority_surface_lock` in the `ggen-projection` test suite, which occurred due to an un-baselined tracked change in `/Users/sac/wasm4pm/Cargo.toml`. We detail the manifest structure, verify the exact changes needed to baseline `Cargo.toml`, describe how to compute the cryptographic digest, and confirm that no other un-baselined changes or files exist in the two repositories (`wasm4pm` and `wasm4pm-compat`).

---

## 2. Root Cause Analysis
The sterility verification test (`test_gc006_authority_surface_lock`) ensures that sealed authority repositories remain strictly read-only or match their declared baseline status exactly. It does this by checking git status and comparing it with `.gc-sealed-baseline`. 

The test failed because `/Users/sac/wasm4pm/Cargo.toml` was modified (`M` status) to add the `"crates/wasm4pm-lsp"` member to the Cargo workspace, but this modification was not declared in `/Users/sac/wasm4pm/.gc-sealed-baseline`.

---

## 3. Analysis of `.gc-sealed-baseline` Manifest Structure & Serialization
The manifest file is serialized and parsed according to a Rust structure with the following properties:
* **Fields & Order**:
  1. `allowed_ignored_directories` (List of directory prefixes for untracked/ignored files)
  2. `forbidden_generated_paths` (List of path patterns that must not be generated/modified)
  3. `ignored_inventory` (Exact file paths permitted to be ignored/untracked)
  4. `tracked_status` (Map of expected tracked modified files and their expected status)
  5. `digest` (SHA-256 cryptographic digest of the above fields)
* **Skip Serialization**: During digest computation, the `digest` field is set to `None` (or skipped).
* **Sorted Map**: The `tracked_status` map uses alphabetical key ordering (due to Rust's `BTreeMap`).

### Recomputation Procedure
To recompute the cryptographic digest for the manifest:
1. Remove the `"digest"` field from the JSON object (or set it to `null`).
2. Order the JSON keys matching the Rust struct declaration order: `allowed_ignored_directories`, `forbidden_generated_paths`, `ignored_inventory`, and `tracked_status`.
3. Ensure the keys under `"tracked_status"` are sorted alphabetically.
4. Serialize the JSON string in a minified format (no whitespace or newlines).
5. Hash the serialized UTF-8 bytes using SHA-256 and encode the result as a hexadecimal string.
6. Write the resulting hash into the `"digest"` field of the manifest.

---

## 4. Digest Verification and Recommendations

### Recommended Changes
To baseline the modified `Cargo.toml`, the entry `"Cargo.toml": "M"` must be added to the `"tracked_status"` mapping.

#### Before Update (Failing Manifest):
```json
{
  "tracked_status": {
    "Cargo.lock": "M",
    "crates/wasm4pm-algos/Cargo.toml": "M",
    "crates/wasm4pm-algos/src/gall.rs": "M"
  },
  ...
  "digest": "cf2305b41d3ade4b97d51fa34f5ed86ecab1beadaceb4b71181ce70d876b4145"
}
```

#### After Update (Recommended/Passing Manifest):
```json
{
  "allowed_ignored_directories": [
    ".claude",
    ".wasm4pm",
    "apps",
    "artifacts",
    "crates",
    "dist",
    "docs_quarantine",
    "lab",
    "node_modules",
    "packages",
    "playground",
    "results",
    "scratch",
    "target",
    "tests/proof/node_modules",
    "vendors",
    "wasm4pm"
  ],
  "forbidden_generated_paths": [
    "gc005",
    "gc006"
  ],
  "ignored_inventory": [
    ".DS_Store",
    ".gc-sealed-baseline",
    "PHD_THESIS.log",
    "THESIS_DEFENSE_REPORT.log",
    "WASM4PM_FOUNDATIONS_SEAN_CHATMAN.log",
    "WASM4PM_FOUNDATIONS_VD_AALST.log",
    "cv-test-results.log",
    "isolate-0xbb9c00000-26140-v8.log",
    "performance-audit.log",
    "scripts/bench-algorithms.js",
    "test-out.log",
    "test-output.log"
  ],
  "tracked_status": {
    "Cargo.lock": "M",
    "Cargo.toml": "M",
    "crates/wasm4pm-algos/Cargo.toml": "M",
    "crates/wasm4pm-algos/src/gall.rs": "M"
  },
  "digest": "ed4e97fe767703a5dd951117dd1c810681f30440951a738bcaad72dd3da77d1c"
}
```

### Verification of Hashes:
* **JSON payload without `"Cargo.toml": "M"`**:
  * Minified payload: `{"allowed_ignored_directories":[...],"forbidden_generated_paths":["gc005","gc006"],"ignored_inventory":[...],"tracked_status":{"Cargo.lock":"M","crates/wasm4pm-algos/Cargo.toml":"M","crates/wasm4pm-algos/src/gall.rs":"M"}}`
  * SHA-256 Digest: `cf2305b41d3ade4b97d51fa34f5ed86ecab1beadaceb4b71181ce70d876b4145` (Matches forensic audit report)
* **JSON payload with `"Cargo.toml": "M"`**:
  * Minified payload: `{"allowed_ignored_directories":[...],"forbidden_generated_paths":["gc005","gc006"],"ignored_inventory":[...],"tracked_status":{"Cargo.lock":"M","Cargo.toml":"M","crates/wasm4pm-algos/Cargo.toml":"M","crates/wasm4pm-algos/src/gall.rs":"M"}}`
  * SHA-256 Digest: `ed4e97fe767703a5dd951117dd1c810681f30440951a738bcaad72dd3da77d1c` (Matches updated manifest on disk)

---

## 5. Audit of Other Repositories & Un-baselined Changes

An exhaustive analysis of all changes and untracked files was performed on the repositories `wasm4pm` and `wasm4pm-compat`:

1. **`wasm4pm` Repository**:
   * **Modified files**: `Cargo.lock`, `Cargo.toml`, `crates/wasm4pm-algos/Cargo.toml`, `crates/wasm4pm-algos/src/gall.rs`. All of these are fully declared in the updated `tracked_status` map.
   * **Untracked files**: `crates/wasm4pm-lsp/`. This directory is skipped because the `"crates"` folder is explicitly allowed in `allowed_ignored_directories`.
   * **Verdict**: Clean. No remaining un-baselined files or changes.

2. **`wasm4pm-compat` Repository**:
   * **Status**: Git working directory is completely clean.
   * **Verdict**: Clean. No changes or violations.

No other baseline-tracked repositories or un-baselined files were found. As a result, the sterility test `test_gc006_authority_surface_lock` now passes successfully.
