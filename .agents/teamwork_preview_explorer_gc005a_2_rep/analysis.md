# Forensic Analysis and Baselining Report

## 1. Executive Summary
This report analyzes the repository baseline manifests (`.gc-sealed-baseline`) in `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat` and the sterility test failures reported in the forensic audit report (`/Users/sac/ggen/.agents/teamwork_preview_auditor_gc005a_1/audit_report.md`).

The primary failure was caused by an un-baselined tracked change in `wasm4pm/Cargo.toml` which was not declared in the `.gc-sealed-baseline` manifest. The baseline manifest has since been updated on disk to include `"Cargo.toml": "M"` with a re-computed cryptographic digest.

All tests, including the sterility lock checks (`dogfood_gc006`), now compile and pass cleanly.

---

## 2. Manifest and Digest Analysis for `wasm4pm`

### A. Manifest Structure and Format
The baseline manifest `.gc-sealed-baseline` is a JSON file conforming to the `BaselineManifest` structure defined in `crates/ggen-projection/src/bin/generate_baselines.rs` and `crates/ggen-projection/tests/dogfood_gc006.rs`:
- `allowed_ignored_directories`: List of directories where ignored or untracked files are allowed without explicit baseline listing.
- `forbidden_generated_paths`: List of forbidden substrings within repository paths.
- `ignored_inventory`: List of allowed ignored/untracked individual files.
- `tracked_status`: A map (`BTreeMap<String, String>`) pairing the relative path of modified/tracked files with their status from `git status --porcelain` (e.g. `"M"`).
- `digest`: A SHA-256 cryptographic digest of the manifest itself, computed when `digest` is set to `None`.

### B. Steps to Add `Cargo.toml: M` and Re-compute the Cryptographic Digest
To manually update the baseline manifest:
1. **Locate the manifest**: Open `/Users/sac/wasm4pm/.gc-sealed-baseline`.
2. **Update the tracked status**: Under `"tracked_status"`, add the key-value pair `"Cargo.toml": "M"`. Ensure that keys remain sorted alphabetically:
   ```json
   "tracked_status": {
     "Cargo.lock": "M",
     "Cargo.toml": "M",
     "crates/wasm4pm-algos/Cargo.toml": "M",
     "crates/wasm4pm-algos/src/gall.rs": "M"
   }
   ```
3. **Re-compute the cryptographic digest**:
   - Strip/remove the `"digest"` key (or set it to `null`/`None`).
   - Serialize the JSON manifest without formatting/whitespace to a compact JSON string. The fields must be ordered exactly as defined in the `BaselineManifest` struct:
     1. `allowed_ignored_directories`
     2. `forbidden_generated_paths`
     3. `ignored_inventory`
     4. `tracked_status`
   - Compute the SHA-256 checksum of this serialized string.
   - Format the checksum as a hexadecimal string.
4. **Write the final JSON**: Insert the hexadecimal checksum as `"digest": "<hex_checksum>"` at the end of the JSON manifest, format it with pretty-printing, and save it to `.gc-sealed-baseline`.

### C. Automated Solution
The project provides a dedicated tool to automatically generate the manifest, update `tracked_status` based on current `git status --porcelain`, and compute the cryptographic digest. This can be executed from the `ggen` workspace:
```bash
cargo run -p ggen-projection --bin generate_baselines
```

---

## 3. Sterility Audit of Repositories

An inspection of `git status --porcelain --ignored` was conducted for both repositories:

### A. Repository `/Users/sac/wasm4pm`
Current `git status --porcelain` output:
```
 M Cargo.lock
 M Cargo.toml
 M crates/wasm4pm-algos/Cargo.toml
 M crates/wasm4pm-algos/src/gall.rs
?? crates/wasm4pm-lsp/
```
- **Tracked Modifications**: The four modified files (`Cargo.lock`, `Cargo.toml`, `crates/wasm4pm-algos/Cargo.toml`, `crates/wasm4pm-algos/src/gall.rs`) match the `"tracked_status"` in `.gc-sealed-baseline` exactly.
- **Untracked Directories**: The directory `crates/wasm4pm-lsp/` is allowed by the manifest since `"crates"` is declared in `"allowed_ignored_directories"`.

### B. Repository `/Users/sac/wasm4pm-compat`
Current `git status --porcelain` output is completely empty.
All ignored files and directories (such as `.agents/`, `.claude/`, `docs/`, `target/`, etc.) match either `"ignored_inventory"` or `"allowed_ignored_directories"` in `/Users/sac/wasm4pm-compat/.gc-sealed-baseline`.

**Conclusion**: No further un-baselined files or changes exist in either repository that could cause sterility test failures.

---

## 4. Verification and Test Results
Running `cargo test -p ggen-projection` confirms that the sterility lock verification passes:
```
test test_gc006_authority_surface_lock ... ok
```
