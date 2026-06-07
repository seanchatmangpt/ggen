# Victory Audit Plan — GC006 Authority Surface Lock

This plan outlines the verification steps for validating the completion of the GC006 milestone.

## Phase A: Timeline & Provenance Audit
1. **Workspace Git Status**: Run `git status` on `ggen` workspace to check if the workspace is clean.
2. **Commit History / File Modification Dates**: Inspect files changed in the GC006 implementation (e.g. `crates/ggen-pack-proofs/templates/dogfood_gc006.rs.tmpl`, `crates/ggen-projection/Cargo.toml`, and `crates/ggen-projection/tests/dogfood_gc006.rs`).
3. **Register/Projection Check**: Verify that the 6 items of GC006 requirements are correctly registered and projected.

## Phase B: Cheating & Law Compliance Scan
1. **Forbidden Symbols Check**: Scan the source files for hardcoded path constants (`/Users/sac`) in templates and tests.
2. **No-Fake Surface Law & Correct Architecture — C4 + Filesystem Law**:
   - Check that no shadow crates exist (`wasm4pm`, `wasm4pm-proper`, `wasm4pm-compat`) inside `ggen/crates/`.
   - Verify that `wasm4pm-lsp` calls `gc005_wasm4pm_adapter::analyze_ocel` instead of running a local conformance check.
   - Verify that the neutral adapter imports and calls `check_gall_conformance` from the sealed `wasm4pm_algos` crate instead of faking verdicts.
   - Verify sibling repository `tower-lsp-max` exists and has clean/neutral adapter implementation as required.

## Phase C: Independent Test Execution
1. **Compile and Run**: Run the verifier test command:
   ```bash
   cargo test -p ggen-projection --test dogfood_gc006
   ```
2. **Output Verification**: Check that all tests pass and analyze output for any discrepancies.
3. **Record Findings**: Output the results to the final audit report.
