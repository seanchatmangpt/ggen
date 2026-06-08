# Forensic Audit Report

**Work Product**: GC005A, crates/gc005-wasm4pm-adapter, wasm4pm-lsp, and `.gc-sealed-baseline` manifests
**Profile**: General Project (Benchmark Mode)
**Verdict**: INTEGRITY VIOLATION

---

### Phase Results

1. **Hardcoded output detection**: **PASS**
   - **Details**: Checked the implementations of `check_gall_conformance` in `wasm4pm_algos/src/gall.rs`, `gc005-wasm4pm-adapter` (both in `ggen` and `tower-lsp-max`), and `wasm4pm-lsp` (`wasm4pm/crates/wasm4pm-lsp/src/main.rs`). None of these components hardcode expected outputs or manufacture fitness values.

2. **Facade detection**: **PASS**
   - **Details**: Every component implements genuine logic. The LSP server spawns the adapter process, feeds it input via stdio, and parses structural JSON diagnostics. The adapter delegates to the authoritative `check_gall_conformance` library. `check_gall_conformance` parses the OCEL and performs actual checkpoint fitness/conformance replaying and receipt chain checking.

3. **Pre-populated artifact detection**: **PASS**
   - **Details**: No pre-populated logs, result files, or fake verification artifacts exist in the workspaces.

4. **Build and run**: **FAIL**
   - **Details**: While `cargo test` runs and compiles cleanly across crates, the cleanliness baseline test `dogfood_gc006::test_gc006_authority_surface_lock` fails. It panics because the sealed repository `wasm4pm` has an uncommitted tracked change (`Cargo.toml`) that is not declared in the `.gc-sealed-baseline` manifest.

5. **Output verification**: **PASS**
   - **Details**: The end-to-end integration tests (`dogfood_gc005::test_gc005_wasm4pm_lsp_observation` and `dogfood_gc007::test_gc007_wasm4pm_lsp_ownership_relocation`) pass successfully. The LSP server correctly publishes `WASM4PM-VERDICT-FIT`, `WASM4PM-VERDICT-DEVIATION`, and `WASM4PM-VERDICT-BLOCKED` diagnostics in response to valid, deviated, and tampered OCEL inputs respectively.

6. **Dependency audit**: **PASS**
   - **Details**: The adapter uses the standard workspace libraries and delegates strictly to the authoritative `wasm4pm-algos` dependency in `/Users/sac/wasm4pm/crates/wasm4pm-algos`. No forbidden libraries or external wrappers are used for core logic.

7. **Cryptographic digest validation**: **PASS**
   - **Details**: The baseline manifest `.gc-sealed-baseline` files contain valid SHA-256 digests. Setting the digest key to `None` and serializing the rest of the manifest keys in alphabetical order produces a JSON representation whose SHA-256 digest matches the declared `digest` value perfectly.

---

### Evidence

#### 1. Verbatim Test Failure (from `dogfood_gc006`)
```
running 1 test
test test_gc006_authority_surface_lock ... FAILED

failures:

---- test_gc006_authority_surface_lock stdout ----

thread 'test_gc006_authority_surface_lock' (1857451) panicked at crates/ggen-projection/tests/dogfood_gc006.rs:125:25:
New tracked change found in sealed repo wasm4pm: Cargo.toml (status:  M)
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

#### 2. Root Cause: Git Status in `/Users/sac/wasm4pm`
```
 M Cargo.lock
 M Cargo.toml
 M crates/wasm4pm-algos/Cargo.toml
 M crates/wasm4pm-algos/src/gall.rs
?? crates/wasm4pm-lsp/
```

#### 3. Git Diff in `/Users/sac/wasm4pm/Cargo.toml`
```diff
diff --git a/Cargo.toml b/Cargo.toml
index dafca394..bf7660bd 100644
--- a/Cargo.toml
+++ b/Cargo.toml
@@ -1,7 +1,8 @@
 [workspace]
 resolver = "2"
 
-members = ["wasm4pm", "tps-metrics", "crates/wasm4pm-types", "crates/wasm4pm-algos", "crates/wasm4pm-cli", "crates/wasm4pm-utils", "crates/miniml-core", "crates/wasm4pm-cognition", "crates/prolog8", "crates/wasm4pm-macros", "crates/ocel-core", "crates/ocpq", "crates/pm-core", "crates/pm4py-lsp"]
+members = [
+  "crates/wasm4pm-lsp","wasm4pm", "tps-metrics", "crates/wasm4pm-types", "crates/wasm4pm-algos", "crates/wasm4pm-cli", "crates/wasm4pm-utils", "crates/miniml-core", "crates/wasm4pm-cognition", "crates/prolog8", "crates/wasm4pm-macros", "crates/ocel-core", "crates/ocpq", "crates/pm-core", "crates/pm4py-lsp"]
 
 [workspace.package]
 version = "26.6.5"
```

#### 4. Baseline Manifest in `/Users/sac/wasm4pm/.gc-sealed-baseline`
```json
{
  "tracked_status": {
    "Cargo.lock": "M",
    "crates/wasm4pm-algos/Cargo.toml": "M",
    "crates/wasm4pm-algos/src/gall.rs": "M"
  },
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
  "digest": "cf2305b41d3ade4b97d51fa34f5ed86ecab1beadaceb4b71181ce70d876b4145"
}
```

#### 5. Baseline Manifest in `/Users/sac/wasm4pm-compat/.gc-sealed-baseline`
```json
{
  "tracked_status": {},
  "ignored_inventory": [
    ".DS_Store",
    ".gc-sealed-baseline",
    "conformance_verdict_is_perfect",
    "libmod.rlib",
    "librust_out.rlib"
  ],
  "allowed_ignored_directories": [
    ".agents",
    ".claude",
    "docs",
    "ggen",
    "target",
    "target-lsp",
    "target_lsp",
    "wasm4pm-compat-lsp",
    "wip"
  ],
  "forbidden_generated_paths": [
    "gc005",
    "gc006"
  ],
  "digest": "6e5bb6a5c792fbe883d79bb820c127207494846403248d858e783ece24f28710"
}
```

---

### Conclusion
The work product has an **authentic, robust implementation** that matches the Chicago TDD requirements. The LSP, adapter, and conformance checker are correctly integrated and function properly. However, under the strict workspace sterility baseline check (`dogfood_gc006`), it fails due to an un-baselined tracked change in `Cargo.toml` in `/Users/sac/wasm4pm`. Consequently, the final audit verdict is **INTEGRITY VIOLATION** due to the failing sterility verification test.
