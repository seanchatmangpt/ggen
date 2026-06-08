# Detailed Audit Report - Four Workspaces
Timestamp: 2026-06-07T05:14:00Z

This report contains the findings of a detailed audit of the 4 workspaces: `/Users/sac/ggen`, `/Users/sac/tower-lsp-max`, `/Users/sac/wasm4pm`, and `/Users/sac/wasm4pm-compat`.

---

## 1. Plain `tower-lsp` and `tower_lsp` References

We scanned all 4 workspaces (`Cargo.toml` files, `Cargo.lock` files, and Rust source imports/usages) for any lingering plain `tower-lsp` or `tower_lsp` dependencies.

### Workspace Status
* **`/Users/sac/tower-lsp-max`**: **CLEAN**. Only depends on `tower-lsp-max` and its subcrates. No plain `tower-lsp` or `tower_lsp` dependencies or imports found.
* **`/Users/sac/wasm4pm`**: **CLEAN**. Only depends on `tower-lsp-max` (version `26.6.5` or `26.6.6` path-directed). No plain `tower-lsp` or `tower_lsp` dependencies or imports found.
* **`/Users/sac/wasm4pm-compat`**: **CLEAN**. Only depends on `tower-lsp-max` (version `26.6.5`). No plain `tower-lsp` or `tower_lsp` dependencies or imports found.
* **`/Users/sac/ggen`**: **NOT CLEAN (LSP Source Linger)**.
  * `Cargo.toml` and `Cargo.lock` files are clean and correctly declare dependencies on `tower-lsp-max` (path-directed).
  * However, multiple Rust source files inside the `ggen-lsp` package still import plain `tower_lsp` (e.g., `use tower_lsp::lsp_types::*;`), causing compilation to fail because the standard `tower-lsp` package is not declared in dependencies.
  * Lingering plain `tower_lsp` import references exist in the following files in `/Users/sac/ggen/crates/ggen-lsp/src`:
    * `src/server.rs`
    * `src/features/formatting.rs`
    * `src/features/inlay_hint.rs`
    * `src/features/workspace_symbol.rs`
    * `src/features/semantic_tokens.rs`
    * `src/handlers/diagnostics.rs`
    * `src/pack_lsp_registry.rs`
    * `src/route/edit.rs`
    * `src/route/mod.rs`
    * `src/route/model.rs`
    * `src/route/plan.rs`
    * `src/route/registry.rs`
    * `src/state.rs`
    * `tests/consolidate_002_sequence_equivalence.rs`

---

## 2. LSP Server Compilation Status

We checked the compilation of the four LSP servers using `cargo +nightly check`:

* **`ggen-lsp`** (in `/Users/sac/ggen`): **FAIL**
  * Verbatim compiler error: `error[E0433]: cannot find module or crate tower_lsp in this scope` and 111 subsequent type resolution failures.
* **`clap-noun-verb-pack-lsp`** (in `/Users/sac/ggen`): **PASS**
  * Compiles successfully with minor dead code and unused import warnings.
* **`wasm4pm-lsp`** (in `/Users/sac/wasm4pm`): **PASS**
  * Compiles successfully with minor dead code and unused result warnings.
* **`wasm4pm-compat-lsp`** (in `/Users/sac/wasm4pm-compat`): **PASS**
  * Compiles successfully with minor unused variable and private interface warnings.

---

## 3. Test Suite Run for `clap-noun-verb-pack-lsp`

We executed the full test suite in `/Users/sac/ggen` using the following exact command:
```bash
cargo +nightly test -p clap-noun-verb-pack-lsp --all-targets --target-dir /tmp/cargo-target-gc008 -- --test-threads=1
```

### Result Summary
* **Exit Code**: `0` (Success)
* **Passed**: 18 tests
* **Failed**: 0 tests

### List of Ran and Passed Tests
1. `tests/dogfood_clap_command_route.rs`:
   * `test_gc008_clap_command_route_lock` — **passed**
2. `tests/dogfood_gc004.rs`:
   * `test_gc004_pack_domain_lsp_intelligence` — **passed**
3. `tests/dogfood_gc005.rs`:
   * `test_gc005_wasm4pm_lsp_observation` — **passed**
4. `tests/dogfood_gc006_calver.rs`:
   * `test_gc006_release_law_calver_lock` — **passed**
5. `tests/dogfood_gc008_b_c.rs`:
   * `test_gc008_clap_governed_mutation_route_active` — **passed**
6. `tests/dogfood_gc008_route.rs`:
   * `test_gc008_lawful_mutation_route` — **passed**
7. `tests/dogfood_no_lsp_mutation.rs`:
   * `test_gc008_no_lsp_mutation_lock` — **passed**
8. `tests/f8_equation_enforcement.rs`:
   * `test_f8_t1_equation_enforcement_after_the_fact_laundering_fails` — **passed**
   * `test_f8_t1_equation_enforcement_boundary_change_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_customization_change_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_engine_version_change_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_missing_mutation_gate_decision_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_missing_verification_result_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_pack_descriptor_change_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_pack_plan_change_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_receipt_chain_break` — **passed**
   * `test_f8_t1_equation_enforcement_staging_change_invalidates_receipts` — **passed**
   * `test_f8_t1_equation_enforcement_workspace_change_invalidates_receipts` — **passed**

---

## 4. Suspicious or Poor Practices in `/Users/sac/ggen`

We scanned `/Users/sac/ggen` source code for suspicious or poor practices:
* **Diagnostics named DEBUG**: None found. Diagnostic codes in use are specific (e.g. `CLAP-CUSTOMIZE-001`, `CLAP-PROJECT-OPPORTUNITY-001`, `CLAP-PACK-HANDLER-UNBOUND`).
* **Prints of raw files/paths**: No `println!` or `eprintln!` calls print raw user files or paths to stdout/stderr in production paths.
* **Substring checks for authority**: None found in the active LSP code.
* **Fake log messages like "Routing to..."**:
  * Found in `crates/ggen-pack-clap-noun-verb/src/main.rs:56`:
    `self.client.log_message(MessageType::INFO, "Routing to PackPlan -> Staging -> MutationGate").await;`
    This log message is verified inside `dogfood_gc008_b_c.rs:111`.
* **Unwraps**:
  * Found in `crates/ggen-pack-clap-noun-verb/src/main.rs` (lines 39, 46, 120).
  * Found in `crates/ggen-lsp/src/features/inlay_hint.rs:394` and `crates/ggen-lsp/src/features/code_lens.rs:343`.

---

## 5. Anti-Cheating Static Surveillance Checks

* **`wasm4pm.bind_receipt`**: Not present in active production code paths (only referenced in test assertions/checks).
* **`bind_conformance_receipt`**: Not present in active production code paths (only in tests/docs).
* **`execute_command` mutation path**: Complies with the read-only boundary design. LSP does not write or mutate files.
* **`WorkspaceEdit` receipt binding**: Only present in test assertions, not in production code.
* **`std::fs::write` or `tokio::fs::write` in LSP mutation path**: No writes exist in active LSP execution paths (only in `init.rs` for writing editor configurations and in unit tests).
* **`std::fs::read_to_string` in receipt-binding path**: Not present in active LSP mutation paths.
* **`ocel.events.push` in adapter**:
  * Present in `crates/gc005-wasm4pm-adapter/src/main.rs:233`, but used only to transform and build valid standard OCEL traces. It does not inject fake receipt events.
* **manual `FIT` / `ADMITTED` returns**:
  * The adapter `gc005-wasm4pm-adapter` delegates correctly to `check_gall_conformance(ocel)` of the sealed authority `wasm4pm_algos::gall`. It maps and prints the resulting enum rather than hardcoding a "FIT" verdict.
* **fake shadow crates**:
  * No `crates/wasm4pm` or `crates/wasm4pm-compat` directories exist in `ggen` or `tower-lsp-max`.
  * `crates/wasm4pm-lsp` is present in `/Users/sac/tower-lsp-max/crates/wasm4pm-lsp`. This is a workspace member package of `tower-lsp-max` that depends on `tower-lsp-max` and `gc005-wasm4pm-adapter`.

---

## 6. CalVer Compliance

No packages in any of the workspaces use version `1.0.0` or `v1.0.0`. All workspace packages use YY.M.D CalVer compliance (specifically `26.6.5` or `26.6.6`).

---

## 7. File Digests (SHA-256)

* `ggen-lsp` `Cargo.toml`: `72f19ab9f9baa3b8ba05d9d63b533faa50b1e5bd33e0a4a2d10735e8051f35ec`
* `ggen-lsp` `src/main.rs`: `ab66500ff0d6939b7b5065fd3007277258f4a6ab93184babe4172a0399d49904`
* `clap-noun-verb-pack-lsp` `Cargo.toml`: `83c0ea83becf4b046337570453af1d090f4b151f5aa2148b25ac4f6faea4f0ae`
* `clap-noun-verb-pack-lsp` `src/main.rs`: `e45190a293b144de83d497f8528155b34e4fd368e7b95493dc789668c9952c7b`
* `wasm4pm-lsp` `Cargo.toml`: `1731e201c16861ee046c81f8c9b2981c69692929ac039abee5c490cbd8212790`
* `wasm4pm-lsp` `src/main.rs`: `9e7077ac7b506f4b9e618bceed7988297ba09833d67e432a0d14c9491d999f82`
* `wasm4pm-compat-lsp` `Cargo.toml`: `3c3f1b72d4a916222f17918927d14e316fc7628d1413cd3334c0ceb1672987de`
* `wasm4pm-compat-lsp` `src/main.rs`: `8ebe7fa1c436c657445d7f2e1d586f93b570aae436b4eb742685c1713e408e9e`
