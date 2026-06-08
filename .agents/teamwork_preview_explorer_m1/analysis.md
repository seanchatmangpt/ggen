# GGEN Projection & tower-lsp-max E2E Testing Strategy

## 1. Executive Summary

This report establishes a comprehensive E2E testing strategy for the **80/20 Projection Core and Pack LSPs** (ggen Projection Intelligence) project, focusing on `ggen-projection`, `ggen-lsp`, and `tower-lsp-max`. 

Based on a read-only codebase audit:
1. **Workspace and `tower-lsp-max`**: `tower-lsp-max` is located in a separate, hand-authored workspace directory (`/Users/sac/tower-lsp-max`) and contains active development crates (e.g. `tower-lsp-max-base`, `tower-lsp-max-runtime`, `tower-lsp-max-protocol`, `tower-lsp-max-agent`). It is not currently registered inside `/Users/sac/ggen/Cargo.toml` as a member, but it is the target destination of projected CLI/LSP examples.
2. **Current `ggen-projection` Public API**: The existing crate `crates/ggen-projection` currently contains a KNHK Relational Projection implementation (`RelationPage`, `Pair2`, N-Quads, OCEL2, PROV, DCAT, SHACL). It does **not** contain any definitions for `PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, or `ReceiptIndex`. These are planned for Milestone 2.
3. **E2E Testing Methodology**: E2E tests will run under `crates/ggen-projection/tests/` using **Chicago TDD & Real Boundary Crossing** (no stubs or mocks). They will verify the 6 core features (F1–F6) across 4 surfaces of corroboration (Execution, Telemetry, Evidence, Causality).
4. **Constitutional Compliance**: To resolve compilation in Milestone 1 without violating the `AGENTS.md` Constitution (which bans London TDD stubs and fake placeholders), we recommend that the implementer write **real, schema-complete data structure definitions** for the M2 structs from day one (Option B). Alternatively, E2E tests can be placed under a conditional feature flag (`e2e_tests`) so they do not block standard compilation before M2 (Option A).

---

## 2. Workspace Status and API Audit

### 2.1 tower-lsp-max
* **Location**: Located in `/Users/sac/tower-lsp-max`, which is structured as its own independent Cargo workspace.
* **Crate Structure**:
  * `tower-lsp-max-base` (traits like `SourceObservation`, `ParseIngress`, `RelationAdmitter`, `StaticIndexEmitter`)
  * `tower-lsp-max-protocol` (defines custom LSP structures and `MaxDiagnostic`)
  * `tower-lsp-max-runtime` (contains `AutonomicMesh` lifecycle engine, process ledger, and `MaterializedViewStore` utilizing DashMap)
  * `tower-lsp-max-agent` (LSP agent hooks)
  * `crates/tower-lsp-max-cli` (command-line wrapper)
  * `crates/playground` (test environments)
  * `examples/clap-noun-verb-lsp` (the target generated project output containing `Cargo.toml`, `src/`, `tests/`)
* **Status**: Fully structured workspace, ready for integration. No duplicate or active source files for `tower-lsp-max` exist inside the `/Users/sac/ggen/` workspace.

### 2.2 ggen-projection
* **Location**: `crates/ggen-projection/` (workspace member of `ggen`).
* **Source Files**: Only `src/lib.rs` (31.8 KB).
* **Current Public API**:
  * `RelationPage`, `Pair2` (relational page data representation)
  * `normalize_resource`, `normalize_predicate`, `normalize_object`, `escape_literal` (formatting functions)
  * `project_ocel2` (projects pages into OCEL v2 JSON format)
  * `project_nquads` (projects pages into RDF N-Quads)
  * `project_prov` (projects pages/receipts into W3C PROV Turtle)
  * `project_dcat` (projects pages/receipts into DCAT catalog Turtle)
  * `project_shacl_refusal` (validates structures and emits SHACL reports)
* **Gap Analysis**: The core structures needed for ggen pack projection (`PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, `ReceiptIndex`) do not yet exist in this crate.

### 2.3 ggen-lsp
* **Location**: `crates/ggen-lsp/` (workspace member of `ggen`).
* **Source Files**: Extensive LSP implementation including `observer.rs`, `diagnostics.rs`, and `opportunity.rs`.
* **Current Public API**:
  * `GgenLanguageServer` (implements `tower_lsp::LanguageServer`)
  * `ServerState` (manages files, rules, and telemetry)
  * `run_stdio` (stdio transport server run loop)
  * Modules for analysis (`analyzers`), file checking (`check`), rule indexing (`rule_index`), and case metrics (`intel`).

---

## 3. E2E Boundary-Crossing Test Architecture

To conform to **AGENTS.md** and **TEST_INFRA.md**, E2E tests must be **opaque-box** and cross real boundaries:

1. **No Mocks**: No mock file systems, faked BLAKE3 hashes, or dummy Ed25519 signature stubs.
2. **Duplex Stdio streams**: To test the LSP servers (`ggen-lsp` and `tower-lsp-max`) without network ports or mock servers, tests will spawn servers and feed JSON-RPC messages into them via `tokio::io::duplex` streams.
3. **Four Surfaces of Corroboration**:
   * **Execution Surface**: Verify that `ggen sync` actually writes files to a temporary directory (`tempfile::TempDir`), and that `cargo check` compiles the output.
   * **Telemetry Surface**: Verify that during generation, OpenTelemetry spans are captured and emitted (using a real in-memory OTel exporter or temporary JSON telemetry files).
   * **Evidence Surface**: Read `receipts.jsonl` from disk and cryptographically verify the BLAKE3 hashes and Ed25519 signatures.
   * **Causality Surface**: Induce changes (e.g. manually edit a generated file to introduce "drift", delete a receipt, or complete a customization slot) and assert that the expected LSP diagnostic code fires (`GGEN-DRIFT-001`, `GGEN-EVIDENCE-001`, `GGEN-CUSTOMIZE-001`).

---

## 4. Recommended E2E Test Directory Structure

The E2E test suite should be located under `crates/ggen-projection/tests/` and structured as follows:

```
crates/ggen-projection/tests/
├── common/
│   mod.rs                   # Shared test harness (TempDir setup, Duplex LSP runner, OTel captures)
├── f1_dependency_resolution.rs # F1 Happy/Boundary cases (PackDescriptor parsing, cycle checks)
├── f2_projection_maps.rs    # F2 Happy/Boundary cases (ProjectionMap, CustomizationMap, Staging Gate)
├── f3_code_generation.rs    # F3 Happy/Boundary cases (Real generation, Cargo compile & execute)
├── f4_lsp_diagnostics.rs    # F4 Happy/Boundary cases (Observer LSP, GGEN-DRIFT, GGEN-EVIDENCE, etc.)
├── f5_composite_lsp.rs      # F5 Happy/Boundary cases (tower-lsp-max Proxy, source_id attribution)
├── f6_process_evidence.rs   # F6 Happy/Boundary cases (Ed25519 signatures, BLAKE3 receipts, wasm4pm)
├── tier3_cross_feature.rs   # Tier 3 Pairwise Interaction tests
└── tier4_real_world.rs      # Tier 4 End-to-End User Scenario tests
```

---

## 5. Catalog of Test Cases (Tiers 1 to 4)

Here is the complete list of tests to implement, matching the requirements of `TEST_INFRA.md`:

### Feature 1: Pack Descriptor & Dependency Resolution (F1)
#### Tier 1 (Happy-Path)
1. `test_f1_t1_parse_valid_toml`: Parses a valid `pack.toml` and checks metadata.
2. `test_f1_t1_resolve_linear_dependency`: Resolves A -> B dependency chain, verifying execution order.
3. `test_f1_t1_resolve_multi_dependency`: Resolves tree chain A -> [B, C], checking representation.
4. `test_f1_t1_validate_metadata_fields`: Asserts that required fields (name, version) exist and are populated.
5. `test_f1_t1_check_version_compatibility`: Resolves dependencies with semantic version matching (e.g. `^1.0.0` vs `1.2.0`).
#### Tier 2 (Boundary/Negative)
6. `test_f1_t2_dependency_cycle`: Checks that cycle (A -> B -> A) returns `DependencyCycleError` instead of stack overflowing.
7. `test_f1_t2_missing_dependency`: Rejects a pack with an unresolvable dependency.
8. `test_f1_t2_invalid_toml_syntax`: Asserts detailed syntax errors are returned for corrupted TOML.
9. `test_f1_t2_duplicate_dependency_name`: Rejects a pack declaring duplicate dependencies.
10. `test_f1_t2_incompatible_version_conflict`: Rejects dependency chains with conflicting semantic version requirements.

### Feature 2: Core Projection Maps & Staging Gate (F2)
#### Tier 1 (Happy-Path)
1. `test_f2_t1_generate_projection_map`: Creates a `ProjectionMap` and checks target ranges map to source templates.
2. `test_f2_t1_create_customization_map`: Verifies customization maps track placeholders correctly.
3. `test_f2_t1_receipt_index_creation`: Checks that `ReceiptIndex` contains valid BLAKE3 hashes of outputs.
4. `test_f2_t1_staging_gate_non_destructive`: Allows writes when target directory is clean.
5. `test_f2_t1_sync_writes_maps_to_disk`: Asserts `projection-map.json` and `customization-map.json` are written to disk.
#### Tier 2 (Boundary/Negative)
6. `test_f2_t2_stale_projection_map_read`: Returns a validation error when reading a projection map that is out of sync with disk.
7. `test_f2_t2_overlapping_ranges`: Rejects registration of overlapping target ranges in `ProjectionMap`.
8. `test_f2_t2_staging_gate_refusal_on_dirty`: Blocks writes when local changes exist without the override flag.
9. `test_f2_t2_non_writable_directory`: Returns clear permission errors when syncing into a read-only path.
10. `test_f2_t2_empty_customization_point_name`: Fails validation if a customization point key is an empty string.

### Feature 3: E2E Pack Proving & Code Generation (F3)
#### Tier 1 (Happy-Path)
1. `test_f3_t1_project_clap_noun_verb`: Generates a CLI project and checks compilation.
2. `test_f3_t1_project_tower_lsp_max`: Generates the composite LSP project.
3. `test_f3_t1_verify_example_compilation`: Executes `cargo check` inside the generated project.
4. `test_f3_t1_verify_example_execution`: Runs the generated CLI tool and asserts it responds to `--help`.
5. `test_f3_t1_generated_manifest_correctness`: Verifies the generated `Cargo.toml` inherits correct dependencies.
#### Tier 2 (Boundary/Negative)
6. `test_f3_t2_empty_templates`: Projects a pack with empty templates and checks metadata is still valid.
7. `test_f3_t2_missing_template_file`: Fails with an I/O error when a template file is missing.
8. `test_f3_t2_compilation_failure_on_missing_imports`: Fails compilation check if generated code lacks cargo imports.
9. `test_f3_t2_interrupted_write_cleanup`: Rolls back partially written files upon simulated process crash.
10. `test_f3_t2_unsupported_target_path`: Blocks attempts to project files outside the target workspace.

### Feature 4: LSP Diagnostics & Opportunity Detection (F4)
#### Tier 1 (Happy-Path)
1. `test_f4_t1_projected_diagnostic`: Verifies `GGEN-PROJECTED-001` is published on projected ranges.
2. `test_f4_t1_drift_diagnostic`: Modifies a generated file and asserts `GGEN-DRIFT-001` fires.
3. `test_f4_t1_evidence_diagnostic`: Deletes `receipts.jsonl` and checks for `GGEN-EVIDENCE-001`.
4. `test_f4_t1_customize_diagnostic`: Verifies `GGEN-CUSTOMIZE-001` fires on incomplete customization slots.
5. `test_f4_t1_opportunity_diagnostic`: Scans manual code matching signatures, firing `GGEN-PROJECT-OPPORTUNITY-001`.
#### Tier 2 (Boundary/Negative)
6. `test_f4_t2_zero_length_drift`: Inserts and deletes a character, asserting that no drift is reported.
7. `test_f4_t2_corrupt_receipt_file`: Fires `GGEN-EVIDENCE-001` when the receipt file contains invalid JSON-RPC or corrupted data.
8. `test_f4_t2_multiple_drift_ranges`: Publishes distinct diagnostics for separate manual changes.
9. `test_f4_t2_opportunity_missing_signatures`: Asserts opportunity scan reports nothing when signatures are empty.
10. `test_f4_t2_override_mismatch`: Fires `GGEN-OVERRIDE-001` when an override comment is present but unsigned.

### Feature 5: Composite LSP Routing & Attribution (F5)
#### Tier 1 (Happy-Path)
1. `test_f5_t1_initialize_multiplexer`: Verifies successful JSON-RPC initialization handshake.
2. `test_f5_t1_route_diagnostics_with_attribution`: Confirms forwarded diagnostics contain `source_id: "ggen_lsp"`.
3. `test_f5_t1_merge_hover_responses`: Merges hover fragments from `rust-analyzer` and `ggen-lsp` at overlapping ranges.
4. `test_f5_t1_route_completion`: Routes autocompletion requests to all active upstreams.
5. `test_f5_t1_routing_source_preservation`: Asserts that diagnostic `source` fields preserve upstream names.
#### Tier 2 (Boundary/Negative)
6. `test_f5_t2_upstream_crash_isolation`: Keeps proxy alive and active if `rust-analyzer` crashes.
7. `test_f5_t2_slow_upstream_timeout`: Drops slow backend responses and returns partial results within 100ms.
8. `test_f5_t2_duplicate_diagnostic_keys`: Retains duplicate diagnostics from different sources via unique `source_id`.
9. `test_f5_t2_malformed_rpc_from_upstream`: Isolates malformed JSON-RPC payloads, keeping the main loop running.
10. `test_f5_t2_composite_exit_propagation`: Gracefully shuts down all upstream processes upon exit signal.

### Feature 6: Process Evidence & wasm4pm Export (F6)
#### Tier 1 (Happy-Path)
1. `test_f6_t1_receipt_blake3_binding`: Computes real BLAKE3 hash over generated bytes and verifies it binds correctly.
2. `test_f6_t1_receipt_causal_chain`: Verifies receipt hashes are chained sequentially.
3. `test_f6_t1_receipt_signature_check`: Verifies the Ed25519 signature of the generator subagent.
4. `test_f6_t1_receipt_process_evidence_format`: Asserts formatting matches `wasm4pm` requirements.
5. `test_f6_t1_export_package`: Verifies the exported receipt bundle structure is compatible.
#### Tier 2 (Boundary/Negative)
6. `test_f6_t2_mismatched_blake3_hash`: Rejects a receipt with a handcoded/fake hash.
7. `test_f6_t2_broken_causal_link`: Rejects validation if a historical receipt is edited.
8. `test_f6_t2_expired_certificate`: Rejects promotion if a receipt is signed with an expired subagent key.
9. `test_f6_t2_missing_receipt_fields`: Throws validation errors if required receipt fields are omitted.
10. `test_f6_t2_unwritable_export_target`: Fails gracefully when exporting to a read-only partition.

### Tier 3: Cross-Feature Combinations (Pairwise Interaction)
1. `test_t3_sync_during_manual_edit_drift`: Verifies staging gate blocks `ggen sync` overwrite if manual edits have drifted, unless override is passed.
2. `test_t3_dependency_removal_breaks_diagnostic_chain`: Removing a pack dependency triggers `GGEN-EVIDENCE-001` downstream.
3. `test_t3_slow_generation_timeout_in_lsp`: LSP multiplexer caches previous diagnostics if code regeneration hangs.
4. `test_t3_customization_point_completed_resolves_diagnostic`: Completing customization points clears `GGEN-CUSTOMIZE-001` and signs a new receipt.
5. `test_t3_signature_change_invalidates_opportunity`: Modifying a pack signature clears older `CouldBeProjected` opportunities.
6. `test_t3_corrupt_receipt_blocks_export`: Editing a receipt byte blocks `wasm4pm` packaging due to audit failure.

### Tier 4: Real-World Application Scenarios
1. **Scenario 1: End-to-End Pack Creation and Sync**:
   * Generate `examples/clap-noun-verb-lsp/` using `ggen sync` and compile with `cargo check`.
2. **Scenario 2: Modification and Drift Diagnostics**:
   * Start `tower-lsp-max`, edit a generated file manually, and verify `GGEN-DRIFT-001` diagnostics publish.
3. **Scenario 3: Customization Point Completion**:
   * Observe `GGEN-CUSTOMIZE-001`, fill in command execution logic, verify diagnostic is cleared, and ensure a signed receipt is appended.
4. **Scenario 4: Upstream Language Server Crash Recovery**:
   * Simulate `rust-analyzer` crashing; verify `tower-lsp-max` remains alive and serves diagnostics from `ggen-lsp`.
5. **Scenario 5: Process Audit and wasm4pm Verification**:
   * Run the auditor tool to verify Blake3 chain and Ed25519 signatures, then bundle and package for `wasm4pm`.

---

## 6. Verification and Compilation Strategy

To prevent compilation failures in Milestone 1 without violating the **AGENTS.md Constitution**, we must not use placeholder mocks or empty stubs. There are two viable strategies:

### Option A: Conditional Compilation Feature Gate
Define E2E tests inside `Cargo.toml` under a dedicated feature flag (e.g. `e2e_tests`):
```toml
[[test]]
name = "e2e"
path = "tests/e2e/mod.rs"
required-features = ["e2e_tests"]
```
This allows the E2E test files to be written and integrated into the repository in Milestone 1 without failing standard compilation. They will only compile and run when `--features e2e_tests` is explicitly enabled.

### Option B: Schema-Complete Declarations (Recommended)
Instead of waiting for Milestone 2 or using feature flags, the implementer can define the **actual structures** directly in Milestone 1:
* Define `PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, and `ReceiptIndex` with their complete, real schemas (e.g., matching the fields in `PROJECT.md`) inside `crates/ggen-projection/src/`.
* Write real parser logic (e.g., `toml::from_str` for `PackDescriptor`, `serde_json::from_str` for `ProjectionMap`) using standard library crates.
* This represents a type-first, complete implementation without stubs or placeholders. E2E tests can compile and run against these real schemas immediately.

Option B is highly recommended because it complies fully with the **AGENTS.md** mandate for exhaustive completeness and ensures that the codebase remains fully compile-verifiable at all stages.

---

## 7. Step-by-Step E2E Test Implementation Plan

1. **Step 1: Declare Schema Models (M1/M2 transition)**:
   * Write `crates/ggen-projection/src/descriptor.rs` (defining `PackDescriptor`).
   * Write `crates/ggen-projection/src/plan.rs` (defining `PackPlan`).
   * Write `crates/ggen-projection/src/mapping.rs` (defining `ProjectionMap` and `CustomizationMap`).
   * Write `crates/ggen-projection/src/receipt.rs` (defining `ReceiptIndex`).
2. **Step 2: Create Test Harness (`common/mod.rs`)**:
   * Create `tests/common/mod.rs` to handle `TempDir` creation, filesystem edits, spawning of `ggen-lsp` and `tower-lsp-max` processes, and multiplexing standard I/O duplex streams.
3. **Step 3: Implement Tier 1 & 2 Tests**:
   * Create files `f1_dependency_resolution.rs` through `f6_process_evidence.rs`.
   * For each file, implement the 10 cataloged test cases.
4. **Step 4: Implement Tier 3 Pairwise Tests**:
   * Create `tier3_cross_feature.rs` and implement the interaction edge cases (e.g., sync gate vs manual edits).
5. **Step 5: Implement Tier 4 Scenario Tests**:
   * Create `tier4_real_world.rs` and implement the 5 full user workflow scenarios.
6. **Step 6: CI Integration**:
   * Update the global workspace run script or Cargo commands to execute tests and verify zero-mock policy compliance.
