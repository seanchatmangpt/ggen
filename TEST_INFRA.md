# E2E Test Infra: 80/20 Projection Core and Pack LSPs

This document outlines the testing philosophy, feature inventory, 4-tier test case methodology, test architecture, and bypass-kill validation for the end-to-end (E2E) verification of the **ggen Projection Intelligence** system.

---

## 1. Test Philosophy

Our testing approach ensures absolute correctness, stability, and conformance of the projection pipeline and observer LSP servers:

1. **Opaque-Box Verification**: Tests interact with the projection sync and LSP servers strictly through public interfaces (CLI binaries, serialized output files, standard LSP JSON-RPC messages). We avoid inspecting internal in-memory structs or private database states directly to ensure validation matches real-world usage.
2. **Chicago TDD & Real Boundary Crossing**: In alignment with the AGENTS.md Constitution, we do not use stubs or mocks for primary evidence paths (such as the generator, compiler, file system changes, or OTel tracers). All boundaries (filesystem writes, shell executions, JSON-RPC queries) must be crossed in tests.
3. **Multi-Surface Corroboration**: Claims of success must be proven across multiple independent surfaces:
   - **Execution Surface**: Verification of actual files generated on disk.
   - **Telemetry Surface**: Verification of real OpenTelemetry spans emitted during sync.
   - **Evidence Surface**: Validation of BLAKE3 hashes and cryptographic signatures in the `receipts.jsonl`.
   - **Causality Surface**: Confirming that modifying a template or ontology causes the correct downstream drift or validation diagnostic to fire.
4. **Anti-Cheating Resilience**: Hardcoding test results or using placeholder hashes (like `"hash_placeholder"`) is strictly forbidden. Tests must fail if any step in the generation or validation chain is faked.

---

## 2. Feature Inventory

This inventory maps the core system features to their verbatim requirements from the project specification, detailing the test cases for each tier.

### Feature 1: Pack Descriptor & Dependency Resolution (F1)
- **Requirement**: Implement `PackDescriptor` representing `pack.toml`. Ensure dependency resolution is handled via the host package manager and dependencies (e.g. `tower-lsp-max` depending on `clap-noun-verb`) are validated.
- **Tiers 1 & 2 coverage**: Validates correct parsing of metadata, detection of dependency cycles, missing dependencies, and version incompatibilities.

### Feature 2: Core Projection Maps & Staging Gate (F2)
- **Requirement**: Implement `ProjectionMap`, `CustomizationMap`, and `ReceiptIndex` structures and a staging/sync write gate in the pipeline.
- **Tiers 1 & 2 coverage**: Validates correct generation of maps, tracking of customization points, and block/file write permissions.

### Feature 3: E2E Pack Proving & Code Generation (F3)
- **Requirement**: `tower-lsp-max-pack` must project/generate target files into `/Users/sac/tower-lsp-max/examples/clap-noun-verb-lsp/` and produce correct maps and receipts.
- **Tiers 1 & 2 coverage**: Validates clean generation of functional code, compilation of generated examples, and correctness of output formats.

### Feature 4: LSP Diagnostics & Opportunity Detection (F4)
- **Requirement**: `ggen-lsp` server watches projection boundaries, publishing diagnostics `GGEN-PROJECTED-001`, `GGEN-DRIFT-001`, `GGEN-EVIDENCE-001`, `GGEN-CUSTOMIZE-001`, `GGEN-OVERRIDE-001`, and opportunity scanner `GGEN-PROJECT-OPPORTUNITY-001`.
- **Tiers 1 & 2 coverage**: Validates diagnostic publication on drift, missing receipts, incomplete customizations, and pattern matching for opportunities.

### Feature 5: Composite LSP Routing & Attribution (F5)
- **Requirement**: `tower-lsp-max` composite server routes and composes diagnostics and inlay hints from `ggen-lsp`, Pack LSPs, and `rust-analyzer` while preserving source attribution (`source_id`).
- **Tiers 1 & 2 coverage**: Validates routing logic, header merge operations, and failure isolation if one upstream crashes.

### Feature 6: Process Evidence & wasm4pm Export (F6)
- **Requirement**: Export receipts in a process-evidence shape (hashes, version numbers, signatures) for future ingestion and process mining by `wasm4pm`.
- **Tiers 1 & 2 coverage**: Validates cryptographic validity, serialization format, and integration checks for export.

---

## 3. The 4-Tier Test Case Methodology

### Tier 1: Feature Coverage (Happy-Path)
*We require at least 5 test cases per feature.*

#### Feature 1: Pack Descriptor & Dependency Resolution
1. `test_f1_t1_parse_valid_toml`: Parses a valid `pack.toml` file and asserts that the version, name, and dependency arrays are populated correctly.
2. `test_f1_t1_resolve_linear_dependency`: Resolves a single dependency chain (A -> B) and verifies that the execution plan orders them [B, A].
3. `test_f1_t1_resolve_multi_dependency`: Resolves a tree dependency chain (A -> [B, C]) and verifies all nodes are represented in the resolved path.
4. `test_f1_t1_validate_metadata_fields`: Verifies that required fields (name, version, templates) exist and are non-empty.
5. `test_f1_t1_check_version_compatibility`: Resolves dependencies with compatible semantic version ranges (e.g. `^1.0.0` matches `1.2.0`).

#### Feature 2: Core Projection Maps & Staging Gate
1. `test_f2_t1_generate_projection_map`: Creates a `ProjectionMap` from generated outputs and verifies target ranges map to source templates.
2. `test_f2_t1_create_customization_map`: Populates a `CustomizationMap` with incomplete slots and verifies they list correct placeholder names.
3. `test_f2_t1_receipt_index_creation`: Emits a `ReceiptIndex` containing Blake3 hashes of generated files.
4. `test_f2_t1_staging_gate_non_destructive`: Verifies that if no files exist in the output directory, the staging gate allows direct writes.
5. `test_f2_t1_sync_writes_maps_to_disk`: Verifies that `projection-map.json` and `customization-map.json` are written to disk upon sync.

#### Feature 3: E2E Pack Proving & Code Generation
1. `test_f3_t1_project_clap_noun_verb`: Runs sync for `ggen-pack-clap-noun-verb` and checks that the generated CLI parser compiles.
2. `test_f3_t1_project_tower_lsp_max`: Runs sync for `ggen-pack-tower-lsp-max` and verifies that the composite LSP server compiles.
3. `test_f3_t1_verify_example_compilation`: Executes `cargo check` inside the generated `examples/clap-noun-verb-lsp/` directory to ensure clean builds.
4. `test_f3_t1_verify_example_execution`: Spawns the generated CLI example and checks that it responds to `--help`.
5. `test_f3_t1_generated_manifest_correctness`: Verifies that the generated `Cargo.toml` in the example matches the target version.

#### Feature 4: LSP Diagnostics & Opportunity Detection
1. `test_f4_t1_projected_diagnostic`: Verifies `ggen-lsp` publishes a `GGEN-PROJECTED-001` diagnostic covering the projected files.
2. `test_f4_t1_drift_diagnostic`: Modifies a line in a generated file and asserts that `ggen-lsp` publishes a `GGEN-DRIFT-001` diagnostic.
3. `test_f4_t1_evidence_diagnostic`: Deletes the `receipts.jsonl` file and asserts that `ggen-lsp` publishes a `GGEN-EVIDENCE-001` diagnostic.
4. `test_f4_t1_customize_diagnostic`: Declares an incomplete customization point and checks for `GGEN-CUSTOMIZE-001` diagnostics.
5. `test_f4_t1_opportunity_diagnostic`: Scans a manual file matching the clap noun-verb pattern and verifies a `GGEN-PROJECT-OPPORTUNITY-001` diagnostic fires.

#### Feature 5: Composite LSP Routing & Attribution
1. `test_f5_t1_initialize_multiplexer`: Starts the `tower-lsp-max` server and completes the initialization handshake.
2. `test_f5_t1_route_diagnostics_with_attribution`: Verifies diagnostics from `ggen-lsp` are forwarded with `source_id: "ggen_lsp"`.
3. `test_f5_t1_merge_hover_responses`: Requests hover info at a projected range and verifies fragments from both `rust-analyzer` and `ggen-lsp` are merged.
4. `test_f5_t1_route_completion`: Verifies autocompletion requests are fanned out to both mock upstreams.
5. `test_f5_t1_routing_source_preservation`: Asserts that `source` attribute contains clear name mapping (e.g. `rust-analyzer` or `ggen-pack-lsp`).

#### Feature 6: Process Evidence & wasm4pm Export
1. `test_f6_t1_receipt_blake3_binding`: Verifies that the receipt hash is calculated using a real Blake3 hasher over target file bytes.
2. `test_f6_t1_receipt_causal_chain`: Verifies that parent receipt hashes are chained sequentially in `receipts.jsonl`.
3. `test_f6_t1_receipt_signature_check`: Verifies the signature block in the receipt parses using a standard Ed25519 validator.
4. `test_f6_t1_receipt_process_evidence_format`: Asserts that receipts conform to the required JSONL process-evidence shape.
5. `test_f6_t1_export_package`: Exports the receipt bundle to the target directory and checks structure compatibility with `wasm4pm`.

---

### Tier 2: Boundary & Corner Cases (Negative/Robustness)
*We require at least 5 test cases per feature.*

#### Feature 1: Pack Descriptor & Dependency Resolution
1. `test_f1_t2_dependency_cycle`: Detects a dependency cycle (A -> B -> A) and returns a clean `DependencyCycleError` rather than stack overflowing.
2. `test_f1_t2_missing_dependency`: Attempts to load a pack with a missing dependency and verifies it returns a `DependencyNotFoundError`.
3. `test_f1_t2_invalid_toml_syntax`: Parses a corrupt TOML pack descriptor and asserts that it emits a detailed syntax syntax error instead of panicking.
4. `test_f1_t2_duplicate_dependency_name`: Declares duplicate dependencies in `pack.toml` and verifies it rejects the file during validation.
5. `test_f1_t2_incompatible_version_conflict`: Attempts to resolve a dependency chain with conflicting version demands (e.g. `^1.0.0` vs `^2.0.0`) and verifies it reports a resolution failure.

#### Feature 2: Core Projection Maps & Staging Gate
1. `test_f2_t2_stale_projection_map_read`: Attempts to read a projection map that is out of sync with the files on disk and asserts it returns a validation error.
2. `test_f2_t2_overlapping_ranges`: Attempts to register overlapping projected ranges in `projection-map.json` and verifies they are rejected.
3. `test_f2_t2_staging_gate_refusal_on_dirty`: Attempts to overwrite dirty local edits without an override flag and verifies the write is refused.
4. `test_f2_t2_non_writable_directory`: Tries to sync maps into a read-only directory and asserts that a clear permission error is returned.
5. `test_f2_t2_empty_customization_point_name`: Declares a customization point with an empty string key and asserts that validation fails.

#### Feature 3: E2E Pack Proving & Code Generation
1. `test_f3_t2_empty_templates`: Projects a pack containing empty templates and verifies it produces empty files but valid mapping metadata.
2. `test_f3_t2_missing_template_file`: Points `pack.toml` to a template file that does not exist and asserts that code generation fails with an I/O error.
3. `test_f3_t2_compilation_failure_on_missing_imports`: Forces compilation checks to fail if the generated example lacks required Rust crates.
4. `test_f3_t2_interrupted_write_cleanup`: Simulates process interruption mid-sync and checks that partially written files are rolled back cleanly.
5. `test_f3_t2_unsupported_target_path`: Attempts to project files outside the target workspace and verifies it is blocked by security boundaries.

#### Feature 4: LSP Diagnostics & Opportunity Detection
1. `test_f4_t2_zero_length_drift`: Inserts a character and immediately deletes it (creating a modified timestamp but no content change) and asserts that drift is NOT reported.
2. `test_f4_t2_corrupt_receipt_file`: Replaces the receipt index with random bytes and verifies a `GGEN-EVIDENCE-001` or `GGEN-DRIFT-001` diagnostic fires.
3. `test_f4_t2_multiple_drift_ranges`: Modifies three separate blocks in a file and asserts that three distinct `GGEN-DRIFT-001` diagnostics are published.
4. `test_f4_t2_opportunity_missing_signatures`: Attempts to run opportunity scanner with no signatures defined and verifies it returns no diagnostics.
5. `test_f4_t2_override_mismatch`: Declares an override block in code but leaves the receipts index signature unsigned, and asserts `GGEN-OVERRIDE-001` fires.

#### Feature 5: Composite LSP Routing & Attribution
1. `test_f5_t2_upstream_crash_isolation`: Simulates a crash of `rust-analyzer` and verifies that `tower-lsp-max` continues routing requests to `ggen-lsp`.
2. `test_f5_t2_slow_upstream_timeout`: Simulates an upstream taking 5 seconds to respond and checks that `tower-lsp-max` drops the response and returns partial results within the timeout limit (e.g. 100ms).
3. `test_f5_t2_duplicate_diagnostic_keys`: Receives identical diagnostics from both backends and verifies that both are visible because of different `source_id` tags.
4. `test_f5_t2_malformed_rpc_from_upstream`: Forwards malformed JSON-RPC from a pack LSP and verifies that the composition layer isolates the error and remains running.
5. `test_f5_t2_composite_exit_propagation`: Sends an `exit` command to `tower-lsp-max` and verifies that it shuts down all upstream observer servers cleanly.

#### Feature 6: Process Evidence & wasm4pm Export
1. `test_f6_t2_mismatched_blake3_hash`: Fabricates a receipt with a handcoded hash (London TDD style) and asserts that the verification gate rejects the receipt.
2. `test_f6_t2_broken_causal_link`: Breaks the hash link chain in `receipts.jsonl` (e.g. modifying a historic receipt) and asserts that verification fails.
3. `test_f6_t2_expired_certificate`: Signs a receipt with an expired subagent key and verifies that the verification gate refuses promotion.
4. `test_f6_t2_missing_receipt_fields`: Omits required fields from a receipt line and checks that the parser throws a validation error.
5. `test_f6_t2_unwritable_export_target`: Tries to export the wasm4pm package to a read-only partition and verifies that it fails gracefully.

---

### Tier 3: Cross-Feature Combinations (Pairwise Interaction)
*We require at least 5 cross-feature integration cases.*

1. `test_t3_sync_during_manual_edit_drift`: Checks that if a user is editing a file (triggering a `GGEN-DRIFT-001` diagnostic) and concurrently runs `ggen sync`, the staging gate refuses the overwrite unless the override flag is set.
2. `test_t3_dependency_removal_breaks_diagnostic_chain`: Removes the dependency of `tower-lsp-max` on `clap-noun-verb` in the config, and asserts that subsequent diagnostics report missing evidence (`GGEN-EVIDENCE-001`) because the dependent generator could not run.
3. `test_t3_slow_generation_timeout_in_lsp`: Simulates a slow code generation sync during an active LSP session, verifying that the `tower-lsp-max` server times out the request while keeping the existing diagnostics and snapshots intact.
4. `test_t3_customization_point_completed_resolves_diagnostic`: Completes an incomplete customization point in the code, and asserts that the `GGEN-CUSTOMIZE-001` diagnostic is cleared while a new `GGEN-PROJECTED-001` diagnostic is published with a new cryptographic receipt.
5. `test_t3_signature_change_invalidates_opportunity`: Modifies the signature in `pack.toml`, asserting that files previously matching the signature no longer fire `CouldBeProjected` opportunities.
6. `test_t3_corrupt_receipt_blocks_export`: Modifies a single byte in `receipts.jsonl`, asserting that the `wasm4pm` export function fails and blocks packaging due to receipt verification failure.

---

### Tier 4: Real-World Application Scenarios
*We require at least 5 complete user scenarios.*

1. **Scenario 1: End-to-End Pack Creation and Sync**
   - User creates the `ggen-pack-tower-lsp-max` pack (inheriting from `ggen-pack-clap-noun-verb`).
   - User executes `ggen sync`.
   - **Verification**: `examples/clap-noun-verb-lsp/` is successfully written. Files contain correct mappings, and `cargo check` compiles successfully.
2. **Scenario 2: Modification and Drift Diagnostics**
   - User opens the generated project in VS Code (which starts the `tower-lsp-max` server).
   - User manually edits a line of generated parser code without declaring an override.
   - **Verification**: `ggen-lsp` publishes a `GGEN-DRIFT-001` diagnostic on the exact line range.
3. **Scenario 3: Customization Point Completion**
   - User observes a `GGEN-CUSTOMIZE-001` diagnostic indicating a required command execution logic is missing.
   - User implements the required logic within the designated slot in the generated CLI project.
   - **Verification**: The diagnostic is cleared, the compiler succeeds, and a new valid receipt is appended to `receipts.jsonl`.
4. **Scenario 4: Upstream Language Server Crash Recovery**
   - User is working on the generated project. The downstream `rust-analyzer` process crashes.
   - **Verification**: `tower-lsp-max` remains alive, isolates the crash, flags `rust-analyzer` as degraded, and continues serving diagnostics and hover hints from `ggen-lsp`.
5. **Scenario 5: Process Audit and wasm4pm Verification**
   - The developer runs the validation script to package the generated project.
   - **Verification**: The validation tool parses the `receipts.jsonl` causal chain, verifies all Blake3 hashes, validates Ed25519 signatures, and packages the artifacts for deployment to `wasm4pm`.

---

## 4. Test Architecture

The E2E test execution flow isolates components and tests them under real boundaries.

```
                  ┌───────────────────────────────────────────────┐
                  │              E2E Test Runner                  │
                  │ (Spawns Cargo Test & Duplex Stream Channels)  │
                  └───────────────┬───────────────────────────────┘
                                  │
                  ┌───────────────▼───────────────────────────────┐
                  │           tower-lsp-max                       │
                  │       (Composite JSON-RPC Proxy)              │
                  └───────────────┬───────────────────────────────┘
                                  │
        ┌─────────────────────────┼─────────────────────────┐
        │                         │                         │
┌───────▼───────┐         ┌───────▼───────┐         ┌───────▼───────┐
│   ggen-lsp    │         │  pack-lsp-max │         │ rust-analyzer │
│ (Observer)    │         │ (Declarative) │         │  (Downstream) │
└───────┬───────┘         └───────────────┘         └───────────────┘
        │
┌───────▼───────┐
│ Target Project│ (examples/clap-noun-verb-lsp/)
│  Maps & Files │
└───────────────┘
```

- **Test Runner**: Directs `cargo test` execution. It establishes standard loop streams (`tokio::io::duplex`) to send standard JSON-RPC payloads into the server without file system hacks.
- **Mock Upstream Framework**: Built inside `tests/mock_server.rs` to allow testing failure isolation (Feature 5). It acts as a standard LSP node that can be configured to fail, timeout, or return corrupted outputs.

---

## 5. Coverage Thresholds

To pass the milestone gates, the test suite must satisfy these coverage limits:
- **Feature Coverage (Tier 1 & 2)**: 100% of features F1-F6 must have $\ge 5$ happy-path and $\ge 5$ boundary cases implemented.
- **Dynamic Path Coverage**: Line coverage for `ggen-projection` core and `ggen-lsp` observer logic must be $\ge 80\%$ as recorded in `coverage.json`.
- **Zero Mocks Policy**: Any test using a mocked file system, faked hashes, or stubs for the signature verifier is invalidated and marked as a failure.

---

## 6. Bypass-Kill Validation

To ensure the test suite is falsifiable, these "kill tests" are run during verification:
1. **Dependency Bypass**: Removing the `clap-noun-verb` dependency in `tower-lsp-max` config must trigger a resolution failure during `PackPlan` construction.
2. **Evidence Bypass**: Deleting `receipts.jsonl` or editing `projection-map.json` must trigger a `GGEN-EVIDENCE-001` diagnostic immediately on server startup.
3. **Drift Bypass**: Changing a single byte inside a generated source file must trigger `GGEN-DRIFT-001`. Faking a match must fail.
4. **Attribution Bypass**: Striping the `source_id` field from any composite diagnostics payload routed through `tower-lsp-max` must cause the composition engine to reject the packet and fail the test.
