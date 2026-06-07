# Verification Analysis: GALL-CHECKPOINT-003

This report details the read-only investigation and mapping analysis of the 12 proofs declared in `crates/ggen-pack-gall-checkpoint-proof/manifest.toml` and verified by `crates/ggen-projection/tests/dogfood_gc003.rs`.

---

## 1. Proof Manifest & Template Analysis

### 1.1 Proof Manifest (`crates/ggen-pack-gall-checkpoint-proof/manifest.toml`)
The manifest defines a single checkpoint `GALL-CHECKPOINT-003` named "Boundary-Receipted Equation Enforcement" with target status `ADMITTED_BY_DOGFOOD`. It defines the boundary and equation form:
- **Boundary Configuration**:
  - `workspace = "."`
  - `target = ".tmp_gc003/target"`
  - `staging_dir = ".tmp_gc003/staging"`
  - `receipt_sink = ".tmp_gc003/receipts"`
- **Equation**: `R_B ⊢ A = μ(O*_B)`
- **Proofs**: Exactly 12 proofs are declared, specifying their unique IDs, kinds (e.g., `receipt-binding`, `receipt-laundering`, `receipt-chain`), field names, mutation/tamper vectors, and expected verification behaviors.

### 1.2 Template Files (`crates/ggen-pack-gall-checkpoint-proof/templates/`)
There are two template files registered under `pack.toml`:
1. `dogfood_gc002.rs.tmpl`: Targets `GALL-CHECKPOINT-002`, asserting basic projection replays, presence of `template_digest`, template mutation detection, missing template refusal, and the absence of forbidden reference copy symbols.
2. `dogfood_gc003.rs.tmpl`: Targets `GALL-CHECKPOINT-003`, dynamically rendering the test code for each proof listed in the manifest via a template iteration over `proofs`. It checks the `proof.mutation` string to generate conditional test blocks using Jinja/Tera syntax.

---

## 2. Dogfood Mapping & Verification Logic

The test file `crates/ggen-projection/tests/dogfood_gc003.rs` is compiled from `dogfood_gc003.rs.tmpl`. It runs the projection pipeline, parses the generated cryptographic receipts, and runs a battery of mutation checks to assert that tampering with any part of the equation or receipt values invalidates the whole projection.

### 2.1 Test Execution Lifecycle in `dogfood_gc003.rs`
1. **Setup**: Creates a temporary directory.
2. **Execution**: Invokes the `sync_target` binary command-line tool. It passes roots for two packs (`ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max`) and points output targets to the temp directory.
3. **Parse Receipts**: Reads `target_dir/receipts.json` and parses it into a `ReceiptIndex` struct.
4. **Order Receipts**: Identifies the head of the chain (`previous_receipt == None`) and sequentially follows the `previous_receipt` references to construct `ordered_paths` (head to tail).
5. **Reference Context**: Extracts the expected `EquationContext` from the first receipt in the chain. It also maps target paths to their original `blake3_hash` (expected artifacts) and `template_digest` (expected templates).
6. **Pre-tamper Validation**: Verifies that `receipt_index.validate_sync` succeeds when no tampering is present.
7. **Mutation Assertions**: Iterates through each of the 12 proof blocks, applying mutations and asserting that `validate_sync` returns the correct `ReceiptValidationError` variant, then restoring the state.

---

## 3. Detail Mapping Table: 12 Proofs

| Proof ID | Kind | Field / Check | Mutation Vector in Test | Expected Error | Validation Logic in `ReceiptIndex::validate_sync` |
|---|---|---|---|---|---|
| **boundary-digest-required** | `receipt-binding` | `boundary_digest` | Set expected boundary digest to `"tampered"` | `BoundaryDigestMismatch` | Asserts `receipt.boundary_digest == expected_eq.boundary_digest` |
| **workspace-digest-required** | `receipt-binding` | `workspace_digest` | Set expected workspace digest to `"tampered"` | `WorkspaceDigestMismatch` | Asserts `receipt.workspace_digest == expected_eq.workspace_digest` |
| **pack-plan-digest-required** | `receipt-binding` | `pack_plan_digest` | Set expected pack plan digest to `"tampered"` | `PackPlanDigestMismatch` | Asserts `receipt.pack_plan_digest == expected_eq.pack_plan_digest` |
| **pack-descriptor-digest-required** | `receipt-binding` | `pack_descriptor_digest` | Set expected pack descriptor digest to `"tampered"` | `PackDescriptorDigestMismatch` | Asserts `receipt.pack_descriptor_digest == expected_eq.pack_descriptor_digest` |
| **template-digest-required** | `receipt-binding` | `template_digest` | Set first file's template digest to `"tampered"` in expectations map | `TemplateDigestMismatch` | Asserts `receipt.template_digest` matches expected template digest for the file |
| **customization-digest-required** | `receipt-binding` | `customization_digest` | Set expected customization digest to `"tampered"` | `CustomizationDigestMismatch` | Asserts `receipt.customization_digest == expected_eq.customization_digest` |
| **staging-digest-required** | `receipt-binding` | `staging_digest` | Set expected staging digest to `"tampered"` | `StagingDigestMismatch` | Asserts `receipt.staging_digest == expected_eq.staging_digest` |
| **mutation-gate-decision-required** | `receipt-binding` | `mutation_gate_decision` | Set decision to `"denied"` in expected context and first receipt | `MutationGateDenied` | Asserts `receipt.mutation_gate_decision` is not empty (if so: `MutationGateMissing`) and is `"admitted"` |
| **verification-result-required** | `receipt-binding` | `verification_result` | Set result to `"failed"` in expected context and first receipt | `VerificationFailed` | Asserts `receipt.verification_result` is not empty (if so: `VerificationMissing`) and is `"passed"` |
| **projection-engine-version-required** | `receipt-binding` | `projection_engine_version` | Set expected engine version to `"tampered"` | `ProjectionEngineMismatch` | Asserts `receipt.projection_engine_version == expected_eq.projection_engine_version` |
| **after-the-fact-hash-laundering-refused** | `receipt-laundering` | `blake3_hash` | Set first file's expected artifact digest to `"tampered"` in expectations map | `AfterTheFactReceiptLaundering` | Asserts `receipt.blake3_hash` matches expected artifact digest for the file |
| **receipt-chain-broken-refused** | `receipt-chain` | `previous_receipt` | Set second receipt's `previous_receipt` link to `Some("tampered".to_string())` | `ReceiptChainBroken` | Sequentially tracks `expected_previous` and asserts `receipt.previous_receipt == expected_previous` |

---

## 4. Compilation & Test Verification Status

The entire verification suite has been executed and confirmed clean:
- **Test Target**: `cargo test -p ggen-projection --test dogfood_gc003`
- **Build Status**: Compiles successfully with only standard unused variable/import warnings.
- **Execution Status**: `test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out`
- **Process Verification**: `sync_target` binary runs correctly, produces valid templates/receipts inside `.tmp_gc003/`, and `validate_sync` successfully triggers expected failures for all 12 tampered scenarios.
