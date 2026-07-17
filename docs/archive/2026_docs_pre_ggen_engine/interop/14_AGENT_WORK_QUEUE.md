<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [14 AGENT WORK QUEUE](#14-agent-work-queue)
  - [Work Packages](#work-packages)
  - [Interop Boundary Summary (WP3 example)](#interop-boundary-summary-wp3-example)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 14 AGENT WORK QUEUE

This queue enumerates the executable work packages required to bridge the Genesis-bearing interchangeable part architecture from its documented state to complete physical implementation.

## Work Packages

| Work package | Owner agent | Files touched | Inputs | Outputs | Tests | DoD | Risk |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **WP1: Genesis Core Extraction** | `ostar-architect` | `src/genesis_core/`, `Cargo.toml` | `docs/interop/04_GENESIS_CORE_SPEC.md` | `genesis-core` crate (pure Rust A=μ(O)) | `tests/core_algebra.rs` [MISSING] | `cargo build -p genesis-core` passes with 0 warnings. | High - Core structural shift. |
| **WP2: Pair2 / RelationPage Impl** | `ostar-operator` | `src/genesis_core/memory.rs` | WP1 Output | Pair2 tuple structs, `RelationPage` allocators | `tests/memory_layout.rs` [MISSING] | No heap allocations inside core Pair2 ops. | Medium - Memory unsafety risks. |
| **WP3: ggen Membrane Adapters** | `ggen-sync` | `src/ggen/adapters/` | JSON/external APIs | `Construct8Packet` payloads | `tests/adapter_boundaries.rs` [MISSING] | Adapters successfully map external to Pair2 without internalizing dependencies. | Low - Well-defined boundary. |
| **WP4: AtomVM Part Custody** | `ostar-operator` | `src/runtime/atomvm.rs` | `Construct8Packet` | WASM compliant payload | `tests/atomvm_custody.rs` [TEST_ONLY] | WASM binaries load and execute via `wasmtime` locally. | Medium - WASM/Rust interop edge cases. |
| **WP5: Receipt Emitting & Truex** | `ostar-auditor` | `src/truex/receipts.rs` | AtomVM output | BLAKE3 verifiable receipts | `tests/receipt_chain.rs` [PARTIAL] | Receipts unforgeable; tampering test fails correctly. | High - Cryptographic trust layer. |
| **WP6: wasm4pm OCEL Projection** | `ostar-doctor` | `src/wasm4pm/ocel.rs` | `src/truex/receipts.rs` | OCEL 2.0 JSON projection | `tests/ocel_projection.rs` [MISSING] | Validation passes via external pm4py tools. | Low - Purely a projection layer. |
| **WP7: Public Vocabulary Interop** | `ostar-governor` | `src/validation/shacl.rs` | `open-ontologies` | SHACL validation results | `tests/shacl_compliance.rs` [MISSING] | Open ontology constraints enforced on RelationPages. | Medium - External vocab drift. |
| **WP8: E2E Integration Suite** | `ostar-auditor` | `tests/e2e/manufacturing.rs` | All WP outputs | Test execution logs | `cargo test --test e2e` | All components interoperate from boundary to boundary. | High - Convergence of all systems. |

## Interop Boundary Summary (WP3 example)
*   **Owner:** `ggen-sync`
*   **Input:** External APIs, JSON
*   **Output:** `Construct8Packet` payloads
*   **Proof:** OTel trace logs / OCEL projection
*   **Replay:** Payload replay cursor available in Truex
*   **Refusal:** `BoundaryEvidenceMissing` error
*   **Validation Path:** SHACL validation via `wasm4pm`
