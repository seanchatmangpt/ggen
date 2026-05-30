<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Proof Surfaces and Receipt Replay Refusal Spec](#proof-surfaces-and-receipt-replay-refusal-spec)
  - [Definitions](#definitions)
  - [Proof Surfaces Matrix](#proof-surfaces-matrix)
  - [Required Refusal Cases](#required-refusal-cases)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Proof Surfaces and Receipt Replay Refusal Spec

This document defines the proof machinery for Genesis-bearing interchangeable parts within the distributed architecture.

**[DOC_ONLY]** This specification defines the ideal state of the proof surfaces. Implementation details must be verified against `src/` and `cgen/` directories.

## Definitions

*   **packet receipt**: A cryptographic binding of a single network packet's payload to its source address, sequence, and immediate Genesis evaluation result. **[MISSING]** (Target: `src/ostar/genesis/receipts.rs`)
*   **segment receipt**: A cryptographic rollup of a contiguous sequence of packet receipts, representing a logical unit of data transfer or a partial relation page update. **[MISSING]** (Target: `src/ostar/genesis/receipts.rs`)
*   **shard receipt**: A cryptographic binding representing the exact state and history of a specific data partition or relation page cluster at a point in time. **[MISSING]** (Target: `src/ostar/genesis/receipts.rs`)
*   **corpus receipt**: The ultimate cryptographic rollup of all shard receipts within a Genesis-bearing part, representing the entire immutable truth state. **[MISSING]** (Target: `src/ostar/genesis/receipts.rs`)
*   **part receipt**: A cryptographic attestation that binds a compiled WASM/AtomVM execution body with its `corpus receipt`, Genesis version, and external capability bindings. **[MISSING]** (Target: `src/ostar/genesis/receipts.rs`)
*   **replay cursor**: A deterministically verifiable pointer to a specific segment or packet receipt, allowing exact reconstruction of state from an event stream. **[MISSING]** (Target: `src/ostar/genesis/replay.rs`)
*   **replay manifest**: A declared sequence of segment receipts required to reach a specific `shard receipt` or `corpus receipt` state, used for deterministic replay and validation. **[MISSING]** (Target: `src/ostar/genesis/replay.rs`)
*   **refusal artifact**: A cryptographically signed record detailing exactly why a packet, segment, or replay manifest was rejected (e.g., unauthorized context, invalid multiplicity), including the exact bounds of the failure. **[MISSING]** (Target: `src/ostar/genesis/refusal.rs`)
*   **BLOCK evidence**: Cryptographic proof that an external boundary or capability was blocked due to missing authorization or invalid state, bound to the part receipt. **[MISSING]** (Target: `src/ostar/genesis/refusal.rs`)
*   **sabotage tests**: A suite of specialized tests (`tests/sabotage_*.rs`) designed to maliciously alter receipts, inject impossible state, or replay invalid manifests to ensure the proof machinery correctly emits `refusal artifacts`. **[TEST_ONLY]** (Target: `tests/sabotage_receipts.rs`)
*   **receipt rollup**: The deterministic algorithmic process of combining lower-level receipts (e.g., packet -> segment -> shard -> corpus) using BLAKE3 chaining. **[MISSING]** (Target: `src/ostar/genesis/receipts.rs`)
*   **proof boundaries**: The explicit interfaces (e.g., WASM boundary, MCP tool boundary, OTel emission boundary) where state transitions MUST emit or validate a receipt. **[DOC_ONLY]**
*   **source digest binding**: The cryptographic inclusion of the exact byte content (or its BLAKE3 hash) of the source data in the `packet receipt`. **[MISSING]**
*   **law/context binding**: The cryptographic inclusion of the specific Genesis relation identifier and multiplicity constraint under which data is being evaluated. **[MISSING]**
*   **output binding**: The cryptographic inclusion of the resulting Genesis state transition (or refusal) in the `segment receipt`. **[MISSING]**
*   **index receipt binding**: The cryptographic attestation that an index was deterministically derived from a specific `shard receipt` or `corpus receipt`, proving the index is not an independent authority. **[MISSING]**

## Proof Surfaces Matrix

| Proof surface | Scope | Inputs bound | Outputs bound | Replayable? | Refusal? | Owner | Validator |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **packet receipt** | Single evaluated payload | Source digest, packet sequence | Genesis evaluation result (Pair2 transition or Refusal) | Yes | Yes | Genesis Core | Replay Cursor |
| **segment receipt** | Contiguous packets | Packet receipts, relation context | Segment state transition | Yes | Yes | Genesis Core | Shard Manager |
| **shard receipt** | Relation page cluster | Segment receipts | Shard index state | Yes | Yes | Genesis Core | Corpus Manager |
| **corpus receipt** | Entire Genesis part | Shard receipts | Global corpus state | Yes | No (State is terminal for part) | Part Custodian | Part Consumer |
| **part receipt** | Executable unit | Corpus receipt, WASM digest | Capability bounds, execution trace | No (Static artifact) | Yes (Load/Init failure) | ggen | Truex / AtomVM |
| **index receipt** | Derived access path | Corpus/Shard receipt | Index byte layout | Yes | Yes (Mismatch) | Genesis Indexer | Query Engine (e.g. QLever) |

## Required Refusal Cases

The Genesis proof machinery MUST emit a `refusal artifact` and halt processing under the following conditions. **[MISSING]** All implementations must be covered by `sabotage tests`.

*   **Need9**: Attempting to process data when a 9-byte source address is expected but not provided or malformed.
*   **Need257**: Attempting to process data when a 257-byte payload context is expected but not provided or malformed.
*   **missing source address**: The `packet receipt` cannot bind because the origin address is null or omitted.
*   **unauthorized relation context**: Attempting to insert a Pair2 tuple into a RelationPage that does not match the bound law/context.
*   **invalid multiplicity**: Attempting a state transition that violates the 1:1, 1:N, or N:M constraints of the target relation.
*   **duplicate inflation**: Attempting to process a packet sequence that has already been receipted and incorporated into the `segment receipt`.
*   **packet overfill**: A packet payload exceeds the deterministic byte limit for a single evaluation cycle.
*   **page overflow**: Attempting to insert a Pair2 tuple into a RelationPage that is already at its deterministic byte capacity.
*   **replay mismatch**: The state generated by following a `replay manifest` does not match the target `shard receipt` or `corpus receipt`.
*   **receipt mismatch**: A `receipt rollup` produces a hash that does not match the expected cryptographic claim.
*   **external projection mismatch**: The evidence emitted to an external system (e.g., OCEL, SHACL) contradicts the `corpus receipt`.
*   **unreceipted index**: Attempting to query an index that cannot provide an `index receipt binding` to a valid `corpus receipt`.
*   **boundary byte emission bypass**: Attempting to cross an interop boundary without emitting the requisite bytes (e.g., missing BLAKE3 hash on WASM exit).
*   **mock/random matter counted as real**: A `refusal artifact` MUST be generated if test fixtures or randomized data are detected entering a production `segment receipt` stream.

## Definition of Done

**[PARTIAL]** The following criteria must be met to consider the Proof Surfaces Spec fully implemented:

1.  **Receipt Structures Defined**: `packet receipt`, `segment receipt`, `shard receipt`, `corpus receipt`, and `part receipt` structures are explicitly defined in Rust (e.g., `src/ostar/genesis/receipts.rs`) using `blake3::Hash`. **[MISSING]**
2.  **Replay Machinery Working**: `replay cursor` and `replay manifest` logic is implemented, capable of reconstructing a corpus from segment receipts. **[MISSING]**
3.  **Refusal Artifacts Emitted**: Every condition in the "Required Refusal Cases" section explicitly returns a `refusal artifact` instead of panicking or ignoring the error. **[MISSING]**
4.  **Sabotage Tests Passing**: `tests/sabotage_receipts.rs` exists and successfully triggers every refusal case by intentionally injecting malformed or impossible state. **[MISSING]**
5.  **Rollup Determinism**: `receipt rollup` logic is pure, deterministic, and relies strictly on byte inputs, with zero dependence on JSON serialization. **[MISSING]**
6.  **No JSON Internals**: The `cgen` and `src` implementations for receipt handling do not use JSON for internal state representation or cryptographic binding. **[AMBIGUOUS]** (Requires codebase audit).
7.  **Evidence Binding**: All receipts explicitly bind to physical file evidence, WASM boundaries, or external observable state, preventing placeholder laundering. **[MISSING]**
