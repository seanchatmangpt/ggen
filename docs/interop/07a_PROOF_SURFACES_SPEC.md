# Proof Surfaces Specification (Receipt, Replay, Refusal)

## 1. Scope
This specification defines the cryptographic and deterministic proof machinery for Genesis-bearing parts.

## 2. Receipt Hierarchy
- **Packet Receipt:** Proof of a single `Construct8` evaluation (≤8 tuples).
- **Segment Receipt:** A chained hash of contiguous Packet Receipts within a single `RelationPage`.
- **Shard Receipt:** A rollup of Segment Receipts representing a bounded domain of enterprise reality.
- **Corpus Receipt:** The global Merkle root of all Shards.
- **Part Receipt:** The manifest signature proving the exact WASM/Rust binary that executed the construction.

## 3. Replay Machinery
- **Replay Cursor:** A deterministic pointer (offset, hash, seed) that allows perfect reconstruction of a `Construct8` packet.
- **Replay Manifest:** The required inputs (`RelationPage`, Symbol Page, `O*` version) needed to satisfy the cursor.

## 4. Refusal Artifacts
Invalid construction must generate durable evidence.
- **BLOCK Evidence:** A cryptographically signed statement that construction was denied.
- **Sabotage Tests:** Test suites explicitly designed to trigger and verify refusals.

## 5. Refusal Cases
- **Need9:** A construction act attempted to output >8 tuples without splitting.
- **Need257:** A `RelationPage` exceeded 256 local symbols.
- **Missing Source Address:** Admitted tuple lacks cryptographic source binding.
- **Unauthorized Relation Context:** `O*` prevents the requested construction.
- **Invalid Multiplicity:** Attempting to treat a stream as a set incorrectly.
- **Duplicate Inflation:** Unauthorized replay attacks.
- **Packet Overfill:** Hard limit byte violation.
- **Page Overflow:** Hard limit symbol violation.
- **Replay Mismatch:** `A != μ(O)` during audit.
- **Receipt Mismatch:** Invalid hash chain.
- **External Projection Mismatch:** ggen output fails DuckDB/QLever validation.
- **Unreceipted Index:** Index materialized without base receipts.
- **Boundary Byte Emission Bypass:** FFI/WASM boundary breach.
- **Mock/Random Matter Counted as Real:** Fake source addressing.

## 6. Proof Boundaries Table

| Proof surface | Scope | Inputs bound | Outputs bound | Replayable? | Refusal? | Owner | Validator |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Packet Hash** | ≤8 tuples | `RelationPage` | `Construct8` | Yes | Need9 | Genesis | Truex |
| **Page Hash** | ≤256 symbols | Symbol Maps | `RelationPage`| Yes | Need257 | ggen | Genesis |
| **Segment Hash**| Stream | Packets | Segment | Yes | Mismatch | Genesis | Truex |
| **Shard Hash** | Domain | Segments | Rollup | Yes | Mismatch | Truex | Enterprise |
| **Projection** | External | `Construct8` | RDF/OCEL | No | Schema Error | ggen | QLever / wasm4pm |

## 7. Definition of Done
Proof surfaces are DONE when a Genesis part can process a million records, output a single Shard Hash, completely reconstruct the exact byte sequence from the Replay Cursor, and deterministically generate Refusal artifacts for all listed sabotage cases.

| Status | Component | File/Artifact Evidence |
| :--- | :--- | :--- |
| **IMPLEMENTED** | Packet Hash Logic | `crates/ggen-graph/src/receipt/mod.rs` |
| **MISSING** | Replay Cursor Implementation | Needs Implementation |
| **PARTIAL** | Sabotage Tests | `crates/ggen-graph/tests/dialect_sabotage.rs` |
