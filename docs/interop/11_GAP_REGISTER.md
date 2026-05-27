# Gap Register

## 1. Scope
This register tracks the structural and implementation gaps identified between the current repository state and the Vision 2030 Genesis-bearing interchangeable part architecture.

## 2. Top Implementation Gaps

| Gap ID | Description | Impact | Target Layer | Affected Files | Status |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **GAP-001** | `genesis-core` isolation | High | Genesis | `crates/genesis-wasm-shell` | OPEN |
| **GAP-002** | `Need257` Split Law | Critical | Genesis | `hot_kernels.rs` | OPEN |
| **GAP-003** | `Need9` Refusal Artifacts | High | Genesis | `crates/ggen-graph/src/receipt` | OPEN |
| **GAP-004** | Replay Cursor Implementation | Critical | Proof Surfaces| N/A | OPEN |
| **GAP-005** | AtomVM/Erlang Part Wrapper | High | Part Runtime | N/A | OPEN |
| **GAP-006** | Real-time `wasm4pm` Bridge | Medium | Process Intel | `crates/ggen-projection` | OPEN |
| **GAP-007** | SHACL CI Validation Hook | Medium | Public Vocab | `scripts/gall/` | OPEN |
| **GAP-008** | Truex Promotion Lifecycle | High | Truex | `tools/truth-gate/` | OPEN |

## 3. Top Documentation Gaps

| Gap ID | Description | Impact | Target Layer | Status |
| :--- | :--- | :--- | :--- | :--- |
| **DOC-001** | Data Algebra formal math proofs | Medium | Data Algebra | OPEN |
| **DOC-002** | Replay Manifest binary schema | High | Proof Surfaces | OPEN |
| **DOC-003** | AtomVM host function FFI spec | High | Part Runtime | OPEN |

## 4. Top Test Gaps

| Gap ID | Description | Impact | Target Layer | Status |
| :--- | :--- | :--- | :--- | :--- |
| **TST-001** | `test_need257_split` boundary check | Critical | Genesis | OPEN |
| **TST-002** | Replay determinism verification | Critical | Proof Surfaces | OPEN |
| **TST-003** | DuckDB Relational Equivalence check | High | Validation | OPEN |
| **TST-004** | `wasm4pm` continuous OCEL ingestion | High | Process Intel | OPEN |
