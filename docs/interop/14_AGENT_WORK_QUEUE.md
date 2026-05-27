# Agent Work Queue

## 1. Scope
Independent, executable work packages required to move from the v0.1 Documentation Phase to the v0.2 Implementation Phase.

## 2. Work Packages

| Work Package | Owner Agent | Files Touched | Inputs | Outputs | Tests | DoD | Risk |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **WP-01: Isolate `genesis-core`** | Rust Architect | `crates/genesis-core/*` | KNHK `hot_kernels.rs` | Pure `#[no_std]` crate | Compilation | Compiles to WASM | Med |
| **WP-02: Implement Need257 Split** | Data Algebra | `crates/genesis-core/src/page.rs`| `RelationPage` bounds | Split Page Logic | `test_need257` | Refusals fire | High |
| **WP-03: Implement Need9 Split** | Data Algebra | `crates/genesis-core/src/packet.rs`| `Construct8` bounds | New Packet Trigger | `test_need9` | Refusals fire | High |
| **WP-04: Membrane JSON Adapter**| Membrane Dev | `crates/ggen-membrane/src/json.rs`| JSON Log Fixture | `RelationPage` struct | `test_json_adapt` | Accurate map | Low |
| **WP-05: Receipt Hash Chain** | Proof Dev | `crates/genesis-core/src/receipt.rs`| `Construct8` output | BLAKE3 Packet Hash| `test_hash_chain`| Matches spec | Med |
| **WP-06: OCEL/wasm4pm Bridge** | Process Intel | `crates/ggen-projection/src/ocel.rs`| `Construct8` packets| OCEL 2.0 JSON | `test_wasm4pm` | Conforms | Med |
| **WP-07: SHACL Validation Gate**| Public Vocab | `scripts/gall/shacl_gate.sh` | Projected Turtle | Pass/Fail Exit code| `test_shacl` | Catch bad URIs | Low |
| **WP-08: Replay Cursor Logic** | Proof Dev | `crates/genesis-core/src/replay.rs`| Seed + Offset | Reconstructed act | `test_replay` | 100% match | High |

## 3. Dependency Graph
`WP-01` must precede all others.
`WP-02`, `WP-03`, `WP-05`, `WP-08` depend on `WP-01`.
`WP-04` and `WP-06` depend on `WP-01`.
`WP-07` depends on `WP-06` (or N-Quads equivalent).

## 4. Execution Rules
Each agent taking a work package MUST read the Interop Contracts and the DFLSS Charter before modifying code.
