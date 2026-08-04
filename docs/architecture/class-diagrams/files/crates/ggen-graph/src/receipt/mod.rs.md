# `crates/ggen-graph/src/receipt/mod.rs`

Source SHA-256: `46205f5f3e5bb55f460e910ba8aaf939e9e9477792655b58ae6d6af1c345feb5`

```mermaid
classDiagram
    class struct_GraphReceipt {
      <<struct>>
      +"version: u8"
      +"timestamp: DateTime~Utc~"
      +"pre_state_hash: [u8; 32]"
      +"post_state_hash: [u8; 32]"
      +"delta_hash: [u8; 32]"
      +"signature_or_hash: [u8; 32]"
      +"coherence_fingerprint: Option~String~"
    }
    class type_TransitionReceipt {
      <<type>>
    }
    class struct_HookReceipt {
      <<struct>>
      +"version: u8"
      +"hook_name: String"
      +"sparql_query: String"
      +"passed: bool"
      +"timestamp: DateTime~Utc~"
      +"graph_state_hash: [u8; 32]"
      +"signature_or_hash: [u8; 32]"
    }
    class struct_ReplayVerifier {
      <<struct>>
      +"seen_signatures: HashSet~[u8; 32]~"
      +"last_state_hash: Option~[u8; 32]~"
    }
    class struct_TransactionBundle {
      <<struct>>
      +"receipts: Vec~GraphReceipt~"
      +"hook_receipts: Vec~HookReceipt~"
      +"bundle_hash: [u8; 32]"
      +"timestamp: DateTime~Utc~"
    }
    note "GraphReceipt"
    note "HookReceipt"
    note "ReplayVerifier"
    note "TransactionBundle"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `crate::GraphError`
- `crate::coherence::CoherenceReport`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashSet`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
