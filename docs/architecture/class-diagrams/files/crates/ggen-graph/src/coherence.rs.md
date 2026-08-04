# `crates/ggen-graph/src/coherence.rs`

Source SHA-256: `96c32576e1ba798792566c53c2581448405702b6adf36bcb2eb969e497a9febb`

```mermaid
classDiagram
    class enum_Pole {
      <<enum>>
    }
    class struct_PoleState {
      <<struct>>
      +"pole: Pole"
      +"hash: String"
      +"item_count: usize"
      +"timestamp: DateTime~Utc~"
    }
    class enum_DriftKind {
      <<enum>>
    }
    class struct_CoherenceDrift {
      <<struct>>
      +"kind: DriftKind"
      +"source_pole: Pole"
      +"target_pole: Pole"
      +"detail: String"
    }
    class struct_CoherenceReport {
      <<struct>>
      +"operation_id: String"
      +"poles: Vec~PoleState~"
      +"drifts: Vec~CoherenceDrift~"
      +"admitted: bool"
    }
    class struct_CoherenceChecker {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "CoherenceChecker"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`
- `uuid::Uuid`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
