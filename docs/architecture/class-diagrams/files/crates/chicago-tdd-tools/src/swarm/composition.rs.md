# `crates/chicago-tdd-tools/src/swarm/composition.rs`

Source SHA-256: `8eba68d66a8b9c262785a5f75791f5441848bac2768281a9feb7d3338a25c54e`

```mermaid
classDiagram
    class struct_CompositionStep {
      <<struct>>
      +"id: String"
      +"sector: String"
      +"operation: String"
      +"input: String"
      +"output: String"
      +"order: u32"
    }
    class struct_OperationChain {
      <<struct>>
      +"id: String"
      +"name: String"
      +"steps: Vec~CompositionStep~"
      +"current_step: usize"
      +"is_completed: bool"
      +"is_deterministic: bool"
    }
    class struct_ComposedOperation {
      <<struct>>
      +"id: String"
      +"chain: OperationChain"
      +"result: String"
      +"composition_merkle: String"
      +"trace: HashMap~String"
      +"total_time_ms: u64"
    }
    class mod_tests {
      <<mod>>
    }
    note "ComposedOperation"
    note "CompositionStep"
    note "OperationChain"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
