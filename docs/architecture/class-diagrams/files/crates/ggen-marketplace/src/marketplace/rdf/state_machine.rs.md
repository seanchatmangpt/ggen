# `crates/ggen-marketplace/src/marketplace/rdf/state_machine.rs`

Source SHA-256: `915b7982cce6189929a3c07c237917fd054980b1224d261749a6842e777087a3`

```mermaid
classDiagram
    class struct_StateMachineExecutor {
      <<struct>>
      +"states: HashMap~String"
      +"transitions: HashMap~String"
      +"guards: HashMap~(String"
    }
    class struct_StateDefinition {
      <<struct>>
      +"name: String"
      +"description: String"
      +"is_initial: bool"
      +"is_final: bool"
      +"allowed_operations: Vec~String~"
    }
    class enum_TransitionGuard {
      <<enum>>
    }
    class struct_TransitionContext {
      <<struct>>
      +"has_authors: bool"
      +"has_keywords: bool"
      +"has_categories: bool"
      +"has_checksum: bool"
      +"quality_score: Option~u32~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for StateMachineExecutor"
    note "StateMachineExecutor"
```

## Dependencies

- `crate::marketplace::error::{Error, Result}`
- `serde::{Deserialize, Serialize}`
- `std::collections::{HashMap, HashSet}`
- `super::*`
- `super::turtle_config::TurtleConfigLoader`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
