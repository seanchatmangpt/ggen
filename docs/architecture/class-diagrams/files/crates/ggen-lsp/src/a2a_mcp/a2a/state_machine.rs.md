# `crates/ggen-lsp/src/a2a_mcp/a2a/state_machine.rs`

Source SHA-256: `9e8d43c606bdb444cfbfa73979fc654dbac89a336084c5bfcb4dfc7790a77e9d`

```mermaid
classDiagram
    class enum_TransitionError {
      <<enum>>
    }
    class struct_StateTransition {
      <<struct>>
      +"to_state: TaskState"
      +"reason: Option~String~"
      +"agent_id: String"
    }
    class struct_TaskStateMachine {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "StateTransition"
    note "TaskStateMachine"
```

## Dependencies

- `chrono::Utc`
- `crate::a2a_mcp::a2a::{Task, TaskState}`
- `serde::{Deserialize, Serialize}`
- `super::*`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
