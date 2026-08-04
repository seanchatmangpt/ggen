# `crates/ggen-lsp/src/a2a_mcp/a2a/mod.rs`

Source SHA-256: `389abd6f03e7a087052a739728013b20e153deb01104e8efe5b247bf0b45345d`

```mermaid
classDiagram
    class mod_artifact {
      <<mod>>
    }
    class mod_ggen_construct {
      <<mod>>
    }
    class mod_receipt {
      <<mod>>
    }
    class mod_state_machine {
      <<mod>>
    }
    class mod_transport {
      <<mod>>
    }
    class enum_TaskState {
      <<enum>>
    }
    class struct_Task {
      <<struct>>
      +"id: Uuid"
      +"state: TaskState"
      +"title: String"
      +"description: Option~String~"
      +"assigned_to: Option~String~"
      +"created_by: String"
      +"parent_id: Option~Uuid~"
      +"dependencies: Vec~Uuid~"
      +"artifacts: HashMap~String"
      +"metadata: HashMap~String"
      +"created_at: DateTime~Utc~"
      +"updated_at: DateTime~Utc~"
      +"completed_at: Option~DateTime~Utc~~"
      +"failure_reason: Option~String~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Task"
    note "TaskState"
```

## Dependencies

- `artifact::{Artifact, ArtifactCollection, ArtifactContent, ArtifactError, ArtifactType}`
- `chrono::{DateTime, Utc}`
- `receipt::{A2ARefusalState, A2AState, A2ATaskReceipt, Avatar8, Jtbd8}`
- `serde::{Deserialize, Serialize}`
- `state_machine::{StateTransition, TaskStateMachine, TransitionError}`
- `std::collections::HashMap`
- `super::*`
- `transport::{Envelope, TaskMessage, Transport, TransportError}`
- `uuid::Uuid`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
