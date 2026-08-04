# `crates/ggen-lsp/src/a2a_mcp/a2a/transport.rs`

Source SHA-256: `5eca688851dd28118c3eb75eefa36ee7de0567a9ad29aba1462014b927414b6a`

```mermaid
classDiagram
    class enum_TransportError {
      <<enum>>
    }
    class enum_TaskMessage {
      <<enum>>
    }
    class struct_Envelope {
      <<struct>>
      +"id: Uuid"
      +"from: String"
      +"to: String"
      +"message: TaskMessage"
      +"timestamp: chrono::DateTime~chrono::Utc~"
    }
    class struct_Transport {
      <<struct>>
      +"agents: Arc~RwLock~HashMap~String"
      +"tasks: Arc~RwLock~HashMap~Uuid"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for Transport"
    note "Envelope"
    note "Transport"
```

## Dependencies

- `crate::a2a_mcp::a2a::{Task, TaskState}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::sync::Arc`
- `super::*`
- `thiserror::Error`
- `tokio::sync::{mpsc, RwLock}`
- `uuid::Uuid`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
