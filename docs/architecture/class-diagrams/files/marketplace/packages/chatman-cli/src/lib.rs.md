# `marketplace/packages/chatman-cli/src/lib.rs`

Source SHA-256: `bcdf7e423b7d1bea93c1421680c80c808f8a60218c4358c535155f050d11ff81`

```mermaid
classDiagram
    class struct_ChatManager {
      <<struct>>
      +"config: Config"
      +"history: Vec~Message~"
    }
    class struct_Config {
      <<struct>>
      +"provider: String"
      +"model: String"
      +"max_history: usize"
      +"timeout_secs: u64"
      +"retry_attempts: u32"
      +"ontology_path: Option~PathBuf~"
    }
    class struct_Message {
      <<struct>>
      +"role: String"
      +"content: String"
      +"timestamp: Option~i64~"
    }
    class struct_KnowledgeHook {
      <<struct>>
      +"ontology_path: PathBuf"
    }
    class mod_tests {
      <<mod>>
    }
    note "ChatManager"
    note "Default for Config"
    note "KnowledgeHook"
    note "Message"
```

## Dependencies

- `anyhow::{Context, Result}`
- `serde::{Deserialize, Serialize}`
- `std::path::PathBuf`
- `super::*`
- `tracing::{debug, info}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
