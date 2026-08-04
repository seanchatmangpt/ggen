# `crates/ggen-lsp/src/a2a_mcp/a2a/artifact.rs`

Source SHA-256: `292179029b1d2c867d68a1999181a67c47840a19e8d32ff9925f485fd84470f0`

```mermaid
classDiagram
    class enum_ArtifactError {
      <<enum>>
    }
    class enum_ArtifactType {
      <<enum>>
    }
    class enum_ArtifactContent {
      <<enum>>
    }
    class struct_Artifact {
      <<struct>>
      +"name: String"
      +"artifact_type: ArtifactType"
      +"content: ArtifactContent"
      +"mime_type: Option~String~"
      +"metadata: HashMap~String"
      +"created_at: DateTime~Utc~"
      +"hash: Option~String~"
    }
    class struct_ArtifactCollection {
      <<struct>>
      +"artifacts: HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    note "Artifact"
    note "ArtifactCollection"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::path::PathBuf`
- `super::*`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
