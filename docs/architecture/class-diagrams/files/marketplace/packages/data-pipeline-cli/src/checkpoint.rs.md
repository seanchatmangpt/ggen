# `marketplace/packages/data-pipeline-cli/src/checkpoint.rs`

Source SHA-256: `e886ec3e6da65db17b5c165787f893fec25dc18b7a385d58361888df19f66c4f`

```mermaid
classDiagram
    class struct_Checkpoint {
      <<struct>>
      +"batch_id: String"
      +"position: usize"
      +"timestamp: chrono::DateTime~chrono::Utc~"
    }
    class struct_CheckpointManager {
      <<struct>>
      +"checkpoints: Vec~Checkpoint~"
    }
    note "CheckpointManager"
    note "Default for CheckpointManager"
```

## Dependencies

- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
