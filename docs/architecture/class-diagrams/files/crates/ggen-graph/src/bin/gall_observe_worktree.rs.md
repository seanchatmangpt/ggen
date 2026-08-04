# `crates/ggen-graph/src/bin/gall_observe_worktree.rs`

Source SHA-256: `f5053df648419f1dc1ec7ebaffd30c7e2dd11e45380df5daacbc974c8192418d`

```mermaid
classDiagram
    class struct_FileMetadata {
      <<struct>>
      +"path: String"
      +"size_bytes: u64"
      +"sha256: String"
      +"blake3: String"
      +"inclusion_status: String"
      +"inclusion_reason: String"
    }
    class struct_Inventory {
      <<struct>>
      +"timestamp: String"
      +"files: Vec~FileMetadata~"
    }
    class fn_walk_dir {
      <<fn>>
    }
    class fn_main {
      <<fn>>
    }
```

## Dependencies

- `chrono::Utc`
- `serde::{Deserialize, Serialize}`
- `sha2::{Digest, Sha256}`
- `std::fs::File`
- `std::io::Read`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
