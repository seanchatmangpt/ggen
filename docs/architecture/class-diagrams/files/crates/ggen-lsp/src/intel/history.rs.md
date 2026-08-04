# `crates/ggen-lsp/src/intel/history.rs`

Source SHA-256: `9a70c126777fae5c060dfa460a9df576eb4af10708795128e704230d2348b7de`

```mermaid
classDiagram
    class enum_RouteStatus {
      <<enum>>
    }
    class struct_RoutePromotionRecord {
      <<struct>>
      +"route_id: String"
      +"route_source: String"
      +"diagnostic_code: String"
      +"family: String"
      +"support: u32"
      +"success_rate: f32"
      +"source_log_hash: String"
      +"routes_hash: String"
      +"promotion_receipt: Option~String~"
      +"promoted_at: String"
      +"supersedes: Option~String~"
      +"status: RouteStatus"
    }
    class struct_PromotionHistory {
      <<struct>>
      +"path: PathBuf"
    }
    class fn_default_history_path {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "PromotionHistory"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::fs::{create_dir_all, OpenOptions}`
- `std::io::{self, Write}`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
