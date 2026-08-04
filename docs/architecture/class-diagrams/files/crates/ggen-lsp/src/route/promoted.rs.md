# `crates/ggen-lsp/src/route/promoted.rs`

Source SHA-256: `bb438a1504b1a6f5e2b76a04e57599ff0b0648d88c9712441fd5c176ded217c4`

```mermaid
classDiagram
    class mod_promotion {
      <<mod>>
    }
    class struct_PromotedRoutes {
      <<struct>>
      +"version: u8"
      +"source_log_hash: String"
      +"routes: Vec~RepairRoute~"
    }
    class fn_default_pack_routes_path {
      <<fn>>
    }
    class fn_is_promotable {
      <<fn>>
    }
    class fn_load_promoted {
      <<fn>>
    }
    class fn_write_promoted {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "PromotedRoutes"
```

## Dependencies

- `crate::route::model::{PartialOrder, RepairFamily, RepairRoute, RouteId}`
- `serde::{Deserialize, Serialize}`
- `std::path::{Path, PathBuf}`
- `super::*`
- `super::model::{Provenance, RepairRoute}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
