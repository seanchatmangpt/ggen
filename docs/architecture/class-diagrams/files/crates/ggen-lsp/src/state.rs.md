# `crates/ggen-lsp/src/state.rs`

Source SHA-256: `f259329b9924794691e2028d76c10504bccd55ab481e90268eefbe3f7cca7535`

```mermaid
classDiagram
    class fn_uri_to_file_path {
      <<fn>>
    }
    class enum_FileType {
      <<enum>>
    }
    class struct_ServerConfig {
      <<struct>>
      +"auto_format_on_save: bool"
      +"show_hints: bool"
      +"workspace_symbol_depth: usize"
    }
    class struct_ServerState {
      <<struct>>
      +"documents: Arc~Mutex~HashMap~Url"
      +"analyzers: Arc~Mutex~HashMap~Url"
      +"config: ServerConfig"
      +"routes: Arc~crate::route::RouteRegistry~"
      +"root: PathBuf"
      +"run_id: String"
      +"seq: Arc~AtomicU64~"
      +"pending_repairs: Arc~Mutex~HashMap~(Url"
      +"published_diags: Arc~Mutex~HashMap~Url"
      +"flagged: Arc~Mutex~HashMap~&'static str"
    }
    class fn_is_ggen_manifest {
      <<fn>>
    }
    class fn_is_harness_surface {
      <<fn>>
    }
    class fn_url_from_path {
      <<fn>>
    }
    class fn_url_from_path_str {
      <<fn>>
    }
    class fn_percent_decode_path {
      <<fn>>
    }
    note "Default for ServerConfig"
    note "Default for ServerState"
    note "FileType"
    note "ServerState"
```

## Dependencies

- `crate::analyzers::DocumentAnalyzer`
- `crate::intel::events::{ diagnostic_raised, gate_result, receipt_emitted, repair_applied, repair_suggested, route_selected, }`
- `crate::project_index::BufferOverlay`
- `lsp_max::lsp_types_max::*`
- `lsp_max_protocol::MaxDiagnostic`
- `std::collections::{HashMap, HashSet}`
- `std::path::{Path, PathBuf}`
- `std::sync::Arc`
- `std::sync::atomic::{AtomicU64, Ordering}`
- `tokio::sync::Mutex`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
