# `crates/ggen-lsp/src/check_ext.rs`

Source SHA-256: `30e0b0e64e3c3bcff0ea604ab61dc9bba68516397a8fed38b947b9ca99d03dce`

```mermaid
classDiagram
    class mod_core {
      <<mod>>
    }
    class fn_check_files {
      <<fn>>
    }
    class fn_check_files_with_routes {
      <<fn>>
    }
    class fn_check_files_in_root {
      <<fn>>
    }
    class fn_capture_request {
      <<fn>>
    }
    class fn_source_contract_groups {
      <<fn>>
    }
    class fn_fold_source_contract {
      <<fn>>
    }
    class fn_paths_match {
      <<fn>>
    }
    class fn_summarize_routes {
      <<fn>>
    }
```

## Dependencies

- `core::{ check_content, discover_law_surfaces, CheckReport, FileReport, NamedCount, RouteSummary, }`
- `lsp_max::lsp_types::DiagnosticSeverity`
- `std::collections::BTreeMap`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
