# `crates/ggen-lsp/src/source_contract.rs`

Source SHA-256: `15bb4977476619183a2a89bb5841a0d7c9f96d89fa54c97c44d8be08e86265da`

```mermaid
classDiagram
    class struct_ModuleDeclaration {
      <<struct>>
      +"name: String"
      +"line: u32"
      +"start_col: u32"
      +"end_col: u32"
    }
    class struct_LexState {
      <<struct>>
      +"block_comment_depth: usize"
      +"raw_string_hashes: Option~usize~"
      +"quote: Option~u8~"
      +"escaped: bool"
    }
    class fn_detect {
      <<fn>>
    }
    class fn_read_overlay_or_disk {
      <<fn>>
    }
    class fn_static_output_path {
      <<fn>>
    }
    class fn_normalize_path {
      <<fn>>
    }
    class fn_module_candidates {
      <<fn>>
    }
    class fn_display_relative {
      <<fn>>
    }
    class fn_module_declarations {
      <<fn>>
    }
    class fn_parse_module_declaration {
      <<fn>>
    }
```

## Dependencies

- `crate::analyzers::diag::{self, Span}`
- `crate::analyzers::source_law_analyzer::GGEN_SRC_004`
- `crate::project_index::{BufferOverlay, ProjectIndex}`
- `crate::rule_index::RuleIndexEntry`
- `lsp_max::lsp_types_max::DiagnosticSeverity`
- `lsp_max_protocol::{LawAxis, MaxDiagnostic}`
- `std::collections::HashSet`
- `std::path::{Component, Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
