# `crates/ggen-lsp/tests/oracle_tape_generator.rs`

Source SHA-256: `3ae4a780b476ec79299546c5d4d13dd290eeb8da370cf0815d736efd795c519d`

```mermaid
classDiagram
    class fn_url_from_path {
      <<fn>>
    }
    class fn_oracle_tapes_dir {
      <<fn>>
    }
    class fn_is_tpl_001 {
      <<fn>>
    }
    class fn_write_project {
      <<fn>>
    }
    class fn_read_log_lines {
      <<fn>>
    }
    class fn_is_template_tpl_event {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::ServerState`
- `lsp_max::lsp_types::{NumberOrString, Url}`
- `lsp_max_protocol::MaxDiagnostic`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
