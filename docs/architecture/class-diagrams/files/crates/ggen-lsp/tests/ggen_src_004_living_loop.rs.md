# `crates/ggen-lsp/tests/ggen_src_004_living_loop.rs`

Source SHA-256: `931aef26848bd1c1732899053a99609cad9bd464ab94f0b79d4f6cd513c42b6e`

```mermaid
classDiagram
    class fn_write_project {
      <<fn>>
    }
    class fn_src_004_count {
      <<fn>>
    }
    class fn_headless_gate_refuses_unowned_generated_module {
      <<fn>>
    }
    class fn_headless_gate_accepts_owned_generated_module {
      <<fn>>
    }
    class fn_headless_gate_ignores_inline_and_path_overridden_modules {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::check::{check_files_in_root, discover_law_surfaces}`
- `lsp_max::lsp_types::{DiagnosticSeverity, NumberOrString}`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
