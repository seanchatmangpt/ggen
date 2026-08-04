# `crates/ggen-lsp/src/analyzers/diag.rs`

Source SHA-256: `677b2121f1fa5b2148671266108b1d853bf7d7d56d7c0a327fb1c498683eb156`

```mermaid
classDiagram
    class fn_at {
      <<fn>>
    }
    class struct_Span {
      <<struct>>
      +"start_line: u32"
      +"start_col: u32"
      +"end_line: u32"
      +"end_col: u32"
    }
    class fn_max {
      <<fn>>
    }
    class fn_from_oxrdfio {
      <<fn>>
    }
    class struct_OxrdfioSpan {
      <<struct>>
      +"line: u64"
      +"column: u64"
      +"end_line: u64"
      +"end_column: u64"
    }
    class fn_max_from_oxrdfio {
      <<fn>>
    }
    class fn_whole_line {
      <<fn>>
    }
    class fn_max_whole_line {
      <<fn>>
    }
```

## Dependencies

- `lsp_max::lsp_types_max::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range}`
- `lsp_max_protocol::{LawAxis, MaxDiagnostic}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
