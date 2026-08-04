# `crates/ggen-lsp/src/features/formatting.rs`

Source SHA-256: `a94df524090d3024811c67ff7fca1fa62a1373c12371c5f167c5c9e33c56c490`

```mermaid
classDiagram
    class fn_format_document {
      <<fn>>
    }
    class fn_format_range {
      <<fn>>
    }
    class fn_format_toml {
      <<fn>>
    }
    class fn_format_turtle {
      <<fn>>
    }
    class fn_parse_turtle_quads {
      <<fn>>
    }
    class fn_format_sparql {
      <<fn>>
    }
    class fn_whole_document_range {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::state::FileType`
- `lsp_max::lsp_types::{Position, Range, TextEdit}`
- `oxigraph::io::{RdfFormat, RdfParser, RdfSerializer}`
- `oxigraph::model::Quad`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
