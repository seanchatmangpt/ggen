# `crates/ggen-graph/src/graph/locate.rs`

Source SHA-256: `405e0783587ac230a244e3904fe2bd1794c9ef9a596f55e3dafa6059ac1792e6`

```mermaid
classDiagram
    class struct_ParseDiagnostic {
      <<struct>>
      +"line: u64"
      +"column: u64"
      +"end_line: u64"
      +"end_column: u64"
      +"offset: u64"
      +"message: String"
    }
    class struct_LocatedParse {
      <<struct>>
      +"quads: Vec~Quad~"
      +"diagnostics: Vec~ParseDiagnostic~"
    }
    class fn_parse_located {
      <<fn>>
    }
    class fn_parse_turtle_located {
      <<fn>>
    }
    class fn_parse_ntriples_located {
      <<fn>>
    }
    class fn_parse_nquads_located {
      <<fn>>
    }
    class fn_extract_prefixes {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::model::Quad`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
