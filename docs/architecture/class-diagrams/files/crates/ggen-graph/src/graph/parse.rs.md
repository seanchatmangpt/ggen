# `crates/ggen-graph/src/graph/parse.rs`

Source SHA-256: `0177f5d488f0e799ed30e50cee3ce3db1633c82f57826a0ab60f783c2cfee3dd`

```mermaid
classDiagram
    class fn_parse_from_reader {
      <<fn>>
    }
    class fn_parse_nquads {
      <<fn>>
    }
    class fn_parse_turtle {
      <<fn>>
    }
    class fn_parse_ntriples {
      <<fn>>
    }
```

## Dependencies

- `crate::GraphError`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::model::Quad`
- `std::io::Read`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
