# `crates/ggen-graph/src/graph/quad.rs`

Source SHA-256: `c6939e13fd4eae9d62200d09967f968513ff1c109456daa7eba34284302443bd`

```mermaid
classDiagram
    class fn_canonical_quad_string {
      <<fn>>
    }
    class fn_parse_nquad {
      <<fn>>
    }
    class struct_QuadBuilder {
      <<struct>>
      +"subject: Option~NamedOrBlankNode~"
      +"predicate: Option~NamedNode~"
      +"object: Option~Term~"
      +"graph_name: Option~GraphName~"
    }
    note "Default for QuadBuilder"
    note "QuadBuilder"
```

## Dependencies

- `crate::GraphError`
- `oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Quad, Term}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
