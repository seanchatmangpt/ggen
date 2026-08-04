# `crates/ggen-graph/src/shacl.rs`

Source SHA-256: `221c6d3d6a2b53eb8480a3e68963de5d08ba03f572bf635219157f5a2999f030`

```mermaid
classDiagram
    class enum_ShaclSeverity {
      <<enum>>
    }
    class struct_ShaclViolation {
      <<struct>>
      +"focus: String"
      +"shape: String"
      +"path: String"
      +"message: String"
      +"severity: ShaclSeverity"
    }
    class fn_load {
      <<fn>>
    }
    class fn_solutions {
      <<fn>>
    }
    class fn_validate_shacl {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::GraphError`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::model::Term`
- `oxigraph::sparql::{QueryResults, SparqlEvaluator}`
- `oxigraph::store::Store`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
