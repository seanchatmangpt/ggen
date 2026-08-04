# `crates/openapi-cnv-reflect/src/reflect.rs`

Source SHA-256: `fbba244a2ce27789681934a432ddb4c12c922182509830565f36a4b2945c187b`

```mermaid
classDiagram
    class fn_iri {
      <<fn>>
    }
    class fn_xsd {
      <<fn>>
    }
    class struct_ReflectWarning {
      <<struct>>
      +"path: String"
      +"method: String"
      +"reason: String"
    }
    class struct_ReflectOutcome {
      <<struct>>
      +"store: Store"
      +"warnings: Vec~ReflectWarning~"
    }
    class struct_Graph {
      <<struct>>
      +"store: Store"
    }
    class struct_ScalarParam {
      <<struct>>
      +"field: String"
      +"value_kind: &'static str"
      +"required: bool"
      +"is_path: bool"
      +"about: String"
    }
    class fn_map_schema_type {
      <<fn>>
    }
    class fn_synthetic_test_value {
      <<fn>>
    }
    class fn_collect_scalar_params {
      <<fn>>
    }
    class fn_reflect {
      <<fn>>
    }
    note "Graph"
```

## Dependencies

- `crate::error::ReflectError`
- `crate::naming::{kebab_case, snake_case}`
- `oxigraph::model::{GraphNameRef, Literal, NamedNode, QuadRef}`
- `oxigraph::store::Store`
- `serde_json::Value`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
