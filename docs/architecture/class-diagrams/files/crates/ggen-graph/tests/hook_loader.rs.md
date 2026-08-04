# `crates/ggen-graph/tests/hook_loader.rs`

Source SHA-256: `25152b91d23147ddbbd08bfcf147f671b813807928113528491b5e00b9891d7a`

```mermaid
classDiagram
    class fn_hook_pack_path {
      <<fn>>
    }
    class fn_test_hook_serialization_and_deserialization {
      <<fn>>
    }
    class fn_test_load_hooks_from_json_array {
      <<fn>>
    }
    class fn_test_load_ttl_hook_pack_has_14_hooks {
      <<fn>>
    }
    class fn_test_all_ttl_hooks_have_nonempty_sparql_query {
      <<fn>>
    }
```

## Dependencies

- `ggen_graph::{DeterministicGraph, KnowledgeHook}`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::sparql::QueryResults`
- `oxigraph::store::Store`
- `std::error::Error`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
