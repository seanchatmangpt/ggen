# `crates/ggen-graph/tests/vocab_projection.rs`

Source SHA-256: `368ed1aa4f5fd614d8cc88d4f5bf446651e34b7a173e3843b6f0050425fd1f51`

```mermaid
classDiagram
    class fn_test_vocab_constants_exist_and_match {
      <<fn>>
    }
    class fn_test_vocab_projection_in_graph {
      <<fn>>
    }
    class fn_test_hook_pack_ttl_contains_no_gall_namespace_terms {
      <<fn>>
    }
    class fn_test_audit_doctest_results_ttl_vocab_is_clean {
      <<fn>>
    }
    class fn_test_all_ttl_files_have_no_gall_terms {
      <<fn>>
    }
```

## Dependencies

- `ggen_graph::{vocab, DeterministicGraph}`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::model::{NamedNode, Quad}`
- `oxigraph::sparql::QueryResults`
- `oxigraph::store::Store`
- `std::error::Error`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
