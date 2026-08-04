# `tests/domain/graph/query_tests.rs`

Source SHA-256: `730eea454a5aaadf2b0aebb9407abab16f7aafa3b3f3bc09d2a8b3351caf4afa`

```mermaid
classDiagram
    class fn_test_query_real_rdf_graph {
      <<fn>>
    }
    class fn_test_query_with_filter {
      <<fn>>
    }
    class fn_test_query_ask_boolean {
      <<fn>>
    }
    class fn_test_query_empty_graph {
      <<fn>>
    }
    class fn_test_query_with_optional {
      <<fn>>
    }
```

## Dependencies

- `anyhow::Result`
- `ggen_cli::domain::graph::{execute_sparql, QueryOptions}`
- `ggen_core::Graph`
- `std::io::Write`
- `tempfile::NamedTempFile`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
