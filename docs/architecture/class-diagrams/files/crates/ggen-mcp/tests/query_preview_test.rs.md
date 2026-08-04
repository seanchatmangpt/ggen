# `crates/ggen-mcp/tests/query_preview_test.rs`

Source SHA-256: `527c41b303de44f983702adecbb068086da5463a2a6c1a89da8fc371f14c415f`

```mermaid
classDiagram
    class fn_write_project {
      <<fn>>
    }
    class fn_zero_row_query_reports_loudly_not_silently {
      <<fn>>
    }
    class fn_matching_query_returns_real_rows {
      <<fn>>
    }
    class fn_malformed_sparql_is_syntax_error_not_graph_load_error {
      <<fn>>
    }
    class fn_nonexistent_root_is_path_traversal_error {
      <<fn>>
    }
    class fn_ask_query_returns_boolean_result {
      <<fn>>
    }
```

## Dependencies

- `ggen_mcp::tools::query_preview::{query_preview, QueryPreviewParams}`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
