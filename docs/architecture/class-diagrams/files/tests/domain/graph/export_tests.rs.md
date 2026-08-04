# `tests/domain/graph/export_tests.rs`

Source SHA-256: `7e578c4cc0903209e3367b4b51b41f286bd1c0f625a575f383a1e7cfa3deb3bd`

```mermaid
classDiagram
    class fn_test_export_turtle_to_real_file {
      <<fn>>
    }
    class fn_test_export_all_formats_create_real_files {
      <<fn>>
    }
    class fn_test_export_pretty_vs_compact {
      <<fn>>
    }
    class fn_test_export_format_parsing {
      <<fn>>
    }
```

## Dependencies

- `anyhow::Result`
- `ggen_cli::domain::graph::{export_graph, ExportFormat, ExportOptions}`
- `std::fs`
- `tempfile::tempdir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
