# `crates/ggen-lsp/tests/field_status_test.rs`

Source SHA-256: `371285db6c403ba8728a38159a6ff0b846ef9e7208c06175145b8d44c4ce0faa`

```mermaid
classDiagram
    class fn_empty_project_reports_no_evidence {
      <<fn>>
    }
    class fn_field_status_breaks_down_by_transport_from_real_evidence {
      <<fn>>
    }
    class fn_verdict_matches_compute_metrics_one_source_of_truth {
      <<fn>>
    }
    class fn_distinct_variants_reflect_distinct_chains {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::intel::MetricValue`
- `ggen_lsp::{ capture_request, check_files_in_root, compute_metrics, field_status, Attribution, FieldReadiness, }`
- `std::fs`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
