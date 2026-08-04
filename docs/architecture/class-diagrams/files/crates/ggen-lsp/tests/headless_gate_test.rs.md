# `crates/ggen-lsp/tests/headless_gate_test.rs`

Source SHA-256: `6f39c84671dd2360ca3ef71e19080bc023b21257a590fd4640d84ad3b3f23e30`

```mermaid
classDiagram
    class fn_values_in_external_rq_is_refused {
      <<fn>>
    }
    class fn_clean_turtle_passes_the_gate {
      <<fn>>
    }
    class fn_malformed_turtle_is_refused_with_location {
      <<fn>>
    }
    class fn_invalid_config_enum_is_refused {
      <<fn>>
    }
    class fn_ggen_does_not_flag_llm_sections {
      <<fn>>
    }
    class fn_multiple_files_aggregate_and_one_bad_fails_the_batch {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::check_files`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
