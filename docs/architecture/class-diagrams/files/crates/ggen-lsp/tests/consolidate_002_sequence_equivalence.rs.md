# `crates/ggen-lsp/tests/consolidate_002_sequence_equivalence.rs`

Source SHA-256: `494faa46d3c9ef8da9ecb1900fed074757c283d30e013b42bf2a6423db40b984`

```mermaid
classDiagram
    class fn_url_from_path {
      <<fn>>
    }
    class fn_fixture_root {
      <<fn>>
    }
    class fn_golden_path {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_read_log_lines {
      <<fn>>
    }
    class struct_NormEvent {
      <<struct>>
      +"activity: String"
      +"file: String"
      +"code: String"
    }
    class fn_object_id {
      <<fn>>
    }
    class fn_root_relative {
      <<fn>>
    }
    class fn_normalized_sequence {
      <<fn>>
    }
    class fn_sequence_to_ndjson {
      <<fn>>
    }
    class fn_ndjson_to_sequence {
      <<fn>>
    }
    class fn_golden_is_genuinely_multispecies {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::ServerState`
- `lsp_max::lsp_types::Url`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
