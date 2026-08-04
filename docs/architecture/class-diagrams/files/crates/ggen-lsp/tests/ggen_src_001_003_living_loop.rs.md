# `crates/ggen-lsp/tests/ggen_src_001_003_living_loop.rs`

Source SHA-256: `111b9a32cbb0f45194d06f9be1e5fab892748209c0b558aaea5312f4b8e0abc7`

```mermaid
classDiagram
    class fn_minimal_ontology {
      <<fn>>
    }
    class fn_write_ggen_toml {
      <<fn>>
    }
    class fn_src_001_fires_for_generated_dir_output {
      <<fn>>
    }
    class fn_src_001_not_raised_for_normal_output {
      <<fn>>
    }
    class fn_src_002_fires_for_do_not_edit_banner {
      <<fn>>
    }
    class fn_src_002_not_raised_for_clean_rs_file {
      <<fn>>
    }
    class fn_src_003_fires_for_source_caste_comment {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::check::{check_files_in_root, discover_law_surfaces}`
- `lsp_max::lsp_types`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
