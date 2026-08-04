# `crates/ggen-lsp/tests/ggen_harness_001_living_loop.rs`

Source SHA-256: `d7bbd2bfedc2f00c8c3c789f1b1d5ab5890b47d52cbee35b32a4f814122a00b3`

```mermaid
classDiagram
    class fn_url_from_path {
      <<fn>>
    }
    class fn_fixture_root {
      <<fn>>
    }
    class fn_is_harness_001 {
      <<fn>>
    }
    class fn_is_tpl_001 {
      <<fn>>
    }
    class fn_count_harness_001 {
      <<fn>>
    }
    class fn_count_tpl_001 {
      <<fn>>
    }
    class fn_has_harness_001_error {
      <<fn>>
    }
    class fn_harness_001_diag {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_write_mismatch_crate {
      <<fn>>
    }
    class fn_read_log_lines {
      <<fn>>
    }
    class fn_has_manifest_event {
      <<fn>>
    }
    class fn_invalid_harness_raises_001_through_headless_gate {
      <<fn>>
    }
    class fn_lawful_harness_stays_clean {
      <<fn>>
    }
    class fn_creating_missing_proof_file_clears_001_through_gate {
      <<fn>>
    }
    class fn_harness_001_route_is_source_law_only {
      <<fn>>
    }
    class fn_headless_gate_writes_no_artifact {
      <<fn>>
    }
    class fn_walk_files {
      <<fn>>
    }
```

## Dependencies

- `ggen_lsp::ServerState`
- `ggen_lsp::check::{check_files_in_root, discover_law_surfaces, CheckReport}`
- `ggen_lsp::route::{Provenance, RouteRegistry}`
- `lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url}`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
