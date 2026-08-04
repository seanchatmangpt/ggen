# `crates/openapi-cnv-reflect/tests/reflect_e2e.rs`

Source SHA-256: `e9f8349a4ab892963f4dada7a17b78b10c801a2b8712672f576af445ec8db971`

```mermaid
classDiagram
    class fn_packs_dir {
      <<fn>>
    }
    class fn_fixture_path {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_count {
      <<fn>>
    }
    class fn_assert_cli_success {
      <<fn>>
    }
    class fn_run_cargo {
      <<fn>>
    }
    class fn_reflecting_the_fixture_produces_the_expected_graph_shape {
      <<fn>>
    }
    class fn_reflected_ontology_passes_through_the_real_zero_code_pipeline {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::cli_proof::CliHarness`
- `oxigraph::sparql::{QueryResults, SparqlEvaluator}`
- `std::path::{Path, PathBuf}`
- `std::process::Command`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
