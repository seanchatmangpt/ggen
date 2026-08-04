# `crates/ggen-engine/tests/pipeline_stage_evidence_test.rs`

Source SHA-256: `cea82495b9bea67c40d1bf377f84e34894fe15db441ea2651130a6359b7e819b`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_successful_sync_populates_stage_specific_report_fields {
      <<fn>>
    }
    class fn_missing_ontology_file_refuses_closed_at_resolve_stage {
      <<fn>>
    }
    class fn_output_path_escaping_root_refuses_closed_at_write_stage {
      <<fn>>
    }
    class fn_dry_run_projection_is_deterministic_across_independent_runs {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions}`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
