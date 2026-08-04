# `crates/ggen-engine/tests/generation_rules_typed_causes_e2e.rs`

Source SHA-256: `f4684a781375678320cf76470eecf0e96eb4b97890a5b4b9545903c92755267a`

```mermaid
classDiagram
    class fn_write_manifest {
      <<fn>>
    }
    class fn_write_ontology {
      <<fn>>
    }
    class fn_sparql_results_alias_is_available_alongside_results {
      <<fn>>
    }
    class fn_static_rule_flattens_first_row_columns_to_top_level_context {
      <<fn>>
    }
    class fn_orphaned_broken_template_file_does_not_abort_unrelated_rule {
      <<fn>>
    }
    class fn_included_broken_template_file_still_fails_loudly {
      <<fn>>
    }
    class fn_missing_context_variable_is_classified_as_variable_missing {
      <<fn>>
    }
    class fn_broken_output_file_pattern_is_classified_as_output_path_invalid {
      <<fn>>
    }
    class fn_unknown_filter_is_classified_as_filter_unknown {
      <<fn>>
    }
    class fn_yaml_file_tree_meta_spec_is_classified_as_schema_incompatible {
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
