# `crates/ggen-engine/tests/generation_rules_e2e.rs`

Source SHA-256: `0d542bc0798ecce1b66ae89cdc1cf5f032fa095273f17c29ae5df8cf0b7c8e02`

```mermaid
classDiagram
    class fn_write_manifest {
      <<fn>>
    }
    class fn_write_ontology {
      <<fn>>
    }
    class fn_static_rule_with_file_query_and_file_template_renders_real_query_results {
      <<fn>>
    }
    class fn_per_row_rule_with_inline_query_and_template_creates_one_file_per_row {
      <<fn>>
    }
    class fn_when_guard_false_and_skip_empty_produce_documented_skips_not_writes {
      <<fn>>
    }
    class fn_merge_mode_preserves_hand_edits_across_two_syncs_with_changed_query_data {
      <<fn>>
    }
    class fn_create_mode_writes_once_then_leaves_hand_edits_alone {
      <<fn>>
    }
    class fn_overwrite_mode_replaces_content_and_skips_when_unchanged {
      <<fn>>
    }
    class fn_unimplemented_query_source_pack_is_a_typed_refusal_not_a_silent_skip {
      <<fn>>
    }
    class fn_unimplemented_template_source_git_is_a_typed_refusal {
      <<fn>>
    }
    class fn_duplicate_render_targets_from_per_row_rule_are_refused {
      <<fn>>
    }
    class fn_two_generation_rules_syncs_chain_receipts {
      <<fn>>
    }
    class fn_empty_rendered_body_is_refused_not_silently_written {
      <<fn>>
    }
    class fn_inference_rule_construct_is_visible_to_generation_rule_query {
      <<fn>>
    }
    class fn_inference_rule_when_guard_false_skips_construct {
      <<fn>>
    }
    class fn_law_gate_denial_violation_refuses_declarative_rules_sync {
      <<fn>>
    }
    class fn_law_gate_violation_refuses_declarative_rules_sync_naming_offending_node {
      <<fn>>
    }
    class fn_legacy_validation_shacl_is_refused_loudly {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH}`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
