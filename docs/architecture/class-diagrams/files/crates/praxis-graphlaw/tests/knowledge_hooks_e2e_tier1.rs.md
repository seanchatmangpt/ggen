# `crates/praxis-graphlaw/tests/knowledge_hooks_e2e_tier1.rs`

Source SHA-256: `6ae7efdcd99551a37ed919611f7866af08b38babd8ca1b14a1b40fa4c373e762`

```mermaid
classDiagram
    class mod_common {
      <<mod>>
    }
    class fn_test_f1_load_valid_single {
      <<fn>>
    }
    class fn_test_f1_load_valid_multiple {
      <<fn>>
    }
    class fn_test_f1_resolve_prefixes {
      <<fn>>
    }
    class fn_test_f1_hook_alias_namespace_rewrite {
      <<fn>>
    }
    class fn_test_f1_load_inline_triples {
      <<fn>>
    }
    class fn_test_f1_query_registered_hooks {
      <<fn>>
    }
    class fn_test_f2_refuse_command {
      <<fn>>
    }
    class fn_test_f2_refuse_shell {
      <<fn>>
    }
    class fn_test_f2_refuse_unrecognized_action {
      <<fn>>
    }
    class fn_test_f2_gating_malformed_shacl {
      <<fn>>
    }
    class fn_test_f2_gating_rollback_state {
      <<fn>>
    }
    class fn_test_f3_sparql_ask_trigger {
      <<fn>>
    }
    class fn_test_f3_sparql_select_trigger {
      <<fn>>
    }
    class fn_test_f3_count_trigger {
      <<fn>>
    }
    class fn_test_f3_threshold_trigger {
      <<fn>>
    }
    class fn_test_f3_delta_trigger {
      <<fn>>
    }
    class fn_test_f3_datalog_trigger_format {
      <<fn>>
    }
    class fn_test_f4_project_add_quad {
      <<fn>>
    }
    class fn_test_f4_project_delete_quad {
      <<fn>>
    }
    class fn_test_f4_project_add_and_delete {
      <<fn>>
    }
    class fn_test_f4_refuse_side_effects {
      <<fn>>
    }
    class fn_test_f4_project_apply_to_graph {
      <<fn>>
    }
    class fn_test_f5_receipt_single_add {
      <<fn>>
    }
    class fn_test_f5_receipt_sort_determinism {
      <<fn>>
    }
    class fn_test_f5_receipt_deletion {
      <<fn>>
    }
    class fn_test_f5_get_hook_receipts_api {
      <<fn>>
    }
    class fn_test_f5_receipt_format_validation {
      <<fn>>
    }
    class fn_test_f6_single_pass_materialization {
      <<fn>>
    }
    class fn_test_f6_multi_pass_cascade {
      <<fn>>
    }
    class fn_test_f6_fixpoint_termination {
      <<fn>>
    }
    class fn_test_f6_refusal_rollback {
      <<fn>>
    }
    class fn_test_f6_query_state_post_materialize {
      <<fn>>
    }
```

## Dependencies

- `common::assert_contains_triple`
- `common::assert_not_contains_triple`
- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::parser::Syntax`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
