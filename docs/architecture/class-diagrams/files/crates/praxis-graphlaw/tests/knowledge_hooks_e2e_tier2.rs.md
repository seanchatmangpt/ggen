# `crates/praxis-graphlaw/tests/knowledge_hooks_e2e_tier2.rs`

Source SHA-256: `5a76ae9dbcd534ac79f2e07b4bb948fc0df7ade85cb78200c297b4828b93fc37`

```mermaid
classDiagram
    class mod_common {
      <<mod>>
    }
    class fn_test_b1_empty_hook_pack {
      <<fn>>
    }
    class fn_test_b1_max_name_length {
      <<fn>>
    }
    class fn_test_b1_exceed_max_hooks {
      <<fn>>
    }
    class fn_test_b1_turtle_formatting {
      <<fn>>
    }
    class fn_test_b1_missing_mandatory_fields {
      <<fn>>
    }
    class fn_test_b2_empty_shacl_law {
      <<fn>>
    }
    class fn_test_b2_hidden_side_effects {
      <<fn>>
    }
    class fn_test_b2_huge_hook_packs {
      <<fn>>
    }
    class fn_test_b2_conflicting_shacl_constraints {
      <<fn>>
    }
    class fn_test_b2_multiple_sequential_loads {
      <<fn>>
    }
    class fn_test_b3_empty_trigger_results {
      <<fn>>
    }
    class fn_test_b3_window_size_bounds {
      <<fn>>
    }
    class fn_test_b3_threshold_boundary_values {
      <<fn>>
    }
    class fn_test_b3_datalog_program_size_limit {
      <<fn>>
    }
    class fn_test_b3_sparql_syntax_error {
      <<fn>>
    }
    class fn_test_b4_construct_empty_result {
      <<fn>>
    }
    class fn_test_b4_construct_literal_subject {
      <<fn>>
    }
    class fn_test_b4_construct_unsupported_clauses {
      <<fn>>
    }
    class fn_test_b4_construct_no_op_addition {
      <<fn>>
    }
    class fn_test_b4_construct_modify_registry {
      <<fn>>
    }
    class fn_test_b5_receipt_blank_nodes {
      <<fn>>
    }
    class fn_test_b5_receipt_unicode_literals {
      <<fn>>
    }
    class fn_test_b5_receipt_huge_literals {
      <<fn>>
    }
    class fn_test_b5_stable_hash_datatypes_lang {
      <<fn>>
    }
    class fn_test_b5_hash_both_add_and_delete {
      <<fn>>
    }
    class fn_test_b6_infinite_loop_detection {
      <<fn>>
    }
    class fn_test_b6_circular_dependency {
      <<fn>>
    }
    class fn_test_b6_gating_refusal_deep_rollback {
      <<fn>>
    }
    class fn_test_b6_empty_base_facts {
      <<fn>>
    }
    class fn_test_b6_multi_strata_evaluation {
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
