# `crates/praxis-graphlaw/src/lib_test.rs`

Source SHA-256: `20068d1605a99cae8fe27596647179ee519eacc4d1edef7cea5b83afaf3a4ada`

```mermaid
classDiagram
    class fn_group_concat_aggregate_refuses_instead_of_panicking {
      <<fn>>
    }
    class fn_group_by_unbound_variable_groups_instead_of_panicking {
      <<fn>>
    }
    class fn_having_count_filters_correctly {
      <<fn>>
    }
    class fn_having_count_excludes_all_when_threshold_too_high {
      <<fn>>
    }
    class fn_sum_over_untagged_literals_computes_the_real_value {
      <<fn>>
    }
    class fn_test_group_by_row_order_is_independent_of_triple_insertion_order {
      <<fn>>
    }
    class fn_test_parse {
      <<fn>>
    }
    class fn_test_store {
      <<fn>>
    }
    class fn_test_incomplete_rule_match {
      <<fn>>
    }
    class fn_test_no_var_query {
      <<fn>>
    }
    class fn_test_single_rule {
      <<fn>>
    }
    class fn_test_multiple_rule {
      <<fn>>
    }
    class fn_test_join_rule {
      <<fn>>
    }
    class fn_test_long_join_rule {
      <<fn>>
    }
    class fn_test_transitive_rule {
      <<fn>>
    }
    class fn_test_hierarchy {
      <<fn>>
    }
    class fn_test_rdf_hierarchy {
      <<fn>>
    }
    class fn_test_shacl_min_count_violation {
      <<fn>>
    }
    class fn_test_shacl_conforms {
      <<fn>>
    }
    class fn_test_shacl_validate_order_is_independent_of_triple_insertion_order {
      <<fn>>
    }
    class fn_test_shacl_closed_shape_violation_order_is_deterministic {
      <<fn>>
    }
    class fn_test_shex_closed_shape_violation_order_is_deterministic {
      <<fn>>
    }
    class fn_test_shacl_report_to_triples {
      <<fn>>
    }
    class fn_test_shacl_node_constraint_violation {
      <<fn>>
    }
    class fn_test_shacl_node_constraint_conforms {
      <<fn>>
    }
    class fn_test_triplestore_validate_shex {
      <<fn>>
    }
    class fn_test_datalog_safety_check_rejects_unsafe_rule {
      <<fn>>
    }
    class fn_test_datalog_safe_rule_stratification {
      <<fn>>
    }
    class fn_materialize_does_not_panic_on_a_directly_constructed_zero_window_hook {
      <<fn>>
    }
    class fn_materialize_still_rolls_back_the_checkpoint_and_returns_err_on_an_ordinary_typed_failure {
      <<fn>>
    }
    class fn_materialize_clears_verdicts_on_rollback_not_just_triple_index {
      <<fn>>
    }
    class fn_test_datalog_empty_ruleset_is_trivially_stratifiable {
      <<fn>>
    }
    class fn_test_add_rules_with_empty_ruleset_does_not_refuse {
      <<fn>>
    }
    class fn_test_n3_rules_forward_chaining {
      <<fn>>
    }
    class fn_create_temp_pack_dir {
      <<fn>>
    }
    class fn_test_load_hook_pack_valid {
      <<fn>>
    }
    class fn_test_load_hook_pack_unsupported_dialect {
      <<fn>>
    }
    class fn_test_load_hook_pack_forbidden_keyword {
      <<fn>>
    }
    class fn_test_load_hook_pack_forbidden_predicate {
      <<fn>>
    }
    class fn_test_load_hook_pack_closed_shape_violation {
      <<fn>>
    }
    class fn_test_load_hook_pack_action_closed_shape_violation {
      <<fn>>
    }
    class fn_test_hook_pack_topological_sorting {
      <<fn>>
    }
    class fn_test_hook_pack_cycle_detection {
      <<fn>>
    }
    class fn_test_fibo_blank_nodes_typed_literals_collections {
      <<fn>>
    }
```

## Dependencies

- `crate::datalog::validate_rules`
- `crate::hooks::{CmpOp, CompiledHook, EffectKind, EventId, HookCondition, HookId}`
- `crate::hooks::{CompiledHook, EffectKind, EventId, HookCondition, HookId}`
- `crate::reasoner::Reasoner`
- `crate::{ BodyLiteral, QueryEngine, Rule, RuleIndex, SimpleQueryEngine, Triple, TripleIndex, TripleStore, VarOrTerm, }`
- `std::collections::HashMap`
- `std::sync::atomic::{AtomicUsize, Ordering}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
