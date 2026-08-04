# `crates/praxis-graphlaw/src/chatman/router_test.rs`

Source SHA-256: `4fbd4dc335abd4b4466a8bf927fa42a7d3e44ad1e8cbd2692f1901dce637005c`

```mermaid
classDiagram
    class fn_profile {
      <<fn>>
    }
    class fn_shape {
      <<fn>>
    }
    class fn_ok_or_unreachable {
      <<fn>>
    }
    class fn_dialect_order_is_ascending_expressive_power {
      <<fn>>
    }
    class fn_routes_map_hot_warm_cold {
      <<fn>>
    }
    class fn_mask_bits_are_distinct_powers_of_two {
      <<fn>>
    }
    class fn_default_enabled_mask_excludes_n3 {
      <<fn>>
    }
    class fn_gates_refuse_n3_in_actuation_mask {
      <<fn>>
    }
    class fn_gates_refuse_actuation_not_subset_of_enabled {
      <<fn>>
    }
    class fn_gates_refuse_hot_budget_over_eight {
      <<fn>>
    }
    class fn_n3_gate_matrix_full {
      <<fn>>
    }
    class fn_ler_ordering_property_over_all_dialect_pairs {
      <<fn>>
    }
    class fn_verify_claim_refuses_more_expressive_claim {
      <<fn>>
    }
    class fn_verify_claim_refuses_hash_drift {
      <<fn>>
    }
    class fn_verify_claim_accepts_exact_decision {
      <<fn>>
    }
    class fn_over_budget_falls_to_warm_when_available {
      <<fn>>
    }
    class fn_over_budget_with_no_warm_dialect_is_warm_path_required {
      <<fn>>
    }
    class fn_nothing_enabled_is_unsupported_dialect {
      <<fn>>
    }
    class fn_actuation_skips_non_permitted_dialects {
      <<fn>>
    }
    class fn_decision_hash_is_deterministic {
      <<fn>>
    }
    class fn_decision_hash_distinguishes_shapes_and_profiles {
      <<fn>>
    }
    class fn_n3_enabled_gates {
      <<fn>>
    }
    class fn_n3_disabled_gates {
      <<fn>>
    }
    class fn_execution {
      <<fn>>
    }
    class fn_rule {
      <<fn>>
    }
    class fn_rule_requesting_actuation {
      <<fn>>
    }
    class fn_n3_cost_bound_exhausts_strictly_over_limit {
      <<fn>>
    }
    class fn_n3_cost_bound_consume_saturates_never_wraps {
      <<fn>>
    }
    class fn_n3_cost_bound_consume_zero_leaves_used_unchanged {
      <<fn>>
    }
    class fn_n3_builtin_mask_bits_are_distinct_powers_of_two {
      <<fn>>
    }
    class fn_n3_executor_refuses_without_capability_grant {
      <<fn>>
    }
    class fn_n3_executor_admits_when_capability_granted_and_no_rules_violate_anything {
      <<fn>>
    }
    class fn_n3_executor_refuses_when_a_single_rule_exceeds_the_bound_before_any_other_runs {
      <<fn>>
    }
    class fn_n3_executor_admits_cumulative_cost_at_exactly_the_bound {
      <<fn>>
    }
    class fn_n3_executor_admits_zero_declared_cost_rule_without_consuming_budget {
      <<fn>>
    }
    class fn_n3_executor_refuses_mid_execution_when_a_later_rule_pushes_cumulative_over_bound {
      <<fn>>
    }
    class fn_n3_executor_refuses_rule_using_non_whitelisted_builtin {
      <<fn>>
    }
    class fn_n3_executor_admits_rule_using_whitelisted_builtin {
      <<fn>>
    }
    class fn_n3_executor_refuses_non_whitelisted_builtin_before_consuming_its_cost {
      <<fn>>
    }
    class fn_n3_executor_refuses_rule_requesting_direct_actuation {
      <<fn>>
    }
    class fn_n3_executor_direct_actuation_refusal_is_distinguishable_from_builtin_refusal {
      <<fn>>
    }
    class fn_n3_executor_refuses_direct_actuation_before_checking_cost_bound {
      <<fn>>
    }
    class fn_n3_executor_refuses_direct_actuation_before_any_later_rule_runs {
      <<fn>>
    }
    class fn_n3_executor_refuses_every_recognized_direct_actuation_builtin {
      <<fn>>
    }
    class fn_n3_executor_direct_actuation_refusal_is_deterministic_across_repeated_runs {
      <<fn>>
    }
    class fn_n3_executor_admission_is_deterministic_across_repeated_runs {
      <<fn>>
    }
    class fn_n3_executor_refusal_is_deterministic_across_repeated_runs {
      <<fn>>
    }
    class fn_ok_or_unreachable_n3 {
      <<fn>>
    }
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
