# `crates/praxis-graphlaw/tests/business_logic_cases.rs`

Source SHA-256: `7486eb72da5596ad99a68b1bc5e8f8e1355fa9114d50fa82bc16c33c7d2cb520`

```mermaid
classDiagram
    class fn_test_suite1_minimal_approval_positive {
      <<fn>>
    }
    class fn_test_suite1_minimal_refusal_surfaces_reason {
      <<fn>>
    }
    class fn_test_suite1_minimal_malformed_missing_kind {
      <<fn>>
    }
    class fn_test_suite1_minimal_malformed_unknown_predicate {
      <<fn>>
    }
    class fn_test_suite1_minimal_approval_no_trigger {
      <<fn>>
    }
    class fn_test_suite2_approval_routing_single_entity {
      <<fn>>
    }
    class fn_test_suite2_approval_routing_100_entities {
      <<fn>>
    }
    class fn_test_suite2_approval_routing_priority_order {
      <<fn>>
    }
    class fn_test_suite2_approval_routing_exceed_12_hooks {
      <<fn>>
    }
    class fn_test_suite2_approval_routing_missing_priority {
      <<fn>>
    }
    class fn_test_suite2_approval_routing_bad_after_reference {
      <<fn>>
    }
    class fn_test_suite3_state_machine_legal_transition_single {
      <<fn>>
    }
    class fn_test_suite3_state_machine_illegal_transition_single {
      <<fn>>
    }
    class fn_test_suite3_state_machine_1000_legal {
      <<fn>>
    }
    class fn_test_suite3_state_machine_malformed_wrong_type {
      <<fn>>
    }
    class fn_test_suite3_state_machine_circular_infinite_loop {
      <<fn>>
    }
    class fn_test_suite4_idempotency_same_key_twice {
      <<fn>>
    }
    class fn_test_suite4_idempotency_no_duplicate_triple {
      <<fn>>
    }
    class fn_test_suite4_idempotency_receipt_sharing_key {
      <<fn>>
    }
    class fn_test_suite4_idempotency_malformed_missing_field {
      <<fn>>
    }
    class fn_test_suite5_sla_escalation_threshold_crossed {
      <<fn>>
    }
    class fn_test_suite5_sla_escalation_below_threshold {
      <<fn>>
    }
    class fn_test_suite5_sla_escalation_exact_threshold {
      <<fn>>
    }
    class fn_test_suite5_sla_escalation_malformed_operator {
      <<fn>>
    }
    class fn_test_suite5_sla_escalation_missing_k {
      <<fn>>
    }
    class fn_test_suite6_policy_conflict_approve_vs_refuse {
      <<fn>>
    }
    class fn_test_suite6_policy_conflict_state_contradictions {
      <<fn>>
    }
    class fn_test_suite6_policy_conflict_multiple_refusals {
      <<fn>>
    }
    class fn_test_suite6_policy_conflict_malformed_dual_effect {
      <<fn>>
    }
    class fn_test_suite6_policy_conflict_unknown_effect_predicate {
      <<fn>>
    }
    class fn_test_integration_complex_approval_workflow {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::parser::Syntax`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
