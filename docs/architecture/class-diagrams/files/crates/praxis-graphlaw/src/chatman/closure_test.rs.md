# `crates/praxis-graphlaw/src/chatman/closure_test.rs`

Source SHA-256: `b7a3a0ad7c6da0ff02d9b532248babaf4c1644c30f5cc8eb60e5d50eb9245622`

```mermaid
classDiagram
    class fn_conforming_evidence {
      <<fn>>
    }
    class fn_nonconforming_evidence {
      <<fn>>
    }
    class fn_root_partial_order_over {
      <<fn>>
    }
    class fn_root_socket {
      <<fn>>
    }
    class fn_leaf_socket {
      <<fn>>
    }
    class fn_all_required_leaves_parent_open_until_every_child_is_admitted {
      <<fn>>
    }
    class fn_all_required_with_all_children_still_open_is_not_closed {
      <<fn>>
    }
    class fn_quorum_two_of_three_closes_even_though_the_third_stays_open {
      <<fn>>
    }
    class fn_quorum_two_of_three_not_closed_with_only_one_admitted {
      <<fn>>
    }
    class fn_declare_refuses_zero_children {
      <<fn>>
    }
    class fn_declare_refuses_quorum_zero {
      <<fn>>
    }
    class fn_declare_refuses_quorum_exceeding_child_count {
      <<fn>>
    }
    class fn_declare_accepts_quorum_equal_to_child_count {
      <<fn>>
    }
    class fn_observe_refuses_unknown_child {
      <<fn>>
    }
    class fn_admit_refuses_unknown_child {
      <<fn>>
    }
    class fn_require_terminal_admitted_refuses_unknown_child {
      <<fn>>
    }
    class fn_require_terminal_admitted_refuses_a_merely_observed_child {
      <<fn>>
    }
    class fn_require_terminal_admitted_refuses_a_still_open_child {
      <<fn>>
    }
    class fn_require_terminal_admitted_accepts_an_admitted_child {
      <<fn>>
    }
    class fn_admit_is_idempotent_and_observe_never_downgrades_an_admitted_child {
      <<fn>>
    }
    class fn_observe_is_idempotent_for_an_already_observed_child {
      <<fn>>
    }
    class fn_any_sufficient_closes_as_soon_as_one_child_is_admitted {
      <<fn>>
    }
    class fn_any_sufficient_not_closed_when_only_observed_not_admitted {
      <<fn>>
    }
    class fn_ordered_subset_closes_once_its_declared_children_are_admitted_even_if_others_stay_open {
      <<fn>>
    }
    class fn_ordered_subset_not_closed_while_a_declared_member_is_merely_observed {
      <<fn>>
    }
    class fn_declare_refuses_empty_ordered_subset {
      <<fn>>
    }
    class fn_declare_refuses_ordered_subset_with_duplicate_entry {
      <<fn>>
    }
    class fn_declare_refuses_ordered_subset_naming_a_non_child {
      <<fn>>
    }
    class fn_policy_decides_stays_open_until_a_decision_is_recorded {
      <<fn>>
    }
    class fn_policy_decides_closes_once_authority_records_an_affirmative_verdict {
      <<fn>>
    }
    class fn_policy_decides_stays_open_on_a_negative_verdict {
      <<fn>>
    }
    class fn_record_policy_decision_refuses_when_law_is_not_policy_decides {
      <<fn>>
    }
    class fn_record_policy_decision_refuses_empty_authority {
      <<fn>>
    }
    class fn_record_policy_decision_last_recorded_governs {
      <<fn>>
    }
    class fn_first_conformant_closes_like_any_sufficient_once_one_child_conforms {
      <<fn>>
    }
    class fn_first_conformant_not_closed_when_only_observed_not_admitted {
      <<fn>>
    }
    class fn_first_conformant_child_resolves_ties_by_canonical_order {
      <<fn>>
    }
    class fn_promote_observed_to_admitted_promotes_on_conforming_evidence {
      <<fn>>
    }
    class fn_promote_observed_to_admitted_refuses_on_nonconforming_evidence_and_leaves_child_observed {
      <<fn>>
    }
    class fn_promote_observed_to_admitted_refuses_a_still_open_child {
      <<fn>>
    }
    class fn_promote_observed_to_admitted_refuses_unknown_child {
      <<fn>>
    }
    class fn_promote_observed_to_admitted_is_idempotent_for_an_already_admitted_child {
      <<fn>>
    }
    class fn_closure_law_name_matches_prd_snake_case_vocabulary {
      <<fn>>
    }
    class fn_declared_child_order_is_deterministic_across_runs {
      <<fn>>
    }
```

## Dependencies

- `crate::shacl::ValidationResult`
- `crate::term::Term`
- `powl2_decompose::{ParentChildClosure, Powl, SocketKind, SocketPath, WorkflowSocketId}`
- `std::collections::BTreeSet`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
