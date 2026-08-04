# `crates/praxis-graphlaw/src/chatman/engine_test.rs`

Source SHA-256: `0b6c15383d7cfa9410eba0efb16371f6a933109763d3a8024b5c9bcd5849658e`

```mermaid
classDiagram
    class fn_test_profile {
      <<fn>>
    }
    class fn_envelope {
      <<fn>>
    }
    class fn_engine_with {
      <<fn>>
    }
    class fn_admit {
      <<fn>>
    }
    class fn_refusal_check {
      <<fn>>
    }
    class fn_happy_path_admits_and_seals_nine_digests {
      <<fn>>
    }
    class fn_owl_closure_lands_in_sibling_graph_not_snapshot {
      <<fn>>
    }
    class fn_double_run_is_byte_identical {
      <<fn>>
    }
    class fn_five_consecutive_runs_receipts_and_stage_seals_are_byte_identical {
      <<fn>>
    }
    class fn_stage_seal_mismatch_refuses_tampered_transition {
      <<fn>>
    }
    class fn_stage_seal_wrong_stage_name_refuses {
      <<fn>>
    }
    class fn_s1_unknown_snapshot_refuses_snapshot_not_found {
      <<fn>>
    }
    class fn_s1_quoted_triple_text_refuses_triple_term {
      <<fn>>
    }
    class fn_s2_owl_disabled_refuses_via_router {
      <<fn>>
    }
    class fn_s3_missing_pddl_refuses_plan_infeasible {
      <<fn>>
    }
    class fn_s3_unreachable_goal_refuses_plan_infeasible {
      <<fn>>
    }
    class fn_s4_duplicate_fire_refuses_trace_unlawful {
      <<fn>>
    }
    class fn_s4_missing_trace_refuses_trace_unlawful {
      <<fn>>
    }
    class fn_envelope_naming_wrong_profile_refuses_profile_hash_mismatch {
      <<fn>>
    }
    class fn_actuate_registers_post_graph_and_dedups {
      <<fn>>
    }
    class fn_verify_replay_accepts_faithful_and_refuses_tampered {
      <<fn>>
    }
    class fn_model_with_external_cut {
      <<fn>>
    }
    class struct_FakeExternalCutCompiler {
      <<struct>>
      +"air_digest_hex: String"
    }
    class fn_verify_replay_with_external_cut_accepts_faithful_and_refuses_tampered_artifact {
      <<fn>>
    }
    class fn_verify_replay_with_external_cut_matches_plain_replay_when_receipt_has_no_digest_10 {
      <<fn>>
    }
    class fn_process_cell_isolation_engine_b_cannot_observe_engine_a {
      <<fn>>
    }
    class fn_closure_root_socket {
      <<fn>>
    }
    class fn_closure_leaf_socket {
      <<fn>>
    }
    class fn_one_leaf_closure {
      <<fn>>
    }
    class fn_conforming_evidence {
      <<fn>>
    }
    class fn_nonconforming_evidence {
      <<fn>>
    }
    class fn_admit_child_completion_admits_through_real_engine_s1_and_closure_gate {
      <<fn>>
    }
    class fn_admit_child_completion_refuses_before_touching_closure_state_when_child_snapshot_is_unresolvable {
      <<fn>>
    }
    class fn_admit_child_completion_refuses_nonconforming_evidence_through_the_real_engine_entry_point {
      <<fn>>
    }
    note "ExternalCutCompiler for FakeExternalCutCompiler"
```

## Dependencies

- `crate::chatman::abi::{InputHandles, InvocationId, OperatorId, ProfileId}`
- `crate::chatman::closure::ClosureLaw`
- `crate::shacl::ValidationResult`
- `crate::term::Term`
- `powl2_decompose::{ParentChildClosure, SocketKind, SocketPath}`
- `std::collections::BTreeSet`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
