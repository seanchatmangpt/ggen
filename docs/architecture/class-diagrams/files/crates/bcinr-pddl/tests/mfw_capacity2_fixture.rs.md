# `crates/bcinr-pddl/tests/mfw_capacity2_fixture.rs`

Source SHA-256: `aa95c442b0ed0cd0c08dba3897d8a74ec1408a4581aa84fcd2824aefe7008843`

```mermaid
classDiagram
    class fn_bounds {
      <<fn>>
    }
    class fn_epoch_from {
      <<fn>>
    }
    class fn_occ {
      <<fn>>
    }
    class fn_real_causal_plan_for_abc {
      <<fn>>
    }
    class fn_hand_built_capacity2_complex {
      <<fn>>
    }
    class fn_link1_real_analyzer_admits_independence_for_every_pair_of_a_genuinely_independent_triple {
      <<fn>>
    }
    class fn_link1b_real_analyzer_does_not_invent_independence_for_a_genuinely_dependent_pair {
      <<fn>>
    }
    class fn_link2_precedes_is_the_full_input_vector_order_even_though_every_pair_is_independent {
      <<fn>>
    }
    class fn_link3a_real_pddl_pipeline_cannot_produce_a_three_way_nonface_from_pairwise_independent_actions {
      <<fn>>
    }
    class fn_link3b_hand_built_capacity2_complex_admits_every_pair_and_rejects_the_triple_by_event_membership {
      <<fn>>
    }
    class fn_abc_causal_plan_with_empty_order {
      <<fn>>
    }
    class fn_link4_real_projector_preserves_the_capacity2_nonface_into_the_powl_model {
      <<fn>>
    }
    class fn_link4_adversarial_fixed_projector_now_agrees_eventset_slot_is_position_not_raw_id {
      <<fn>>
    }
    class fn_link5_compiled_guard_table_rejects_the_triple_and_admits_every_pair {
      <<fn>>
    }
    class fn_three_parallel_legacy_tape {
      <<fn>>
    }
    class fn_capacity2_guard_table_for_slots_0_1_2 {
      <<fn>>
    }
    class fn_link6_real_scheduler_never_fires_the_triple_when_the_ready_set_is_the_triple {
      <<fn>>
    }
    class fn_link7_execution_receipt_fired_pair_differs_from_the_genuinely_ready_triple {
      <<fn>>
    }
    class fn_link8a_a_real_receipt_for_the_legal_pair_is_well_formed {
      <<fn>>
    }
    class fn_link8b_seal_and_verify_execution_receipt_reject_a_hand_fabricated_triple_receipt {
      <<fn>>
    }
```

## Dependencies

- `bcinr_mfw_ir::{ ActionOccurrence, ActionOccurrenceId, CausalAnalyzer, CausalPlan, ConcurrencyAnalyzer, ConcurrencyConflictWitness, Digest, EventSet, ExecutableConcurrencyComplex, FluentId, IndependenceRelation, MinimalNonFace, PlanningEpochId, PowlProjector as PowlProjectorTrait, ResourceConflictWitness, StrictPartialOrder, }`
- `bcinr_pddl::capability::GroundedPlanningEpoch`
- `bcinr_pddl::{ domain_from_pddl, problem_from_pddl, GroundProblem, PddlCausalAnalyzer, PddlConcurrencyAnalyzer, }`
- `bcinr_powl::compiler::v2::compile_powl_v2`
- `bcinr_powl::compiler::{compile_powl, PowlAstNode}`
- `bcinr_powl::scheduler::{ scheduler_tick, scheduler_tick_guarded, PowlRunState, StableMaximalSelector, }`
- `bcinr_powl::tape::v2::{CompiledNonFace, ConcurrencyGuardTable}`
- `bcinr_powl_receipt::execution::{ digest_legacy_tape, seal_execution_receipt, tick_and_seal_execution_receipt, verify_execution_receipt, ExecutionIntegrityError, ExecutionReceipt, }`
- `std::collections::BTreeMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
