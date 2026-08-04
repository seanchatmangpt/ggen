# `crates/bcinr-pddl/src/capability.rs`

Source SHA-256: `17b0be5899779f2d891e2017e8adc95569c628ea5ec07d242f115076d41715e1`

```mermaid
classDiagram
    class enum_PddlFeature {
      <<enum>>
    }
    class enum_SemanticSupport {
      <<enum>>
    }
    class trait_CapabilityProfile {
      <<trait>>
      +"support(&self, feature: PddlFeature) -~ SemanticSupport"
    }
    class struct_DefaultCapabilityProfile {
      <<struct>>
    }
    class fn_requirement_implies {
      <<fn>>
    }
    class fn_effect_uses_conditional_or_quantified {
      <<fn>>
    }
    class fn_effect_list_uses_conditional_or_quantified {
      <<fn>>
    }
    class fn_effect_uses_continuous_effect_sentinel {
      <<fn>>
    }
    class fn_effect_list_uses_continuous_effect_sentinel {
      <<fn>>
    }
    class fn_effect_uses_object_fluent_sentinel {
      <<fn>>
    }
    class fn_effect_list_uses_object_fluent_sentinel {
      <<fn>>
    }
    class struct_AdmittedPlanningTask {
      <<struct>>
      +"domain: Pddl31Domain"
      +"problem: Pddl31Problem"
      +"theory_digest: Digest"
    }
    class fn_admit_planning_task {
      <<fn>>
    }
    class fn_domain_problem_digest {
      <<fn>>
    }
    class fn_hash_typed_params {
      <<fn>>
    }
    class fn_hash_function {
      <<fn>>
    }
    class fn_hash_atom {
      <<fn>>
    }
    class fn_hash_numeric_op {
      <<fn>>
    }
    class fn_hash_numeric_expr {
      <<fn>>
    }
    class fn_hash_compare_op {
      <<fn>>
    }
    class fn_hash_time_specifier {
      <<fn>>
    }
    class fn_hash_condition {
      <<fn>>
    }
    class fn_hash_numeric_effect {
      <<fn>>
    }
    class fn_hash_effect {
      <<fn>>
    }
    class fn_hash_duration_constraint {
      <<fn>>
    }
    class struct_GroundedPlanningEpoch {
      <<struct>>
      +"id: PlanningEpochId"
      +"theory_digest: Digest"
      +"initial_state: BTreeSet~Pddl8GroundAtom~"
      +"goal: Vec~Pddl8GroundAtom~"
      +"actions: Vec~Pddl8GroundAction~"
      +"bounds: EpochBounds"
    }
    class mod_tests {
      <<mod>>
    }
    note "CapabilityProfile for DefaultCapabilityProfile"
    note "GroundedPlanningEpoch"
```

## Dependencies

- `SemanticSupport::{Approximate, Exact, Unsupported}`
- `bcinr_mfw_ir::{ Digest, EpochBounds, InconsistencyWitness, PlannerOutcome, PlanningEpochId, UnsupportedFeature, }`
- `crate::ground::GroundProblem`
- `crate::parse::{domain31_from_pddl, problem31_from_pddl}`
- `std::collections::BTreeSet`
- `super::*`
- `wasm4pm_compat::pddl::{ CompareOp, DurationConstraint, NumericEffect, NumericExpr, NumericOp, Pddl31Domain, Pddl31Problem, Pddl8Atom, Pddl8GroundAction, Pddl8GroundAtom, PddlCondition, PddlEffect, PddlFunction, TimeSpecifier, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
