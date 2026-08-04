# `crates/bcinr-pddl/src/consequence.rs`

Source SHA-256: `7c9c94fdbea8e000792c966dfffc4d58d4355395bac11af81d89669865db52d2`

```mermaid
classDiagram
    class struct_PlanningResult {
      <<struct>>
      +"final_state: BTreeSet~Pddl8GroundAtom~"
      +"goal: Vec~Pddl8GroundAtom~"
      +"step_count: usize"
      +"makespan_milli: i64"
    }
    class trait_ConsequenceHorizon {
      <<trait>>
      +"id(&self) -~ ConsequenceHorizonId"
      +"observe(&self, result: &PlanningResult) -~ Self::Consequence"
    }
    class struct_GoalReachabilityHorizon {
      <<struct>>
    }
    class struct_MakespanObservation {
      <<struct>>
      +"step_count: usize"
      +"makespan_milli: i64"
    }
    class struct_MinimumMakespanHorizon {
      <<struct>>
    }
    class struct_ExactStateKey {
      <<struct>>
      +"state_digest: Digest"
      +"theory_digest: Digest"
      +"horizon: ConsequenceHorizonId"
    }
    class struct_StandingConsequenceCache {
      <<struct>>
      +"entries: BTreeMap~ExactStateKey"
      +"horizon: H"
    }
    class fn_plan_with_standing_cache {
      <<fn>>
    }
    class enum_ResidualDecision {
      <<enum>>
    }
    class struct_ResidualObligation {
      <<struct>>
      +"unsatisfied_goal_atoms: Vec~Pddl8GroundAtom~"
      +"candidate_supporting_actions: Vec~Pddl8GroundAction~"
    }
    class struct_Residualizer {
      <<struct>>
      +"_horizon: PhantomData~H~"
    }
    class mod_tests {
      <<mod>>
    }
    note "ConsequenceHorizon for GoalReachabilityHorizon"
    note "ConsequenceHorizon for MinimumMakespanHorizon"
    note "Default for Residualizer~H~"
    note "PlanningResult"
    note "Residualizer~GoalReachabilityHorizon~"
    note "Residualizer~H~"
    note "StandingConsequenceCache~H~"
```

## Dependencies

- `bcinr_mfw_ir::{ConsequenceHorizonId, Digest, PlannerOutcome}`
- `crate::ground::GroundProblem`
- `crate::parse::{domain_from_pddl, problem_from_pddl}`
- `std::cell::Cell`
- `std::collections::{BTreeMap, BTreeSet}`
- `std::marker::PhantomData`
- `super::*`
- `wasm4pm_compat::pddl::{Pddl8GroundAction, Pddl8GroundAtom, Pddl8Tape}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
