# `crates/bcinr-pddl/src/ground/mod.rs`

Source SHA-256: `d6fa34657dfeea857e3aa4e12e1d2cae4805610ffb48814a3eb70d9853f08143`

```mermaid
classDiagram
    class mod_dict {
      <<mod>>
    }
    class mod_facts {
      <<mod>>
    }
    class mod_lazy {
      <<mod>>
    }
    class mod_xorf {
      <<mod>>
    }
    class fn_search_digest {
      <<fn>>
    }
    class struct_GroundProblem {
      <<struct>>
      +"initial_state: BTreeSet~Pddl8GroundAtom~"
      +"goal: Vec~Pddl8GroundAtom~"
      +"actions: Vec~Pddl8GroundAction~"
      +"action_index: HashMap~Pddl8GroundAtom"
      +"always_applicable: Vec~usize~"
      +"constraints: Vec~PddlCondition~"
      +"derived_predicates: Vec~GroundDerivedPredicate~"
      +"quant_domain: QuantifierDomain"
    }
    class struct_GroundDerivedPredicate {
      <<struct>>
      +"head: Pddl8GroundAtom"
      +"condition: PddlCondition"
    }
    class struct_TypeIndex {
      <<struct>>
      +"object_type: HashMap~String"
      +"parent: HashMap~String"
    }
    class struct_QuantifierDomain {
      <<struct>>
      +"objects: Vec~String~"
      +"type_index: TypeIndex"
    }
    class struct_GroundDurativeAction {
      <<struct>>
      +"schema_name: String"
      +"label: String"
      +"args: Vec~String~"
      +"duration_min: f64"
      +"duration_max: f64"
      +"conditions: Vec~PddlCondition~"
      +"effects: Vec~PddlEffect~"
    }
    class struct_GroundTemporalProblem {
      <<struct>>
      +"initial_atoms: BTreeSet~Pddl8GroundAtom~"
      +"initial_fn_values: HashMap~String"
      +"timed_inits: Vec~TimedLiteral~"
      +"goal: PddlCondition"
      +"actions: Vec~Pddl8GroundAction~"
      +"durative_actions: Vec~GroundDurativeAction~"
      +"constraints: Vec~PddlCondition~"
      +"derived_predicates: Vec~GroundDerivedPredicate~"
      +"quant_domain: QuantifierDomain"
    }
    class fn_eval_condition {
      <<fn>>
    }
    class fn_eval_quantifier {
      <<fn>>
    }
    class fn_apply_effect_at_start {
      <<fn>>
    }
    class fn_apply_effect_at_end {
      <<fn>>
    }
    class fn_apply_effect_ground {
      <<fn>>
    }
    class fn_apply_numeric_effect {
      <<fn>>
    }
    class fn_eval_numeric {
      <<fn>>
    }
    class fn_resolve_duration {
      <<fn>>
    }
    class fn_fn_key {
      <<fn>>
    }
    class fn_ground_schema {
      <<fn>>
    }
    class fn_ground_durative_schema {
      <<fn>>
    }
    class fn_subst_atom {
      <<fn>>
    }
    class fn_subst_numeric {
      <<fn>>
    }
    class fn_subst_function {
      <<fn>>
    }
    class fn_subst_condition {
      <<fn>>
    }
    class fn_subst_numeric_effect {
      <<fn>>
    }
    class fn_subst_effect {
      <<fn>>
    }
    class fn_instantiate {
      <<fn>>
    }
    class fn_ground_derived_schema {
      <<fn>>
    }
    class fn_compute_derived_closure {
      <<fn>>
    }
    class mod_quantifier_tests {
      <<mod>>
    }
    class mod_bound_exceeded_tests {
      <<mod>>
    }
    note "GroundProblem"
    note "GroundTemporalProblem"
    note "QuantifierDomain"
    note "TypeIndex"
```

## Dependencies

- `bcinr_mfw_ir::{ BoundHit, BoundKind, Digest, ExhaustionWitness, PlannerOutcome, SearchProfileId, }`
- `crate::error::Pddl8Error`
- `std::collections::VecDeque`
- `std::collections::{BTreeSet, HashMap}`
- `super::*`
- `wasm4pm_compat::pddl::NumericEffect`
- `wasm4pm_compat::pddl::TimeSpecifier`
- `wasm4pm_compat::pddl::{ CompareOp, DerivedPredicate, DurationConstraint, DurativeAction, NumericExpr, Pddl8ActionSchema, Pddl8Atom, Pddl8Domain, Pddl8GroundAction, Pddl8GroundAtom, Pddl8Problem, Pddl8Tape, PddlCondition, PddlEffect, PddlFunction, TemporalPlan, TemporalPlanStep, TimedLiteral, PDDL8_MAX_GROUND, PDDL8_MAX_PLAN_DEPTH, }`
- `wasm4pm_compat::pddl::{NumericExpr, NumericOp}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
