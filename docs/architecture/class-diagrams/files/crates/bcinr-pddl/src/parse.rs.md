# `crates/bcinr-pddl/src/parse.rs`

Source SHA-256: `8141b83be6dd7bb7673428988f635bd93042eb23c5883b095ea36d2222b013b5`

```mermaid
classDiagram
    class fn_domain_from_pddl {
      <<fn>>
    }
    class fn_problem_from_pddl {
      <<fn>>
    }
    class fn_domain31_from_pddl {
      <<fn>>
    }
    class fn_problem31_from_pddl {
      <<fn>>
    }
    class fn_num_f64 {
      <<fn>>
    }
    class fn_lower_action {
      <<fn>>
    }
    class fn_lower_precond_defs {
      <<fn>>
    }
    class fn_collect_precond_def {
      <<fn>>
    }
    class fn_collect_pref_gd {
      <<fn>>
    }
    class fn_collect_gd {
      <<fn>>
    }
    class fn_lower_effects {
      <<fn>>
    }
    class fn_collect_conditional_effect {
      <<fn>>
    }
    class fn_collect_primitive_effect {
      <<fn>>
    }
    class fn_lower_af_term {
      <<fn>>
    }
    class fn_lower_pred_af {
      <<fn>>
    }
    class type_LoweredInit {
      <<type>>
    }
    class fn_lower_init_full {
      <<fn>>
    }
    class fn_lower_types {
      <<fn>>
    }
    class fn_lower_functions {
      <<fn>>
    }
    class fn_lower_typed_variables {
      <<fn>>
    }
    class fn_type_to_string {
      <<fn>>
    }
    class fn_lower_action31 {
      <<fn>>
    }
    class fn_lower_precond_defs_full {
      <<fn>>
    }
    class fn_lower_precond_def_full {
      <<fn>>
    }
    class fn_lower_pref_gd_full {
      <<fn>>
    }
    class fn_lower_condition {
      <<fn>>
    }
    class fn_lower_goal_full {
      <<fn>>
    }
    class fn_lower_effect_list {
      <<fn>>
    }
    class fn_lower_conditional_effect_full {
      <<fn>>
    }
    class fn_lower_primitive_effect_full {
      <<fn>>
    }
    class fn_lower_assign_op_numeric {
      <<fn>>
    }
    class fn_lower_function_head {
      <<fn>>
    }
    class fn_lower_fluent_expression {
      <<fn>>
    }
    class fn_lower_da_fluent_expression {
      <<fn>>
    }
    class fn_lower_binary_op {
      <<fn>>
    }
    class fn_lower_multi_op {
      <<fn>>
    }
    class fn_lower_durative_action {
      <<fn>>
    }
    class fn_lower_duration_constraint {
      <<fn>>
    }
    class fn_lower_simple_duration_constraint {
      <<fn>>
    }
    class fn_lower_duration_value {
      <<fn>>
    }
    class fn_lower_da_gd {
      <<fn>>
    }
    class fn_lower_pref_timed_gd {
      <<fn>>
    }
    class fn_lower_timed_gd {
      <<fn>>
    }
    class fn_lower_time_specifier {
      <<fn>>
    }
    class fn_lower_da_effect {
      <<fn>>
    }
    class fn_lower_da_gd_condition {
      <<fn>>
    }
    class fn_lower_timed_effect {
      <<fn>>
    }
    class fn_lower_effect_condition_ec {
      <<fn>>
    }
    class fn_lower_derived_predicate {
      <<fn>>
    }
    class fn_lower_constraint_gd {
      <<fn>>
    }
    class fn_lower_trajectory_constraint {
      <<fn>>
    }
    class fn_lower_con2gd_condition {
      <<fn>>
    }
    class fn_lower_metric {
      <<fn>>
    }
    class fn_lower_metric_expr {
      <<fn>>
    }
```

## Dependencies

- `crate::error::Pddl8Error`
- `pddl::BinaryComparison`
- `pddl::DurativeActionFluentExpression`
- `pddl::Literal`
- `pddl::{ parsers::Span, AssignOp, BinaryOp, ConditionalEffect, ConstraintGoalDefinition, ConstraintGoalDefinitionInner, DurationConstraint as PddlDurationConstraint, DurationOperator, DurationValue, DurativeActionEffect, DurativeActionGoalDefinition, FluentExpression, FunctionHead, GoalDefinition, InitElement, MetricFluentExpression, MultiOp, Optimization, Parser, PreconditionGoalDefinition, PredicateAtomicFormula, PreferenceGoalDefinition, PreferenceTimedGoalDefinition, PrimitiveEffect, SimpleDurationConstraint, StructureDef, TimedEffect, TimedGoalDefinition, }`
- `wasm4pm_compat::pddl::{ CompareOp, DerivedPredicate, DurationConstraint, DurativeAction, Metric, MetricDir, MetricExpr, NumericEffect, NumericExpr, NumericOp, Pddl31Action, Pddl31Domain, Pddl31Problem, Pddl8ActionSchema, Pddl8Atom, Pddl8Domain, Pddl8Problem, PddlCondition, PddlConstraint, PddlEffect, PddlEvent, PddlFunction, PddlPreference, PddlProcess, PddlType, TimeSpecifier, TimedLiteral, TrajectoryConstraint, PDDL8_MAX_ARITY, PDDL8_MAX_CONJUNCTS, PDDL8_MAX_PARAMS, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
