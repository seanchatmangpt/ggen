# `crates/bcinr-pddl/tests/depth_bound_exhaustion_conflation.rs`

Source SHA-256: `d779f1fcf172949b28406607d3c1e7dee3ad976921e7aa3d9871c17afe16d541`

```mermaid
classDiagram
    class fn_chain_domain_and_problem {
      <<fn>>
    }
    class fn_find_plan_reports_bounded_not_exhausted_for_depth_cutoff {
      <<fn>>
    }
    class fn_shorter_chain_well_within_depth_bound_is_genuinely_found {
      <<fn>>
    }
    class fn_chain_domain_with_unreachable_predicate {
      <<fn>>
    }
    class fn_genuinely_unreachable_goal_within_depth_bound_still_reports_exhausted {
      <<fn>>
    }
```

## Dependencies

- `bcinr_mfw_ir::BoundKind`
- `bcinr_mfw_ir::PlannerOutcome`
- `bcinr_pddl::ground::GroundProblem`
- `bcinr_pddl::parse::{domain_from_pddl, problem_from_pddl}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
