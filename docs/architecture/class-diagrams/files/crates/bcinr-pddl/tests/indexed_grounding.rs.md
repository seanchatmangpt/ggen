# `crates/bcinr-pddl/tests/indexed_grounding.rs`

Source SHA-256: `0e23243c2406f60211d76512f5e649f69d2153809161fd76bbf06e8489865d10`

```mermaid
classDiagram
    class fn_test_differential_grounding {
      <<fn>>
    }
    class fn_chain_domain_and_problem {
      <<fn>>
    }
    class fn_indexed_find_plan_reports_bounded_not_exhausted_for_depth_cutoff {
      <<fn>>
    }
    class fn_indexed_genuinely_unreachable_goal_within_depth_bound_still_reports_exhausted {
      <<fn>>
    }
```

## Dependencies

- `bcinr_mfw_ir::BoundKind`
- `bcinr_pddl::ground::lazy::IndexedGroundProblem`
- `bcinr_pddl::{domain_from_pddl, problem_from_pddl, GroundProblem}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
