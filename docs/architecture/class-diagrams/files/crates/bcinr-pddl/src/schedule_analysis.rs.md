# `crates/bcinr-pddl/src/schedule_analysis.rs`

Source SHA-256: `db39be9c4eff75f60fea02a774dd5bd2db9e28461dd0367841e8a53201022a38`

```mermaid
classDiagram
    class struct_AnalysisSubstageNs {
      <<struct>>
      +"resource_key_collect_ns: u128"
      +"base_plan_ns: u128"
      +"perturb_minus_ns: u128"
      +"perturb_plus_ns: u128"
      +"sensitivity_compute_ns: u128"
      +"result_build_ns: u128"
    }
    class struct_CapacityDelta {
      <<struct>>
      +"minus_one_makespan: Option~f64~"
      +"baseline_makespan: f64"
      +"plus_one_makespan: Option~f64~"
    }
    class struct_ScheduleAnalysis64 {
      <<struct>>
      +"makespan: f64"
      +"critical_path_mask: u64"
      +"max_parallelism: u8"
      +"binding_resource_mask: u64"
      +"slack_by_op: [f64; 64]"
      +"op_count: usize"
      +"capacity_delta: Option~CapacityDelta~"
    }
    class fn_analyze_schedule {
      <<fn>>
    }
    class fn_analyze_schedule_instrumented {
      <<fn>>
    }
    class fn_forward_pass {
      <<fn>>
    }
    class fn_backward_pass {
      <<fn>>
    }
    class fn_max_parallelism {
      <<fn>>
    }
    class fn_replan_with_perturbed_capacity {
      <<fn>>
    }
```

## Dependencies

- `crate::Pddl8Error`
- `crate::ground::GroundTemporalProblem`
- `crate::powl_bridge::{temporal_plan_to_powl_tape, PowlOpSpec}`
- `std::time::Instant`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
