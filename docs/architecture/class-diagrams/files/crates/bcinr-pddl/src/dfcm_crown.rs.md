# `crates/bcinr-pddl/src/dfcm_crown.rs`

Source SHA-256: `4da215dc794c75fddcae5337e4ee89607695cb2aa8afa011d38209f0f791a708`

```mermaid
classDiagram
    class struct_DfcmBenchReceipt {
      <<struct>>
      +"wall_clock_ms: u128"
      +"topology_ns: u128"
      +"planning_ns: u128"
      +"analysis_ns: u128"
      +"admission_ns: u128"
      +"receipt_ns: u128"
      +"replay_ns: u128"
      +"max_ops: u8"
      +"max_parallelism: u8"
      +"suite_passed_5s_gate: bool"
      +"cold_topology_once_ns: u128"
      +"warm_plan_ns: u128"
      +"warm_analysis_ns: u128"
      +"warm_admission_ns: u128"
      +"warm_receipt_ns: u128"
      +"warm_replay_existing_receipt_ns: u128"
      +"alloc_count_by_stage: Option~AllocStageStats~"
      +"bytes_allocated_by_stage: Option~AllocStageStats~"
      +"admission_substage: SubstageNs"
      +"replay_substage: SubstageNs"
      +"analysis_substage: AnalysisSubstageNs"
    }
    class struct_AllocStageStats {
      <<struct>>
      +"topology: u64"
      +"planning: u64"
      +"analysis: u64"
      +"admission: u64"
      +"receipt: u64"
      +"replay: u64"
    }
    class fn_problem_text {
      <<fn>>
    }
    class fn_run_dfcm_crown_suite {
      <<fn>>
    }
    class fn_add_analysis_substage {
      <<fn>>
    }
    class fn_record_stage_alloc {
      <<fn>>
    }
    class fn_add_substage {
      <<fn>>
    }
```

## Dependencies

- `crate::execute::{compute_plan_chain, execute_temporal_plan_instrumented, SubstageNs}`
- `crate::ground::GroundTemporalProblem`
- `crate::powl_bridge::temporal_plan_to_powl_tape`
- `crate::schedule_analysis::{analyze_schedule_instrumented, AnalysisSubstageNs}`
- `crate::{domain_from_pddl, problem_from_pddl, Pddl8Error}`
- `std::time::Instant`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
