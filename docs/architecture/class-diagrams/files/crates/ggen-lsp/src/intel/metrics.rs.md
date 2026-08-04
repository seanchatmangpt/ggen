# `crates/ggen-lsp/src/intel/metrics.rs`

Source SHA-256: `63a1408be761bcfbf5139e8b82f5eca36cce9b3a0228cbc41216ac438a5a6d11`

```mermaid
classDiagram
    class enum_MetricValue {
      <<enum>>
    }
    class fn_ser_insufficient {
      <<fn>>
    }
    class struct_ImproveMetrics {
      <<struct>>
      +"route_hit_rate: MetricValue"
      +"promoted_route_hit_rate: MetricValue"
      +"seed_displacement_rate: MetricValue"
      +"repair_cycle_time_secs: MetricValue"
      +"gate_pass_rate_seed: MetricValue"
      +"gate_pass_rate_mined: MetricValue"
      +"repeat_failure_rate: MetricValue"
      +"promotion_survival_rate: MetricValue"
      +"promotion_churn: MetricValue"
      +"receipt_density: MetricValue"
      +"cycles: usize"
      +"verdict: String"
    }
    class struct_Episode {
      <<struct>>
      +"code: String"
      +"events: Vec~OcelEvent~"
    }
    class fn_group_episodes {
      <<fn>>
    }
    class fn_has_activity {
      <<fn>>
    }
    class fn_route_source {
      <<fn>>
    }
    class fn_rate {
      <<fn>>
    }
    class fn_closed {
      <<fn>>
    }
    class fn_compute_metrics {
      <<fn>>
    }
    class fn_verdict {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::intel::IntelLog`
- `crate::intel::events::{ diagnostic_raised, gate_result, new_run_id, receipt_emitted, route_selected, }`
- `crate::intel::history::{RoutePromotionRecord, RouteStatus}`
- `ggen_graph::ocel::{OcelEvent, OcelLog}`
- `serde::Serialize`
- `std::collections::BTreeMap`
- `super::*`
- `super::events::{activity, obj_type}`
- `super::history::{PromotionHistory, RouteStatus}`
- `super::log::IntelLog`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
