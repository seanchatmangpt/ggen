# `crates/ggen-lsp/src/intel/field.rs`

Source SHA-256: `3ace50cf570a170e87e7974424be9a8b77c11fc53c3db8ebb57080e4109ed9ea`

```mermaid
classDiagram
    class enum_FieldReadiness {
      <<enum>>
    }
    class struct_FieldStatus {
      <<struct>>
      +"event_count: usize"
      +"episode_count: usize"
      +"by_transport: BTreeMap~String"
      +"by_agent: BTreeMap~String"
      +"distinct_sessions: usize"
      +"distinct_variants: usize"
      +"variant_explosion: bool"
      +"conformance_rate: MetricValue"
      +"cycles: usize"
      +"verdict: String"
      +"readiness: FieldReadiness"
      +"reasons: Vec~String~"
    }
    class fn_episode_attr {
      <<fn>>
    }
    class fn_episode_variant {
      <<fn>>
    }
    class fn_field_status {
      <<fn>>
    }
```

## Dependencies

- `serde::Serialize`
- `std::collections::{BTreeMap, BTreeSet}`
- `super::log::IntelLog`
- `super::metrics::{closed, compute_metrics, group_episodes, Episode, MetricValue}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
