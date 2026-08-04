# `tests/chaos/injection/mod.rs`

Source SHA-256: `ae7d0d2b8682713f1c3835b2b579ecdcd0720690fea93dcd5e1c5e34a93098a5`

```mermaid
classDiagram
    class struct_ChaosConfig {
      <<struct>>
      +"enable_panics: bool"
      +"enable_network: bool"
      +"enable_clock_skew: bool"
      +"base_latency_ms: u64"
      +"max_jitter_ms: u64"
      +"failure_probability: f64"
    }
    class struct_ChaosEngine {
      <<struct>>
      +"config: ChaosConfig"
      +"event_store: Arc~tokio::sync::Mutex~Box~dyn EventStore~~~"
      +"active_failures: Arc~tokio::sync::Mutex~Vec~ActiveFailure~~~"
      +"is_active: Arc~AtomicBool~"
    }
    class struct_ActiveFailure {
      <<struct>>
      +"component: String"
      +"failure_type: String"
      +"start_time: u64"
      +"duration_ms: u64"
    }
    class struct_PanicInjection {
      <<struct>>
      +"component: String"
      +"message: String"
    }
    class mod_tests {
      <<mod>>
    }
    note "ChaosEngine"
    note "Default for ChaosConfig"
    note "PanicInjection"
```

## Dependencies

- `crate::chaos::event_store::{EventStore, FailureEvent, InMemoryEventStore}`
- `rand::Rng`
- `std::sync::Arc`
- `std::sync::atomic::{AtomicBool, Ordering}`
- `std::time::Duration`
- `super::*`
- `tokio::time::sleep`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
