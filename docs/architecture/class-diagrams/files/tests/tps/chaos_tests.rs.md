# `tests/tps/chaos_tests.rs`

Source SHA-256: `c3a71b5a04b904ab548bd392be88302156e435c8bf8432a864ee1f5da2d7e5d1`

```mermaid
classDiagram
    class struct_ChaosNetworkSystem {
      <<struct>>
      +"delay_ms: u64"
    }
    class struct_ChaosPacketLossSystem {
      <<struct>>
      +"loss_rate: f64"
    }
    class struct_ChaosUnavailabilitySystem {
      <<struct>>
      +"available: Arc~RwLock~bool~~"
    }
    class struct_ChaosMemoryPressureSystem {
      <<struct>>
      +"memory: Arc~RwLock~usize~~"
      +"max_mb: usize"
    }
    class struct_ChaosComponentSystem {
      <<struct>>
      +"failed: Arc~RwLock~Vec~String~~~"
    }
    class struct_ChaosCircuitBreakerSystem {
      <<struct>>
      +"failures: Arc~RwLock~usize~~"
      +"failure_threshold: usize"
      +"state: Arc~RwLock~String~~"
    }
    class struct_ChaosConcurrentFailureSystem {
      <<struct>>
      +"health: Arc~RwLock~bool~~"
    }
    class struct_ChaosDataCorruptionSystem {
      <<struct>>
      +"data: Arc~RwLock~Vec~u8~~~"
      +"corrupted: Arc~RwLock~bool~~"
      +"andon: Arc~RwLock~Vec~String~~~"
    }
    class struct_ChaosTimeoutSystem {
      <<struct>>
      +"timeout_ms: u64"
    }
    class struct_ChaosOverloadSystem {
      <<struct>>
    }
    note "ChaosCircuitBreakerSystem"
    note "ChaosComponentSystem"
    note "ChaosConcurrentFailureSystem"
    note "ChaosDataCorruptionSystem"
    note "ChaosMemoryPressureSystem"
    note "ChaosNetworkSystem"
    note "ChaosOverloadSystem"
    note "ChaosPacketLossSystem"
    note "ChaosTimeoutSystem"
    note "ChaosUnavailabilitySystem"
```

## Dependencies

- `futures`
- `rand`
- `std::sync::Arc`
- `std::time::{Duration, Instant}`
- `tokio::sync::RwLock`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
