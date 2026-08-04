# `tests/tps/mod.rs`

Source SHA-256: `ea0e9a7a118ab84175f36639ac0210d259b7e0a57cac7f6fc0c4409aba5e5f19`

```mermaid
classDiagram
    class mod_end_to_end_workflow_tests {
      <<mod>>
    }
    class mod_smoke_tests {
      <<mod>>
    }
    class mod_regression_tests {
      <<mod>>
    }
    class mod_performance_regression_tests {
      <<mod>>
    }
    class mod_chaos_tests {
      <<mod>>
    }
    class mod_test_data_generation {
      <<mod>>
    }
    class fn_increment_test_count {
      <<fn>>
    }
    class fn_get_test_count {
      <<fn>>
    }
    class struct_TestEnvironment {
      <<struct>>
      +"enable_logging: bool"
      +"enable_metrics: bool"
      +"enable_tracing: bool"
      +"timeout_secs: u64"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for TestEnvironment"
    note "TestEnvironment"
```

## Dependencies

- `std::sync::Arc`
- `std::sync::atomic::{AtomicUsize, Ordering}`
- `super::*`
- `test_data_generation::{ AllTestDataProvider, CrossPrincipleScenario, CrossPrincipleScenarioBuilder, DeploymentTestCase, DeploymentTestDataBuilder, FailureScenario, FailureScenarioBuilder, LoadDataPoint, LoadProfileBuilder, PaymentTestCase, PaymentTestDataBuilder, QueueWorkloadBuilder, WorkItem, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
