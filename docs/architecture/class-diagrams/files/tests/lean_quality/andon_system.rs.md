# `tests/lean_quality/andon_system.rs`

Source SHA-256: `8f5b9b21ed1a6d11755a76a692f3923825fe2e86621bf8142572348f1c91ca42`

```mermaid
classDiagram
    class enum_Severity {
      <<enum>>
    }
    class struct_AndonSignal {
      <<struct>>
      +"severity: Severity"
      +"test_name: String"
      +"error_message: String"
      +"timestamp: SystemTime"
      +"remediation: String"
      +"category: FailureCategory"
    }
    class enum_FailureCategory {
      <<enum>>
    }
    class struct_TestHealthDashboard {
      <<struct>>
      +"total_tests: usize"
      +"passing: usize"
      +"failing: usize"
      +"flaky: usize"
      +"timeout: usize"
      +"fail_threshold: f32"
      +"flaky_threshold: f32"
      +"failures: Vec~AndonSignal~"
      +"test_history: HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    note "AndonSignal"
    note "TestHealthDashboard"
```

## Dependencies

- `serde::{Serialize, Deserialize}`
- `std::collections::HashMap`
- `std::time::{SystemTime, Duration}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
