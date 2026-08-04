# `tests/integration/andon_alerts.rs`

Source SHA-256: `3f72aae37cba3d8f088c0bf12bc6c86e38cd1e641a3dc23f9e4901c2b8ad1780`

```mermaid
classDiagram
    class enum_TestStatus {
      <<enum>>
    }
    class enum_FailureCategory {
      <<enum>>
    }
    class struct_TestResult {
      <<struct>>
      +"name: String"
      +"status: TestStatus"
      +"duration: Duration"
      +"failure_category: Option~FailureCategory~"
      +"error_message: Option~String~"
    }
    class enum_AndonLevel {
      <<enum>>
    }
    class struct_AndonAlert {
      <<struct>>
      +"test_results: Vec~TestResult~"
      +"level: AndonLevel"
      +"failure_count: usize"
      +"total_count: usize"
    }
    class struct_CicdIntegration {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "AndonAlert"
    note "AndonLevel"
    note "CicdIntegration"
    note "FailureCategory"
```

## Dependencies

- `std::collections::HashMap`
- `std::time::{Duration, Instant}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
