# `tests/integration/fmea_risk_assessment.rs`

Source SHA-256: `8b8820958b26ba1088de21cf85024ba9b6e720da95ef116138ff2c03523c3d74`

```mermaid
classDiagram
    class struct_FailureMode {
      <<struct>>
      +"error_code: &'static str"
      +"description: &'static str"
      +"severity: u8"
      +"occurrence: u8"
      +"detection: u8"
    }
    class enum_RiskLevel {
      <<enum>>
    }
    class struct_FmeaAssessment {
      <<struct>>
      +"failure_modes: Vec~FailureMode~"
      +"total_rpn: u32"
    }
    class struct_FixEffectiveness {
      <<struct>>
      +"before_rpn: u32"
      +"after_rpn: u32"
      +"reduction_percent: f64"
    }
    class mod_tests {
      <<mod>>
    }
    note "FailureMode"
    note "FixEffectiveness"
    note "FmeaAssessment"
    note "RiskLevel"
```

## Dependencies

- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
