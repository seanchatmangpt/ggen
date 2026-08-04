# `crates/chicago-tdd-tools/src/core/fail_fast.rs`

Source SHA-256: `0a46989bec743853eefa37972fe2edec5da71c195fd87ff666b130bff89b3114`

```mermaid
classDiagram
    class enum_PhaseResult {
      <<enum>>
    }
    class struct_StrictExecutionContext {
      <<struct>>
      +"contract_id: String"
      +"thermal_validator: ThermalValidator"
      +"effect_validator: Option~EffectValidator~"
      +"state_validator: Option~StateValidator~"
      +"receipt_validator: ReceiptValidator"
      +"phases_completed: Vec~PhaseLabel~"
      +"receipts: BTreeMap~String"
    }
    class enum_PhaseLabel {
      <<enum>>
    }
    class struct_ReceiptData {
      <<struct>>
      +"version: u32"
      +"checksum: u32"
      +"phase_label: PhaseLabel"
    }
    class fn_assert_invariant {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "PhaseResult"
    note "StrictExecutionContext"
```

## Dependencies

- `crate::core::invariants::{ ContractValidator, EffectValidator, InvariantResult, ReceiptValidator, StateValidator, ThermalValidator, UnrecoverableInvariantViolation, }`
- `std::collections::BTreeMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
