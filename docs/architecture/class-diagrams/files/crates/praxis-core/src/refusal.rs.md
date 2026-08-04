# `crates/praxis-core/src/refusal.rs`

Source SHA-256: `5b44f130670649cd41f5e53a963b1b93b2a255682a7ce3a84f2c0a078508a6ee`

```mermaid
classDiagram
    class enum_RefusalCategory {
      <<enum>>
    }
    class enum_RefusalScenario {
      <<enum>>
    }
    class fn_scenario_for_denial_lane {
      <<fn>>
    }
    class fn_denial_lane {
      <<fn>>
    }
    class fn_compose_denials {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "From~"
    note "RefusalScenario"
    note "std::fmt::Display for RefusalCategory"
```

## Dependencies

- `bcinr_powl_receipt::denial::DenialPolarity`
- `crate::law::Obligation`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
