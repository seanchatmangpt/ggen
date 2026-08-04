# `crates/chicago-tdd-tools/src/operator_registry.rs`

Source SHA-256: `f397370e4535e1e12aec338509008920861a2f0460aa41851f4454af5968a8db`

```mermaid
classDiagram
    class struct_OperatorDescriptor {
      <<struct>>
      +"hook_id: String"
      +"pattern_number: u32"
      +"pattern_name: String"
      +"pattern_category: String"
      +"properties: OperatorProperties"
      +"max_latency_ns: i64"
      +"required_guards: Vec~GuardType~"
      +"slo: Option~String~"
    }
    class struct_OperatorProperties {
      <<struct>>
      +"deterministic: bool"
      +"idempotent: bool"
      +"type_preserving: bool"
      +"bounded: bool"
    }
    class enum_GuardType {
      <<enum>>
    }
    class struct_OperatorRegistry {
      <<struct>>
      +"operators: HashMap~String"
    }
    class fn_global_registry {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for OperatorRegistry"
    note "OperatorDescriptor"
    note "OperatorRegistry"
    note "std::fmt::Display for GuardType"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::sync::OnceLock`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
