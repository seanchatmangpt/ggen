# `marketplace/packages/robo-advisor/src/rust/lib.rs`

Source SHA-256: `f8a59487e5c51aad931fb2637e53f085c18be71843379839bf6b78581e4f6b3b`

```mermaid
classDiagram
    class struct_Client {
      <<struct>>
      +"id: String"
      +"name: String"
      +"age: u32"
      +"risk_profile: RiskProfile"
    }
    class struct_RiskProfile {
      <<struct>>
      +"risk_score: f64"
      +"risk_category: String"
      +"time_horizon: u32"
    }
    class struct_Portfolio {
      <<struct>>
      +"id: String"
      +"client_id: String"
      +"total_value: f64"
      +"cash_balance: f64"
      +"allocations: HashMap~String"
    }
    class struct_AllocationEngine {
      <<struct>>
    }
    class struct_TaxOptimizer {
      <<struct>>
    }
    note "AllocationEngine"
    note "TaxOptimizer"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
