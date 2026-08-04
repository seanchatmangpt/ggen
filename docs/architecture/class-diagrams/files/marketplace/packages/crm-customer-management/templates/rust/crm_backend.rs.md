# `marketplace/packages/crm-customer-management/templates/rust/crm_backend.rs`

Source SHA-256: `f4d4eb7bdf28a40dde8821f7a18c577184b1916223c8575e7ef852c3e31831bf`

```mermaid
classDiagram
    class enum_OpportunityStage {
      <<enum>>
    }
    class struct_Opportunity {
      <<struct>>
      +"id: String"
      +"name: String"
      +"account_id: String"
      +"amount: Decimal"
      +"stage: OpportunityStage"
      +"close_date: NaiveDate"
      +"owner_id: String"
      +"created_at: DateTime~Utc~"
    }
    class struct_Pipeline {
      <<struct>>
      +"opportunities: Vec~Opportunity~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Opportunity"
    note "OpportunityStage"
    note "Pipeline"
```

## Dependencies

- `chrono::{DateTime, Utc, NaiveDate}`
- `rust_decimal::Decimal`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
