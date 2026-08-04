# `crates/ggen-lsp/src/route/compact.rs`

Source SHA-256: `02067b11e8aeae038157f3ed52b6f094a03b443fe089523bc0c57a62c54a0340`

```mermaid
classDiagram
    class struct_CompactEventRow {
      <<struct>>
      +"index: u32"
      +"activity: String"
      +"dt: i64"
      +"objects: Vec~String~"
    }
    class struct_CompactPowlView {
      <<struct>>
      +"steps: Vec~String~"
    }
    class struct_CompactTraceView {
      <<struct>>
      +"case_id: String"
      +"family: String"
      +"diagnostic_code: String"
      +"file: String"
      +"route_id: Option~String~"
      +"provenance: Option~String~"
      +"status: String"
      +"events: Vec~CompactEventRow~"
      +"dfg: Vec~(String"
      +"powl: Option~CompactPowlView~"
      +"receipt_head: Option~String~"
    }
    class mod_tests {
      <<mod>>
    }
    note "CompactTraceView"
```

## Dependencies

- `crate::route::model::{ EditTemplate, PartialOrder, Provenance, RepairFamily, RepairRoute, RepairStep, RouteBindings, RouteId, StepId, }`
- `crate::route::{edit::route_plan, plan::DiagnosticRef}`
- `lsp_max::lsp_types::{Position, Range}`
- `serde::{Deserialize, Serialize}`
- `super::*`
- `super::model::Provenance`
- `super::plan::RoutePlan`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
