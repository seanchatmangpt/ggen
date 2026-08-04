# `crates/ggen-lsp/src/route/plan.rs`

Source SHA-256: `6a6bad9393cebea2e6f64991645424835b9ed5e4e2fec24c2b49a1adac6e9147`

```mermaid
classDiagram
    class struct_RoutePlanRef {
      <<struct>>
      +"v: u16"
      +"route_id: RouteId"
      +"family: RepairFamily"
      +"bindings: RouteBindings"
    }
    class struct_DiagnosticRef {
      <<struct>>
      +"code: String"
      +"message: String"
      +"range: Range"
      +"severity: String"
    }
    class struct_RoutePlanStep {
      <<struct>>
      +"title: String"
      +"edit: Option~TextEdit~"
    }
    class struct_RoutePlan {
      <<struct>>
      +"route_id: RouteId"
      +"family: RepairFamily"
      +"title: String"
      +"provenance: Provenance"
      +"ordered_steps: Vec~RoutePlanStep~"
      +"target: DiagnosticRef"
    }
    note "DiagnosticRef"
    note "RoutePlanRef"
```

## Dependencies

- `lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Range, TextEdit}`
- `serde::{Deserialize, Serialize}`
- `super::model::{Provenance, RepairFamily, RouteBindings, RouteId}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
