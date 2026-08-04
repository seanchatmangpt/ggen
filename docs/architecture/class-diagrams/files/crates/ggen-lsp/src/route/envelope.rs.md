# `crates/ggen-lsp/src/route/envelope.rs`

Source SHA-256: `f13ba314748496acba775f87a480f6ccddf151bfe8051638597afb260c6af6a7`

```mermaid
classDiagram
    class struct_RouteEnvelope {
      <<struct>>
      +"case_id: String"
      +"route_id: String"
      +"route_source: String"
      +"diagnostic_code: String"
      +"file: String"
      +"span: String"
      +"family: String"
      +"title: String"
      +"ordered_steps: Vec~RoutePlanStep~"
      +"receipt_head: Option~String~"
      +"compact_trace: CompactTraceView"
      +"target: DiagnosticRef"
    }
    class fn_route_case_id {
      <<fn>>
    }
    class struct_RouteRefusal {
      <<struct>>
      +"diagnostic_code: String"
      +"span: String"
      +"reason: String"
    }
    note "RouteEnvelope"
    note "RouteRefusal"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `super::compact::CompactTraceView`
- `super::plan::{DiagnosticRef, RoutePlan, RoutePlanStep}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
