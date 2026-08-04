# `crates/ggen-graph/src/ocel/lifecycle.rs`

Source SHA-256: `69d726caaa56f00fecec6a4fe3b8ceb3b8d6dd9177d1a0035d687ff15435cfd6`

```mermaid
classDiagram
    class fn_check_lifecycle_order {
      <<fn>>
    }
    class fn_check_guard {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `chrono::{TimeZone, Utc}`
- `crate::GraphError`
- `crate::graph::DeterministicGraph`
- `crate::ocel::{EvidenceProjector, OcelEvent, OcelLog, OcelObjectRef}`
- `oxigraph::sparql::QueryResults`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
