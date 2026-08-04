# `crates/ggen-graph/src/ocel/ocel_types.rs`

Source SHA-256: `9a73aad5be5c7c1a5eb2fa56aef8b162844d3502165df97e84e1e59a0cc22936`

```mermaid
classDiagram
    class struct_OcelLog {
      <<struct>>
      +"objects: Vec~OcelObject~"
      +"events: Vec~OcelEvent~"
    }
    class struct_OcelObject {
      <<struct>>
      +"id: String"
      +"attributes: HashMap~String"
    }
    class struct_OcelEvent {
      <<struct>>
      +"id: String"
      +"activity: String"
      +"timestamp: chrono::DateTime~chrono::Utc~"
      +"objects: Vec~OcelObjectRef~"
      +"attributes: HashMap~String"
    }
    class struct_OcelObjectRef {
      <<struct>>
      +"id: String"
      +"qualifier: Option~String~"
    }
    class fn_to_compat_ocel {
      <<fn>>
    }
    note "OcelLog"
```

## Dependencies

- `chrono::FixedOffset`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::collections::HashSet`
- `wasm4pm_compat::ocel::{OCELEvent, OCELObject, OCELRelationship, OCELType, OCEL}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
