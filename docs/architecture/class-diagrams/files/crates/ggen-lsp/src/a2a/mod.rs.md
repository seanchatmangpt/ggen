# `crates/ggen-lsp/src/a2a/mod.rs`

Source SHA-256: `bd7ce15e4a5062426404ca8b2408245d8a60d135128655685b1529615272b97c`

```mermaid
classDiagram
    class fn_to_adapter_err {
      <<fn>>
    }
    class fn_dispatch_tool {
      <<fn>>
    }
    class fn_agent_card {
      <<fn>>
    }
    class struct_RepairRouteAdapter {
      <<struct>>
      +"initialized: bool"
    }
    note "Adapter for RepairRouteAdapter"
    note "RepairRouteAdapter"
```

## Dependencies

- `async_trait::async_trait`
- `crate::a2a_mcp::a2a_generated::adapter::{ Adapter, AdapterCapabilities, AdapterError, AdapterErrorType, }`
- `serde_json::{json, Value}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
