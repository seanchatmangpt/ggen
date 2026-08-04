# `crates/ggen-mcp/src/tools/capability_status.rs`

Source SHA-256: `79cc4bbe5181f4716407198babf55356830ddcd2c2e867a959526ba92e43dc35`

```mermaid
classDiagram
    class struct_CapabilityStatusParams {
      <<struct>>
      +"root: String"
    }
    class struct_InertField {
      <<struct>>
      +"field: String"
      +"code: String"
      +"reason: String"
      +"tracked_at: String"
      +"used_by_rules: Vec~String~"
    }
    class struct_CapabilityStatusResult {
      <<struct>>
      +"ok: bool"
      +"project_is_affected: bool"
      +"inert_fields: Vec~InertField~"
    }
    class fn_capability_status {
      <<fn>>
    }
```

## Dependencies

- `crate::error::{ErrorCategory, McpError}`
- `crate::project_root::resolve_root`
- `schemars::JsonSchema`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
