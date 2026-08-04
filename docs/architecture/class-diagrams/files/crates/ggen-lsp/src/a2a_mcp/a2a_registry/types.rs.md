# `crates/ggen-lsp/src/a2a_mcp/a2a_registry/types.rs`

Source SHA-256: `4004c4a637dc4522e492ea9eb506766f96e16f997eeff6af3027bbbf8ee0c339`

```mermaid
classDiagram
    class enum_HealthStatus {
      <<enum>>
    }
    class struct_AgentEntry {
      <<struct>>
      +"id: String"
      +"name: String"
      +"agent_type: String"
      +"endpoint_url: String"
      +"capabilities: Vec~String~"
      +"health: HealthStatus"
      +"registered_at: DateTime~Utc~"
      +"last_heartbeat: DateTime~Utc~"
    }
    class struct_Registration {
      <<struct>>
      +"agent_id: String"
      +"registered_at: DateTime~Utc~"
    }
    class struct_HealthConfig {
      <<struct>>
      +"check_interval: std::time::Duration"
      +"ping_timeout: std::time::Duration"
      +"offline_threshold: u32"
    }
    note "Default for HealthConfig"
    note "std::fmt::Display for HealthStatus"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
