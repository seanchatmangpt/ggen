# `crates/ggen-lsp/src/a2a_mcp/a2a_registry/health.rs`

Source SHA-256: `a0ba0e166887f3232a8d1a47eb2db523057c0bbd72df1ee7d191f11e40f6f01c`

```mermaid
classDiagram
    class struct_HealthMonitor {
      <<struct>>
      +"config: HealthConfig"
      +"store: Arc~dyn AgentStore~"
      +"running: Arc~AtomicBool~"
      +"cancel: Arc~Notify~"
      +"handle: tokio::sync::Mutex~Option~JoinHandle~()~~~"
      +"failure_counts: Arc~tokio::sync::RwLock~HashMap~String"
    }
    note "Drop for HealthMonitor"
    note "HealthMonitor"
```

## Dependencies

- `crate::a2a_mcp::a2a_registry::error::RegistryError`
- `crate::a2a_mcp::a2a_registry::store::AgentStore`
- `crate::a2a_mcp::a2a_registry::types::HealthConfig`
- `crate::a2a_mcp::a2a_registry::types::HealthStatus`
- `std::collections::HashMap`
- `std::sync::Arc`
- `std::sync::atomic::{AtomicBool, Ordering}`
- `std::time::Duration`
- `tokio::sync::Notify`
- `tokio::task::JoinHandle`
- `tokio::time::{interval, timeout}`
- `tracing::{debug, info, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
