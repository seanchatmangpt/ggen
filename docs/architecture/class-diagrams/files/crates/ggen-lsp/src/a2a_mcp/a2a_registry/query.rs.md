# `crates/ggen-lsp/src/a2a_mcp/a2a_registry/query.rs`

Source SHA-256: `fda00be996aae96092c67607bf8bad47d5f993f88a1fa8399049af98d74c49dc`

```mermaid
classDiagram
    class struct_AgentQuery {
      <<struct>>
      +"agent_type: Option~String~"
      +"capability: Option~String~"
      +"health: Option~HealthStatus~"
      +"limit: usize"
    }
    class fn_matches_query {
      <<fn>>
    }
    note "AgentQuery"
```

## Dependencies

- `crate::a2a_mcp::a2a_registry::types::HealthStatus`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
