# `crates/ggen-lsp/src/a2a_mcp/mcp_server.rs`

Source SHA-256: `38a7e2067c433733ccd54a55eb379b0e696af884ee2dbae369955e4390f9e932`

```mermaid
classDiagram
    class struct_HelloParams {
      <<struct>>
      +"name: String"
    }
    class struct_ConstructParams {
      <<struct>>
      +"task_id: String"
      +"jtbd: String"
      +"avatar: String"
    }
    class struct_GgenMcpServer {
      <<struct>>
      +"tool_router: rmcp::handler::server::router::tool::ToolRouter~Self~"
    }
    note "Default for GgenMcpServer"
    note "GgenMcpServer"
    note "ServerHandler for GgenMcpServer"
```

## Dependencies

- `rmcp::handler::server::wrapper::Parameters`
- `rmcp::model::{CallToolResult, Content, Implementation, InitializeResult, ServerCapabilities}`
- `rmcp::{tool, tool_handler, tool_router, ServerHandler, ServiceExt}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
