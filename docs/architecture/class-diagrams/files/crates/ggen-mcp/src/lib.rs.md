# `crates/ggen-mcp/src/lib.rs`

Source SHA-256: `84838197d2bce092918e5de72c4e553c95fdf8a81e843a115b39e7abb083737d`

```mermaid
classDiagram
    class mod_error {
      <<mod>>
    }
    class mod_limits {
      <<mod>>
    }
    class mod_project_root {
      <<mod>>
    }
    class mod_tools {
      <<mod>>
    }
    class fn_make_tool {
      <<fn>>
    }
    class fn_read_only {
      <<fn>>
    }
    class fn_destructive {
      <<fn>>
    }
    class struct_GgenMcpServer {
      <<struct>>
      +"tools: Arc~Vec~Tool~~"
    }
    class fn_dispatch {
      <<fn>>
    }
    note "Default for GgenMcpServer"
    note "GgenMcpServer"
    note "ServerHandler for GgenMcpServer"
```

## Dependencies

- `rmcp::ServiceExt`
- `rmcp::{model::*, service::RequestContext, ErrorData as McpError, RoleServer, ServerHandler}`
- `std::sync::Arc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
