# rmcp 1.3.0 — API Facts and Gotchas

Learned during pm4py-rust MCP integration session (2026-03-28).
These are compile-and-run verified facts, not documentation guesses.

---

## 1. `#[tool_handler]` is REQUIRED on `impl ServerHandler`

`#[tool_router]` only generates the `tool_router()` associated function on the struct impl block.
The default `ServerHandler` trait implementations return empty tool lists and -32601.
`#[tool_handler]` wires `self.tool_router` into `list_tools()` and `call_tool()` dispatch.

```rust
#[tool_router]
impl MyServer {
    #[tool(description = "does a thing")]
    async fn do_thing(&self, #[tool(aggr)] params: DoThingParams) -> String {
        format!("{:?}", params)
    }
}

// REQUIRED — without this, list_tools() → [], call_tool() → -32601
#[tool_handler]
impl ServerHandler for MyServer {
    fn get_info(&self) -> ServerInfo { /* ... */ }
}
```

**Symptom when missing:** `list_tools()` returns an empty vec; every `call_tool` returns
JSON-RPC error -32601 "Method not found".

---

## 2. Keep the server alive with `.waiting()`

`serve()` returns `RunningService<R, S>` which carries a `DropGuard`. If `RunningService`
is dropped, the `DropGuard` fires and the transport connection is cancelled immediately.

```rust
// WRONG — `_` drops RunningService on the next statement
let _ = server.serve(transport).await;

// CORRECT — block until client disconnects
if let Ok(svc) = server.serve(transport).await {
    let _ = svc.waiting().await;
}
```

`waiting()` is **consuming** (takes `mut self`). `cancel()` is also consuming.
Clone or extract anything you need from `RunningService` before calling either.

---

## 3. Client API surface

```rust
// Peer info (not server_info)
let info = svc.peer_info();     // Option<&R::PeerInfo>

// Clone before close — close takes &mut self
let info = svc.peer_info().cloned();
svc.close().await?;

// List tools — no arguments
let tools = svc.list_all_tools().await?;   // not list_all_tools(None)

// Close is non-consuming
svc.close().await?;   // &mut self

// Wait until done (consuming)
let _ = svc.waiting().await;   // mut self

// Cancel (consuming)
svc.cancel().await;            // mut self
```

---

## 4. `CallToolRequestParams` builder

```rust
use rmcp::model::CallToolRequestParams;
use serde_json::json;

let params = CallToolRequestParams::new("tool_name")
    .with_arguments(
        json!({"key": "value"})
            .as_object()
            .unwrap()
            .clone()
    );
```

Arguments must be a `Map<String, Value>` — use `.as_object().unwrap().clone()` to convert
a `serde_json::Value::Object` to the required type.

---

## 5. Reading tool call results

```rust
// Content = Annotated<RawContent>
// .raw field → RawContent
// .as_text() → Option<&RawTextContent>
// .text → String

let text = result.content[0].raw.as_text().unwrap().text.clone();
```

Do NOT use `.as_str()` on `Cow<'static, str>` — that method is unstable on `Cow`.
Use `.as_ref()` to get `&str`.

---

## 6. `Tool::name` is `Cow<'static, str>`

```rust
// Unstable on Cow — may not compile
let name: &str = tool.name.as_str();

// Correct
let name: &str = tool.name.as_ref();
```

---

## 7. Correct imports

```rust
// Server side
use rmcp::{
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{Implementation, ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router, ServerHandler,
};

// Client side
use rmcp::model::{CallToolRequestParams, ClientInfo};
use rmcp::service::RunningService;
use rmcp::{ClientHandler, RoleClient, ServiceExt};
```

---

## 8. Test helper — duplex transport

The canonical pattern for integration tests that exercise both client and server
without starting a real process:

```rust
use rmcp::{ClientHandler, RoleClient, ServiceExt, service::RunningService};
use rmcp::model::ClientInfo;
use tokio::io::duplex;

#[derive(Default)]
struct TestClient;

impl ClientHandler for TestClient {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn make_server_client() -> anyhow::Result<RunningService<RoleClient, TestClient>> {
    let (s_transport, c_transport) = duplex(65_536);
    tokio::spawn(async move {
        if let Ok(svc) = MyServer::new().serve(s_transport).await {
            let _ = svc.waiting().await;
        }
    });
    Ok(TestClient.serve(c_transport).await?)
}
```

Key points:
- Server is spawned into a Tokio task (non-blocking)
- Server uses `.waiting()` inside the spawn, not `let _ =`
- Client gets the other end of the duplex and also calls `.serve()`
- Return the client `RunningService` to the test for assertions

---

## 9. Cargo.toml dependency

```toml
[dependencies]
rmcp = { version = "0.1", features = ["server", "client", "transport-io", "macros"] }
# schemars is re-exported by rmcp; use rmcp::schemars, not a direct dep
```

---

## References

- crates.io: https://crates.io/crates/rmcp
- Source: https://github.com/modelcontextprotocol/rust-sdk
- Session file: `/Users/sac/chatmangpt/memory/weaver_automation_session_2026-03-28.md`
