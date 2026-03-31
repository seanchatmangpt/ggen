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

## 9. Resources, Prompts, and Completions

Override these methods in `impl ServerHandler` (they have no-op defaults):

```rust
async fn list_resources(
    &self,
    request: Option<PaginatedRequestParams>,
    _ctx: RequestContext<RoleServer>,
) -> Result<ListResourcesResult, McpError> {
    // Build Vec<Resource> = Vec<Annotated<RawResource>>
    let resources = vec![
        RawResource::new("ggen://example/foo", "foo")
            .with_description("Example foo")
            .no_annotation(),
    ];
    Ok(ListResourcesResult::with_all_items(resources))
}

async fn read_resource(
    &self,
    request: ReadResourceRequestParams,  // .uri: String
    _ctx: RequestContext<RoleServer>,
) -> Result<ReadResourceResult, McpError> {
    let content = std::fs::read_to_string(&request.uri).unwrap_or_default();
    Ok(ReadResourceResult::new(vec![
        ResourceContents::text(content, &request.uri),
    ]))
}

async fn list_prompts(
    &self, _: Option<PaginatedRequestParams>, _: RequestContext<RoleServer>,
) -> Result<ListPromptsResult, McpError> {
    Ok(ListPromptsResult::with_all_items(vec![
        Prompt::new("my-prompt", Some("Description"), Some(vec![
            PromptArgument::new("arg1").with_required(true),
        ])),
    ]))
}

async fn get_prompt(
    &self,
    request: GetPromptRequestParams,  // .name, .arguments: Option<JsonObject>
    _: RequestContext<RoleServer>,
) -> Result<GetPromptResult, McpError> {
    Ok(GetPromptResult::new(vec![
        PromptMessage::new_text(PromptMessageRole::User, "Your prompt text here"),
    ]))
}

async fn complete(
    &self,
    request: CompleteRequestParams,  // .r#ref: Reference, .argument: ArgumentInfo
    _: RequestContext<RoleServer>,
) -> Result<CompleteResult, McpError> {
    let values = vec!["option-a".to_string(), "option-b".to_string()];
    let info = CompletionInfo::with_all_values(values).unwrap_or_default();
    Ok(CompleteResult::new(info))
}
```

**Enable in `get_info()`:**
```rust
let capabilities = ServerCapabilities::builder()
    .enable_tools()
    .enable_resources()    // no subscriptions
    .enable_prompts()      // no list_changed
    .enable_completions()
    .build();
```

**Key types:**
- `Resource = Annotated<RawResource>` — construct via `RawResource::new(uri, name).no_annotation()`
- `ResourceContents::text(text, uri)` — creates `TextResourceContents`
- `ListResourcesResult::with_all_items(vec)` — no pagination cursor
- `Prompt::new(name, description, arguments)` — all `Option`
- `PromptArgument::new(name).with_required(true)`
- `GetPromptResult::new(messages)` — `.with_description(...)` optional
- `PromptMessage::new_text(role, text)` — role is `PromptMessageRole::User` or `::Assistant`
- `CompletionInfo::with_all_values(values)` → `Result<CompletionInfo, String>`; max 100 values
- `CompleteResult::new(completion_info)`
- `Reference::for_prompt(name)` / `Reference::for_resource(uri)` for `CompleteRequestParams`
- `ArgumentInfo { name: String, value: String }` — current argument being completed

**Cursor-based pagination for resources:**
```rust
let start = request.and_then(|r| r.cursor.as_deref())
    .and_then(|c| c.parse::<usize>().ok()).unwrap_or(0);
let page: Vec<Resource> = all.iter().skip(start).take(PAGE_SIZE).cloned().collect();
let next_cursor = (start + PAGE_SIZE < all.len())
    .then(|| Cursor::from((start + PAGE_SIZE).to_string()));
Ok(ListResourcesResult { meta: None, next_cursor, resources: page })
```

---

## 10. Reading content from tool results (test side)

```rust
// RawContent::Text is a tuple variant, not struct
if let rmcp::model::RawContent::Text(tc) = &result.content[0].raw {
    println!("{}", tc.text);
}
```

---

## 11. Cargo.toml dependency

```toml
[dependencies]
rmcp = { version = "1.3.0", features = ["server", "macros", "transport-io"] }
schemars = "1.0"   # required for #[derive(schemars::JsonSchema)] on params

[dev-dependencies]
rmcp = { version = "1.3.0", features = ["server", "client", "macros", "transport-io"] }
```

---

## References

- crates.io: https://crates.io/crates/rmcp
- Source: https://github.com/modelcontextprotocol/rust-sdk
- Session file: `/Users/sac/chatmangpt/memory/weaver_automation_session_2026-03-28.md`
