<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Developing an MCP Server](#developing-an-mcp-server)
  - [What You'll Build](#what-youll-build)
  - [Prerequisites](#prerequisites)
  - [Understanding MCP Servers](#understanding-mcp-servers)
  - [Step 1: Basic Server Structure](#step-1-basic-server-structure)
  - [Step 2: Implementing Tools](#step-2-implementing-tools)
  - [Step 3: Transport Layer](#step-3-transport-layer)
  - [Step 4: Advanced Features](#step-4-advanced-features)
  - [Step 5: Testing](#step-5-testing)
  - [Complete Example](#complete-example)
  - [Running the Example](#running-the-example)
  - [Testing Your Server](#testing-your-server)
  - [Common Patterns](#common-patterns)
  - [Troubleshooting](#troubleshooting)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC -->

# Developing an MCP Server

**Goal**: Build a production-ready MCP (Model Context Protocol) server in Rust from scratch.

**Time**: 45 minutes

---

## What You'll Build

You'll create a complete MCP server that:
- Handles JSON-RPC 2.0 communication
- Implements multiple tools with input validation
- Supports stdio and HTTP transports
- Includes resource management and prompts
- Handles errors gracefully
- Can be tested end-to-end

**You'll learn**:
- MCP server architecture and JSON-RPC 2.0 protocol
- How to implement tools, resources, and prompts
- Transport layer implementation (stdio, HTTP, SSE)
- Testing strategies for MCP servers
- Production best practices

---

## Prerequisites

- Rust 1.70 or newer
- Completed [Your First MCP Tool](./01-first-mcp-tool.md)
- Basic understanding of async Rust (tokio)
- Familiarity with JSON and JSON Schema

**Required dependencies** (add to `Cargo.toml`):

```toml
[dependencies]
tokio = { version = "1.47", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
async-trait = "0.1"
```

---

## Understanding MCP Servers

An MCP server is a JSON-RPC 2.0 server that exposes:
- **Tools**: Executable functions AI assistants can call
- **Resources**: Data sources that can be read
- **Prompts**: Template generators for messages

**Request-Response Flow**:

```
Client                    Server
  |                         |
  |----- initialize ------> |
  |<---- initialized ------ |
  |                         |
  |----- tools/list ------> |
  |<---- tool manifests ---- |
  |                         |
  |----- tools/call ------> |
  |<---- tool result ------- |
```

**Key concepts**:
- **JSON-RPC 2.0**: All communication uses JSON-RPC messages
- **Transport-agnostic**: Same protocol works over stdio, HTTP, WebSocket
- **Capability discovery**: Clients discover what's available via `initialize`

---

## Step 1: Basic Server Structure

Create a minimal MCP server that handles initialization:

```rust
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::io::{self, BufRead, Write};

/// JSON-RPC 2.0 request
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: Option<RequestId>,
    method: String,
    #[serde(default)]
    params: Option<JsonValue>,
}

/// JSON-RPC 2.0 response
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: Option<RequestId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error
#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
}

/// Request identifier
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
enum RequestId {
    Null,
    Number(u64),
    String(String),
}

/// Server capabilities announced during initialization
#[derive(Debug, Serialize)]
struct ServerCapabilities {
    tools: Option<ToolsCapability>,
    resources: Option<ResourcesCapability>,
    prompts: Option<PromptsCapability>,
}

#[derive(Debug, Serialize)]
struct ToolsCapability {}

#[derive(Debug, Serialize)]
struct ResourcesCapability {
    subscribe: bool,
    list_changed: bool,
}

#[derive(Debug, Serialize)]
struct PromptsCapability {}

/// Initialize request parameters
#[derive(Debug, Deserialize)]
struct InitializeParams {
    protocol_version: String,
    capabilities: ClientCapabilities,
    #[serde(default)]
    client_info: Option<ClientInfo>,
}

#[derive(Debug, Deserialize)]
struct ClientCapabilities {
    #[serde(default)]
    experimental: Option<JsonValue>,
    #[serde(default)]
    sampling: Option<JsonValue>,
}

#[derive(Debug, Deserialize)]
struct ClientInfo {
    name: String,
    version: String,
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let mut stdin = stdin.lock();

    // Read line from stdin
    let mut line = String::new();
    while stdin.read_line(&mut line)? > 0 {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Parse request
        if let Ok(request) = serde_json::from_str::<JsonRpcRequest>(line) {
            let response = handle_request(request);
            if let Ok(json) = serde_json::to_string(&response) {
                writeln!(stdout, "{}", json)?;
            }
        }

        line.clear();
    }

    Ok(())
}

fn handle_request(request: JsonRpcRequest) -> JsonRpcResponse {
    match request.method.as_str() {
        "initialize" => handle_initialize(request),
        "notifications/initialized" => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(JsonValue::Null),
            error: None,
        },
        _ => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32601,
                message: format!("Method not found: {}", request.method),
            }),
        },
    }
}

fn handle_initialize(request: JsonRpcRequest) -> JsonRpcResponse {
    let result = serde_json::json!({
        "protocolVersion": "2024-11-05",
        "capabilities": {
            "tools": {},
            "resources": {
                "subscribe": false,
                "listChanged": false
            },
            "prompts": {}
        },
        "serverInfo": {
            "name": "my-mcp-server",
            "version": "1.0.0"
        }
    });

    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(result),
        error: None,
    }
}
```

**Expected output** when testing:

```bash
$ echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05"}}' | cargo run --example mcp-server
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05","capabilities":{...}}}
```

---

## Step 2: Implementing Tools

Tools are the primary way AI assistants interact with your server. Each tool has:
- A unique name
- A description
- An input schema (JSON Schema)

**Add tool support to your server**:

```rust
/// Tool definition
#[derive(Debug, Clone, Serialize)]
struct Tool {
    name: String,
    description: String,
    input_schema: JsonValue,
}

/// Tool call parameters
#[derive(Debug, Deserialize)]
struct ToolCallParams {
    name: String,
    #[serde(default)]
    arguments: Option<JsonValue>,
}

/// Tool execution result
#[derive(Debug, Serialize)]
struct ToolResult {
    content: Vec<ContentItem>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_error: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
enum ContentItem {
    #[serde(rename = "text")]
    Text { text: String },
    #[serde(rename = "image")]
    Image { data: String, mime_type: String },
    #[serde(rename = "resource")]
    Resource { uri: String },
}

/// Available tools registry
fn get_tools() -> Vec<Tool> {
    vec![
        Tool {
            name: "calculate".to_string(),
            description: "Perform basic arithmetic calculations".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "expression": {
                        "type": "string",
                        "description": "Mathematical expression to evaluate (e.g., '2 + 2')"
                    }
                },
                "required": ["expression"]
            }),
        },
        Tool {
            name: "get_weather".to_string(),
            description: "Get current weather for a location".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "location": {
                        "type": "string",
                        "description": "City name or zip code"
                    },
                    "unit": {
                        "type": "string",
                        "enum": ["celsius", "fahrenheit"],
                        "description": "Temperature unit"
                    }
                },
                "required": ["location"]
            }),
        },
    ]
}

/// Execute a tool call
fn execute_tool(name: &str, arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    match name {
        "calculate" => execute_calculate(arguments),
        "get_weather" => execute_get_weather(arguments),
        _ => Err(format!("Unknown tool: {}", name)),
    }
}

fn execute_calculate(arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    let args = arguments.ok_or("Missing arguments")?;
    let expression = args["expression"]
        .as_str()
        .ok_or("Missing 'expression' parameter")?;

    // Simple expression evaluator (for demo purposes)
    let result = if expression.contains('+') {
        let parts: Vec<i32> = expression
            .split('+')
            .map(|s| s.trim().parse().unwrap_or(0))
            .collect();
        parts.iter().sum()
    } else if expression.contains('-') {
        let parts: Vec<&str> = expression.split('-').collect();
        if parts.len() == 2 {
            parts[0].trim().parse::<i32>().unwrap_or(0)
                - parts[1].trim().parse::<i32>().unwrap_or(0)
        } else {
            return Err("Invalid expression".to_string());
        }
    } else {
        return Err("Only + and - operations supported".to_string());
    };

    Ok(ToolResult {
        content: vec![ContentItem::Text {
            text: format!("{} = {}", expression, result),
        }],
        is_error: Some(false),
    })
}

fn execute_get_weather(arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    let args = arguments.ok_or("Missing arguments")?;
    let location = args["location"]
        .as_str()
        .ok_or("Missing 'location' parameter")?;

    // Mock weather data
    Ok(ToolResult {
        content: vec![ContentItem::Text {
            text: format!(
                "Weather for {}: 72°F, Partly cloudy\nHumidity: 45%\nWind: 10 mph",
                location
            ),
        }],
        is_error: Some(false),
    })
}

/// Handle tools/list request
fn handle_tools_list(request: JsonRpcRequest) -> JsonRpcResponse {
    let tools = get_tools();
    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::json!({ "tools": tools })),
        error: None,
    }
}

/// Handle tools/call request
fn handle_tools_call(request: JsonRpcRequest) -> JsonRpcResponse {
    let params = match request.params {
        Some(p) => p,
        None => {
            return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32602,
                    message: "Missing params".to_string(),
                }),
            }
        }
    };

    let call_params: ToolCallParams = match serde_json::from_value(params) {
        Ok(p) => p,
        Err(e) => {
            return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32602,
                    message: format!("Invalid params: {}", e),
                }),
            }
        }
    };

    match execute_tool(&call_params.name, call_params.arguments) {
        Ok(result) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        },
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::json!({
                "content": [{"type": "text", "text": err}],
                "isError": true
            })),
            error: None,
        },
    }
}
```

**Update `handle_request` to include tool methods**:

```rust
fn handle_request(request: JsonRpcRequest) -> JsonRpcResponse {
    match request.method.as_str() {
        "initialize" => handle_initialize(request),
        "notifications/initialized" => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(JsonValue::Null),
            error: None,
        },
        "tools/list" => handle_tools_list(request),
        "tools/call" => handle_tools_call(request),
        _ => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32601,
                message: format!("Method not found: {}", request.method),
            }),
        },
    }
}
```

**Expected output** for tool listing:

```bash
$ echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | cargo run --example mcp-server
{"jsonrpc":"2.0","id":1,"result":{"tools":[{"name":"calculate","description":"Perform basic arithmetic calculations","inputSchema":{...}},...]}}
```

**Expected output** for tool call:

```bash
$ echo '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"calculate","arguments":{"expression":"5 + 3"}}}' | cargo run --example mcp-server
{"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"5 + 3 = 8"}],"isError":false}}
```

---

## Step 3: Transport Layer

MCP is transport-agnostic. The same protocol works over:
- **stdio**: Standard input/output (default for Claude Desktop)
- **HTTP**: REST-like JSON-RPC over HTTP
- **SSE**: Server-Sent Events for real-time updates

### Stdio Transport (Already Implemented)

The stdio transport is the simplest and most common:

```rust
use std::io::{self, BufRead, Write};

fn run_stdio_server() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let mut stdin = stdin.lock();

    for line in stdin.lines() {
        let line = line?;
        if line.is_empty() {
            continue;
        }

        if let Ok(request) = serde_json::from_str::<JsonRpcRequest>(&line) {
            let response = handle_request(request);
            if let Ok(json) = serde_json::to_string(&response) {
                writeln!(stdout, "{}", json)?;
                stdout.flush()?;
            }
        }
    }

    Ok(())
}
```

### HTTP Transport

For HTTP, you need an async runtime and web server:

**Add dependencies**:

```toml
axum = "0.8"
tokio = { version = "1.47", features = ["full"] }
```

**HTTP server implementation**:

```rust
use axum::{
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Json},
    routing::post,
    Router,
};
use std::sync::Arc;
use tokio::net::TcpListener;

#[derive(Clone)]
struct AppState {
    // Add any shared state here
}

async fn handle_http_request(
    State(_state): State<Arc<AppState>>,
    Json(request): Json<JsonRpcRequest>,
) -> impl IntoResponse {
    let response = handle_request(request);
    Json(response).into_response()
}

pub async fn run_http_server(port: u16) -> anyhow::Result<()> {
    let state = Arc::new(AppState {});

    let app = Router::new()
        .route("/mcp", post(handle_http_request))
        .with_state(state);

    let listener = TcpListener::bind(format!("0.0.0.0:{}", port)).await?;
    println!("MCP HTTP server listening on http://0.0.0.0:{}", port);

    axum::serve(listener, app).await?;

    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 && args[1] == "--http" {
        let port = args.get(2).and_then(|p| p.parse().ok()).unwrap_or(3000);
        run_http_server(port).await?;
    } else {
        run_stdio_server()?;
    }

    Ok(())
}
```

**Test HTTP server**:

```bash
# Start HTTP server
cargo run --example mcp-server -- --http 3000

# In another terminal, test it
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

### SSE Transport (Server-Sent Events)

For real-time updates from server to client:

**Add dependencies**:

```toml
async-stream = "0.3"
headers = "0.4"
```

**SSE implementation**:

```rust
use async_stream::stream;
use axum::{
    response::{sse::Event, IntoResponse, Sse},
    Json,
};
use futures::stream::Stream;
use std::convert::Infallible;
use std::sync::atomic::{AtomicU64, Ordering};

async fn handle_sse(
    Json(request): Json<JsonRpcRequest>,
) -> impl IntoResponse {
    // Process the initial request
    let response = handle_request(request);

    // Create a stream of server-sent events
    let counter = Arc::new(AtomicU64::new(0));
    let stream = stream! {
        // Send the initial response
        yield Ok::<_, Infallible>(Event::default()
            .json_data(serde_json::to_value(&response).unwrap()));

        // Send periodic updates (example)
        loop {
            tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
            let count = counter.fetch_add(1, Ordering::Relaxed);
            let notification = serde_json::json!({
                "jsonrpc": "2.0",
                "method": "notifications/progress",
                "params": {
                    "progressToken": count,
                    "progress": 0.5
                }
            });
            yield Ok::<_, Infallible>(Event::default().json_data(notification));
        }
    };

    Sse::new(stream).keep_alive(
        axum::response::sse::KeepAlive::new()
            .interval(std::time::Duration::from_secs(1))
            .text("keepalive"),
    )
}
```

---

## Step 4: Advanced Features

### Resources (Data Access)

Resources represent data that clients can read:

```rust
/// Resource definition
#[derive(Debug, Clone, Serialize)]
struct Resource {
    uri: String,
    name: String,
    description: Option<String>,
    mime_type: Option<String>,
}

/// Resource content
#[derive(Debug, Serialize)]
struct ResourceContent {
    uri: String,
    mime_type: Option<String>,
    text: Option<String>,
    blob: Option<String>, // base64 encoded
}

fn list_resources() -> Vec<Resource> {
    vec![
        Resource {
            uri: "file:///config/settings.json".to_string(),
            name: "settings".to_string(),
            description: Some("Application settings".to_string()),
            mime_type: Some("application/json".to_string()),
        },
        Resource {
            uri: "file:///logs/latest.log".to_string(),
            name: "logs".to_string(),
            description: Some("Latest log entries".to_string()),
            mime_type: Some("text/plain".to_string()),
        },
    ]
}

fn read_resource(uri: &str) -> Result<ResourceContent, String> {
    match uri {
        "file:///config/settings.json" => Ok(ResourceContent {
            uri: uri.to_string(),
            mime_type: Some("application/json".to_string()),
            text: Some(serde_json::to_string_pretty(&serde_json::json!({
                "theme": "dark",
                "language": "en"
            })).unwrap()),
            blob: None,
        }),
        "file:///logs/latest.log" => Ok(ResourceContent {
            uri: uri.to_string(),
            mime_type: Some("text/plain".to_string()),
            text: Some("[INFO] Server started\n[INFO] Client connected".to_string()),
            blob: None,
        }),
        _ => Err(format!("Resource not found: {}", uri)),
    }
}

fn handle_resources_list(request: JsonRpcRequest) -> JsonRpcResponse {
    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::json!({ "resources": list_resources() })),
        error: None,
    }
}

fn handle_resources_read(request: JsonRpcRequest) -> JsonRpcResponse {
    let uri = request
        .params
        .and_then(|p| p.get("uri"))
        .and_then(|u| u.as_str())
        .unwrap_or("");

    match read_resource(uri) {
        Ok(content) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::to_value(content).unwrap()),
            error: None,
        },
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32602,
                message: err,
            }),
        },
    }
}
```

### Prompts (Template Generation)

Prompts generate templated messages:

```rust
/// Prompt definition
#[derive(Debug, Clone, Serialize)]
struct Prompt {
    name: String,
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    arguments: Option<Vec<PromptArgument>>,
}

#[derive(Debug, Clone, Serialize)]
struct PromptArgument {
    name: String,
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    required: Option<bool>,
}

/// Prompt message
#[derive(Debug, Serialize)]
struct PromptMessage {
    role: String,
    content: PromptContent,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
enum PromptContent {
    #[serde(rename = "text")]
    Text { text: String },
    #[serde(rename = "image")]
    Image { data: String, mime_type: String },
    #[serde(rename = "resource")]
    Resource { uri: String },
}

fn list_prompts() -> Vec<Prompt> {
    vec![
        Prompt {
            name: "code_review".to_string(),
            description: Some("Generate code review feedback".to_string()),
            arguments: Some(vec![
                PromptArgument {
                    name: "file".to_string(),
                    description: Some("File to review".to_string()),
                    required: Some(true),
                },
                PromptArgument {
                    name: "focus".to_string(),
                    description: Some("Review focus area".to_string()),
                    required: Some(false),
                },
            ]),
        },
    ]
}

fn get_prompt(name: &str, args: Option<JsonValue>) -> Result<Vec<PromptMessage>, String> {
    match name {
        "code_review" => {
            let file = args
                .as_ref()
                .and_then(|a| a.get("file"))
                .and_then(|f| f.as_str())
                .unwrap_or("unknown");
            let focus = args
                .as_ref()
                .and_then(|a| a.get("focus"))
                .and_then(|f| f.as_str())
                .unwrap_or("general");

            Ok(vec![
                PromptMessage {
                    role: "user".to_string(),
                    content: PromptContent::Text {
                        text: format!(
                            "Please review the file: {}\n\nFocus on: {}\n\nProvide feedback on:",
                            file, focus
                        ),
                    },
                },
                PromptMessage {
                    role: "user".to_string(),
                    content: PromptContent::Text {
                        text: "1. Code quality and style\n2. Potential bugs\n3. Performance issues\n4. Security concerns".to_string(),
                    },
                },
            ])
        }
        _ => Err(format!("Prompt not found: {}", name)),
    }
}

fn handle_prompts_list(request: JsonRpcRequest) -> JsonRpcResponse {
    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::json!({ "prompts": list_prompts() })),
        error: None,
    }
}

fn handle_prompts_get(request: JsonRpcRequest) -> JsonRpcResponse {
    let name = request
        .params
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .unwrap_or("");
    let args = request.params.and_then(|p| p.get("arguments").cloned());

    match get_prompt(name, args) {
        Ok(messages) => {
            let result = serde_json::json!({
                "description": list_prompts()
                    .iter()
                    .find(|p| p.name == name)
                    .and_then(|p| p.description.clone())
                    .unwrap_or_default(),
                "messages": messages
            });
            JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: Some(result),
                error: None,
            }
        }
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32602,
                message: err,
            }),
        },
    }
}
```

---

## Step 5: Testing

Write comprehensive tests for your MCP server:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn make_request(method: &str, params: Option<JsonValue>) -> JsonRpcRequest {
        JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(RequestId::Number(1)),
            method: method.to_string(),
            params,
        }
    }

    #[test]
    fn test_initialize() {
        let request = make_request("initialize", Some(serde_json::json!({
            "protocolVersion": "2024-11-05"
        })));
        let response = handle_initialize(request);

        assert!(response.result.is_some());
        assert!(response.error.is_none());

        let result = response.result.unwrap();
        assert_eq!(result["protocolVersion"], "2024-11-05");
        assert!(result["capabilities"].is_object());
    }

    #[test]
    fn test_tools_list() {
        let request = make_request("tools/list", None);
        let response = handle_tools_list(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let tools = result["tools"].as_array().unwrap();
        assert!(!tools.is_empty());
    }

    #[test]
    fn test_calculate_tool() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "calculate",
                "arguments": { "expression": "5 + 3" }
            })),
        );
        let response = handle_tools_call(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let content = result["content"].as_array().unwrap();
        assert!(content[0]["text"].as_str().unwrap().contains("8"));
    }

    #[test]
    fn test_unknown_tool() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "unknown_tool",
                "arguments": {}
            })),
        );
        let response = handle_tools_call(request);

        let result = response.result.unwrap();
        assert!(result["isError"].as_bool().unwrap());
    }

    #[test]
    fn test_invalid_expression() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "calculate",
                "arguments": { "expression": "invalid" }
            })),
        );
        let response = handle_tools_call(request);

        let result = response.result.unwrap();
        assert!(result["isError"].as_bool().unwrap());
    }

    #[test]
    fn test_resources_list() {
        let request = make_request("resources/list", None);
        let response = handle_resources_list(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let resources = result["resources"].as_array().unwrap();
        assert!(!resources.is_empty());
    }

    #[test]
    fn test_resources_read() {
        let request = make_request(
            "resources/read",
            Some(serde_json::json!({
                "uri": "file:///config/settings.json"
            })),
        );
        let response = handle_resources_read(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        assert!(result["text"].is_string());
    }

    #[test]
    fn test_prompts_list() {
        let request = make_request("prompts/list", None);
        let response = handle_prompts_list(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let prompts = result["prompts"].as_array().unwrap();
        assert!(!prompts.is_empty());
    }

    #[test]
    fn test_prompts_get() {
        let request = make_request(
            "prompts/get",
            Some(serde_json::json!({
                "name": "code_review",
                "arguments": { "file": "main.rs", "focus": "security" }
            })),
        );
        let response = handle_prompts_get(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let messages = result["messages"].as_array().unwrap();
        assert!(!messages.is_empty());
        assert!(messages[0]["content"]["text"].as_str().unwrap().contains("main.rs"));
    }

    #[test]
    fn test_unknown_method() {
        let request = make_request("unknown/method", None);
        let response = handle_request(request);

        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32601);
    }
}
```

---

## Complete Example

Here's the complete working MCP server. Save this as `examples/mcp-server.rs`:

```rust
//! Complete MCP Server Example
//!
//! A production-ready Model Context Protocol server demonstrating:
//! - JSON-RPC 2.0 communication
//! - Tools, resources, and prompts
//! - Stdio and HTTP transports
//! - Error handling and validation

use axum::{
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Json},
    routing::post,
    Router,
};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::io::{self, BufRead, Write};
use std::sync::Arc;
use tokio::net::TcpListener;

// ============================================================================
// JSON-RPC Types
// ============================================================================

/// JSON-RPC 2.0 request
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    #[serde(default)]
    id: Option<RequestId>,
    method: String,
    #[serde(default)]
    params: Option<JsonValue>,
}

/// JSON-RPC 2.0 response
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<RequestId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error
#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
}

/// Request identifier
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
enum RequestId {
    Null,
    Number(u64),
    String(String),
}

// ============================================================================
// MCP Types
// ============================================================================

/// Server capabilities
#[derive(Debug, Serialize)]
struct ServerCapabilities {
    tools: Option<ToolsCapability>,
    resources: Option<ResourcesCapability>,
    prompts: Option<PromptsCapability>,
}

#[derive(Debug, Serialize)]
struct ToolsCapability {}

#[derive(Debug, Serialize)]
struct ResourcesCapability {
    subscribe: bool,
    list_changed: bool,
}

#[derive(Debug, Serialize)]
struct PromptsCapability {}

/// Tool definition
#[derive(Debug, Clone, Serialize)]
struct Tool {
    name: String,
    description: String,
    input_schema: JsonValue,
}

/// Tool execution result
#[derive(Debug, Serialize)]
struct ToolResult {
    content: Vec<ContentItem>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_error: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
enum ContentItem {
    #[serde(rename = "text")]
    Text { text: String },
}

/// Resource definition
#[derive(Debug, Clone, Serialize)]
struct Resource {
    uri: String,
    name: String,
    description: Option<String>,
    mime_type: Option<String>,
}

/// Prompt definition
#[derive(Debug, Clone, Serialize)]
struct Prompt {
    name: String,
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    arguments: Option<Vec<PromptArgument>>,
}

#[derive(Debug, Clone, Serialize)]
struct PromptArgument {
    name: String,
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    required: Option<bool>,
}

/// Prompt message
#[derive(Debug, Serialize)]
struct PromptMessage {
    role: String,
    content: PromptContent,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
enum PromptContent {
    #[serde(rename = "text")]
    Text { text: String },
}

// ============================================================================
// Application State
// ============================================================================

#[derive(Clone)]
struct AppState {
    server_name: String,
    server_version: String,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            server_name: "mcp-server-example".to_string(),
            server_version: env!("CARGO_PKG_VERSION").to_string(),
        }
    }
}

// ============================================================================
// Request Handlers
// ============================================================================

fn handle_request(state: &AppState, request: JsonRpcRequest) -> JsonRpcResponse {
    match request.method.as_str() {
        "initialize" => handle_initialize(state, request),
        "notifications/initialized" => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(JsonValue::Null),
            error: None,
        },
        "tools/list" => handle_tools_list(request),
        "tools/call" => handle_tools_call(request),
        "resources/list" => handle_resources_list(request),
        "resources/read" => handle_resources_read(request),
        "prompts/list" => handle_prompts_list(request),
        "prompts/get" => handle_prompts_get(request),
        _ => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32601,
                message: format!("Method not found: {}", request.method),
            }),
        },
    }
}

fn handle_initialize(state: &AppState, request: JsonRpcRequest) -> JsonRpcResponse {
    let result = serde_json::json!({
        "protocolVersion": "2024-11-05",
        "capabilities": {
            "tools": {},
            "resources": {
                "subscribe": false,
                "listChanged": false
            },
            "prompts": {}
        },
        "serverInfo": {
            "name": state.server_name,
            "version": state.server_version
        }
    });

    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(result),
        error: None,
    }
}

fn handle_tools_list(request: JsonRpcRequest) -> JsonRpcResponse {
    let tools = get_available_tools();
    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::json!({ "tools": tools })),
        error: None,
    }
}

fn handle_tools_call(request: JsonRpcRequest) -> JsonRpcResponse {
    let params = match request.params {
        Some(p) => p,
        None => {
            return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32602,
                    message: "Missing params".to_string(),
                }),
            }
        }
    };

    let name = params.get("name")
        .and_then(|n| n.as_str())
        .unwrap_or("");
    let arguments = params.get("arguments").cloned();

    match execute_tool(name, arguments) {
        Ok(result) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        },
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::json!({
                "content": [{"type": "text", "text": err}],
                "isError": true
            })),
            error: None,
        },
    }
}

fn handle_resources_list(request: JsonRpcRequest) -> JsonRpcResponse {
    let resources = list_resources();
    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::json!({ "resources": resources })),
        error: None,
    }
}

fn handle_resources_read(request: JsonRpcRequest) -> JsonRpcResponse {
    let uri = request
        .params
        .and_then(|p| p.get("uri"))
        .and_then(|u| u.as_str())
        .unwrap_or("");

    match read_resource(uri) {
        Ok(content) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::to_value(content).unwrap()),
            error: None,
        },
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32602,
                message: err,
            }),
        },
    }
}

fn handle_prompts_list(request: JsonRpcRequest) -> JsonRpcResponse {
    let prompts = list_prompts();
    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::json!({ "prompts": prompts })),
        error: None,
    }
}

fn handle_prompts_get(request: JsonRpcRequest) -> JsonRpcResponse {
    let name = request
        .params
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .unwrap_or("");
    let args = request.params.and_then(|p| p.get("arguments").cloned());

    match get_prompt(name, args) {
        Ok(messages) => {
            let result = serde_json::json!({
                "messages": messages
            });
            JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: Some(result),
                error: None,
            }
        }
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(JsonRpcError {
                code: -32602,
                message: err,
            }),
        },
    }
}

// ============================================================================
// Tool Implementations
// ============================================================================

fn get_available_tools() -> Vec<Tool> {
    vec![
        Tool {
            name: "calculate".to_string(),
            description: "Perform basic arithmetic calculations".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "expression": {
                        "type": "string",
                        "description": "Mathematical expression to evaluate (e.g., '2 + 2')"
                    }
                },
                "required": ["expression"]
            }),
        },
        Tool {
            name: "get_weather".to_string(),
            description: "Get current weather for a location".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "location": {
                        "type": "string",
                        "description": "City name or zip code"
                    },
                    "unit": {
                        "type": "string",
                        "enum": ["celsius", "fahrenheit"],
                        "description": "Temperature unit"
                    }
                },
                "required": ["location"]
            }),
        },
        Tool {
            name: "echo".to_string(),
            description: "Echo back the input text".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "text": {
                        "type": "string",
                        "description": "Text to echo back"
                    }
                },
                "required": ["text"]
            }),
        },
    ]
}

fn execute_tool(name: &str, arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    match name {
        "calculate" => execute_calculate(arguments),
        "get_weather" => execute_get_weather(arguments),
        "echo" => execute_echo(arguments),
        _ => Err(format!("Unknown tool: {}", name)),
    }
}

fn execute_calculate(arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    let args = arguments.ok_or("Missing arguments")?;
    let expression = args["expression"]
        .as_str()
        .ok_or("Missing 'expression' parameter")?;

    // Simple expression evaluator for demo
    let result = if expression.contains('+') {
        let parts: Vec<i32> = expression
            .split('+')
            .map(|s| s.trim().parse().unwrap_or(0))
            .collect();
        parts.iter().sum::<i32>().to_string()
    } else if expression.contains('-') {
        let parts: Vec<&str> = expression.split('-').collect();
        if parts.len() == 2 {
            (parts[0].trim().parse::<i32>().unwrap_or(0)
                - parts[1].trim().parse::<i32>().unwrap_or(0))
            .to_string()
        } else {
            return Err("Invalid expression format".to_string());
        }
    } else if expression.contains('*') {
        let parts: Vec<i32> = expression
            .split('*')
            .map(|s| s.trim().parse().unwrap_or(0))
            .collect();
        parts.iter().product::<i32>().to_string()
    } else {
        expression.to_string()
    };

    Ok(ToolResult {
        content: vec![ContentItem::Text {
            text: format!("{} = {}", expression, result),
        }],
        is_error: Some(false),
    })
}

fn execute_get_weather(arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    let args = arguments.ok_or("Missing arguments")?;
    let location = args["location"]
        .as_str()
        .ok_or("Missing 'location' parameter")?;

    Ok(ToolResult {
        content: vec![ContentItem::Text {
            text: format!(
                "Weather for {}: 72°F, Partly cloudy\nHumidity: 45%\nWind: 10 mph",
                location
            ),
        }],
        is_error: Some(false),
    })
}

fn execute_echo(arguments: Option<JsonValue>) -> Result<ToolResult, String> {
    let args = arguments.ok_or("Missing arguments")?;
    let text = args["text"]
        .as_str()
        .ok_or("Missing 'text' parameter")?;

    Ok(ToolResult {
        content: vec![ContentItem::Text {
            text: text.to_string(),
        }],
        is_error: Some(false),
    })
}

// ============================================================================
// Resource Implementations
// ============================================================================

fn list_resources() -> Vec<Resource> {
    vec![
        Resource {
            uri: "file:///config/settings.json".to_string(),
            name: "settings".to_string(),
            description: Some("Application settings".to_string()),
            mime_type: Some("application/json".to_string()),
        },
        Resource {
            uri: "file:///logs/latest.log".to_string(),
            name: "logs".to_string(),
            description: Some("Latest log entries".to_string()),
            mime_type: Some("text/plain".to_string()),
        },
    ]
}

fn read_resource(uri: &str) -> Result<ResourceContent, String> {
    match uri {
        "file:///config/settings.json" => Ok(ResourceContent {
            uri: uri.to_string(),
            mime_type: Some("application/json".to_string()),
            text: Some(serde_json::to_string_pretty(&serde_json::json!({
                "theme": "dark",
                "language": "en"
            })).unwrap()),
        }),
        "file:///logs/latest.log" => Ok(ResourceContent {
            uri: uri.to_string(),
            mime_type: Some("text/plain".to_string()),
            text: Some("[INFO] Server started\n[INFO] Client connected".to_string()),
        }),
        _ => Err(format!("Resource not found: {}", uri)),
    }
}

#[derive(Debug, Serialize)]
struct ResourceContent {
    uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    mime_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    text: Option<String>,
}

// ============================================================================
// Prompt Implementations
// ============================================================================

fn list_prompts() -> Vec<Prompt> {
    vec![
        Prompt {
            name: "code_review".to_string(),
            description: Some("Generate code review feedback".to_string()),
            arguments: Some(vec![
                PromptArgument {
                    name: "file".to_string(),
                    description: Some("File to review".to_string()),
                    required: Some(true),
                },
                PromptArgument {
                    name: "focus".to_string(),
                    description: Some("Review focus area".to_string()),
                    required: Some(false),
                },
            ]),
        },
        Prompt {
            name: "summarize".to_string(),
            description: Some("Summarize text content".to_string()),
            arguments: Some(vec![
                PromptArgument {
                    name: "text".to_string(),
                    description: Some("Text to summarize".to_string()),
                    required: Some(true),
                },
                PromptArgument {
                    name: "max_length".to_string(),
                    description: Some("Maximum summary length".to_string()),
                    required: Some(false),
                },
            ]),
        },
    ]
}

fn get_prompt(name: &str, args: Option<JsonValue>) -> Result<Vec<PromptMessage>, String> {
    match name {
        "code_review" => {
            let file = args
                .as_ref()
                .and_then(|a| a.get("file"))
                .and_then(|f| f.as_str())
                .unwrap_or("unknown");
            let focus = args
                .as_ref()
                .and_then(|a| a.get("focus"))
                .and_then(|f| f.as_str())
                .unwrap_or("general");

            Ok(vec![
                PromptMessage {
                    role: "user".to_string(),
                    content: PromptContent::Text {
                        text: format!(
                            "Please review the file: {}\n\nFocus on: {}\n\nProvide feedback on:",
                            file, focus
                        ),
                    },
                },
                PromptMessage {
                    role: "user".to_string(),
                    content: PromptContent::Text {
                        text: "1. Code quality and style\n2. Potential bugs\n3. Performance issues\n4. Security concerns".to_string(),
                    },
                },
            ])
        }
        "summarize" => {
            let text = args
                .as_ref()
                .and_then(|a| a.get("text"))
                .and_then(|t| t.as_str())
                .unwrap_or("");

            Ok(vec![
                PromptMessage {
                    role: "user".to_string(),
                    content: PromptContent::Text {
                        text: format!("Please summarize the following text:\n\n{}", text),
                    },
                },
            ])
        }
        _ => Err(format!("Prompt not found: {}", name)),
    }
}

// ============================================================================
// Stdio Server
// ============================================================================

fn run_stdio_server(state: AppState) -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let mut stdin = stdin.lock();

    for line in stdin.lines() {
        let line = line?;
        if line.is_empty() {
            continue;
        }

        if let Ok(request) = serde_json::from_str::<JsonRpcRequest>(&line) {
            let response = handle_request(&state, request);
            if let Ok(json) = serde_json::to_string(&response) {
                writeln!(stdout, "{}", json)?;
                stdout.flush()?;
            }
        }
    }

    Ok(())
}

// ============================================================================
// HTTP Server
// ============================================================================

async fn handle_http_request(
    State(state): State<Arc<AppState>>,
    Json(request): Json<JsonRpcRequest>,
) -> impl IntoResponse {
    let response = handle_request(&state, request);
    (StatusCode::OK, Json(response)).into_response()
}

async fn run_http_server(state: AppState, port: u16) -> anyhow::Result<()> {
    let state = Arc::new(state);

    let app = Router::new()
        .route("/mcp", post(handle_http_request))
        .with_state(state);

    let listener = TcpListener::bind(format!("0.0.0.0:{}", port)).await?;
    println!("MCP HTTP server listening on http://0.0.0.0:{}", port);

    axum::serve(listener, app).await?;

    Ok(())
}

// ============================================================================
// Main Entry Point
// ============================================================================

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let state = AppState::default();

    if args.len() > 1 && args[1] == "--http" {
        let port = args.get(2).and_then(|p| p.parse().ok()).unwrap_or(3000);
        run_http_server(state, port).await?;
    } else {
        run_stdio_server(state)?;
    }

    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_request(method: &str, params: Option<JsonValue>) -> JsonRpcRequest {
        JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(RequestId::Number(1)),
            method: method.to_string(),
            params,
        }
    }

    fn default_state() -> AppState {
        AppState::default()
    }

    #[test]
    fn test_initialize() {
        let state = default_state();
        let request = make_request("initialize", Some(serde_json::json!({
            "protocolVersion": "2024-11-05"
        })));
        let response = handle_initialize(&state, request);

        assert!(response.result.is_some());
        assert!(response.error.is_none());

        let result = response.result.unwrap();
        assert_eq!(result["protocolVersion"], "2024-11-05");
        assert!(result["capabilities"]["tools"].is_object());
    }

    #[test]
    fn test_tools_list() {
        let request = make_request("tools/list", None);
        let response = handle_tools_list(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let tools = result["tools"].as_array().unwrap();
        assert_eq!(tools.len(), 3);

        let tool_names: Vec<&str> = tools
            .iter()
            .filter_map(|t| t["name"].as_str())
            .collect();
        assert!(tool_names.contains(&"calculate"));
        assert!(tool_names.contains(&"get_weather"));
        assert!(tool_names.contains(&"echo"));
    }

    #[test]
    fn test_calculate_tool() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "calculate",
                "arguments": { "expression": "5 + 3" }
            })),
        );
        let response = handle_tools_call(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        assert!(!result["isError"].as_bool().unwrap_or(true));

        let content = result["content"].as_array().unwrap();
        assert!(content[0]["text"].as_str().unwrap().contains("8"));
    }

    #[test]
    fn test_calculate_multiplication() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "calculate",
                "arguments": { "expression": "6 * 7" }
            })),
        );
        let response = handle_tools_call(request);

        let result = response.result.unwrap();
        let text = result["content"][0]["text"].as_str().unwrap();
        assert!(text.contains("42"));
    }

    #[test]
    fn test_echo_tool() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "echo",
                "arguments": { "text": "Hello, MCP!" }
            })),
        );
        let response = handle_tools_call(request);

        let result = response.result.unwrap();
        let text = result["content"][0]["text"].as_str().unwrap();
        assert_eq!(text, "Hello, MCP!");
    }

    #[test]
    fn test_unknown_tool() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "unknown_tool",
                "arguments": {}
            })),
        );
        let response = handle_tools_call(request);

        let result = response.result.unwrap();
        assert!(result["isError"].as_bool().unwrap());
    }

    #[test]
    fn test_resources_list() {
        let request = make_request("resources/list", None);
        let response = handle_resources_list(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let resources = result["resources"].as_array().unwrap();
        assert_eq!(resources.len(), 2);
    }

    #[test]
    fn test_resources_read() {
        let request = make_request(
            "resources/read",
            Some(serde_json::json!({
                "uri": "file:///config/settings.json"
            })),
        );
        let response = handle_resources_read(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        assert!(result["text"].is_string());
        assert!(result["text"].as_str().unwrap().contains("dark"));
    }

    #[test]
    fn test_resources_read_not_found() {
        let request = make_request(
            "resources/read",
            Some(serde_json::json!({
                "uri": "file:///unknown/file"
            })),
        );
        let response = handle_resources_read(request);

        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32602);
    }

    #[test]
    fn test_prompts_list() {
        let request = make_request("prompts/list", None);
        let response = handle_prompts_list(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let prompts = result["prompts"].as_array().unwrap();
        assert_eq!(prompts.len(), 2);
    }

    #[test]
    fn test_prompts_get() {
        let request = make_request(
            "prompts/get",
            Some(serde_json::json!({
                "name": "code_review",
                "arguments": { "file": "main.rs", "focus": "security" }
            })),
        );
        let response = handle_prompts_get(request);

        assert!(response.result.is_some());

        let result = response.result.unwrap();
        let messages = result["messages"].as_array().unwrap();
        assert!(!messages.is_empty());
        assert!(messages[0]["content"]["text"].as_str().unwrap().contains("main.rs"));
        assert!(messages[0]["content"]["text"].as_str().unwrap().contains("security"));
    }

    #[test]
    fn test_unknown_method() {
        let state = default_state();
        let request = make_request("unknown/method", None);
        let response = handle_request(&state, request);

        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32601);
    }

    #[test]
    fn test_full_flow() {
        let state = default_state();

        // 1. Initialize
        let init_req = make_request("initialize", Some(serde_json::json!({
            "protocolVersion": "2024-11-05"
        })));
        let init_resp = handle_request(&state, init_req);
        assert!(init_resp.result.is_some());

        // 2. List tools
        let list_req = make_request("tools/list", None);
        let list_resp = handle_request(&state, list_req);
        assert!(list_resp.result.is_some());

        // 3. Call tool
        let call_req = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "echo",
                "arguments": { "text": "test" }
            })),
        );
        let call_resp = handle_request(&state, call_req);
        assert!(call_resp.result.is_some());

        let result = call_resp.result.unwrap();
        assert_eq!(result["content"][0]["text"], "test");
    }
}
```

---

## Running the Example

**Build the example**:

```bash
cargo build --example mcp-server
```

**Run with stdio transport** (default):

```bash
cargo run --example mcp-server
```

**Run with HTTP transport**:

```bash
cargo run --example mcp-server -- --http 3000
```

**Test stdio server**:

```bash
# Initialize
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05"}}' | \
  cargo run --example mcp-server

# List tools
echo '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' | cargo run --example mcp-server

# Call tool
echo '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"calculate","arguments":{"expression":"5 + 3"}}}' | \
  cargo run --example mcp-server
```

**Test HTTP server**:

```bash
# Start HTTP server in one terminal
cargo run --example mcp-server -- --http 3000

# In another terminal, test it
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05"}}'

curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'

curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"echo","arguments":{"text":"Hello, HTTP!"}}}'
```

**Run tests**:

```bash
cargo test --example mcp-server
```

---

## Testing Your Server

### Unit Tests

```bash
# Run all tests
cargo test --example mcp-server

# Run specific test
cargo test --example mcp-server test_calculate_tool

# Run with output
cargo test --example mcp-server -- --nocapture
```

### Integration Tests

Create integration tests in `tests/mcp_server_test.rs`:

```rust
use std::process::{Command, Stdio};

#[test]
fn test_stdio_server_initialize() {
    let mut child = Command::new("cargo")
        .args(["run", "--example", "mcp-server"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to spawn server");

    let stdin = child.stdin.as_mut().expect("Failed to open stdin");
    writeln!(stdin, '{{"jsonrpc":"2.0","id":1,"method":"initialize","params":{{"protocolVersion":"2024-11-05"}}}}').unwrap();

    // ... verify output
}
```

---

## Common Patterns

### Pattern 1: Request Validation

```rust
fn validate_tool_call(params: Option<JsonValue>) -> Result<(String, Option<JsonValue>), JsonRpcError> {
    let params = params.ok_or(JsonRpcError {
        code: -32602,
        message: "Missing params".to_string(),
    })?;

    let name = params
        .get("name")
        .and_then(|n| n.as_str())
        .ok_or(JsonRpcError {
            code: -32602,
            message: "Missing tool name".to_string(),
        })?;

    let arguments = params.get("arguments").cloned();

    Ok((name.to_string(), arguments))
}
```

### Pattern 2: Error Handling

```rust
fn handle_request(state: &AppState, request: JsonRpcRequest) -> JsonRpcResponse {
    let handler = || -> Result<JsonValue, JsonRpcError> {
        match request.method.as_str() {
            "initialize" => Ok(handle_initialize(state, request)?.result.unwrap()),
            "tools/call" => Ok(handle_tools_call(request)?.result.unwrap()),
            _ => Err(JsonRpcError {
                code: -32601,
                message: format!("Method not found: {}", request.method),
            }),
        }
    };

    match handler() {
        Ok(result) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(result),
            error: None,
        },
        Err(err) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: None,
            error: Some(err),
        },
    }
}
```

### Pattern 3: Async Tool Execution

```rust
async fn execute_tool_async(name: &str, args: Option<JsonValue>) -> Result<ToolResult, String> {
    match name {
        "fetch_url" => {
            let url = args?.get("url")?.as_str().ok_or("Missing url")?;

            let response = reqwest::get(url).await
                .map_err(|e| format!("Request failed: {}", e))?;

            let text = response.text().await
                .map_err(|e| format!("Failed to read response: {}", e))?;

            Ok(ToolResult {
                content: vec![ContentItem::Text { text }],
                is_error: Some(false),
            })
        }
        _ => Err(format!("Unknown tool: {}", name)),
    }
}
```

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Server doesn't respond | Check JSON formatting - MCP requires strict JSON-RPC 2.0 format |
| Tools not found | Ensure `tools/list` returns correct tool names matching your calls |
| Invalid params | Verify input schema matches your JSON Schema definition |
| Stdio buffer issues | Flush stdout after each write: `stdout.flush()?` |
| HTTP CORS errors | Add CORS middleware if accessing from browser |

**Debug logging**:

```rust
fn handle_request(state: &AppState, request: JsonRpcRequest) -> JsonRpcResponse {
    eprintln!("DEBUG: Received request: {}", serde_json::to_string(&request).unwrap());
    let response = handle_request_impl(state, request);
    eprintln!("DEBUG: Sending response: {}", serde_json::to_string(&response).unwrap());
    response
}
```

---

## Next Steps

**Continue Your Journey**:

1. **[MCP & A2A Integration Guide](../../MCP_A2A_INTEGRATION.md)** - Advanced integration patterns
2. **[rig-mcp Library](../../../marketplace/packages/rig-mcp/)** - Production-ready MCP client/server library
3. **[Transport Options](../../explanations/fundamentals/mcp-protocol.md#transport-layer)** - Deep dive on transports
4. **[A2A Protocol](../../explanations/fundamentals/a2a-protocol.md)** - Agent-to-agent communication

**Advanced Topics**:
- **[Streaming Responses](../../how-to/mcp/configure-server.md)** - Progress tokens and streaming
- **[Authentication](../../how-to/mcp/setup-authentication.md)** - Security patterns
- **[Resource Subscriptions](../../how-to/mcp/configure-server.md)** - Real-time updates

**Join the community**:
- [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- [Examples Repository](https://github.com/seanchatmangpt/ggen/tree/master/examples)
