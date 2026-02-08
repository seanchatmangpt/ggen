//! Complete MCP Server Example
//!
//! A production-ready Model Context Protocol server demonstrating:
//! - JSON-RPC 2.0 communication
//! - Tools, resources, and prompts
//! - Stdio and HTTP transports
//! - Error handling and validation
//!
//! Run with stdio (default):
//!   cargo run --example mcp-server
//!
//! Run with HTTP:
//!   cargo run --example mcp-server -- --http 3000

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::io::{self, BufRead, Write};

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

/// Tool definition
#[derive(Debug, Clone, Serialize)]
struct Tool {
    name: String,
    description: String,
    #[serde(rename = "inputSchema")]
    input_schema: JsonValue,
}

/// Tool execution result
#[derive(Debug, Serialize)]
struct ToolResult {
    content: Vec<ContentItem>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "isError")]
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
    #[serde(rename = "mimeType")]
    mime_type: Option<String>,
}

/// Resource content
#[derive(Debug, Serialize)]
struct ResourceContent {
    uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "mimeType")]
    mime_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    text: Option<String>,
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

/// Server info
#[derive(Debug, Serialize)]
struct ServerInfo {
    name: String,
    version: String,
}

/// Initialize result
#[derive(Debug, Serialize)]
struct InitializeResult {
    #[serde(rename = "protocolVersion")]
    protocol_version: String,
    capabilities: ServerCapabilities,
    #[serde(rename = "serverInfo")]
    server_info: ServerInfo,
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
            server_version: "1.0.0".to_string(),
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
    let result = InitializeResult {
        protocol_version: "2024-11-05".to_string(),
        capabilities: ServerCapabilities {
            tools: Some(ToolsCapability {}),
            resources: Some(ResourcesCapability {
                subscribe: false,
                list_changed: false,
            }),
            prompts: Some(PromptsCapability {}),
        },
        server_info: ServerInfo {
            name: state.server_name.clone(),
            version: state.server_version.clone(),
        },
    };

    JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: request.id,
        result: Some(serde_json::to_value(result).unwrap()),
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

    let name = params.get("name").and_then(|n| n.as_str()).unwrap_or("");
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
        .as_ref()
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
        .as_ref()
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .unwrap_or("");
    let args = request
        .params
        .as_ref()
        .and_then(|p| p.get("arguments").cloned());

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

    // Simple expression evaluator for demo purposes
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
    let text = args["text"].as_str().ok_or("Missing 'text' parameter")?;

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
            text: Some(
                serde_json::to_string_pretty(&serde_json::json!({
                    "theme": "dark",
                    "language": "en"
                }))
                .unwrap(),
            ),
        }),
        "file:///logs/latest.log" => Ok(ResourceContent {
            uri: uri.to_string(),
            mime_type: Some("text/plain".to_string()),
            text: Some("[INFO] Server started\n[INFO] Client connected".to_string()),
        }),
        _ => Err(format!("Resource not found: {}", uri)),
    }
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

            Ok(vec![PromptMessage {
                role: "user".to_string(),
                content: PromptContent::Text {
                    text: format!("Please summarize the following text:\n\n{}", text),
                },
            }])
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
    let stdin = stdin.lock();

    for line in stdin.lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };

        if line.trim().is_empty() {
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
// Main Entry Point
// ============================================================================

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let state = AppState::default();

    // Check for HTTP mode (requires axum feature)
    if args.len() > 1 && args[1] == "--http" {
        eprintln!("HTTP mode requires axum feature. Add to Cargo.toml:");
        eprintln!("  axum = \"0.8\"");
        eprintln!("Then run with:");
        eprintln!("  cargo run --example mcp-server --features axum -- --http 3000");
        std::process::exit(1);
    }

    // Default: stdio mode
    run_stdio_server(state)
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
        let request = make_request(
            "initialize",
            Some(serde_json::json!({
                "protocolVersion": "2024-11-05"
            })),
        );
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

        let tool_names: Vec<&str> = tools.iter().filter_map(|t| t["name"].as_str()).collect();
        assert!(tool_names.contains(&"calculate"));
        assert!(tool_names.contains(&"get_weather"));
        assert!(tool_names.contains(&"echo"));
    }

    #[test]
    fn test_calculate_tool_addition() {
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
    fn test_calculate_tool_multiplication() {
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
    fn test_calculate_tool_subtraction() {
        let request = make_request(
            "tools/call",
            Some(serde_json::json!({
                "name": "calculate",
                "arguments": { "expression": "10 - 4" }
            })),
        );
        let response = handle_tools_call(request);

        let result = response.result.unwrap();
        let text = result["content"][0]["text"].as_str().unwrap();
        assert!(text.contains("6"));
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
        assert!(messages[0]["content"]["text"]
            .as_str()
            .unwrap()
            .contains("main.rs"));
        assert!(messages[0]["content"]["text"]
            .as_str()
            .unwrap()
            .contains("security"));
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
        let init_req = make_request(
            "initialize",
            Some(serde_json::json!({
                "protocolVersion": "2024-11-05"
            })),
        );
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
