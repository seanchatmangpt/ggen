//! Minimal MCP Server - Proof of Concept
//!
//! This is a standalone, zero-dependency MCP server that demonstrates
//! the protocol works correctly. It has 3 simple tools and requires
//! no external dependencies (no ggen-ai, no ggen-core).
//!
//! Usage:
//!   cargo build --example minimal_mcp_server --release
//!   claude mcp add minimal-ggen ./target/release/examples/minimal_mcp_server
//!
//! Then in Claude Code:
//!   "Use minimal-ggen to echo hello world"
//!   "Use minimal-ggen to add 5 and 7"
//!   "Use minimal-ggen to get server info"

use rmcp::{
    model::{
        CallToolRequestParam, CallToolResult, Content, InitializeRequestParam, InitializeResult,
        Implementation, ListToolsResult, PaginatedRequestParam, ProtocolVersion,
        ServerCapabilities, Tool, ToolsCapability,
    },
    service::{RequestContext, RoleServer},
    transport::stdio,
    ErrorData, ServerHandler, ServiceExt,
};
use serde_json::{json, Map, Value};
use std::{borrow::Cow, collections::HashMap, sync::Arc};

/// Minimal MCP Server with 3 proof-of-concept tools
#[derive(Debug, Clone)]
pub struct MinimalMcpServer {
    name: String,
    version: String,
    call_count: Arc<std::sync::atomic::AtomicU64>,
}

impl MinimalMcpServer {
    pub fn new() -> Self {
        Self {
            name: "minimal-ggen".to_string(),
            version: "0.1.0".to_string(),
            call_count: Arc::new(std::sync::atomic::AtomicU64::new(0)),
        }
    }

    /// Tool 1: Echo - Simply echoes back the input
    fn tool_echo(&self, params: Value) -> Result<Value, String> {
        let message = params
            .get("message")
            .and_then(|v| v.as_str())
            .unwrap_or("No message provided");

        Ok(json!({
            "echo": message,
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "call_number": self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1
        }))
    }

    /// Tool 2: Add - Adds two numbers
    fn tool_add(&self, params: Value) -> Result<Value, String> {
        let a = params
            .get("a")
            .and_then(|v| v.as_f64())
            .ok_or("Parameter 'a' is required and must be a number")?;

        let b = params
            .get("b")
            .and_then(|v| v.as_f64())
            .ok_or("Parameter 'b' is required and must be a number")?;

        let result = a + b;

        Ok(json!({
            "operation": "addition",
            "a": a,
            "b": b,
            "result": result,
            "call_number": self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1
        }))
    }

    /// Tool 3: Server Info - Returns information about this server
    fn tool_server_info(&self, _params: Value) -> Result<Value, String> {
        Ok(json!({
            "server_name": self.name,
            "version": self.version,
            "protocol": "Model Context Protocol (MCP)",
            "rmcp_version": "0.8.0",
            "total_calls": self.call_count.load(std::sync::atomic::Ordering::SeqCst),
            "tools": ["echo", "add", "server_info"],
            "status": "operational",
            "uptime_since": chrono::Utc::now().to_rfc3339()
        }))
    }

    /// Execute a tool by name
    fn execute_tool(&self, name: &str, params: Value) -> Result<Value, String> {
        eprintln!("[MINIMAL-MCP] Executing tool: {}", name);
        eprintln!("[MINIMAL-MCP] Parameters: {:?}", params);

        let result = match name {
            "echo" => self.tool_echo(params),
            "add" => self.tool_add(params),
            "server_info" => self.tool_server_info(params),
            _ => Err(format!("Unknown tool: {}", name)),
        };

        match &result {
            Ok(value) => eprintln!("[MINIMAL-MCP] Success: {:?}", value),
            Err(err) => eprintln!("[MINIMAL-MCP] Error: {}", err),
        }

        result
    }
}

impl Default for MinimalMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

impl ServerHandler for MinimalMcpServer {
    async fn initialize(
        &self,
        _params: InitializeRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<InitializeResult, ErrorData> {
        eprintln!("[MINIMAL-MCP] Server initializing...");

        Ok(InitializeResult {
            protocol_version: ProtocolVersion::default(),
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability::default()),
                ..Default::default()
            },
            server_info: Implementation {
                name: self.name.clone(),
                version: self.version.clone(),
                title: Some("Minimal MCP Server - Proof of Concept".to_string()),
                website_url: Some("https://github.com/seanchatmangpt/ggen".to_string()),
                icons: None,
            },
            instructions: Some(
                "This is a minimal MCP server demonstrating the protocol works correctly. \
                 It provides 3 simple tools: echo (echoes input), add (adds two numbers), \
                 and server_info (returns server status)."
                    .to_string(),
            ),
        })
    }

    async fn list_tools(
        &self,
        _pagination: Option<PaginatedRequestParam>,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<ListToolsResult, ErrorData> {
        eprintln!("[MINIMAL-MCP] Listing tools...");

        let tools = vec![
            Tool {
                name: Cow::Borrowed("echo"),
                description: Some(Cow::Borrowed(
                    "Echoes back the input message with a timestamp and call number",
                )),
                input_schema: Arc::new({
                    let mut schema = Map::new();
                    schema.insert("type".to_string(), json!("object"));
                    schema.insert(
                        "properties".to_string(),
                        json!({
                            "message": {
                                "type": "string",
                                "description": "The message to echo back"
                            }
                        }),
                    );
                    schema.insert("required".to_string(), json!(["message"]));
                    schema
                }),
                title: Some(Cow::Borrowed("Echo Tool")),
                output_schema: None,
                annotations: None,
                icons: None,
            },
            Tool {
                name: Cow::Borrowed("add"),
                description: Some(Cow::Borrowed("Adds two numbers together")),
                input_schema: Arc::new({
                    let mut schema = Map::new();
                    schema.insert("type".to_string(), json!("object"));
                    schema.insert(
                        "properties".to_string(),
                        json!({
                            "a": {
                                "type": "number",
                                "description": "First number"
                            },
                            "b": {
                                "type": "number",
                                "description": "Second number"
                            }
                        }),
                    );
                    schema.insert("required".to_string(), json!(["a", "b"]));
                    schema
                }),
                title: Some(Cow::Borrowed("Addition Tool")),
                output_schema: None,
                annotations: None,
                icons: None,
            },
            Tool {
                name: Cow::Borrowed("server_info"),
                description: Some(Cow::Borrowed(
                    "Returns information about the MCP server including version, status, and statistics",
                )),
                input_schema: Arc::new({
                    let mut schema = Map::new();
                    schema.insert("type".to_string(), json!("object"));
                    schema.insert("properties".to_string(), json!({}));
                    schema
                }),
                title: Some(Cow::Borrowed("Server Information")),
                output_schema: None,
                annotations: None,
                icons: None,
            },
        ];

        Ok(ListToolsResult {
            tools,
            next_cursor: None,
        })
    }

    async fn call_tool(
        &self,
        params: CallToolRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<CallToolResult, ErrorData> {
        let tool_name = params.name.to_string();
        eprintln!("[MINIMAL-MCP] Tool called: {}", tool_name);

        let args = Value::Object(params.arguments.unwrap_or_default());

        match self.execute_tool(&tool_name, args) {
            Ok(result) => {
                let response = serde_json::to_string_pretty(&result).map_err(|e| {
                    ErrorData::internal_error(
                        format!("Failed to serialize response: {}", e),
                        None,
                    )
                })?;

                Ok(CallToolResult {
                    content: vec![Content::text(response)],
                    is_error: Some(false),
                    meta: None,
                    structured_content: None,
                })
            }
            Err(error_msg) => Err(ErrorData::invalid_params(error_msg, None)),
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("========================================");
    eprintln!("  MINIMAL MCP SERVER - PROOF OF CONCEPT");
    eprintln!("========================================");
    eprintln!("Server: minimal-ggen v0.1.0");
    eprintln!("Protocol: Model Context Protocol (MCP)");
    eprintln!("RMCP SDK: v0.8.0");
    eprintln!("Transport: stdio");
    eprintln!("");
    eprintln!("Available Tools:");
    eprintln!("  1. echo         - Echo back input messages");
    eprintln!("  2. add          - Add two numbers");
    eprintln!("  3. server_info  - Get server status");
    eprintln!("");
    eprintln!("Waiting for MCP requests on stdin...");
    eprintln!("========================================");
    eprintln!("");

    let server = MinimalMcpServer::new();
    let service = server.serve(stdio()).await?;

    eprintln!("[MINIMAL-MCP] Initialization complete");
    eprintln!("[MINIMAL-MCP] Server is ready to accept tool calls");

    service.waiting().await?;

    eprintln!("[MINIMAL-MCP] Server shutting down");
    Ok(())
}
