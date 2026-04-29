//! MCP server entry points for ggen.
//!
//! Exposes ggen code generation capabilities as an MCP server
//! over stdio (for Claude Desktop / MCP clients) or HTTP.

use crate::error::A2aMcpError;
use crate::ggen_server::GgenMcpServer;
use axum::{
    body::Body,
    extract::{Request, State},
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
    routing::post,
    Router,
};
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;
use tracing::info;

/// Run ggen as an MCP server over stdio transport.
///
/// This is the standard entry point for MCP clients like Claude Desktop.
/// Reads JSON-RPC messages from stdin, writes responses to stdout.
pub async fn serve_stdio() -> Result<(), A2aMcpError> {
    let server = GgenMcpServer::new();
    server.serve(rmcp::transport::stdio()).await.map_err(|e| {
        let error_span = tracing::error_span!(
            "ggen.error",
            error.type = "server",
            error.message = format!("stdio serve error: {}", e),
        );
        let _guard = error_span.enter();
        A2aMcpError::Server(e.to_string())
    })
}

/// Run ggen as an MCP server over HTTP transport.
///
/// Starts an axum HTTP server that accepts JSON-RPC requests via POST.
pub async fn serve_http(host: &str, port: u16) -> Result<(), A2aMcpError> {
    let server = Arc::new(GgenMcpServer::new());

    // Build router with CORS and trace layers
    let app = Router::new()
        .route("/", post(handle_mcp_request))
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any),
        )
        .layer(TraceLayer::new_for_http())
        .with_state(server);

    let addr = format!("{}:{}", host, port);
    let listener = tokio::net::TcpListener::bind(&addr).await.map_err(|e| {
        let error_span = tracing::error_span!(
            "ggen.error",
            error.type = "server",
            error.message = format!("Failed to bind to {}: {}", addr, e),
        );
        let _guard = error_span.enter();
        A2aMcpError::Server(format!("Failed to bind to {}: {}", addr, e))
    })?;

    info!("ggen MCP HTTP server listening on {}", addr);

    axum::serve(listener, app).await.map_err(|e| {
        let error_span = tracing::error_span!(
            "ggen.error",
            error.type = "server",
            error.message = format!("HTTP server error: {}", e),
        );
        let _guard = error_span.enter();
        A2aMcpError::Server(format!("HTTP server error: {}", e))
    })
}

/// Handle incoming MCP JSON-RPC requests via HTTP.
///
/// Validates content-type, parses JSON-RPC, dispatches to tool handlers,
/// and returns JSON-RPC response.
async fn handle_mcp_request(
    State(_server): State<Arc<GgenMcpServer>>, headers: HeaderMap, req: Request<Body>,
) -> Result<impl IntoResponse, StatusCode> {
    // Note: ToolRouter import removed — MCP dispatch handled directly below

    // Validate content-type
    let content_type = headers
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    if !content_type.contains("application/json") {
        return Ok((
            StatusCode::BAD_REQUEST,
            "Invalid content-type: expected application/json",
        )
            .into_response());
    }

    // Read request body
    let bytes = match axum::body::to_bytes(req.into_body(), 10 * 1024 * 1024).await {
        Ok(b) => b,
        Err(e) => {
            return Ok((
                StatusCode::BAD_REQUEST,
                format!("Failed to read request body: {}", e),
            )
                .into_response());
        }
    };

    // Parse JSON-RPC request
    let json_req: serde_json::Value = match serde_json::from_slice(&bytes) {
        Ok(v) => v,
        Err(e) => {
            return Ok((StatusCode::BAD_REQUEST, format!("Invalid JSON: {}", e)).into_response());
        }
    };

    // Extract request fields
    let method = json_req
        .get("method")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let _params = json_req
        .get("params")
        .cloned()
        .unwrap_or(serde_json::json!({}));
    let id = json_req
        .get("id")
        .cloned()
        .unwrap_or(serde_json::Value::Null);

    // Dispatch to tool handler via rmcp's tool router
    let response = match method {
        "list_tools" => {
            // List all available tools
            let tool_definitions = vec![
                serde_json::json!({
                    "name": "generate",
                    "description": "Generate code from a RDF ontology file via the ggen μ₁-μ₅ pipeline",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "ontology_path": { "type": "string" },
                            "queries_dir": { "type": "string" },
                            "output_dir": { "type": "string" },
                            "language": { "type": "string" }
                        },
                        "required": ["ontology_path"]
                    }
                }),
                serde_json::json!({
                    "name": "validate",
                    "description": "Validate a Turtle (.ttl) ontology content string",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "ttl": { "type": "string" }
                        },
                        "required": ["ttl"]
                    }
                }),
                serde_json::json!({
                    "name": "sync",
                    "description": "Run the full ggen sync pipeline",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "ontology_path": { "type": "string" },
                            "queries_dir": { "type": "string" },
                            "output_dir": { "type": "string" },
                            "language": { "type": "string" },
                            "dry_run": { "type": "boolean" }
                        },
                        "required": ["ontology_path"]
                    }
                }),
                serde_json::json!({
                    "name": "create_task",
                    "description": "Create a new task with title and description",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "title": { "type": "string" },
                            "description": { "type": "string" }
                        },
                        "required": ["title"]
                    }
                }),
                serde_json::json!({
                    "name": "update_task_state",
                    "description": "Update task state with state transition",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "task_id": { "type": "string" },
                            "new_state": { "type": "string" },
                            "reason": { "type": "string" }
                        },
                        "required": ["task_id", "new_state"]
                    }
                }),
                serde_json::json!({
                    "name": "list_tasks",
                    "description": "List all tasks, optionally filtered by state",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "state_filter": { "type": "string" }
                        }
                    }
                }),
            ];
            serde_json::json!({
                "jsonrpc": "2.0",
                "result": {
                    "tools": tool_definitions
                },
                "id": id
            })
        }
        "create_task" => {
            // Extract and validate params
            match serde_json::from_value::<crate::ggen_server::CreateTaskParams>(_params.clone()) {
                Ok(_params) => {
                    // Delegate to ggen_server HTTP handler (direct implementation)
                    _server.http_create_task(&_params).await
                }
                Err(e) => {
                    serde_json::json!({
                        "jsonrpc": "2.0",
                        "error": {
                            "code": -32602,
                            "message": format!("Invalid params: {}", e)
                        },
                        "id": id
                    })
                }
            }
        }
        "update_task_state" => {
            // Extract and validate params
            match serde_json::from_value::<crate::ggen_server::UpdateTaskStateParams>(
                _params.clone(),
            ) {
                Ok(params) => {
                    // Delegate to ggen_server HTTP handler (direct implementation)
                    _server.http_update_task_state(&params).await
                }
                Err(e) => {
                    serde_json::json!({
                        "jsonrpc": "2.0",
                        "error": {
                            "code": -32602,
                            "message": format!("Invalid params: {}", e)
                        },
                        "id": id
                    })
                }
            }
        }
        "list_tasks" => {
            // Extract and validate params
            match serde_json::from_value::<crate::ggen_server::ListTasksParams>(_params.clone()) {
                Ok(params) => {
                    // Delegate to ggen_server HTTP handler (direct implementation)
                    _server.http_list_tasks(&params).await
                }
                Err(e) => {
                    serde_json::json!({
                        "jsonrpc": "2.0",
                        "error": {
                            "code": -32602,
                            "message": format!("Invalid params: {}", e)
                        },
                        "id": id
                    })
                }
            }
        }
        _ => {
            // Unknown method
            serde_json::json!({
                "jsonrpc": "2.0",
                "error": {
                    "code": -32601,
                    "message": format!("Method not found: {}", method)
                },
                "id": id
            })
        }
    };

    Ok(Response::builder()
        .status(StatusCode::OK)
        .header("content-type", "application/json")
        .header("access-control-allow-origin", "*")
        .body(Body::from(serde_json::to_string(&response).unwrap()))
        .unwrap())
}
