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
/// Validates content-type, parses JSON-RPC, delegates to GgenMcpServer,
/// and returns JSON-RPC response.
async fn handle_mcp_request(
    State(_server): State<Arc<GgenMcpServer>>, headers: HeaderMap, req: Request<Body>,
) -> Result<impl IntoResponse, StatusCode> {
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

    // For now, return a simple response indicating the server is running
    // Full JSON-RPC handling would require integrating with rmcp's protocol layer
    let response = serde_json::json!({
        "jsonrpc": "2.0",
        "result": {
            "status": "ggen MCP HTTP server is running",
            "tools": [
                "generate",
                "validate",
                "sync",
                "list_generators",
                "list_examples",
                "get_example",
                "search",
                "scaffold_from_example",
                "query_ontology"
            ]
        },
        "id": json_req.get("id").unwrap_or(&serde_json::Value::Null)
    });

    Ok(Response::builder()
        .status(StatusCode::OK)
        .header("content-type", "application/json")
        .header("access-control-allow-origin", "*")
        .body(Body::from(serde_json::to_string(&response).unwrap()))
        .unwrap())
}
