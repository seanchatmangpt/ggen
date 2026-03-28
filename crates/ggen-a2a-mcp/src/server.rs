//! MCP server entry points for ggen.
//!
//! Exposes ggen code generation capabilities as an MCP server
//! over stdio (for Claude Desktop / MCP clients) or HTTP.

use crate::error::A2aMcpError;
use crate::ggen_server::GgenMcpServer;

/// Run ggen as an MCP server over stdio transport.
///
/// This is the standard entry point for MCP clients like Claude Desktop.
/// Reads JSON-RPC messages from stdin, writes responses to stdout.
pub async fn serve_stdio() -> Result<(), A2aMcpError> {
    let server = GgenMcpServer::new();
    server
        .serve(rmcp::transport::stdio())
        .await
        .map_err(|e| A2aMcpError::Server(e.to_string()))
}

/// Run ggen as an MCP server over streamable HTTP transport.
///
/// Placeholder — HTTP transport support is planned for a future release.
pub async fn serve_http(_host: &str, _port: u16) -> Result<(), A2aMcpError> {
    Err(A2aMcpError::Server(
        "HTTP transport not yet implemented; use serve_stdio()".to_string(),
    ))
}
