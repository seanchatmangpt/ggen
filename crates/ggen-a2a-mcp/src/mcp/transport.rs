//! STDIO transport for MCP JSON-RPC protocol

use crate::error::{A2aMcpError, A2aMcpResult};
use crate::mcp::protocol::McpResponse;
use std::io::{BufRead, BufReader, Write};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Clone, Default)]
struct ServerState {
    initialized: bool,
}

/// STDIO transport for MCP
pub struct StdioTransport {
    state: Arc<RwLock<ServerState>>,
}

impl StdioTransport {
    pub fn new() -> Self {
        Self {
            state: Arc::new(RwLock::new(ServerState::default())),
        }
    }

    /// Run the STDIO message loop (blocking)
    pub fn run(&mut self) -> A2aMcpResult<()> {
        let stdin = std::io::stdin();
        let stdout = std::io::stdout();
        let mut stdout_lock = stdout.lock();
        let reader = BufReader::new(stdin);

        for line_result in reader.lines() {
            let line = line_result.map_err(|e| A2aMcpError::Io(e))?;

            if line.trim().is_empty() {
                continue;
            }

            // Parse request (simplified - will be enhanced)
            let response = self.handle_request(&line)?;

            // Write response
            let response_json =
                serde_json::to_string(&response).map_err(|e| A2aMcpError::Json(e))?;
            writeln!(stdout_lock, "{}", response_json).map_err(|e| A2aMcpError::Io(e))?;
        }

        Ok(())
    }

    fn handle_request(&self, line: &str) -> A2aMcpResult<McpResponse> {
        // Parse JSON-RPC request
        let value: serde_json::Value =
            serde_json::from_str(line).map_err(|e| A2aMcpError::Json(e))?;

        let method = value.get("method").and_then(|m| m.as_str());

        match method {
            Some("initialize") => {
                let request_id = value.get("id").and_then(|i| i.as_i64()).unwrap_or(0);
                Ok(McpResponse::initialize_success(request_id))
            }
            Some("tools/list") => {
                let request_id = value.get("id").and_then(|i| i.as_i64()).unwrap_or(0);
                Ok(McpResponse::tools_list_success(request_id, vec![]))
            }
            Some("ping") => {
                let request_id = value.get("id").and_then(|i| i.as_i64()).unwrap_or(0);
                Ok(McpResponse::ping_success(request_id))
            }
            _ => {
                let request_id = value.get("id").and_then(|i| i.as_i64()).unwrap_or(0);
                Ok(McpResponse::error(request_id, -32601, "Method not found"))
            }
        }
    }
}
