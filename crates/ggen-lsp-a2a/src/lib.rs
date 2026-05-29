//! ggen-lsp-a2a — A2A bridge for the ggen route engine.
//!
//! Exposes the three `ggen-lsp-mcp` tools (`repair_route` / `replay_case` /
//! `metrics`) as an A2A [`Adapter`], so remote agents actuate the SAME route
//! engine that backs the editor (LSP) and non-editor (MCP) channels — with no
//! route drift and no dependency cycle.
//!
//! This is a LEAF crate: it depends on `ggen-lsp-mcp` and `ggen-a2a-mcp`; nothing
//! depends back. `ggen-a2a-mcp` never depends on `ggen-lsp` (the
//! `ggen-core → ggen-a2a-mcp` edge must stay acyclic), so the bridge lives here.

use std::collections::HashMap;

use async_trait::async_trait;
use ggen_a2a_mcp::a2a_generated::adapter::{
    Adapter, AdapterCapabilities, AdapterError, AdapterErrorType,
};
use serde_json::{json, Value};

/// The tools this bridge exposes — the agent card's capability set.
pub const TOOLS: &[&str] = &[
    "ggen.lsp.repair_route",
    "ggen.lsp.replay_case",
    "ggen.lsp.metrics",
];

fn to_adapter_err<E: std::fmt::Display>(e: E) -> AdapterError {
    AdapterError::new(e.to_string(), AdapterErrorType::ConversionFailed)
}

/// Dispatch an A2A route task `{ "tool": <name>, "arguments": { ... } }` to the
/// corresponding `ggen-lsp-mcp` tool. The result is byte-identical to calling the
/// MCP tool directly — one route engine, two transports.
///
/// # Errors
/// Returns `AdapterError` for an unknown tool or invalid arguments (the MCP
/// validation refusal is preserved).
pub fn dispatch_tool(tool: &str, arguments: &Value) -> Result<Value, AdapterError> {
    let args = arguments.as_object().cloned();
    match tool {
        "ggen.lsp.repair_route" => ggen_lsp_mcp::repair_route_result(args).map_err(to_adapter_err),
        "ggen.lsp.replay_case" => ggen_lsp_mcp::replay_case_result(args).map_err(to_adapter_err),
        "ggen.lsp.metrics" => ggen_lsp_mcp::metrics_result(args).map_err(to_adapter_err),
        other => Err(AdapterError::new(
            format!("unknown tool: {other}"),
            AdapterErrorType::UnsupportedFormat,
        )),
    }
}

/// The A2A agent card advertising this bridge's capabilities.
#[must_use]
pub fn agent_card() -> Value {
    json!({
        "name": "ggen-lsp-route",
        "version": "26.5.28",
        "description": "ggen route engine over A2A: repair routes, replay, and improvement metrics.",
        "capabilities": {
            "ggen.lsp.repair_route": "Return the canonical RouteEnvelope for a law-surface file's diagnostics.",
            "ggen.lsp.replay_case": "Reconstruct a recorded episode or verify the promotion binding.",
            "ggen.lsp.metrics": "Compute IMPROVE-1 metrics + earned verdict."
        }
    })
}

/// A2A adapter exposing the ggen route engine. `from_a2a` actuates: an A2A task is
/// executed against the route engine and its result returned.
#[derive(Debug, Default, Clone)]
pub struct RepairRouteAdapter {
    initialized: bool,
}

impl RepairRouteAdapter {
    #[must_use]
    pub fn new() -> Self {
        Self { initialized: false }
    }
}

#[async_trait]
impl Adapter for RepairRouteAdapter {
    fn name(&self) -> &str {
        "ggen-lsp-route"
    }

    fn version(&self) -> &str {
        "26.5.28"
    }

    async fn initialize(&mut self, _config: Value) -> Result<(), AdapterError> {
        self.initialized = true;
        Ok(())
    }

    fn can_handle(&self, format: &str) -> bool {
        TOOLS.contains(&format)
    }

    async fn to_a2a(&self, message: &Value) -> Result<Value, AdapterError> {
        // A tool result is already A2A-shaped JSON; pass through.
        Ok(message.clone())
    }

    async fn from_a2a(&self, message: &Value) -> Result<Value, AdapterError> {
        // An A2A task `{ tool, arguments }` → execute against the route engine.
        let tool = message
            .get("tool")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                AdapterError::new("missing 'tool'".to_string(), AdapterErrorType::ConversionFailed)
            })?;
        let arguments = message
            .get("arguments")
            .cloned()
            .unwrap_or_else(|| json!({}));
        let result = dispatch_tool(tool, &arguments)?;
        // Field-evidence gauge: a route request with a real root leaves attributed
        // a2a evidence so remote-agent field hours accumulate. dispatch_tool stays
        // pure (no capture); the side effect lives at this actuation boundary.
        if tool == "ggen.lsp.repair_route" {
            if let (Some(root), Some(file), Some(content)) = (
                arguments.get("root").and_then(Value::as_str),
                arguments.get("file_path").and_then(Value::as_str),
                arguments.get("file_content").and_then(Value::as_str),
            ) {
                let agent = arguments.get("agent_id").and_then(Value::as_str).unwrap_or("a2a");
                ggen_lsp::capture_request(
                    std::path::Path::new(root),
                    file,
                    content,
                    &ggen_lsp::Attribution::request(agent, "a2a"),
                );
            }
        }
        Ok(result)
    }

    fn capabilities(&self) -> AdapterCapabilities {
        let mut caps = HashMap::new();
        caps.insert("agent_card".to_string(), agent_card());
        AdapterCapabilities {
            supported_formats: TOOLS.iter().map(|s| (*s).to_string()).collect(),
            max_message_size: 1 << 20,
            supports_encryption: false,
            supports_compression: false,
            capabilities: caps,
        }
    }

    async fn shutdown(&mut self) -> Result<(), AdapterError> {
        self.initialized = false;
        Ok(())
    }
}
