use crate::{
    protocol::{JsonRpcRequest, JsonRpcResponse},
    tools::ToolRegistry,
};
use serde_json::json;

pub struct McpServer {
    name: String,
    version: String,
    tools: ToolRegistry,
}

impl McpServer {
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
            tools: ToolRegistry::new(),
        }
    }

    pub fn handle(&self, req: JsonRpcRequest) -> JsonRpcResponse {
        match req.method.as_str() {
            "initialize" => JsonRpcResponse::ok(
                req.id,
                json!({
                    "protocolVersion": "2024-11-05",
                    "serverInfo": { "name": self.name, "version": self.version },
                    "capabilities": { "tools": {} }
                }),
            ),
            "tools/list" => JsonRpcResponse::ok(req.id, self.tools.list()),
            "tools/call" => {
                let params = req.params.unwrap_or(json!({}));
                let name = params.get("name").and_then(|v| v.as_str()).unwrap_or("");
                let args = params.get("arguments").cloned().unwrap_or(json!({}));
                match self.tools.call(name, args) {
                    Ok(result) => JsonRpcResponse::ok(
                        req.id,
                        json!({ "content": [{ "type": "text", "text": result.to_string() }] }),
                    ),
                    Err(e) => JsonRpcResponse::err(req.id, -32601, e.to_string()),
                }
            }
            _ => JsonRpcResponse::err(req.id, -32601, format!("method not found: {}", req.method)),
        }
    }
}
