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

    fn tools_list(&self) -> serde_json::Value {
        let mut base = self.tools.list();
        let extra = json!([
            {
                "name": "workspace/info",
                "description": "Returns workspace metadata including name, version, crates and Rust edition",
                "inputSchema": {
                    "type": "object",
                    "properties": {},
                    "required": []
                }
            },
            {
                "name": "item/validate-name",
                "description": "Validates an item name: must be non-empty after trim and at most 255 characters",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "name": {
                            "type": "string",
                            "description": "The item name to validate"
                        }
                    },
                    "required": ["name"]
                }
            }
        ]);

        if let Some(tools_arr) = base.get_mut("tools").and_then(|v| v.as_array_mut()) {
            if let Some(extra_arr) = extra.as_array() {
                tools_arr.extend(extra_arr.iter().cloned());
            }
        }
        base
    }

    fn call_workspace_info() -> serde_json::Value {
        json!({
            "name": env!("CARGO_PKG_NAME"),
            "version": env!("CARGO_PKG_VERSION"),
            "description": env!("CARGO_PKG_DESCRIPTION"),
            "crates": ["bp-core", "bp-config", "domain", "service", "bp-sqlite", "mcp-server"],
            "rust_edition": "2021"
        })
    }

    fn call_validate_name(args: &serde_json::Value) -> serde_json::Value {
        let raw = args.get("name").and_then(|v| v.as_str()).unwrap_or("");
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            json!({ "valid": false, "error": "name must not be empty" })
        } else if trimmed.len() > 255 {
            json!({ "valid": false, "error": "name must not exceed 255 characters" })
        } else {
            json!({ "valid": true, "name": trimmed })
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
            "tools/list" => JsonRpcResponse::ok(req.id, self.tools_list()),
            "tools/call" => {
                let params = req.params.unwrap_or(json!({}));
                let name = params.get("name").and_then(|v| v.as_str()).unwrap_or("");
                let args = params.get("arguments").cloned().unwrap_or(json!({}));

                let result: anyhow::Result<serde_json::Value> = match name {
                    "workspace/info" => Ok(Self::call_workspace_info()),
                    "item/validate-name" => Ok(Self::call_validate_name(&args)),
                    _ => self.tools.call(name, args),
                };

                match result {
                    Ok(val) => JsonRpcResponse::ok(
                        req.id,
                        json!({ "content": [{ "type": "text", "text": val.to_string() }] }),
                    ),
                    Err(e) => JsonRpcResponse::err(req.id, -32601, e.to_string()),
                }
            }
            _ => JsonRpcResponse::err(req.id, -32601, format!("method not found: {}", req.method)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::protocol::JsonRpcRequest;
    use serde_json::{json, Value};

    fn make_server() -> McpServer {
        McpServer::new("test-server", "0.0.0")
    }

    fn call_tool(server: &McpServer, tool: &str, arguments: Value) -> Value {
        let req = JsonRpcRequest {
            id: json!(1),
            method: "tools/call".into(),
            params: Some(json!({ "name": tool, "arguments": arguments })),
        };
        let resp = server.handle(req);
        // Extract the text content from the MCP response envelope
        let text = resp
            .result
            .as_ref()
            .and_then(|r| r.get("content"))
            .and_then(|c| c.get(0))
            .and_then(|item| item.get("text"))
            .and_then(|t| t.as_str())
            .expect("response should contain content[0].text");
        serde_json::from_str(text).expect("content text should be valid JSON")
    }

    #[test]
    fn workspace_info_tool_returns_name_and_version() {
        let server = make_server();
        let result = call_tool(&server, "workspace/info", json!({}));

        assert!(result.get("name").is_some(), "should have 'name' field");
        assert!(result.get("version").is_some(), "should have 'version' field");
        assert_eq!(
            result["rust_edition"].as_str(),
            Some("2021"),
            "rust_edition should be 2021"
        );
        assert!(
            result["crates"].as_array().is_some(),
            "crates should be an array"
        );
        assert!(
            result["crates"]
                .as_array()
                .unwrap()
                .iter()
                .any(|c| c.as_str() == Some("mcp-server")),
            "crates should include mcp-server"
        );
    }

    #[test]
    fn validate_name_tool_accepts_valid_name() {
        let server = make_server();
        let result = call_tool(&server, "item/validate-name", json!({ "name": "  hello world  " }));

        assert_eq!(result["valid"], json!(true));
        assert_eq!(result["name"], json!("hello world"), "name should be trimmed");
    }

    #[test]
    fn validate_name_tool_rejects_empty_name() {
        let server = make_server();

        // Blank string
        let result = call_tool(&server, "item/validate-name", json!({ "name": "   " }));
        assert_eq!(result["valid"], json!(false));
        assert!(result["error"].as_str().is_some(), "should have error message");

        // Completely absent name key
        let result2 = call_tool(&server, "item/validate-name", json!({}));
        assert_eq!(result2["valid"], json!(false));
    }

    #[test]
    fn tools_list_includes_new_tools() {
        let server = make_server();
        let req = JsonRpcRequest {
            id: json!(1),
            method: "tools/list".into(),
            params: None,
        };
        let resp = server.handle(req);
        let tools = resp
            .result
            .as_ref()
            .and_then(|r| r.get("tools"))
            .and_then(|t| t.as_array())
            .expect("result should have tools array");

        let names: Vec<&str> = tools
            .iter()
            .filter_map(|t| t.get("name").and_then(|n| n.as_str()))
            .collect();

        assert!(names.contains(&"workspace/info"), "should list workspace/info");
        assert!(names.contains(&"item/validate-name"), "should list item/validate-name");
    }
}
