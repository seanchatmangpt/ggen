use serde_json::{json, Value};

pub struct ToolRegistry {
    tools: Vec<ToolDef>,
}

struct ToolDef {
    name: &'static str,
    description: &'static str,
    handler: fn(Value) -> anyhow::Result<Value>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        Self {
            tools: vec![
                ToolDef {
                    name: "echo",
                    description: "Echo the input message back",
                    handler: |params| {
                        let msg = params.get("message").and_then(Value::as_str).unwrap_or("");
                        Ok(json!({ "echo": msg }))
                    },
                },
            ],
        }
    }

    pub fn list(&self) -> Value {
        let tools: Vec<_> = self.tools.iter().map(|t| json!({
            "name": t.name,
            "description": t.description,
            "inputSchema": { "type": "object" }
        })).collect();
        json!({ "tools": tools })
    }

    pub fn call(&self, name: &str, params: Value) -> anyhow::Result<Value> {
        self.tools
            .iter()
            .find(|t| t.name == name)
            .map(|t| (t.handler)(params))
            .unwrap_or_else(|| anyhow::bail!("unknown tool: {name}"))
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}
