use anyhow::Result;
use rmcp::{transport::stdio, ServiceExt, ServerHandler, ErrorData};
use rmcp::service::{RequestContext, RoleServer};
use rmcp::model::{
    CallToolRequestParam, CallToolResult, Content,
    InitializeRequestParam, InitializeResult, Implementation,
    ListToolsResult, PaginatedRequestParam, ProtocolVersion,
    ServerCapabilities, Tool, ToolsCapability
};
use serde_json::{Map, Value, json};
use std::{borrow::Cow, collections::HashMap, sync::Arc};
use tracing_subscriber;

use ggen_ai::client::{LlmClient, LlmConfig};
use ggen_ai::config::OllamaConfig;
use ggen_ai::providers::ollama::OllamaClient;

struct OllamaMcpServer {
    client: OllamaClient,
}

impl OllamaMcpServer {
    fn new() -> Result<Self> {
        let config = OllamaConfig::new();
        let client = OllamaClient::new(config)?;
        Ok(Self { client })
    }

    async fn execute_complete(&self, params: Value) -> Result<Value> {
        let prompt = params["prompt"].as_str()
            .ok_or_else(|| anyhow::anyhow!("Missing 'prompt' parameter"))?;

        let model = params["model"].as_str().unwrap_or("qwen3-coder:30b");
        let max_tokens = params["max_tokens"].as_u64().map(|v| v as u32);
        let temperature = params["temperature"].as_f64().map(|v| v as f32);

        let config = LlmConfig {
            model: model.to_string(),
            max_tokens,
            temperature,
            ..Default::default()
        };

        let response = self.client.complete(prompt, Some(config)).await
            .map_err(|e| anyhow::anyhow!("Ollama error: {}", e))?;

        Ok(json!({
            "content": response.content,
            "model": response.model,
            "usage": response.usage
        }))
    }

    async fn list_models(&self) -> Result<Value> {
        // Call Ollama API to list models
        let response = reqwest::get("http://localhost:11434/api/tags").await?;
        let models: Value = response.json().await?;
        Ok(models)
    }
}

impl ServerHandler for OllamaMcpServer {
    async fn initialize(
        &self,
        _params: InitializeRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<InitializeResult, ErrorData> {
        Ok(InitializeResult {
            protocol_version: ProtocolVersion::default(),
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability::default()),
                ..Default::default()
            },
            server_info: Implementation {
                name: "ollama-mcp".to_string(),
                version: "0.1.0".to_string(),
                title: None,
                website_url: None,
                icons: None,
            },
            instructions: None,
        })
    }

    async fn list_tools(
        &self,
        _pagination: Option<PaginatedRequestParam>,
        _context: RequestContext<RoleServer>,
    ) -> std::result::Result<ListToolsResult, ErrorData> {
        let mut schema1 = Map::new();
        schema1.insert("type".to_string(), json!("object"));
        schema1.insert("required".to_string(), json!(["prompt"]));
        let mut props1 = Map::new();
        props1.insert("prompt".to_string(), json!({"type": "string", "description": "The prompt to send to Ollama"}));
        props1.insert("model".to_string(), json!({"type": "string", "description": "Model to use (default: qwen3-coder:30b)"}));
        props1.insert("max_tokens".to_string(), json!({"type": "number", "description": "Maximum tokens to generate"}));
        props1.insert("temperature".to_string(), json!({"type": "number", "description": "Temperature (0.0-2.0)"}));
        schema1.insert("properties".to_string(), json!(props1));

        let mut schema2 = Map::new();
        schema2.insert("type".to_string(), json!("object"));
        schema2.insert("properties".to_string(), json!({}));

        let tools = vec![
            Tool {
                name: Cow::Borrowed("ollama_complete"),
                description: Some(Cow::Borrowed("Generate text completion using Ollama local models")),
                input_schema: Arc::new(schema1),
                title: None,
                output_schema: None,
                annotations: None,
                icons: None,
            },
            Tool {
                name: Cow::Borrowed("ollama_list_models"),
                description: Some(Cow::Borrowed("List available Ollama models")),
                input_schema: Arc::new(schema2),
                title: None,
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
        let args = Value::Object(params.arguments.unwrap_or_default());

        let result = match params.name.as_ref() {
            "ollama_complete" => self.execute_complete(args).await,
            "ollama_list_models" => self.list_models().await,
            _ => Err(anyhow::anyhow!("Unknown tool: {}", params.name)),
        };

        match result {
            Ok(value) => Ok(CallToolResult {
                content: vec![Content::text(serde_json::to_string_pretty(&value)
                    .map_err(|e| ErrorData::internal_error(e.to_string(), None))?)],
                is_error: None,
                meta: None,
                structured_content: None,
            }),
            Err(e) => Err(ErrorData::internal_error(e.to_string(), None)),
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting Ollama MCP server");

    let server = OllamaMcpServer::new()?;
    tracing::info!("Ollama MCP Server initialized, serving on stdio");

    let service = server.serve(stdio()).await?;
    service.waiting().await?;

    Ok(())
}
