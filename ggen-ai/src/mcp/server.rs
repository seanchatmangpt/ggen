//! AI-enhanced MCP server

use rmcp::{
    ErrorData, ServerHandler,
    model::{
        CallToolRequestParam, CallToolResult, Content,
        InitializeRequestParam, InitializeResult, Implementation,
        ListToolsResult, PaginatedRequestParam, ProtocolVersion,
        ServerCapabilities, Tool, ToolsCapability
    },
    service::RequestContext, RoleServer
};
use serde_json::{json, Map, Value};
use std::{borrow::Cow, collections::HashMap, sync::Arc};

use crate::error::{GgenAiError, Result};
use crate::mcp::tools::AiMcpTools;
use crate::client::LlmConfig;

/// AI-enhanced MCP server
#[derive(Debug)]
pub struct GgenAiMcpServer {
    tools: HashMap<String, ToolDef>,
    ai_tools: AiMcpTools,
}

#[derive(Debug, Clone)]
pub struct ToolDef {
    pub name: String,
    pub description: String,
    pub input_schema: Value,
}

impl GgenAiMcpServer {
    /// Create a new AI-enhanced MCP server
    pub fn new() -> Self {
        let mut tools = HashMap::new();
        
        // AI-specific tools
        tools.insert(
            "ai_generate_template".to_string(),
            ToolDef {
                name: "ai_generate_template".to_string(),
                description: "Generate ggen templates from natural language descriptions".to_string(),
                input_schema: ai_generate_template_schema(),
            },
        );
        
        tools.insert(
            "ai_generate_sparql".to_string(),
            ToolDef {
                name: "ai_generate_sparql".to_string(),
                description: "Generate SPARQL queries from natural language intent".to_string(),
                input_schema: ai_generate_sparql_schema(),
            },
        );
        
        tools.insert(
            "ai_generate_ontology".to_string(),
            ToolDef {
                name: "ai_generate_ontology".to_string(),
                description: "Generate RDF/OWL ontologies from domain descriptions".to_string(),
                input_schema: ai_generate_ontology_schema(),
            },
        );
        
        tools.insert(
            "ai_refactor_code".to_string(),
            ToolDef {
                name: "ai_refactor_code".to_string(),
                description: "Suggest code refactoring improvements using AI".to_string(),
                input_schema: ai_refactor_code_schema(),
            },
        );
        
        tools.insert(
            "ai_explain_graph".to_string(),
            ToolDef {
                name: "ai_explain_graph".to_string(),
                description: "Explain RDF graph content in natural language".to_string(),
                input_schema: ai_explain_graph_schema(),
            },
        );
        
        tools.insert(
            "ai_suggest_delta".to_string(),
            ToolDef {
                name: "ai_suggest_delta".to_string(),
                description: "Suggest intelligent merge strategies for delta-driven projection".to_string(),
                input_schema: ai_suggest_delta_schema(),
            },
        );

        // Autonomous tools
        tools.insert(
            "autonomous_evolve_graph".to_string(),
            ToolDef {
                name: "autonomous_evolve_graph".to_string(),
                description: "Evolve RDF graph from natural language using autonomous AI inference".to_string(),
                input_schema: autonomous_evolve_graph_schema(),
            },
        );

        tools.insert(
            "autonomous_regenerate".to_string(),
            ToolDef {
                name: "autonomous_regenerate".to_string(),
                description: "Trigger autonomous template regeneration based on graph changes".to_string(),
                input_schema: autonomous_regenerate_schema(),
            },
        );

        tools.insert(
            "autonomous_status".to_string(),
            ToolDef {
                name: "autonomous_status".to_string(),
                description: "Get status of autonomous MCP-AI system including evolution engine and orchestrator".to_string(),
                input_schema: autonomous_status_schema(),
            },
        );

        Self {
            tools,
            ai_tools: AiMcpTools::new(),
        }
    }
    
    /// Initialize with OpenAI client
    pub fn with_openai(mut self, _api_key: String) -> Self {
        let _config = LlmConfig {
            model: "gpt-3.5-turbo".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: std::collections::HashMap::new(),
        };
        self.ai_tools = self.ai_tools.with_openai(config);
        self
    }
    
    /// Initialize with Anthropic client
    pub fn with_anthropic(mut self, _api_key: String) -> Self {
        let _config = LlmConfig {
            model: "claude-3-sonnet-20240229".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: std::collections::HashMap::new(),
        };
        self.ai_tools = self.ai_tools.with_anthropic(config);
        self
    }
    
    /// Initialize with Ollama client
    pub fn with_ollama(mut self) -> Self {
        let _config = LlmConfig {
            model: "qwen3-coder:30b".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: std::collections::HashMap::new(),
        };
        self.ai_tools = self.ai_tools.with_ollama();
        self
    }
    
    /// Initialize with Ollama client and specific model
    pub fn with_ollama_model(mut self, model: &str) -> Self {
        self.ai_tools = self.ai_tools.with_ollama_model(model);
        self
    }
    
    /// Execute a tool
    async fn execute_tool(&self, name: &str, params: Value) -> Result<Value> {
        match name {
            "ai_generate_template" => self.ai_tools.ai_generate_template(params).await,
            "ai_generate_sparql" => self.ai_tools.ai_generate_sparql(params).await,
            "ai_generate_ontology" => self.ai_tools.ai_generate_ontology(params).await,
            "ai_refactor_code" => self.ai_tools.ai_refactor_code(params).await,
            "ai_explain_graph" => self.ai_tools.ai_explain_graph(params).await,
            "ai_suggest_delta" => self.ai_tools.ai_suggest_delta(params).await,
            "autonomous_evolve_graph" => self.ai_tools.autonomous_evolve_graph(params).await,
            "autonomous_regenerate" => self.ai_tools.autonomous_regenerate(params).await,
            "autonomous_status" => self.ai_tools.autonomous_status(params).await,
            _ => Err(GgenAiError::validation(format!("Unknown tool: {}", name))),
        }
    }
}

impl ServerHandler for GgenAiMcpServer {
    fn initialize(
        &self,
        _params: InitializeRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = std::result::Result<InitializeResult, ErrorData>> + Send + '_ {
        async move {
            Ok(InitializeResult {
                protocol_version: ProtocolVersion::default(),
                capabilities: ServerCapabilities {
                    tools: Some(ToolsCapability::default()),
                    ..Default::default()
                },
                server_info: Implementation {
                    name: "ggen-ai-mcp".to_string(),
                    version: env!("CARGO_PKG_VERSION").to_string(),
                    title: Some("GGen AI MCP Server".to_string()),
                    website_url: Some("https://github.com/seanchatmangpt/ggen".to_string()),
                    icons: None,
                },
                instructions: Some("AI-powered code generation and refactoring tools for ggen".to_string()),
            })
        }
    }
    
    fn list_tools(
        &self,
        _pagination: Option<PaginatedRequestParam>,
        _context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = std::result::Result<ListToolsResult, ErrorData>> + Send + '_ {
        async move {
            let tools = self
                .tools
                .values()
                .map(|tool_def| {
                    let schema_map = match &tool_def.input_schema {
                        Value::Object(map) => map.clone(),
                        _ => Map::new(),
                    };
                    
                    Tool {
                        name: Cow::Owned(tool_def.name.clone()),
                        description: Some(Cow::Owned(tool_def.description.clone())),
                        input_schema: Arc::new(schema_map),
                        title: None,
                        output_schema: None,
                        icons: None,
                        annotations: None,
                    }
                })
                .collect();
            
            Ok(ListToolsResult {
                tools,
                next_cursor: None,
            })
        }
    }
    
    fn call_tool(
        &self,
        params: CallToolRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = std::result::Result<CallToolResult, ErrorData>> + Send + '_ {
        async move {
            let args = Value::Object(params.arguments.unwrap_or_default());
            let result = self
                .execute_tool(&params.name, args)
                .await
                .map_err(|e| ErrorData::internal_error(e.to_string(), None))?;
            
            Ok(CallToolResult {
                content: vec![Content::text(
                    serde_json::to_string_pretty(&result)
                        .map_err(|e| ErrorData::internal_error(e.to_string(), None))?,
                )],
                is_error: None,
                meta: None,
                structured_content: None,
            })
        }
    }
}

impl Default for GgenAiMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

/// Schema for ai_generate_template tool
fn ai_generate_template_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "description": {
                "type": "string",
                "description": "Natural language description of the template to generate"
            },
            "examples": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Example requirements or use cases"
            },
            "language": {
                "type": "string",
                "description": "Target programming language"
            },
            "framework": {
                "type": "string",
                "description": "Target framework"
            }
        },
        "required": ["description"]
    })
}

/// Schema for ai_generate_sparql tool
fn ai_generate_sparql_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "intent": {
                "type": "string",
                "description": "Natural language description of what the query should do"
            },
            "graph": {
                "type": "string",
                "description": "RDF graph data in Turtle format"
            }
        },
        "required": ["intent", "graph"]
    })
}

/// Schema for ai_generate_ontology tool
fn ai_generate_ontology_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "domain": {
                "type": "string",
                "description": "Domain description for the ontology"
            },
            "requirements": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Specific requirements for the ontology"
            }
        },
        "required": ["domain"]
    })
}

/// Schema for ai_refactor_code tool
fn ai_refactor_code_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "code": {
                "type": "string",
                "description": "Code to refactor"
            },
            "language": {
                "type": "string",
                "description": "Programming language of the code"
            }
        },
        "required": ["code"]
    })
}

/// Schema for ai_explain_graph tool
fn ai_explain_graph_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "graph": {
                "type": "string",
                "description": "RDF graph data in Turtle format"
            },
            "focus": {
                "type": "string",
                "description": "What aspect of the graph to focus on",
                "default": "general overview"
            }
        },
        "required": ["graph"]
    })
}

/// Schema for ai_suggest_delta tool
fn ai_suggest_delta_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "baseline": {
                "type": "string",
                "description": "Baseline version of the code"
            },
            "current": {
                "type": "string",
                "description": "Current generated version"
            },
            "manual": {
                "type": "string",
                "description": "Manual modifications",
                "default": ""
            }
        },
        "required": ["baseline", "current"]
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_server_creation() {
        let server = GgenAiMcpServer::new();
        assert_eq!(server.tools.len(), 9);
        assert!(server.tools.contains_key("ai_generate_template"));
        assert!(server.tools.contains_key("ai_generate_sparql"));
        assert!(server.tools.contains_key("ai_generate_ontology"));
        assert!(server.tools.contains_key("ai_refactor_code"));
        assert!(server.tools.contains_key("ai_explain_graph"));
        assert!(server.tools.contains_key("ai_suggest_delta"));
        assert!(server.tools.contains_key("autonomous_evolve_graph"));
        assert!(server.tools.contains_key("autonomous_regenerate"));
        assert!(server.tools.contains_key("autonomous_status"));
    }
    
    #[tokio::test]
    async fn test_server_with_openai() {
        let server = GgenAiMcpServer::new().with_openai("test-key".to_string());
        assert_eq!(server.tools.len(), 9);
    }

    #[tokio::test]
    async fn test_server_with_anthropic() {
        let server = GgenAiMcpServer::new().with_anthropic("test-key".to_string());
        assert_eq!(server.tools.len(), 9);
    }

    #[tokio::test]
    async fn test_server_with_ollama() {
        let server = GgenAiMcpServer::new().with_ollama();
        assert_eq!(server.tools.len(), 9);
    }
}

/// Schema for autonomous_evolve_graph tool
fn autonomous_evolve_graph_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "text": {
                "type": "string",
                "description": "Natural language description of graph changes"
            },
            "confidence_threshold": {
                "type": "number",
                "description": "Minimum confidence threshold for accepting inferred triples (0.0-1.0)",
                "default": 0.7
            }
        },
        "required": ["text"]
    })
}

/// Schema for autonomous_regenerate tool
fn autonomous_regenerate_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "template_id": {
                "type": "string",
                "description": "Template identifier to regenerate"
            }
        },
        "required": ["template_id"]
    })
}

/// Schema for autonomous_status tool
fn autonomous_status_schema() -> Value {
    json!({
        "type": "object",
        "properties": {},
        "required": []
    })
}

