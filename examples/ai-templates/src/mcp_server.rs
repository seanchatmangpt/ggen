// MCP Server Implementation for Template Management
// Demonstrates: Agent discovery of templates, template application via MCP tools
// ggen Pattern: Agents call MCP tools to discover and render templates

use crate::{Template, TemplateRegistry, TemplateDiscoveryRequest, DiscoveryType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum McpServerError {
    #[error("Tool not found: {0}")]
    ToolNotFound(String),

    #[error("Tool execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Invalid request: {0}")]
    InvalidRequest(String),
}

pub type McpServerResult<T> = Result<T, McpServerError>;

// ============================================================================
// MCP TOOL DEFINITIONS
// ============================================================================

/// Represents an MCP tool endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpTool {
    pub name: String,
    pub description: String,
    pub input_schema: serde_json::Value,
    pub path: String,
}

/// Request to discover available templates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoverTemplatesRequest {
    pub query: String,
    pub category_filter: Option<String>,
}

/// Response listing available templates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoverTemplatesResponse {
    pub total: usize,
    pub templates: Vec<TemplateMetadata>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateMetadata {
    pub name: String,
    pub description: String,
    pub category: Option<String>,
    pub language: Option<String>,
    pub variable_count: usize,
}

/// Request to apply a template
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplyTemplateRequest {
    pub template_name: String,
    pub variables: HashMap<String, String>,
}

/// Response with rendered template
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplyTemplateResponse {
    pub template_name: String,
    pub rendered_output: String,
    pub variable_count: usize,
}

// ============================================================================
// MCP SERVER
// ============================================================================

pub struct McpServer {
    registry: Arc<RwLock<TemplateRegistry>>,
    tools: HashMap<String, McpTool>,
}

impl McpServer {
    pub fn new() -> Self {
        let mut tools = HashMap::new();

        // Register tool: discover_templates
        tools.insert(
            "discover_templates".to_string(),
            McpTool {
                name: "discover_templates".to_string(),
                description: "Discover available templates via query".to_string(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "query": {"type": "string"},
                        "category_filter": {"type": ["string", "null"]}
                    },
                    "required": ["query"]
                }),
                path: "/mcp/tools/discover_templates".to_string(),
            },
        );

        // Register tool: apply_template
        tools.insert(
            "apply_template".to_string(),
            McpTool {
                name: "apply_template".to_string(),
                description: "Apply a template with variable substitution".to_string(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "template_name": {"type": "string"},
                        "variables": {"type": "object"}
                    },
                    "required": ["template_name", "variables"]
                }),
                path: "/mcp/tools/apply_template".to_string(),
            },
        );

        // Register tool: list_tools
        tools.insert(
            "list_tools".to_string(),
            McpTool {
                name: "list_tools".to_string(),
                description: "List all available MCP tools".to_string(),
                input_schema: serde_json::json!({"type": "object"}),
                path: "/mcp/tools/list".to_string(),
            },
        );

        Self {
            registry: Arc::new(RwLock::new(TemplateRegistry::new())),
            tools,
        }
    }

    pub async fn register_template(&self, template: Template) {
        let mut registry = self.registry.write().await;
        registry.register(template);
    }

    pub async fn discover_templates(
        &self,
        request: DiscoverTemplatesRequest,
    ) -> McpServerResult<DiscoverTemplatesResponse> {
        let registry = self.registry.read().await;
        let discovery_req = TemplateDiscoveryRequest {
            query_type: if request.query == "*" {
                DiscoveryType::All
            } else {
                DiscoveryType::ByName
            },
            parameter: Some(request.query),
        };

        let discovery_result = registry.discover(&discovery_req);

        let templates: Vec<TemplateMetadata> = discovery_result
            .results
            .iter()
            .map(|t| {
                let metadata = registry.get_metadata(&t.name).ok();
                TemplateMetadata {
                    name: t.name.clone(),
                    description: t.description.clone(),
                    category: metadata.as_ref().and_then(|m| m.category.clone()),
                    language: metadata.and_then(|m| m.language),
                    variable_count: extract_variables(&t.content).len(),
                }
            })
            .collect();

        Ok(DiscoverTemplatesResponse {
            total: templates.len(),
            templates,
        })
    }

    pub async fn apply_template(
        &self,
        request: ApplyTemplateRequest,
    ) -> McpServerResult<ApplyTemplateResponse> {
        let registry = self.registry.read().await;

        let template = registry
            .get(&request.template_name)
            .ok_or_else(|| {
                McpServerError::ExecutionFailed(format!(
                    "Template not found: {}",
                    request.template_name
                ))
            })?;

        let rendered_output = template
            .render(&request.variables)
            .map_err(|e| McpServerError::ExecutionFailed(format!("Rendering failed: {}", e)))?;

        Ok(ApplyTemplateResponse {
            template_name: request.template_name,
            rendered_output,
            variable_count: request.variables.len(),
        })
    }

    pub fn list_tools(&self) -> Vec<McpTool> {
        self.tools.values().cloned().collect()
    }

    pub fn get_tool(&self, name: &str) -> McpServerResult<McpTool> {
        self.tools
            .get(name)
            .cloned()
            .ok_or_else(|| McpServerError::ToolNotFound(name.to_string()))
    }
}

impl Default for McpServer {
    fn default() -> Self {
        Self::new()
    }
}

// Helper: extract variables from template
fn extract_variables(content: &str) -> Vec<String> {
    use regex::Regex;
    if let Ok(re) = Regex::new(r"\{\{(\w+)\}\}") {
        re.captures_iter(content)
            .map(|cap| cap[1].to_string())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect()
    } else {
        Vec::new()
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mcp_server_creation() {
        let server = McpServer::new();
        assert!(!server.tools.is_empty());
    }

    #[tokio::test]
    async fn test_list_tools() {
        let server = McpServer::new();
        let tools = server.list_tools();
        assert_eq!(tools.len(), 3);
    }

    #[tokio::test]
    async fn test_get_tool() {
        let server = McpServer::new();
        let tool = server.get_tool("discover_templates");
        assert!(tool.is_ok());
        assert_eq!(tool.unwrap().name, "discover_templates");
    }

    #[tokio::test]
    async fn test_get_nonexistent_tool() {
        let server = McpServer::new();
        let tool = server.get_tool("missing_tool");
        assert!(tool.is_err());
    }

    #[tokio::test]
    async fn test_register_template() {
        let server = McpServer::new();
        let template = Template::new("test", "Hello {{name}}");
        server.register_template(template).await;

        let registry = server.registry.read().await;
        assert!(registry.get("test").is_some());
    }

    #[tokio::test]
    async fn test_discover_templates_all() {
        let server = McpServer::new();
        server.register_template(Template::new("t1", "Content 1")).await;
        server.register_template(Template::new("t2", "Content 2")).await;

        let request = DiscoverTemplatesRequest {
            query: "*".to_string(),
            category_filter: None,
        };

        let response = server.discover_templates(request).await.unwrap();
        assert_eq!(response.total, 2);
    }

    #[tokio::test]
    async fn test_discover_templates_by_name() {
        let server = McpServer::new();
        server.register_template(Template::new("greeting", "Hello {{name}}")).await;

        let request = DiscoverTemplatesRequest {
            query: "greeting".to_string(),
            category_filter: None,
        };

        let response = server.discover_templates(request).await.unwrap();
        assert_eq!(response.total, 1);
    }

    #[tokio::test]
    async fn test_discover_templates_empty() {
        let server = McpServer::new();

        let request = DiscoverTemplatesRequest {
            query: "*".to_string(),
            category_filter: None,
        };

        let response = server.discover_templates(request).await.unwrap();
        assert_eq!(response.total, 0);
    }

    #[tokio::test]
    async fn test_apply_template_success() {
        let server = McpServer::new();
        server.register_template(Template::new("greeting", "Hello {{name}}!")).await;

        let mut vars = HashMap::new();
        vars.insert("name".to_string(), "World".to_string());

        let request = ApplyTemplateRequest {
            template_name: "greeting".to_string(),
            variables: vars,
        };

        let response = server.apply_template(request).await.unwrap();
        assert_eq!(response.rendered_output, "Hello World!");
        assert_eq!(response.variable_count, 1);
    }

    #[tokio::test]
    async fn test_apply_template_missing() {
        let server = McpServer::new();

        let request = ApplyTemplateRequest {
            template_name: "missing".to_string(),
            variables: HashMap::new(),
        };

        let result = server.apply_template(request).await;
        assert!(result.is_err());
    }

    #[test]
    fn test_mcp_tool_creation() {
        let tool = McpTool {
            name: "test_tool".to_string(),
            description: "A test tool".to_string(),
            input_schema: serde_json::json!({}),
            path: "/mcp/tools/test".to_string(),
        };

        assert_eq!(tool.name, "test_tool");
    }

    #[test]
    fn test_discover_request_serialization() {
        let request = DiscoverTemplatesRequest {
            query: "test".to_string(),
            category_filter: Some("controller".to_string()),
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("test"));
        assert!(json.contains("controller"));
    }

    #[tokio::test]
    async fn test_apply_template_with_multiple_vars() {
        let server = McpServer::new();
        server.register_template(
            Template::new("config", "{{host}}:{{port}}")
        ).await;

        let mut vars = HashMap::new();
        vars.insert("host".to_string(), "localhost".to_string());
        vars.insert("port".to_string(), "8080".to_string());

        let request = ApplyTemplateRequest {
            template_name: "config".to_string(),
            variables: vars,
        };

        let response = server.apply_template(request).await.unwrap();
        assert_eq!(response.rendered_output, "localhost:8080");
    }

    #[test]
    fn test_extract_variables_basic() {
        let content = "Hello {{name}}, welcome to {{place}}";
        let vars = extract_variables(content);
        assert_eq!(vars.len(), 2);
    }

    #[test]
    fn test_extract_variables_duplicate() {
        let content = "{{var}} and {{var}} again";
        let vars = extract_variables(content);
        assert_eq!(vars.len(), 1);
    }

    #[test]
    fn test_extract_variables_empty() {
        let content = "No variables here";
        let vars = extract_variables(content);
        assert!(vars.is_empty());
    }

    #[test]
    fn test_mcp_tool_schema() {
        let server = McpServer::new();
        let tool = server.get_tool("apply_template").unwrap();

        let schema = &tool.input_schema;
        assert!(schema["type"] == "object");
        assert!(schema["properties"]["template_name"].is_object());
    }
}
