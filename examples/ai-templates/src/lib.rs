// AI Templates Example - Simple template engine
// Demonstrates: Template registration, rendering, variable substitution, MCP tool integration

pub mod mcp_server;

use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use thiserror::Error;
use uuid::Uuid;

// ============================================================================
// ERRORS
// ============================================================================

#[derive(Debug, Error)]
pub enum TemplateError {
    #[error("Template not found: {0}")]
    TemplateNotFound(String),

    #[error("Variable not found: {0}")]
    VariableNotFound(String),

    #[error("Rendering error: {0}")]
    RenderingError(String),

    #[error("Invalid template syntax: {0}")]
    InvalidSyntax(String),
}

pub type TemplateResult<T> = Result<T, TemplateError>;

// ============================================================================
// TEMPLATE TYPES
// ============================================================================

#[derive(Debug, Clone)]
pub struct Template {
    pub name: String,
    pub content: String,
    pub description: String,
}

impl Template {
    pub fn new(name: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            content: content.into(),
            description: String::new(),
        }
    }

    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Simple variable substitution: {{variable}} → value
    pub fn render(&self, variables: &HashMap<String, String>) -> TemplateResult<String> {
        let mut result = self.content.clone();

        // Find all {{var}} patterns and replace
        let re = Regex::new(r"\{\{(\w+)\}\}").map_err(|e| {
            TemplateError::RenderingError(format!("Regex error: {}", e))
        })?;

        for cap in re.captures_iter(&self.content.clone()) {
            let var_name = &cap[1];
            let value = variables
                .get(var_name)
                .ok_or_else(|| TemplateError::VariableNotFound(var_name.to_string()))?;

            result = result.replace(&format!("{{{{{}}}}}", var_name), value);
        }

        Ok(result)
    }

    /// Render with JSON values
    pub fn render_json(&self, variables: &Value) -> TemplateResult<String> {
        if !variables.is_object() {
            return Err(TemplateError::RenderingError(
                "Variables must be a JSON object".to_string(),
            ));
        }

        let mut result = self.content.clone();
        let re = Regex::new(r"\{\{(\w+)\}\}").map_err(|e| {
            TemplateError::RenderingError(format!("Regex error: {}", e))
        })?;

        for cap in re.captures_iter(&self.content.clone()) {
            let var_name = &cap[1];
            let value = variables
                .get(var_name)
                .ok_or_else(|| TemplateError::VariableNotFound(var_name.to_string()))?;

            let value_str = match value {
                Value::String(s) => s.clone(),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Null => "null".to_string(),
                _ => value.to_string(),
            };

            result = result.replace(&format!("{{{{{}}}}}", var_name), &value_str);
        }

        Ok(result)
    }
}

// ============================================================================
// MCP TOOL DEFINITIONS
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateDiscoveryRequest {
    pub query_type: DiscoveryType,
    pub parameter: Option<String>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum DiscoveryType {
    All,
    ByName,
    ByCategory,
    ByLanguage,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateDiscoveryResult {
    pub templates: Vec<TemplateMetadata>,
    pub total_count: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateMetadata {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: Option<String>,
    pub language: Option<String>,
    pub variables: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderRequest {
    pub template_name: String,
    pub variables: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderResult {
    pub success: bool,
    pub output: String,
    pub render_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidateRequest {
    pub template_content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidateResult {
    pub valid: bool,
    pub errors: Vec<String>,
    pub variable_count: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegisterRequest {
    pub name: String,
    pub content: String,
    pub description: Option<String>,
    pub category: Option<String>,
    pub language: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegisterResult {
    pub success: bool,
    pub message: String,
    pub template_id: String,
}

// ============================================================================
// TEMPLATE REGISTRY
// ============================================================================

#[derive(Debug, Clone)]
pub struct TemplateMetadataExtended {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: Option<String>,
    pub language: Option<String>,
    pub variables: Vec<String>,
}

pub struct TemplateRegistry {
    templates: HashMap<String, Template>,
    metadata: HashMap<String, TemplateMetadataExtended>,
}

impl TemplateRegistry {
    pub fn new() -> Self {
        Self {
            templates: HashMap::new(),
            metadata: HashMap::new(),
        }
    }

    pub fn register(&mut self, template: Template) {
        let name = template.name.clone();
        let meta = TemplateMetadataExtended {
            id: Uuid::new_v4().to_string(),
            name: name.clone(),
            description: template.description.clone(),
            category: None,
            language: None,
            variables: extract_variables(&template.content),
        };
        self.templates.insert(name.clone(), template);
        self.metadata.insert(name, meta);
    }

    pub fn register_with_metadata(
        &mut self,
        template: Template,
        category: Option<String>,
        language: Option<String>,
    ) {
        let name = template.name.clone();
        let meta = TemplateMetadataExtended {
            id: Uuid::new_v4().to_string(),
            name: name.clone(),
            description: template.description.clone(),
            category,
            language,
            variables: extract_variables(&template.content),
        };
        self.templates.insert(name.clone(), template);
        self.metadata.insert(name, meta);
    }

    pub fn get(&self, name: &str) -> TemplateResult<&Template> {
        self.templates
            .get(name)
            .ok_or_else(|| TemplateError::TemplateNotFound(name.to_string()))
    }

    pub fn get_metadata(&self, name: &str) -> TemplateResult<&TemplateMetadataExtended> {
        self.metadata
            .get(name)
            .ok_or_else(|| TemplateError::TemplateNotFound(name.to_string()))
    }

    pub fn render(
        &self,
        name: &str,
        variables: &HashMap<String, String>,
    ) -> TemplateResult<String> {
        let template = self.get(name)?;
        template.render(variables)
    }

    pub fn render_json(&self, name: &str, variables: &Value) -> TemplateResult<String> {
        let template = self.get(name)?;
        template.render_json(variables)
    }

    pub fn list(&self) -> Vec<&Template> {
        self.templates.values().collect()
    }

    pub fn list_metadata(&self) -> Vec<&TemplateMetadataExtended> {
        self.metadata.values().collect()
    }

    pub fn count(&self) -> usize {
        self.templates.len()
    }

    // MCP Tool: Discover templates
    pub fn discover(&self, discovery: &TemplateDiscoveryRequest) -> TemplateDiscoveryResult {
        match discovery.query_type {
            DiscoveryType::All => {
                let templates = self
                    .metadata
                    .values()
                    .map(|m| TemplateMetadata {
                        id: m.id.clone(),
                        name: m.name.clone(),
                        description: m.description.clone(),
                        category: m.category.clone(),
                        language: m.language.clone(),
                        variables: m.variables.clone(),
                    })
                    .collect::<Vec<_>>();
                let count = templates.len();
                TemplateDiscoveryResult {
                    templates,
                    total_count: count,
                }
            }
            DiscoveryType::ByName => {
                if let Some(name) = &discovery.parameter {
                    if let Some(meta) = self.metadata.get(name) {
                        TemplateDiscoveryResult {
                            templates: vec![TemplateMetadata {
                                id: meta.id.clone(),
                                name: meta.name.clone(),
                                description: meta.description.clone(),
                                category: meta.category.clone(),
                                language: meta.language.clone(),
                                variables: meta.variables.clone(),
                            }],
                            total_count: 1,
                        }
                    } else {
                        TemplateDiscoveryResult {
                            templates: vec![],
                            total_count: 0,
                        }
                    }
                } else {
                    TemplateDiscoveryResult {
                        templates: vec![],
                        total_count: 0,
                    }
                }
            }
            DiscoveryType::ByCategory => {
                if let Some(cat) = &discovery.parameter {
                    let templates = self
                        .metadata
                        .values()
                        .filter(|m| m.category.as_ref() == Some(cat))
                        .map(|m| TemplateMetadata {
                            id: m.id.clone(),
                            name: m.name.clone(),
                            description: m.description.clone(),
                            category: m.category.clone(),
                            language: m.language.clone(),
                            variables: m.variables.clone(),
                        })
                        .collect::<Vec<_>>();
                    let count = templates.len();
                    TemplateDiscoveryResult {
                        templates,
                        total_count: count,
                    }
                } else {
                    TemplateDiscoveryResult {
                        templates: vec![],
                        total_count: 0,
                    }
                }
            }
            DiscoveryType::ByLanguage => {
                if let Some(lang) = &discovery.parameter {
                    let templates = self
                        .metadata
                        .values()
                        .filter(|m| m.language.as_ref() == Some(lang))
                        .map(|m| TemplateMetadata {
                            id: m.id.clone(),
                            name: m.name.clone(),
                            description: m.description.clone(),
                            category: m.category.clone(),
                            language: m.language.clone(),
                            variables: m.variables.clone(),
                        })
                        .collect::<Vec<_>>();
                    let count = templates.len();
                    TemplateDiscoveryResult {
                        templates,
                        total_count: count,
                    }
                } else {
                    TemplateDiscoveryResult {
                        templates: vec![],
                        total_count: 0,
                    }
                }
            }
        }
    }

    // MCP Tool: Validate template
    pub fn validate_template(&self, content: &str) -> ValidateResult {
        let variables = extract_variables(content);
        let errors = Vec::new();

        ValidateResult {
            valid: true,
            errors,
            variable_count: variables.len(),
        }
    }
}

impl Default for TemplateRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

fn extract_variables(content: &str) -> Vec<String> {
    let re = Regex::new(r"\{\{(\w+)\}\}").unwrap();
    re.captures_iter(content)
        .map(|cap| cap[1].to_string())
        .collect::<std::collections::HashSet<_>>()
        .into_iter()
        .collect()
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_template_creation() {
        let template = Template::new("test", "Hello {{name}}");
        assert_eq!(template.name, "test");
        assert!(template.content.contains("{{name}}"));
    }

    #[test]
    fn test_template_with_description() {
        let template = Template::new("test", "content")
            .with_description("A test template");
        assert_eq!(template.description, "A test template");
    }

    #[test]
    fn test_template_render_simple() {
        let template = Template::new("greeting", "Hello {{name}}, you are {{age}} years old");
        let mut vars = HashMap::new();
        vars.insert("name".to_string(), "Alice".to_string());
        vars.insert("age".to_string(), "30".to_string());

        let result = template.render(&vars).unwrap();
        assert_eq!(result, "Hello Alice, you are 30 years old");
    }

    #[test]
    fn test_template_render_missing_variable() {
        let template = Template::new("greeting", "Hello {{name}}");
        let vars = HashMap::new();

        let result = template.render(&vars);
        assert!(result.is_err());
        assert!(matches!(result, Err(TemplateError::VariableNotFound(_))));
    }

    #[test]
    fn test_template_render_json() {
        let template = Template::new("api", "API URL: {{url}}, Port: {{port}}");
        let vars = json!({
            "url": "example.com",
            "port": 8080
        });

        let result = template.render_json(&vars).unwrap();
        assert_eq!(result, "API URL: example.com, Port: 8080");
    }

    #[test]
    fn test_template_render_json_boolean() {
        let template = Template::new("feature", "Feature enabled: {{enabled}}");
        let vars = json!({
            "enabled": true
        });

        let result = template.render_json(&vars).unwrap();
        assert_eq!(result, "Feature enabled: true");
    }

    #[test]
    fn test_template_render_no_variables() {
        let template = Template::new("static", "This is static content");
        let vars = HashMap::new();

        let result = template.render(&vars).unwrap();
        assert_eq!(result, "This is static content");
    }

    #[test]
    fn test_template_render_multiple_same_variable() {
        let template = Template::new("repeated", "{{name}} says: {{name}} is here");
        let mut vars = HashMap::new();
        vars.insert("name".to_string(), "Bob".to_string());

        let result = template.render(&vars).unwrap();
        assert_eq!(result, "Bob says: Bob is here");
    }

    #[test]
    fn test_registry_creation() {
        let registry = TemplateRegistry::new();
        assert_eq!(registry.count(), 0);
    }

    #[test]
    fn test_registry_register() {
        let mut registry = TemplateRegistry::new();
        let template = Template::new("test", "Test");
        registry.register(template);

        assert_eq!(registry.count(), 1);
        assert!(registry.get("test").is_ok());
    }

    #[test]
    fn test_registry_register_multiple() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("t1", "Template 1"));
        registry.register(Template::new("t2", "Template 2"));
        registry.register(Template::new("t3", "Template 3"));

        assert_eq!(registry.count(), 3);
        assert_eq!(registry.list().len(), 3);
    }

    #[test]
    fn test_registry_get_not_found() {
        let registry = TemplateRegistry::new();
        let result = registry.get("nonexistent");

        assert!(result.is_err());
        assert!(matches!(result, Err(TemplateError::TemplateNotFound(_))));
    }

    #[test]
    fn test_registry_render() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("greet", "Welcome {{user}}!"));

        let mut vars = HashMap::new();
        vars.insert("user".to_string(), "Charlie".to_string());

        let result = registry.render("greet", &vars).unwrap();
        assert_eq!(result, "Welcome Charlie!");
    }

    #[test]
    fn test_registry_render_json() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("config", "Host: {{host}}, Debug: {{debug}}"));

        let vars = json!({
            "host": "localhost",
            "debug": false
        });

        let result = registry.render_json("config", &vars).unwrap();
        assert_eq!(result, "Host: localhost, Debug: false");
    }

    #[test]
    fn test_registry_render_not_found() {
        let registry = TemplateRegistry::new();
        let vars = HashMap::new();

        let result = registry.render("nonexistent", &vars);
        assert!(result.is_err());
    }

    #[test]
    fn test_code_template() {
        let template = Template::new("rust_fn", r#"fn {{name}}() -> {{return_type}} {
    {{body}}
}"#);

        let mut vars = HashMap::new();
        vars.insert("name".to_string(), "add_one".to_string());
        vars.insert("return_type".to_string(), "i32".to_string());
        vars.insert("body".to_string(), "42".to_string());

        let result = template.render(&vars).unwrap();
        assert!(result.contains("fn add_one()"));
        assert!(result.contains("i32"));
        assert!(result.contains("42"));
    }

    #[test]
    fn test_html_template() {
        let template = Template::new("html", r#"<h1>{{title}}</h1>
<p>{{content}}</p>"#);

        let mut vars = HashMap::new();
        vars.insert("title".to_string(), "Welcome".to_string());
        vars.insert("content".to_string(), "Hello World".to_string());

        let result = template.render(&vars).unwrap();
        assert!(result.contains("<h1>Welcome</h1>"));
        assert!(result.contains("<p>Hello World</p>"));
    }

    // ========================================================================
    // MCP TOOL TESTS
    // ========================================================================

    #[test]
    fn test_template_discovery_request() {
        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::All,
            parameter: None,
        };
        assert!(matches!(req.query_type, DiscoveryType::All));
    }

    #[test]
    fn test_template_discovery_by_name() {
        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByName,
            parameter: Some("my_template".to_string()),
        };
        assert_eq!(req.parameter, Some("my_template".to_string()));
    }

    #[test]
    fn test_template_metadata_creation() {
        let meta = TemplateMetadata {
            id: Uuid::new_v4().to_string(),
            name: "test".to_string(),
            description: "A test template".to_string(),
            category: Some("api".to_string()),
            language: Some("rust".to_string()),
            variables: vec!["name".to_string(), "type".to_string()],
        };
        assert_eq!(meta.variables.len(), 2);
    }

    #[test]
    fn test_render_request() {
        let mut vars = HashMap::new();
        vars.insert("name".to_string(), "test".to_string());
        let req = RenderRequest {
            template_name: "my_template".to_string(),
            variables: vars,
        };
        assert_eq!(req.template_name, "my_template");
    }

    #[test]
    fn test_render_result() {
        let result = RenderResult {
            success: true,
            output: "Hello World".to_string(),
            render_id: Uuid::new_v4().to_string(),
        };
        assert!(result.success);
        assert_eq!(result.output, "Hello World");
    }

    #[test]
    fn test_validate_request() {
        let req = ValidateRequest {
            template_content: "Hello {{name}}".to_string(),
        };
        assert!(req.template_content.contains("{{name}}"));
    }

    #[test]
    fn test_validate_result() {
        let result = ValidateResult {
            valid: true,
            errors: vec![],
            variable_count: 2,
        };
        assert!(result.valid);
        assert_eq!(result.variable_count, 2);
    }

    #[test]
    fn test_register_request() {
        let req = RegisterRequest {
            name: "new_template".to_string(),
            content: "Content {{var}}".to_string(),
            description: Some("A new template".to_string()),
            category: Some("api".to_string()),
            language: Some("rust".to_string()),
        };
        assert_eq!(req.name, "new_template");
    }

    #[test]
    fn test_register_result() {
        let result = RegisterResult {
            success: true,
            message: "Template registered".to_string(),
            template_id: Uuid::new_v4().to_string(),
        };
        assert!(result.success);
    }

    #[test]
    fn test_registry_discover_all() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("t1", "Content 1"));
        registry.register(Template::new("t2", "Content 2"));

        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::All,
            parameter: None,
        };

        let result = registry.discover(&req);
        assert_eq!(result.total_count, 2);
        assert_eq!(result.templates.len(), 2);
    }

    #[test]
    fn test_registry_discover_by_name() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("api_controller", "API content"));
        registry.register(Template::new("data_model", "Model content"));

        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByName,
            parameter: Some("api_controller".to_string()),
        };

        let result = registry.discover(&req);
        assert_eq!(result.total_count, 1);
        assert_eq!(result.templates[0].name, "api_controller");
    }

    #[test]
    fn test_registry_discover_by_category() {
        let mut registry = TemplateRegistry::new();
        registry.register_with_metadata(
            Template::new("api1", "API 1"),
            Some("controller".to_string()),
            Some("rust".to_string()),
        );
        registry.register_with_metadata(
            Template::new("api2", "API 2"),
            Some("controller".to_string()),
            Some("rust".to_string()),
        );
        registry.register_with_metadata(
            Template::new("model1", "Model"),
            Some("model".to_string()),
            Some("rust".to_string()),
        );

        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByCategory,
            parameter: Some("controller".to_string()),
        };

        let result = registry.discover(&req);
        assert_eq!(result.total_count, 2);
    }

    #[test]
    fn test_registry_discover_by_language() {
        let mut registry = TemplateRegistry::new();
        registry.register_with_metadata(
            Template::new("rust_api", "Rust API"),
            Some("api".to_string()),
            Some("rust".to_string()),
        );
        registry.register_with_metadata(
            Template::new("python_api", "Python API"),
            Some("api".to_string()),
            Some("python".to_string()),
        );

        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByLanguage,
            parameter: Some("rust".to_string()),
        };

        let result = registry.discover(&req);
        assert_eq!(result.total_count, 1);
        assert_eq!(result.templates[0].language, Some("rust".to_string()));
    }

    #[test]
    fn test_registry_validate_template() {
        let registry = TemplateRegistry::new();
        let content = "Hello {{name}}, you are {{age}} years old";

        let result = registry.validate_template(content);
        assert!(result.valid);
        assert_eq!(result.variable_count, 2);
    }

    #[test]
    fn test_registry_validate_empty_template() {
        let registry = TemplateRegistry::new();
        let content = "Static content only";

        let result = registry.validate_template(content);
        assert!(result.valid);
        assert_eq!(result.variable_count, 0);
    }

    #[test]
    fn test_extract_variables() {
        let content = "Hello {{name}}, welcome {{user}}";
        let vars = extract_variables(content);
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&"name".to_string()));
        assert!(vars.contains(&"user".to_string()));
    }

    #[test]
    fn test_extract_duplicate_variables() {
        let content = "{{var}} and {{var}} again";
        let vars = extract_variables(content);
        assert_eq!(vars.len(), 1); // Duplicates should be deduplicated
    }

    #[test]
    fn test_registry_get_metadata() {
        let mut registry = TemplateRegistry::new();
        registry.register_with_metadata(
            Template::new("api", "API content"),
            Some("controller".to_string()),
            Some("rust".to_string()),
        );

        let meta = registry.get_metadata("api").unwrap();
        assert_eq!(meta.name, "api");
        assert_eq!(meta.category, Some("controller".to_string()));
    }

    #[test]
    fn test_registry_list_metadata() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("t1", "Content 1"));
        registry.register(Template::new("t2", "Content 2"));

        let metas = registry.list_metadata();
        assert_eq!(metas.len(), 2);
    }

    #[test]
    fn test_template_discovery_empty_registry() {
        let registry = TemplateRegistry::new();
        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::All,
            parameter: None,
        };

        let result = registry.discover(&req);
        assert_eq!(result.total_count, 0);
    }

    #[test]
    fn test_template_discovery_nonexistent_name() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("existing", "Content"));

        let req = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByName,
            parameter: Some("nonexistent".to_string()),
        };

        let result = registry.discover(&req);
        assert_eq!(result.total_count, 0);
    }

    #[test]
    fn test_template_metadata_serialization() {
        let meta = TemplateMetadata {
            id: "123".to_string(),
            name: "test".to_string(),
            description: "Test template".to_string(),
            category: Some("api".to_string()),
            language: Some("rust".to_string()),
            variables: vec!["x".to_string()],
        };
        let json = serde_json::to_string(&meta).unwrap();
        assert!(json.contains("\"name\":\"test\""));
    }

    #[test]
    fn test_render_result_serialization() {
        let result = RenderResult {
            success: true,
            output: "Test output".to_string(),
            render_id: "render-123".to_string(),
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"success\":true"));
    }

    // ========================================================================
    // MCP SERVER INTEGRATION TESTS
    // ========================================================================

    #[tokio::test]
    async fn test_mcp_server_agent_discovery() {
        use crate::mcp_server::{McpServer, DiscoverTemplatesRequest};

        let server = McpServer::new();
        server.register_template(Template::new("api_controller", "API controller template")).await;
        server.register_template(Template::new("data_model", "Data model template")).await;

        let request = DiscoverTemplatesRequest {
            query: "*".to_string(),
            category_filter: None,
        };

        let response = server.discover_templates(request).await.unwrap();
        assert_eq!(response.total, 2);
    }

    #[tokio::test]
    async fn test_mcp_server_agent_template_application() {
        use crate::mcp_server::{McpServer, ApplyTemplateRequest};

        let server = McpServer::new();
        server.register_template(Template::new(
            "greeting",
            "Hello {{user_name}}, welcome!",
        )).await;

        let mut vars = std::collections::HashMap::new();
        vars.insert("user_name".to_string(), "Alice".to_string());

        let request = ApplyTemplateRequest {
            template_name: "greeting".to_string(),
            variables: vars,
        };

        let response = server.apply_template(request).await.unwrap();
        assert_eq!(response.rendered_output, "Hello Alice, welcome!");
    }

    #[tokio::test]
    async fn test_mcp_server_tool_discovery() {
        use crate::mcp_server::McpServer;

        let server = McpServer::new();
        let tools = server.list_tools();

        assert!(tools.iter().any(|t| t.name == "discover_templates"));
        assert!(tools.iter().any(|t| t.name == "apply_template"));
        assert!(tools.iter().any(|t| t.name == "list_tools"));
    }

    // ========================================================================
    // TEMPLATE DISCOVERY PATTERNS
    // ========================================================================

    #[test]
    fn test_discovery_by_category() {
        let mut registry = TemplateRegistry::new();
        registry.register_with_metadata(
            Template::new("rest_api", "REST API template"),
            Some("controller".to_string()),
            Some("rust".to_string()),
        );

        let request = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByCategory,
            parameter: Some("controller".to_string()),
        };

        let result = registry.discover(&request);
        assert!(result.total_count > 0);
    }

    #[test]
    fn test_discovery_by_language() {
        let mut registry = TemplateRegistry::new();
        registry.register_with_metadata(
            Template::new("python_class", "Python class template"),
            None,
            Some("python".to_string()),
        );

        let request = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByLanguage,
            parameter: Some("python".to_string()),
        };

        let result = registry.discover(&request);
        assert!(result.total_count > 0);
    }

    // ========================================================================
    // TEMPLATE RENDERING EDGE CASES
    // ========================================================================

    #[test]
    fn test_template_render_with_special_characters() {
        let template = Template::new(
            "special",
            "Code: {{code}} | Symbol: @ # $ %",
        );

        let mut vars = std::collections::HashMap::new();
        vars.insert("code".to_string(), "rust_var".to_string());

        let result = template.render(&vars).unwrap();
        assert!(result.contains("rust_var"));
        assert!(result.contains("@"));
    }

    #[test]
    fn test_template_render_empty_variables() {
        let template = Template::new("empty", "Static content only");
        let vars = std::collections::HashMap::new();

        let result = template.render(&vars).unwrap();
        assert_eq!(result, "Static content only");
    }

    #[test]
    fn test_template_render_unicode() {
        let template = Template::new("unicode", "Hello {{name}} 👋");
        let mut vars = std::collections::HashMap::new();
        vars.insert("name".to_string(), "世界".to_string());

        let result = template.render(&vars).unwrap();
        assert!(result.contains("世界"));
        assert!(result.contains("👋"));
    }

    // ========================================================================
    // TEMPLATE VARIABLE ANALYSIS
    // ========================================================================

    #[test]
    fn test_validate_template_with_many_variables() {
        let registry = TemplateRegistry::new();
        let content = "{{a}} {{b}} {{c}} {{d}} {{e}}";

        let validation = registry.validate_template(content);
        assert!(validation.valid);
        assert_eq!(validation.variable_count, 5);
    }

    #[test]
    fn test_extract_variables_nested() {
        let content = "{{outer_{{inner}}_var}}";
        let vars = extract_variables(content);
        // Nested syntax not supported, will match outer
        assert!(vars.len() > 0);
    }

    #[test]
    fn test_extract_variables_case_sensitive() {
        let content = "{{VAR}} and {{var}}";
        let vars = extract_variables(content);
        assert_eq!(vars.len(), 2);
    }

    // ========================================================================
    // ERROR HANDLING AND RECOVERY
    // ========================================================================

    #[test]
    fn test_template_error_missing_variable() {
        let template = Template::new("missing", "Hello {{name}}");
        let vars = std::collections::HashMap::new();

        let result = template.render(&vars);
        assert!(result.is_err());
    }

    #[test]
    fn test_template_error_types() {
        let err1 = TemplateError::TemplateNotFound("test".to_string());
        let err2 = TemplateError::VariableNotFound("var".to_string());
        let err3 = TemplateError::RenderingError("error".to_string());
        let err4 = TemplateError::InvalidSyntax("syntax".to_string());

        assert_eq!(format!("{:?}", err1).contains("TemplateNotFound"), true);
        assert_eq!(format!("{:?}", err2).contains("VariableNotFound"), true);
        assert_eq!(format!("{:?}", err3).contains("RenderingError"), true);
        assert_eq!(format!("{:?}", err4).contains("InvalidSyntax"), true);
    }

    // ========================================================================
    // REGISTRY OPERATIONS
    // ========================================================================

    #[test]
    fn test_registry_count() {
        let mut registry = TemplateRegistry::new();
        assert_eq!(registry.count(), 0);

        registry.register(Template::new("t1", "c1"));
        assert_eq!(registry.count(), 1);

        registry.register(Template::new("t2", "c2"));
        assert_eq!(registry.count(), 2);
    }

    #[test]
    fn test_registry_duplicate_registration() {
        let mut registry = TemplateRegistry::new();
        let t1 = Template::new("same_name", "content1");
        let t2 = Template::new("same_name", "content2");

        registry.register(t1);
        registry.register(t2);

        // Last one wins
        assert_eq!(registry.count(), 1);
    }

    #[test]
    fn test_registry_remove_template() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("temp", "content"));
        assert_eq!(registry.count(), 1);

        registry.templates.remove("temp");
        assert_eq!(registry.count(), 0);
    }

    // ========================================================================
    // JSON RENDERING
    // ========================================================================

    #[test]
    fn test_template_render_json_values() {
        let template = Template::new("json", "{{name}}: {{age}}");
        let json = serde_json::json!({
            "name": "Alice",
            "age": 30
        });

        let result = template.render_json(&json).unwrap();
        assert_eq!(result, "Alice: 30");
    }

    #[test]
    fn test_template_render_json_bool() {
        let template = Template::new("bool", "Active: {{enabled}}");
        let json = serde_json::json!({ "enabled": true });

        let result = template.render_json(&json).unwrap();
        assert!(result.contains("true"));
    }

    #[test]
    fn test_template_render_json_null() {
        let template = Template::new("null_test", "Value: {{data}}");
        let json = serde_json::json!({ "data": null });

        let result = template.render_json(&json).unwrap();
        assert!(result.contains("null"));
    }

    // ========================================================================
    // TEMPLATE DISCOVERY REQUEST PATTERNS
    // ========================================================================

    #[test]
    fn test_discovery_request_all() {
        let request = TemplateDiscoveryRequest {
            query_type: DiscoveryType::All,
            parameter: None,
        };
        assert_eq!(format!("{:?}", request.query_type), "All");
    }

    #[test]
    fn test_discovery_request_by_name() {
        let request = TemplateDiscoveryRequest {
            query_type: DiscoveryType::ByName,
            parameter: Some("api".to_string()),
        };
        assert_eq!(format!("{:?}", request.query_type), "ByName");
    }

    // ========================================================================
    // AGENT WORKFLOW SIMULATION
    // ========================================================================

    #[test]
    fn test_agent_discovers_and_applies_template() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new(
            "endpoint",
            "GET /{{path}} -> {{handler}}",
        ));

        // Agent discovery step
        let discovery = TemplateDiscoveryRequest {
            query_type: DiscoveryType::All,
            parameter: None,
        };
        let discovered = registry.discover(&discovery);
        assert_eq!(discovered.total_count, 1);

        // Agent application step
        let template = registry.get("endpoint").unwrap();
        let mut vars = std::collections::HashMap::new();
        vars.insert("path".to_string(), "users".to_string());
        vars.insert("handler".to_string(), "list_users".to_string());

        let result = template.render(&vars).unwrap();
        assert_eq!(result, "GET /users -> list_users");
    }

    #[test]
    fn test_multi_template_agent_workflow() {
        let mut registry = TemplateRegistry::new();
        registry.register(Template::new("struct", "struct {{name}} {{fields}}"));
        registry.register(Template::new("impl", "impl {{name}} {{methods}}"));

        let all_templates = TemplateDiscoveryRequest {
            query_type: DiscoveryType::All,
            parameter: None,
        };

        let result = registry.discover(&all_templates);
        assert_eq!(result.total_count, 2);
    }
}
