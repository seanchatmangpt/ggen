// AI Templates Example - Simple template engine
// Demonstrates: Template registration, rendering, variable substitution

use regex::Regex;
use serde_json::Value;
use std::collections::HashMap;
use thiserror::Error;

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
// TEMPLATE REGISTRY
// ============================================================================

pub struct TemplateRegistry {
    templates: HashMap<String, Template>,
}

impl TemplateRegistry {
    pub fn new() -> Self {
        Self {
            templates: HashMap::new(),
        }
    }

    pub fn register(&mut self, template: Template) {
        self.templates.insert(template.name.clone(), template);
    }

    pub fn get(&self, name: &str) -> TemplateResult<&Template> {
        self.templates
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

    pub fn count(&self) -> usize {
        self.templates.len()
    }
}

impl Default for TemplateRegistry {
    fn default() -> Self {
        Self::new()
    }
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
}
