//! Template context for variable resolution
//!
//! Provides context management for template variable substitution.

use anyhow::{Context as AnyhowContext, Result};
use serde_json::Value;
use std::collections::BTreeMap;
use tera::Context;

/// Template context for variable resolution
#[derive(Debug, Clone)]
pub struct TemplateContext {
    /// Variables for template rendering
    variables: BTreeMap<String, Value>,
}

impl TemplateContext {
    /// Create a new empty template context
    pub fn new() -> Self {
        Self {
            variables: BTreeMap::new(),
        }
    }

    /// Create from a map of variables
    pub fn from_map(variables: BTreeMap<String, String>) -> Result<Self> {
        let mut ctx = Self::new();
        for (key, value) in variables {
            ctx.set(key, value)?;
        }
        Ok(ctx)
    }

    /// Set a variable in the context
    pub fn set<K: Into<String>, V: Into<Value>>(&mut self, key: K, value: V) -> Result<()> {
        self.variables.insert(key.into(), value.into());
        Ok(())
    }

    /// Get a variable from the context
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.variables.get(key)
    }

    /// Get a variable as a string
    pub fn get_string(&self, key: &str) -> Option<String> {
        self.variables
            .get(key)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
    }

    /// Check if a variable exists
    pub fn contains(&self, key: &str) -> bool {
        self.variables.contains_key(key)
    }

    /// Merge another context into this one
    pub fn merge(&mut self, other: &TemplateContext) {
        for (key, value) in &other.variables {
            self.variables.insert(key.clone(), value.clone());
        }
    }

    /// Convert to Tera Context
    pub fn to_tera_context(&self) -> Result<Context> {
        let mut context = Context::new();

        for (key, value) in &self.variables {
            match value {
                Value::String(s) => context.insert(key, s),
                Value::Number(n) => context.insert(key, n),
                Value::Bool(b) => context.insert(key, b),
                Value::Array(arr) => context.insert(key, arr),
                Value::Object(obj) => context.insert(key, obj),
                Value::Null => context.insert(key, &Option::<String>::None),
            }
        }

        Ok(context)
    }

    /// Get all variable names
    pub fn variable_names(&self) -> Vec<&str> {
        self.variables.keys().map(|s| s.as_str()).collect()
    }

    /// Validate that all required variables are present
    pub fn validate_required(&self, required: &[String]) -> Result<()> {
        let missing: Vec<_> = required
            .iter()
            .filter(|var| !self.variables.contains_key(*var))
            .collect();

        if !missing.is_empty() {
            anyhow::bail!(
                "Missing required template variables: {}",
                missing
                    .iter()
                    .map(|v| v.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        Ok(())
    }

    /// Apply defaults for missing variables
    pub fn apply_defaults(&mut self, defaults: &BTreeMap<String, String>) {
        for (key, value) in defaults {
            if !self.variables.contains_key(key) {
                self.variables
                    .insert(key.clone(), Value::String(value.clone()));
            }
        }
    }

    /// Render a template string with this context
    pub fn render_string(&self, template: &str) -> Result<String> {
        let mut tera = tera::Tera::default();
        let context = self.to_tera_context()?;

        tera.render_str(template, &context)
            .context("Failed to render template string")
    }

    /// Clone the variables map
    pub fn variables(&self) -> &BTreeMap<String, Value> {
        &self.variables
    }
}

impl Default for TemplateContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_context() {
        let ctx = TemplateContext::new();
        assert!(ctx.variables.is_empty());
    }

    #[test]
    fn test_set_and_get() {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();

        assert_eq!(ctx.get_string("name"), Some("test".to_string()));
        assert!(ctx.contains("name"));
    }

    #[test]
    fn test_from_map() {
        let mut vars = BTreeMap::new();
        vars.insert("service_name".to_string(), "my-service".to_string());
        vars.insert("port".to_string(), "8080".to_string());

        let ctx = TemplateContext::from_map(vars).unwrap();

        assert_eq!(
            ctx.get_string("service_name"),
            Some("my-service".to_string())
        );
        assert_eq!(ctx.get_string("port"), Some("8080".to_string()));
    }

    #[test]
    fn test_merge() {
        let mut ctx1 = TemplateContext::new();
        ctx1.set("name", "test1").unwrap();

        let mut ctx2 = TemplateContext::new();
        ctx2.set("port", "8080").unwrap();

        ctx1.merge(&ctx2);

        assert_eq!(ctx1.get_string("name"), Some("test1".to_string()));
        assert_eq!(ctx1.get_string("port"), Some("8080".to_string()));
    }

    #[test]
    fn test_validate_required() {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();

        let required = vec!["name".to_string(), "port".to_string()];
        let result = ctx.validate_required(&required);

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("port"));
    }

    #[test]
    fn test_apply_defaults() {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();

        let mut defaults = BTreeMap::new();
        defaults.insert("name".to_string(), "default-name".to_string());
        defaults.insert("port".to_string(), "8080".to_string());

        ctx.apply_defaults(&defaults);

        // Existing value should not be overwritten
        assert_eq!(ctx.get_string("name"), Some("test".to_string()));
        // Default should be applied for missing value
        assert_eq!(ctx.get_string("port"), Some("8080".to_string()));
    }

    #[test]
    fn test_render_string() {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "World").unwrap();
        ctx.set("count", 42).unwrap();

        let rendered = ctx
            .render_string("Hello, {{ name }}! Count: {{ count }}")
            .unwrap();
        assert_eq!(rendered, "Hello, World! Count: 42");
    }

    #[test]
    fn test_variable_names() {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();
        ctx.set("port", "8080").unwrap();

        let names = ctx.variable_names();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&"name"));
        assert!(names.contains(&"port"));
    }
}
