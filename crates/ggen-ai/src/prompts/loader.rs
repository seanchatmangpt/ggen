//! Prompt template loader - "Eating our own dogfood"
//!
//! This module uses ggen's own template system to load and render AI prompts.
//! Instead of hardcoded strings, we use `.tmpl` files with YAML frontmatter.

use crate::error::{GgenAiError, Result};
use ggen_core::Template;
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use tera::Tera;

/// Prompt template loader using ggen's template system
#[derive(Debug)]
pub struct PromptTemplateLoader {
    templates_dir: PathBuf,
    tera: Tera,
}

impl PromptTemplateLoader {
    /// Create a new prompt template loader
    pub fn new() -> Result<Self> {
        // Find templates directory relative to this crate
        let templates_dir = Self::find_templates_dir()?;

        // Initialize Tera template engine
        let tera = Tera::default();

        Ok(Self {
            templates_dir,
            tera,
        })
    }

    /// Find the templates directory
    fn find_templates_dir() -> Result<PathBuf> {
        // Try multiple locations
        let candidates = vec![
            // Relative to cargo workspace
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("templates/prompts"),
            // Relative to current directory
            PathBuf::from("ggen-ai/templates/prompts"),
            PathBuf::from("templates/prompts"),
        ];

        for candidate in candidates {
            if candidate.exists() && candidate.is_dir() {
                return Ok(candidate);
            }
        }

        Err(GgenAiError::configuration(
            "Could not find prompt templates directory. Expected at ggen-ai/templates/prompts",
        ))
    }

    /// Load a prompt template by category and name
    pub fn load_template(&self, category: &str, name: &str) -> Result<Template> {
        let template_path = self
            .templates_dir
            .join(category)
            .join(format!("{}.tmpl", name));

        if !template_path.exists() {
            return Err(GgenAiError::configuration(format!(
                "Prompt template not found: {}/{}",
                category, name
            )));
        }

        let content = std::fs::read_to_string(&template_path).map_err(|e| {
            GgenAiError::configuration(format!(
                "Failed to read prompt template {}/{}: {}",
                category, name, e
            ))
        })?;

        Template::parse(&content).map_err(|e| {
            GgenAiError::configuration(format!(
                "Failed to parse prompt template {}/{}: {}",
                category, name, e
            ))
        })
    }

    /// Render a prompt template with variables
    pub fn render_prompt(
        &mut self, category: &str, name: &str, vars: HashMap<String, Value>,
    ) -> Result<String> {
        let template = self.load_template(category, name)?;

        // Create Tera context from vars
        let mut context = tera::Context::new();
        for (key, value) in vars {
            // Convert serde_json::Value to tera::Value
            match value {
                Value::String(s) => context.insert(&key, &s),
                Value::Number(n) => {
                    if let Some(i) = n.as_i64() {
                        context.insert(&key, &i);
                    } else if let Some(f) = n.as_f64() {
                        context.insert(&key, &f);
                    }
                }
                Value::Bool(b) => context.insert(&key, &b),
                Value::Array(arr) => {
                    // Convert array elements
                    let tera_arr: Vec<String> = arr
                        .iter()
                        .filter_map(|v| v.as_str().map(|s| s.to_string()))
                        .collect();
                    context.insert(&key, &tera_arr);
                }
                Value::Object(obj) => {
                    // For nested objects, insert as JSON string
                    context.insert(&key, &serde_json::to_string(&obj).unwrap_or_default());
                }
                Value::Null => {}
            }
        }

        // Render template
        template
            .render(&mut self.tera, &context)
            .map_err(|e| GgenAiError::configuration(format!("Failed to render prompt: {}", e)))
    }

    /// Convenience method for natural language search prompts
    pub fn render_natural_search(&mut self, query: &str) -> Result<String> {
        let mut vars = HashMap::new();
        vars.insert("query".to_string(), Value::String(query.to_string()));
        self.render_prompt("natural_search", "search", vars)
    }

    /// Convenience method for SPARQL query generation prompts
    pub fn render_sparql_query(
        &mut self, intent: &str, schema: Option<&str>, prefixes: Vec<(String, String)>,
        examples: Vec<String>, constraints: Vec<String>,
    ) -> Result<String> {
        let mut vars = HashMap::new();
        vars.insert("intent".to_string(), Value::String(intent.to_string()));

        if let Some(schema_str) = schema {
            vars.insert("schema".to_string(), Value::String(schema_str.to_string()));
        }

        // Convert prefixes to JSON array
        let prefixes_json: Vec<Value> = prefixes
            .into_iter()
            .map(|(k, v)| Value::Array(vec![Value::String(k), Value::String(v)]))
            .collect();
        vars.insert("prefixes".to_string(), Value::Array(prefixes_json));

        vars.insert(
            "examples".to_string(),
            Value::Array(examples.into_iter().map(Value::String).collect()),
        );
        vars.insert(
            "constraints".to_string(),
            Value::Array(constraints.into_iter().map(Value::String).collect()),
        );

        self.render_prompt("sparql", "query_generation", vars)
    }

    /// Convenience method for template generation prompts
    pub fn render_template_generation(
        &mut self, description: &str, examples: Vec<String>, requirements: Vec<String>,
        language: Option<&str>, framework: Option<&str>,
    ) -> Result<String> {
        let mut vars = HashMap::new();
        vars.insert(
            "description".to_string(),
            Value::String(description.to_string()),
        );
        vars.insert(
            "examples".to_string(),
            Value::Array(examples.into_iter().map(Value::String).collect()),
        );
        vars.insert(
            "requirements".to_string(),
            Value::Array(requirements.into_iter().map(Value::String).collect()),
        );

        if let Some(lang) = language {
            vars.insert("language".to_string(), Value::String(lang.to_string()));
        }
        if let Some(fw) = framework {
            vars.insert("framework".to_string(), Value::String(fw.to_string()));
        }

        self.render_prompt("template", "generation", vars)
    }

    /// Convenience method for ontology generation prompts
    pub fn render_ontology_generation(
        &mut self, domain: &str, requirements: Vec<String>, examples: Vec<String>,
        format: Option<&str>,
    ) -> Result<String> {
        let mut vars = HashMap::new();
        vars.insert("domain".to_string(), Value::String(domain.to_string()));
        vars.insert(
            "requirements".to_string(),
            Value::Array(requirements.into_iter().map(Value::String).collect()),
        );
        vars.insert(
            "examples".to_string(),
            Value::Array(examples.into_iter().map(Value::String).collect()),
        );

        if let Some(fmt) = format {
            vars.insert("format".to_string(), Value::String(fmt.to_string()));
        }

        self.render_prompt("ontology", "generation", vars)
    }

    /// Convenience method for code generation prompts
    pub fn render_code_generation(
        &mut self, description: &str, language: &str, framework: Option<&str>,
        requirements: Vec<String>, examples: Vec<String>, patterns: Vec<String>,
    ) -> Result<String> {
        let mut vars = HashMap::new();
        vars.insert(
            "description".to_string(),
            Value::String(description.to_string()),
        );
        vars.insert("language".to_string(), Value::String(language.to_string()));
        vars.insert(
            "requirements".to_string(),
            Value::Array(requirements.into_iter().map(Value::String).collect()),
        );
        vars.insert(
            "examples".to_string(),
            Value::Array(examples.into_iter().map(Value::String).collect()),
        );
        vars.insert(
            "patterns".to_string(),
            Value::Array(patterns.into_iter().map(Value::String).collect()),
        );

        if let Some(fw) = framework {
            vars.insert("framework".to_string(), Value::String(fw.to_string()));
        }

        self.render_prompt("code", "generation", vars)
    }
}

// NOTE: Default implementation removed - PromptTemplateLoader::new() can fail (file system)
// Use PromptTemplateLoader::new() explicitly and handle the Result
// This prevents panic!() in production code per PHASE 1.1 requirements

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_templates_dir() {
        let dir = PromptTemplateLoader::find_templates_dir();
        // Should find the directory we just created
        // Note: This test verifies the function works regardless of feature flags
        assert!(dir.is_ok());
    }

    #[test]
    fn test_render_natural_search() {
        if let Ok(mut loader) = PromptTemplateLoader::new() {
            let prompt = loader
                .render_natural_search("I need authentication")
                .unwrap();
            assert!(prompt.contains("authentication"));
            assert!(prompt.contains("User Query"));
        }
    }

    #[test]
    fn test_render_sparql_query() {
        if let Ok(mut loader) = PromptTemplateLoader::new() {
            let prompt = loader
                .render_sparql_query(
                    "Find all users",
                    Some("User class defined"),
                    vec![("ex".to_string(), "http://example.org/".to_string())],
                    vec!["SELECT ?user WHERE { ?user a ex:User }".to_string()],
                    vec!["Return user names".to_string()],
                )
                .unwrap();
            assert!(prompt.contains("Find all users"));
            assert!(prompt.contains("SPARQL"));
        }
    }
}
