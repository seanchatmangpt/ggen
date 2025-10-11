//! AI-powered template generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::generators::validator::{TemplateValidator, ValidationResult};
use crate::prompts::TemplatePromptBuilder;
use futures::StreamExt;
use ggen_core::Template;
use std::sync::Arc;

/// AI-powered template generator
#[derive(Debug)]
pub struct TemplateGenerator {
    client: Arc<dyn LlmClient>,
}

impl TemplateGenerator {
    /// Create a new template generator
    pub fn new(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Create a new template generator with custom config
    pub fn with_config(client: Arc<dyn LlmClient>, _config: LlmConfig) -> Self {
        Self { client }
    }

    /// Create a new template generator with a client
    pub fn with_client(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Generate a template from natural language description
    pub async fn generate_template(
        &self, description: &str, examples: Vec<&str>,
    ) -> Result<Template> {
        let prompt = TemplatePromptBuilder::new(description.to_string())
            .with_examples(examples.iter().map(|s| s.to_string()).collect())
            .build()?;

        let response = self.client.complete(&prompt).await?;

        // Parse the generated template
        self.parse_template(&response.content)
    }

    /// Generate a template with streaming
    pub async fn generate_template_stream(
        &self, description: &str, examples: Vec<&str>,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prompt = TemplatePromptBuilder::new(description.to_string())
            .with_examples(examples.iter().map(|s| s.to_string()).collect())
            .build()?;

        let stream = self.client.complete_stream(&prompt).await?;

        Ok(Box::pin(stream.map(|chunk| Ok(chunk.content))))
    }

    /// Get the LLM client
    pub fn client(&self) -> &Arc<dyn LlmClient> {
        &self.client
    }

    /// Get the current configuration
    pub fn config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    /// Update the configuration
    pub fn set_config(&mut self, _config: LlmConfig) {
        // Note: This would require mutable access to the client
        // For now, we'll skip this functionality
    }

    /// Generate a REST controller template
    pub async fn generate_rest_controller(
        &self, description: &str, language: &str, framework: &str,
    ) -> Result<Template> {
        let examples = vec![
            format!("REST API controller for {}", description),
            format!("Using {} with {}", language, framework),
        ];

        let example_refs: Vec<&str> = examples.iter().map(|s| s.as_str()).collect();
        self.generate_template(description, example_refs).await
    }

    /// Generate a data model template
    pub async fn generate_data_model(&self, description: &str, language: &str) -> Result<Template> {
        let examples = vec![
            format!("Data model for {}", description),
            format!("Using {}", language),
        ];

        let example_refs: Vec<&str> = examples.iter().map(|s| s.as_str()).collect();
        self.generate_template(description, example_refs).await
    }

    /// Validate a generated template
    pub async fn validate_template(&self, template: &Template) -> Result<ValidationResult> {
        let validator = TemplateValidator::new();
        validator.validate_template(template).await
    }

    /// Parse generated template content
    fn parse_template(&self, content: &str) -> Result<Template> {
        // Extract template content from markdown code blocks if present
        let template_content = if content.contains("```yaml") {
            // Extract content between ```yaml and ```
            let start = content.find("```yaml").ok_or_else(|| {
                GgenAiError::template_generation(
                    "Could not find opening ```yaml marker".to_string(),
                )
            })?;
            let search_start = start + 7;
            let end_offset = content[search_start..].find("```").ok_or_else(|| {
                GgenAiError::template_generation("Could not find closing ``` marker".to_string())
            })?;
            let yaml_content = &content[search_start..search_start + end_offset].trim();

            // Check if this is already a complete template with frontmatter
            if yaml_content.starts_with("---") {
                yaml_content.to_string()
            } else {
                // Wrap in frontmatter if not present
                format!("---\n{}\n---\nTemplate content", yaml_content)
            }
        } else if content.contains("---") && content.matches("---").count() >= 2 {
            // Handle direct template format without code blocks
            content.to_string()
        } else {
            // Fallback: wrap content in basic template structure
            format!(
                "---\nto: \"generated.tmpl\"\nvars:\n  name: \"example\"\n---\n{}",
                content
            )
        };

        // Parse using ggen-core
        let mut template = Template::parse(&template_content)?;

        // Render frontmatter to populate the front field
        let mut tera = tera::Tera::default();
        let ctx = tera::Context::new();
        template
            .render_frontmatter(&mut tera, &ctx)
            .map_err(|e| GgenAiError::template_generation(&e.to_string()))?;

        Ok(template)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_template_generation() {
        let client = MockClient::with_response(
            "---\nto: \"test.tmpl\"\nvars:\n  name: \"test\"\n---\nHello {{ name }}!",
        );
        let generator = TemplateGenerator::new(Arc::new(client));

        let template = generator
            .generate_template("A simple greeting template", vec!["Include name variable"])
            .await
            .expect("Template generation should succeed in test");

        assert_eq!(template.body, "Hello {{ name }}!");
    }

    #[tokio::test]
    async fn test_template_generation_with_markdown() {
        let client = MockClient::with_response(
            "```yaml\n---\nto: \"test.tmpl\"\nvars:\n  name: \"test\"\n---\nHello {{ name }}!\n```",
        );
        let generator = TemplateGenerator::new(Arc::new(client));

        let template = generator
            .generate_template("A simple greeting template", vec!["Include name variable"])
            .await
            .expect("Template generation should succeed in test");

        assert_eq!(template.body, "Hello {{ name }}!");
    }
}
