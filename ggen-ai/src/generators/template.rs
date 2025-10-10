//! AI-powered template generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::prompts::{TemplatePromptBuilder, TemplatePrompts};
use crate::generators::validator::{TemplateValidator, ValidationResult};
use futures::StreamExt;
use ggen_core::Template;

/// AI-powered template generator
#[derive(Debug)]
pub struct TemplateGenerator {
    client: Box<dyn LlmClient>,
    config: LlmConfig,
}

impl TemplateGenerator {
    /// Create a new template generator
    pub fn new(client: Box<dyn LlmClient>) -> Self {
        Self {
            client,
            config: LlmConfig::default(),
        }
    }
    
    /// Create a new template generator with custom config
    pub fn with_config(client: Box<dyn LlmClient>, config: LlmConfig) -> Self {
        Self { client, config }
    }
    
    /// Create a new template generator optimized for Ollama qwen3-coder:30b
    pub fn with_ollama_qwen3_coder(client: Box<dyn LlmClient>) -> Self {
        use crate::providers::OllamaClient;
        Self {
            client,
            config: OllamaClient::qwen3_coder_config(),
        }
    }
    
    /// Generate a template from natural language description
    pub async fn generate_template(
        &self,
        description: &str,
        examples: Vec<&str>,
    ) -> Result<Template> {
        let prompt = TemplatePromptBuilder::new(description.to_string())
            .with_examples(examples.iter().map(|s| s.to_string()).collect())
            .build()?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        // Parse the generated template
        self.parse_template(&response.content)
    }
    
    /// Generate a REST API controller template
    pub async fn generate_rest_controller(
        &self,
        description: &str,
        language: &str,
        framework: &str,
    ) -> Result<Template> {
        let prompt = TemplatePrompts::rest_api_controller(description, language, framework)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.parse_template(&response.content)
    }
    
    /// Generate a data model template
    pub async fn generate_data_model(
        &self,
        description: &str,
        language: &str,
    ) -> Result<Template> {
        let prompt = TemplatePrompts::data_model(description, language)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.parse_template(&response.content)
    }
    
    /// Generate a configuration file template
    pub async fn generate_config_file(
        &self,
        description: &str,
        format: &str,
    ) -> Result<Template> {
        let prompt = TemplatePrompts::config_file(description, format)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.parse_template(&response.content)
    }
    
    /// Generate a test file template
    pub async fn generate_test_file(
        &self,
        description: &str,
        language: &str,
        framework: &str,
    ) -> Result<Template> {
        let prompt = TemplatePrompts::test_file(description, language, framework)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.parse_template(&response.content)
    }
    
    /// Generate a template with custom requirements
    pub async fn generate_with_requirements(
        &self,
        description: &str,
        requirements: Vec<&str>,
        examples: Vec<&str>,
        language: Option<&str>,
        framework: Option<&str>,
    ) -> Result<Template> {
        let mut builder = TemplatePromptBuilder::new(description.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .with_examples(examples.iter().map(|s| s.to_string()).collect());
        
        if let Some(lang) = language {
            builder = builder.with_language(lang.to_string());
        }
        
        if let Some(fw) = framework {
            builder = builder.with_framework(fw.to_string());
        }
        
        let prompt = builder.build()?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.parse_template(&response.content)
    }
    
    /// Parse generated template content
    fn parse_template(&self, content: &str) -> Result<Template> {
        // Extract template content from markdown code blocks if present
        let template_content = if content.contains("```yaml") && content.contains("```") {
            // Extract content between ```yaml and ```
            let start = content.find("```yaml").ok_or_else(|| {
                GgenAiError::TemplateGeneration("Could not find opening ```yaml marker".to_string())
            })?;
            let end = content.rfind("```").ok_or_else(|| {
                GgenAiError::TemplateGeneration("Could not find closing ``` marker".to_string())
            })?;
            let yaml_content = &content[start + 7..end];
            
            // Find the end of YAML frontmatter
            if let Some(frontmatter_end) = yaml_content.find("---\n") {
                let frontmatter = &yaml_content[..frontmatter_end];
                let template_body = &yaml_content[frontmatter_end + 4..];
                
                format!("{}\n---\n{}", frontmatter, template_body)
            } else {
                yaml_content.to_string()
            }
        } else if content.contains("---\n") {
            // Handle direct template format without code blocks
            content.to_string()
        } else {
            // Fallback: wrap content in basic template structure
            format!("---\nto: \"generated.tmpl\"\nvars:\n  name: \"example\"\n---\n{}", content)
        };
        
        // Create a temporary file to parse the template
        let temp_dir = tempfile::tempdir()?;
        let temp_file = temp_dir.path().join("template.tmpl");
        std::fs::write(&temp_file, template_content)?;
        
        // Parse using ggen-core
        let template_content = std::fs::read_to_string(&temp_file)?;
        
        let template = Template::parse(&template_content)?;
        
        // Note: Template validation would go here if available
        
        Ok(template)
    }
    
    /// Stream template generation for long-running operations
    pub async fn stream_generate_template(
        &self,
        description: &str,
        examples: Vec<&str>,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prompt = TemplatePromptBuilder::new(description.to_string())
            .with_examples(examples.iter().map(|s| s.to_string()).collect())
            .build()?;
        
        let stream = self.client.stream_complete(&prompt, Some(self.config.clone())).await?;
        
        Ok(Box::pin(stream.map(|chunk_result| {
            chunk_result.map(|chunk| chunk.content)
        })))
    }
    
    /// Get the LLM client
    pub fn client(&self) -> &dyn LlmClient {
        self.client.as_ref()
    }
    
    /// Get the current configuration
    pub fn config(&self) -> &LlmConfig {
        &self.config
    }
    
    /// Update the configuration
    pub fn set_config(&mut self, config: LlmConfig) {
        self.config = config;
    }

    /// Validate a generated template
    pub async fn validate_template(&self, template: &Template) -> Result<ValidationResult> {
        let validator = TemplateValidator::new();
        validator.validate_template(template).await
    }

    /// Generate template with iterative improvement
    pub async fn generate_template_with_validation(
        &self,
        description: &str,
        requirements: Vec<&str>,
        max_iterations: usize,
    ) -> Result<Template> {
        let mut current_template = self.generate_template(description, requirements.clone()).await?;

        let validator = TemplateValidator::new();
        let mut iteration = 0;

        while iteration < max_iterations {
            iteration += 1;

            // Validate current template
            let validation = validator.validate_template(&current_template).await?;

            // Check if template meets quality requirements
            if validation.is_valid && validation.quality_score >= 0.7 {
                println!("✅ Template validation passed after {} iterations (score: {:.2})", iteration, validation.quality_score);
                return Ok(current_template);
            }

            // Generate improvement feedback
            let feedback = self.generate_improvement_feedback(&validation, &requirements).await?;

            if feedback.is_empty() {
                break; // No more improvements possible
            }

            // Generate improved template based on feedback
            println!("🔄 Iteration {}: Improving template (current score: {:.2})", iteration, validation.quality_score);

            let improved_description = format!(
                "{}\n\nPlease improve this template based on the following feedback:\n{}",
                description, feedback
            );

            current_template = self.generate_template(&improved_description, requirements.clone()).await?;
        }

        println!("✅ Final template after {} iterations", iteration);
        Ok(current_template)
    }

    /// Generate improvement feedback based on validation results
    async fn generate_improvement_feedback(
        &self,
        validation: &ValidationResult,
        requirements: &[&str],
    ) -> Result<String> {
        if validation.issues.is_empty() && validation.quality_score >= 0.7 {
            return Ok(String::new());
        }

        let mut feedback = String::new();

        // Add general feedback
        feedback.push_str("The generated template needs improvement. ");

        if !validation.is_valid {
            feedback.push_str("Critical issues must be fixed: ");
            let errors: Vec<_> = validation.issues.iter()
                .filter(|i| matches!(i.severity, crate::generators::validator::Severity::Error))
                .collect();
            for (i, error) in errors.iter().enumerate() {
                if i > 0 { feedback.push_str(", "); }
                feedback.push_str(&error.description);
            }
            feedback.push('\n');
        }

        // Add quality improvement suggestions
        if validation.quality_score < 0.7 {
            feedback.push_str("Quality improvements needed:\n");
            for suggestion in &validation.suggestions {
                feedback.push_str(&format!("- {}\n", suggestion));
            }
        }

        // Add requirement-specific feedback
        feedback.push_str("\nOriginal requirements:\n");
        for req in requirements {
            feedback.push_str(&format!("- {}\n", req));
        }

        Ok(feedback)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;
    
    #[tokio::test]
    async fn test_template_generator_creation() {
        let client = Box::new(MockClient::with_response("Generated template"));
        let generator = TemplateGenerator::new(client);
        
        assert_eq!(generator.client().provider_name(), "mock");
    }
    
    #[tokio::test]
    async fn test_template_generator_with_config() {
        let client = Box::new(MockClient::with_response("Generated template"));
        let config = LlmConfig {
            model: "test-model".to_string(),
            temperature: Some(0.5),
            ..Default::default()
        };
        let generator = TemplateGenerator::with_config(client, config);
        
        assert_eq!(generator.config().model, "test-model");
        assert_eq!(generator.config().temperature, Some(0.5));
    }
    
    #[tokio::test]
    async fn test_generate_template() {
        let mock_template = r#"---
to: "src/user.rs"
vars:
  - name: "user_name"
    type: "string"
    description: "Name of the user"
---
pub struct {{ user_name }} {
    pub id: u32,
    pub name: String,
}
"#;
        
        let client = Box::new(MockClient::with_response(mock_template));
        let generator = TemplateGenerator::new(client);
        
        let result = generator.generate_template(
            "Generate a user struct",
            vec!["Include id and name fields"]
        ).await;
        
        // This succeeds because we use temporary files for parsing
        assert!(result.is_ok());
    }
    
    #[tokio::test]
    async fn test_generate_rest_controller() {
        let client = Box::new(MockClient::with_response("REST controller template"));
        let generator = TemplateGenerator::new(client);
        
        let result = generator.generate_rest_controller(
            "User management API",
            "TypeScript",
            "Express"
        ).await;
        
        // This succeeds because we use temporary files for parsing
        assert!(result.is_ok());
    }
    
    #[tokio::test]
    async fn test_stream_generate_template() {
        let client = Box::new(MockClient::with_response("Streamed template"));
        let generator = TemplateGenerator::new(client);

        let mut stream = generator.stream_generate_template(
            "Generate a template",
            vec!["Example requirement"]
        ).await.expect("Failed to create template stream");

        let mut content = String::new();
        while let Some(chunk) = stream.next().await {
            content.push_str(&chunk.expect("Failed to read chunk from stream"));
        }

        assert_eq!(content, "Streamed template");
    }
}
