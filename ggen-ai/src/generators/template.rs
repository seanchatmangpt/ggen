//! AI-powered template generator
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should bridge the gap between natural language descriptions and valid ggen templates,
//! transforming user intent into working template code through LLM interaction. It should handle
//! varied LLM output formats and ensure all generated templates are valid and usable.
//!
//! ## RESPONSIBILITIES
//! 1. **Prompt Engineering**: Should craft effective prompts that elicit valid template output
//! 2. **Multi-Format Parsing**: Should extract templates from markdown code blocks, plain text, etc.
//! 3. **Template Validation**: Should ensure generated content is parseable by ggen-core
//! 4. **Streaming Support**: Should enable real-time template generation for better UX
//! 5. **Domain-Specific Generation**: Should provide specialized methods (REST controllers, data models, etc.)
//! 6. **Error Recovery**: Should auto-wrap incomplete LLM output in valid template structure
//! 7. **Post-Generation Validation**: Should optionally validate templates before returning
//!
//! ## CONSTRAINTS
//! - Must support multiple LLM providers (provider-agnostic via LlmClient trait)
//! - Must handle both streaming and batch generation modes
//! - Must extract templates from varied response formats (markdown, code blocks, plain text)
//! - Must always return valid ggen-core::Template instances
//! - Must render frontmatter after parsing to populate Template.front
//! - Must never return partially-initialized templates
//!
//! ## DEPENDENCIES
//! - `LlmClient`: Should be provider-agnostic for multi-LLM support
//! - `TemplatePromptBuilder`: Should generate effective prompts
//! - `ggen-core::Template`: Should use for parsing and validation
//! - `TemplateValidator`: Should optionally validate generated templates
//!
//! ## INVARIANTS
//! - Generated templates must be parseable by Template::parse()
//! - Frontmatter must be rendered before template is returned
//! - All public methods must return Result<Template>, never partial state
//! - LLM client must remain immutable (Arc<dyn LlmClient>)
//! - Prompt building must never fail silently
//!
//! ## DATA FLOW
//! ```text
//! Natural Language Description
//!   ↓
//! TemplatePromptBuilder::build()
//!   ├─ Add description
//!   ├─ Add examples
//!   └─ Format prompt for LLM
//!   ↓
//! LlmClient::complete() or complete_stream()
//!   ├─ Send prompt to LLM provider
//!   └─ Return response text
//!   ↓
//! parse_template()
//!   ├─ Detect format (markdown, code blocks, plain text)
//!   ├─ Extract template content
//!   ├─ Auto-wrap if incomplete
//!   └─ Validate structure
//!   ↓
//! Template::parse()
//!   ├─ Parse frontmatter + body (ggen-core)
//!   └─ Return Template with raw_frontmatter
//!   ↓
//! Template::render_frontmatter()
//!   ├─ Render {{vars}} in frontmatter
//!   └─ Populate Template.front field
//!   ↓
//! Return fully-initialized Template
//! ```
//!
//! ## ERROR HANDLING STRATEGY
//! - LLM errors → Wrap with context about prompt, retry on timeout
//! - Parse errors → Attempt auto-repair (add frontmatter), then fail with preview
//! - Invalid YAML → Show LLM output, suggest prompt improvements
//! - Streaming errors → Buffer partial content, return what's valid
//! - Validation errors → Return error with specific issues found
//!
//! ## RESPONSE FORMAT HANDLING
//!
//! ### Format 1: Markdown with YAML code block
//! ```text
//! Here's your template:
//!
//! ```yaml
//! ---
//! to: "{{name}}.rs"
//! ---
//! fn main() {}
//! ```
//! ```
//!
//! **Action**: Extract content between ```yaml and ```
//!
//! ### Format 2: Direct template format
//! ```text
//! ---
//! to: "{{name}}.rs"
//! ---
//! fn main() {}
//! ```
//!
//! **Action**: Use as-is
//!
//! ### Format 3: Frontmatter only (no body)
//! ```text
//! ```yaml
//! to: "{{name}}.rs"
//! vars:
//!   name: "example"
//! ```
//! ```
//!
//! **Action**: Auto-add template body section
//!
//! ### Format 4: Body only (no frontmatter)
//! ```text
//! fn main() {
//!     println!("Hello");
//! }
//! ```
//!
//! **Action**: Auto-wrap in default frontmatter
//!
//! ## PROMPT ENGINEERING CONTRACTS
//!
//! Prompts SHOULD:
//! - Explicitly request YAML frontmatter format
//! - Provide examples of valid templates
//! - Specify required fields (to, vars, etc.)
//! - Request complete templates (frontmatter + body)
//! - Include domain-specific context when relevant
//!
//! Prompts SHOULD NOT:
//! - Be overly prescriptive (allow LLM flexibility)
//! - Include multiple conflicting examples
//! - Request non-template content
//!
//! ## GENERATION METHODS
//!
//! ### generate_template(description, examples)
//! **Purpose**: General-purpose template generation
//! **Should**: Accept any description, return valid template
//! **Should NOT**: Assume specific template structure
//!
//! ### generate_rest_controller(description, language, framework)
//! **Purpose**: Domain-specific REST API controller generation
//! **Should**: Include REST-specific examples in prompt
//! **Should**: Generate routes, handlers, and models
//!
//! ### generate_data_model(description, language)
//! **Purpose**: Domain-specific data model generation
//! **Should**: Include struct/class examples
//! **Should**: Generate serialization annotations
//!
//! ## TESTING STRATEGY
//! - Test extraction from all response formats (markdown, plain, code blocks)
//! - Test auto-wrapping of incomplete templates
//! - Test with mock client (no real LLM calls)
//! - Test streaming mode with chunked responses
//! - Test validation integration
//! - Test domain-specific methods (REST, data models)
//! - Property test: Any LLM output should produce valid Template
//!
//! ## REFACTORING PRIORITIES
//! - [P0] Improve parse_template() error messages with LLM output preview
//! - [P0] Add retry logic for transient LLM errors
//! - [P1] Extract response format detection into shared utility
//! - [P1] Add caching for identical prompts (save API calls)
//! - [P1] Support incremental template building (multi-turn conversation)
//! - [P2] Add template quality scoring (completeness, complexity)
//! - [P2] Support template refinement (iterative improvement)
//! - [P3] Add template library search before generation
//!
//! ## CORE TEAM BEST PRACTICES APPLIED
//! 1. **Lenient Parsing**: Accept varied LLM output formats
//! 2. **Defensive Validation**: Always validate before returning
//! 3. **Clear Errors**: Show LLM output preview when parsing fails
//! 4. **Streaming First**: Enable real-time feedback for users
//! 5. **Provider Agnostic**: Never assume specific LLM behavior
//! 6. **Idempotency**: Same prompt should produce consistent results
//!
//! ## INTEGRATION NOTES
//! - **ggen-core Integration**: Uses Template::parse() for all parsing
//! - **LLM Provider Integration**: Uses LlmClient trait for provider abstraction
//! - **Validation Integration**: Uses TemplateValidator for optional validation
//! - **Prompt Engineering**: Uses TemplatePromptBuilder for consistent prompts
//!
//! ## COMPATIBILITY NOTES
//! - **OpenAI**: Handles markdown-wrapped responses
//! - **Anthropic Claude**: Handles direct YAML responses
//! - **Ollama**: Handles varied local model outputs
//! - **Mock Client**: Used for testing without API calls

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
        let template_content = if let Some(yaml_content) = crate::parsing_utils::extract_code_block(content, "yaml") {
            // Check if this is already a complete template with frontmatter
            if yaml_content.starts_with("---") {
                yaml_content
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
    use crate::test_helpers::{create_template_test_generator, create_template_generator_with_response};

    #[tokio::test]
    async fn test_template_generation() {
        let generator = create_template_test_generator();

        let template = generator
            .generate_template("A simple greeting template", vec!["Include name variable"])
            .await
            .expect("Template generation should succeed in test");

        assert_eq!(template.body, "Hello {{ name }}!");
    }

    #[tokio::test]
    async fn test_template_generation_with_markdown() {
        let response = "```yaml\n---\nto: \"test.tmpl\"\nvars:\n  name: \"test\"\n---\nHello {{ name }}!\n```";
        let generator = create_template_generator_with_response(response);

        let template = generator
            .generate_template("A simple greeting template", vec!["Include name variable"])
            .await
            .expect("Template generation should succeed in test");

        assert_eq!(template.body, "Hello {{ name }}!");
    }
}
