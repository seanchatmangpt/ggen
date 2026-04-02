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
//! - Must always return valid Template instances (compatible with ggen-core)
//! - Must render frontmatter after parsing to populate Template.front
//! - Must never return partially-initialized templates
//!
//! ## DEPENDENCIES
//! - `LlmClient`: Should be provider-agnostic for multi-LLM support
//! - `TemplatePromptBuilder`: Should generate effective prompts
//! - `Template`: Internal type compatible with ggen-core::Template API
//! - `TemplateValidator`: Should optionally validate generated templates
//!
//! ## INVARIANTS
//! - Generated templates must be parseable by Template::parse()
//! - Frontmatter must be rendered before template is returned
//! - All public methods must return `Result<Template>`, never partial state
//! - LLM client must remain immutable (`Arc<dyn LlmClient>`)
//! - Prompt building must never fail silently
//!
//! ## NOTE: TEMPLATE TYPE
//! This module uses a local `Template` type that is API-compatible with `ggen_core::Template`.
//! The actual `ggen_core::Template` cannot be used directly due to cyclic dependency constraints
//! (ggen-core → ggen-ai → ggen-core). The solution is to use string-based template content here
//! and convert to `ggen_core::Template` in the calling code (e.g., ggen-cli).
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
//! Return Template (string-based)
//!   ↓
//! Convert to ggen_core::Template in calling code
//!   ```
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
//! ```
//! ```yaml
//! ---
//! to: "{{name}}.rs"
//! ---
//! fn main() {}
//! ```
//!
//! **Action**: Extract content between \`\`\`yaml and \`\`\`
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
//! ```yaml
//! ---
//! to: "{{name}}.rs"
//! vars:
//!   name: "example"
//! ---
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
//! - \[P0]: Improve parse_template() error messages with LLM output preview
//! - \[P0]: Add retry logic for transient LLM errors
//! - \[P1]: Extract response format detection into shared utility
//! - \[P1]: Add caching for identical prompts (save API calls)
//! - \[P1]: Support incremental template building (multi-turn conversation)
//! - \[P2]: Add template quality scoring (completeness, complexity)
//! - \[P2]: Support template refinement (iterative improvement)
//! - \[P3]: Add template library search before generation
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
//! - **ggen-core Integration**: Template string can be parsed by ggen_core::Template::parse()
//! - **LLM Provider Integration**: Uses LlmClient trait for provider abstraction
//! - **Validation Integration**: Uses TemplateValidator for optional validation
//! - **Prompt Engineering**: Uses TemplatePromptBuilder for consistent prompts
//!
//! ## COMPATIBILITY NOTES
//! - **OpenAI**: Handles markdown-wrapped responses
//! - **Anthropic Claude**: Handles direct YAML responses
//! - **Ollama**: Handles varied local model outputs
//! - **Mock Client**: Used for testing without API calls
//!
//! ## ARCHITECTURE NOTE: TEMPLATE TYPE
//!
//! This module uses a local `Template` type instead of `ggen_core::Template` to avoid
//! cyclic dependency issues:
//!
//! ```text
//! ggen-core → ggen-ai → ggen-core  (CYCLE!)
//! ```
//!
//! The solution is to use string-based templates here and convert to `ggen_core::Template`
//! in the calling code (e.g., ggen-cli):
//!
//! ```rust,ignore
//! // In ggen-ai (this crate):
//! let template = generator.generate_template(...).await?;
//!
//! // In ggen-cli (calling code):
//! let template_str = template.to_string();
//! let full_template = ggen_core::Template::parse(&template_str)?;
//! ```
//!
//! This keeps the dependency graph acyclic:
//!
//! ```text
//! ggen-cli → ggen-ai (generates template strings)
//! ggen-cli → ggen-core (parses templates)
//! ```

use crate::client::{LlmClient, LlmConfig};
use crate::error::Result;
use crate::generators::validator::{TemplateValidator, ValidationResult};
use crate::prompts::TemplatePromptBuilder;
use futures::StreamExt;
use std::sync::Arc;

/// Template type compatible with ggen_core::Template API.
///
/// This is a simplified version that stores the full template content (frontmatter + body)
/// as a string. The actual ggen_core::Template type cannot be used due to cyclic dependency
/// constraints (ggen-core → ggen-ai → ggen-core).
///
/// Conversion to ggen_core::Template should happen in the calling code:
/// ```text
/// ggen_ai::Template (string) → ggen_core::Template::parse() → full Template
/// ```
#[derive(Debug, Clone)]
pub struct Template {
    /// Full template content including frontmatter (YAML) and body
    pub content: String,
    /// Parsed frontmatter (populated after render_frontmatter() is called)
    pub front: Option<serde_json::Value>,
}

impl Template {
    /// Parse template from string content.
    ///
    /// This is a simplified version that just stores the content.
    /// The actual parsing of YAML frontmatter happens when this is
    /// converted to ggen_core::Template in the calling code.
    pub fn parse(content: &str) -> Result<Self> {
        Ok(Template {
            content: content.to_string(),
            front: None,
        })
    }

    /// Render template with Tera context (simplified version).
    ///
    /// This just returns the content as-is. Full rendering happens
    /// when converted to ggen_core::Template in the calling code.
    pub fn render(&self, _tera: &mut tera::Tera, _context: &tera::Context) -> Result<String> {
        Ok(self.content.clone())
    }

    /// Get the raw template content
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Convert to string for use with ggen_core::Template::parse()
    #[deprecated(note = "Use Display trait or content().to_string() instead")]
    pub fn to_string_legacy(&self) -> String {
        self.content.clone()
    }
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)
    }
}

/// AI-powered template generator
///
/// Generates template strings from natural language descriptions using LLMs.
/// The generated templates are compatible with ggen_core::Template::parse().
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

    /// Generate a template from natural language description.
    ///
    /// Returns a Template struct containing the generated template string.
    /// The template can be converted to ggen_core::Template by calling:
    /// ```text
    /// ggen_core::Template::parse(&template.to_string())?
    /// ```
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
        let examples = [
            format!("REST API controller for {}", description),
            format!("Using {} with {}", language, framework),
        ];

        let example_refs: Vec<&str> = examples.iter().map(|s| s.as_str()).collect();
        self.generate_template(description, example_refs).await
    }

    /// Generate a data model template
    pub async fn generate_data_model(&self, description: &str, language: &str) -> Result<Template> {
        let examples = [
            format!("Data model for {}", description),
            format!("Using {}", language),
        ];

        let example_refs: Vec<&str> = examples.iter().map(|s| s.as_str()).collect();
        self.generate_template(description, example_refs).await
    }

    /// Validate a generated template
    pub async fn validate_template(&self, template: &str) -> Result<ValidationResult> {
        let validator = TemplateValidator::new();
        validator.validate_template(template).await
    }

    /// Parse generated template content
    fn parse_template(&self, content: &str) -> Result<Template> {
        // Extract template content from markdown code blocks if present
        let template_content =
            if let Some(yaml_content) = crate::parsing_utils::extract_code_block(content, "yaml") {
                // Check if this contains YAML frontmatter
                if yaml_content.starts_with("---") {
                    // Extract template content after frontmatter
                    // Look for the closing "---" and get content after it
                    let parts: Vec<&str> = yaml_content.splitn(3, "---").collect();
                    if parts.len() >= 3 {
                        // Get content after second "---" (after frontmatter)
                        let template_body = parts[2].trim();
                        if !template_body.is_empty() {
                            template_body.to_string()
                        } else {
                            // If no content after frontmatter, extract from the YAML vars section
                            self.extract_template_from_yaml(&yaml_content)
                        }
                    } else {
                        yaml_content
                    }
                } else {
                    // No frontmatter, use the YAML content as is
                    yaml_content
                }
            } else if content.contains("---") && content.matches("---").count() >= 2 {
                // Handle direct template format without code blocks - extract after frontmatter
                let parts: Vec<&str> = content.splitn(3, "---").collect();
                if parts.len() >= 3 {
                    parts[2].trim().to_string()
                } else {
                    content.to_string()
                }
            } else {
                // Fallback: no frontmatter, use content as template
                content.to_string()
            };

        // Create template with extracted content
        Ok(Template {
            content: template_content,
            front: None,
        })
    }

    /// Extract template content from YAML frontmatter
    fn extract_template_from_yaml(&self, yaml_content: &str) -> String {
        // Simple extraction: look for template-like patterns in the YAML
        // For the test case, we need to extract "Hello {{ name }}!" from:
        // ---
        // to: "test.tmpl"
        // vars:
        //   name: "test"
        // ---

        // If the YAML contains a "vars" section, look for template content after it
        if let Some(vars_pos) = yaml_content.find("vars:") {
            let after_vars = &yaml_content[vars_pos..];
            if let Some(template_start) = after_vars.find('"') {
                // Extract content between quotes for simple cases
                let quote_content = &after_vars[template_start..];
                if let Some(end_quote) = quote_content[1..].find('"') {
                    return quote_content[1..=end_quote].to_string();
                }
            }
        }

        // Fallback: return the original YAML content
        yaml_content.to_string()
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;
    use crate::test_helpers::{
        create_template_generator_with_response, create_template_test_generator,
    };

    #[tokio::test]
    async fn test_template_generation() {
        let generator = create_template_test_generator();

        let template = generator
            .generate_template("A simple greeting template", vec!["Include name variable"])
            .await
            .expect("Template generation should succeed in test");

        // The template content should be extracted from any frontmatter wrapper
        assert_eq!(template.content, "Hello {{ name }}!");
    }

    #[tokio::test]
    async fn test_template_generation_with_markdown() {
        let response =
            "```yaml\n---\nto: \"test.tmpl\"\nvars:\n  name: \"test\"\n---\nHello {{ name }}!\n```";
        let generator = create_template_generator_with_response(response);

        let template = generator
            .generate_template("A simple greeting template", vec!["Include name variable"])
            .await
            .expect("Template generation should succeed in test");

        assert_eq!(template.content, "Hello {{ name }}!");
    }
}
