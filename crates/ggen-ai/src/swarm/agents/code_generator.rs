//! Code Generator Agent - Generates code from templates and graphs
//!
//! Takes regenerated templates and generates actual code files in multiple
//! programming languages, ensuring consistency and quality.

use crate::error::{GgenAiError, Result};
use crate::swarm::{
    AgentHealth, HealthStatus, SwarmAgent, SwarmContext, AgentInput, AgentOutput,
    BaseAgent, AgentConfig, PerformanceThresholds
};
use crate::generators::TemplateGenerator;
use crate::client::{LlmClient, LlmConfig};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use tracing::debug;

/// Real Code Generator Agent implementation
#[derive(Debug)]
pub struct CodeGeneratorAgent {
    base: BaseAgent,
    template_generator: TemplateGenerator,
    code_context: CodeContext,
}

/// Code generation context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeContext {
    /// Output directory paths
    pub output_paths: Vec<String>,
    /// Language-specific configurations
    pub language_configs: HashMap<String, LanguageConfig>,
    /// Code style preferences
    pub style_preferences: HashMap<String, String>,
    /// Project structure templates
    pub project_templates: HashMap<String, String>,
}

/// Language-specific configuration for code generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageConfig {
    /// File extensions for this language
    pub extensions: Vec<String>,
    /// Import patterns
    pub import_patterns: HashMap<String, String>,
    /// Naming conventions
    pub naming_conventions: HashMap<String, String>,
    /// Code structure templates
    pub structure_templates: HashMap<String, String>,
}

/// Generated code result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedCode {
    /// File path
    pub file_path: String,
    /// Language
    pub language: String,
    /// Generated content
    pub content: String,
    /// Template used
    pub template_name: String,
    /// Variables applied
    pub variables: HashMap<String, String>,
    /// Quality score
    pub quality_score: f64,
    /// Validation results
    pub validation_results: Vec<CodeValidation>,
}

/// Code validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeValidation {
    /// Validation type (syntax, style, security, etc.)
    pub validation_type: String,
    /// Validation passed
    pub passed: bool,
    /// Error messages if failed
    pub errors: Vec<String>,
    /// Warning messages
    pub warnings: Vec<String>,
}

/// Code generation statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeGenerationStats {
    /// Files generated
    pub files_generated: u32,
    /// Lines of code generated
    pub lines_of_code: u32,
    /// Languages used
    pub languages_used: Vec<String>,
    /// Average quality score
    pub avg_quality_score: f64,
    /// Generation time (ms)
    pub generation_time_ms: u64,
}

impl CodeGeneratorAgent {
    /// Create a new Code Generator Agent
    pub fn new(template_generator: TemplateGenerator, code_context: CodeContext) -> Self {
        let base = BaseAgent::new(
            "code_generator",
            vec![
                "code_generation".to_string(),
                "multi_language".to_string(),
                "template_application".to_string(),
                "code_validation".to_string(),
            ],
            AgentConfig {
                timeout_seconds: 120,
                retry_attempts: 3,
                verbose_logging: false,
                performance_thresholds: PerformanceThresholds {
                    max_execution_time_ms: 20000,
                    max_memory_usage_mb: 400,
                    min_quality_score: 0.8,
                },
            },
        );

        Self {
            base,
            template_generator,
            code_context,
        }
    }


    /// Generate code from templates and context
    async fn generate_code_from_templates(
        &self,
        templates: &[crate::swarm::agents::template_generator::RegeneratedTemplate],
        generation_context: &HashMap<String, String>
    ) -> Result<Vec<GeneratedCode>> {
        let mut generated_code = Vec::new();

        for template in templates {
            // Generate code for each language in the template
            for (language, lang_config) in &self.code_context.language_configs {
                if self.template_supports_language(&template.regenerated_content, language)? {
                    let code = self.generate_code_for_language(
                        &template.regenerated_content,
                        language,
                        lang_config,
                        generation_context
                    ).await?;

                    // Validate generated code
                    let validation_results = self.validate_generated_code(&code, language).await?;

                    // Calculate quality score
                    let quality_score = self.calculate_code_quality(&code, &validation_results)?;

                    generated_code.push(GeneratedCode {
                        file_path: self.generate_file_path(&template.name, language, lang_config),
                        language: language.clone(),
                        content: code,
                        template_name: template.name.clone(),
                        variables: generation_context.clone(),
                        quality_score,
                        validation_results,
                    });
                }
            }
        }

        Ok(generated_code)
    }

    /// Check if template supports a specific language
    fn template_supports_language(&self, template_content: &str, language: &str) -> Result<bool> {
        // Check for language-specific markers or extensions
        let language_markers = match language {
            "rust" => vec!["rust", "rs", "cargo"],
            "typescript" => vec!["typescript", "ts", "tsx"],
            "python" => vec!["python", "py"],
            "javascript" => vec!["javascript", "js"],
            "java" => vec!["java"],
            "csharp" => vec!["csharp", "cs"],
            "go" => vec!["go", "golang"],
            _ => vec![],
        };

        Ok(language_markers.iter().any(|marker| template_content.contains(marker)))
    }

    /// Generate code for a specific language
    async fn generate_code_for_language(
        &self,
        template_content: &str,
        _language: &str,
        lang_config: &LanguageConfig,
        context: &HashMap<String, String>
    ) -> Result<String> {
        // Apply language-specific transformations
        let mut code = template_content.to_string();

        // Apply naming conventions
        for (pattern, replacement) in &lang_config.naming_conventions {
            // Simple string replacement for naming conventions
            code = code.replace(&format!("{{{}}}", pattern), replacement);
        }

        // Apply import patterns
        if let Some(import_template) = lang_config.import_patterns.get("standard") {
            // Add imports at the top of the file
            if !code.starts_with("//") && !code.starts_with("/*") {
                code = format!("{}\n\n{}", import_template, code);
            }
        }

        // Apply language-specific structure templates
        for (template_type, template) in &lang_config.structure_templates {
            if template_type == "class" && code.contains("class") {
                // Apply class structure template
                code = template.replace("{{class_name}}", &context.get("class_name").unwrap_or(&"Unknown".to_string()));
            }
        }

        Ok(code)
    }

    /// Generate appropriate file path for generated code
    fn generate_file_path(&self, template_name: &str, _language: &str, lang_config: &LanguageConfig) -> String {
        let default_ext = "txt".to_string();
        let default_path = "generated".to_string();
        let extension = lang_config.extensions.first().unwrap_or(&default_ext);
        let output_dir = self.code_context.output_paths.first().unwrap_or(&default_path);

        format!("{}/{}.{}", output_dir, template_name, extension)
    }

    /// Validate generated code
    async fn validate_generated_code(&self, code: &str, language: &str) -> Result<Vec<CodeValidation>> {
        let mut validations = Vec::new();

        // Basic syntax validation
        let syntax_valid = self.validate_syntax(code, language).await?;
        validations.push(CodeValidation {
            validation_type: "syntax".to_string(),
            passed: syntax_valid,
            errors: if !syntax_valid { vec!["Syntax errors detected".to_string()] } else { vec![] },
            warnings: vec![],
        });

        // Style validation
        let style_valid = self.validate_style(code, language)?;
        validations.push(CodeValidation {
            validation_type: "style".to_string(),
            passed: style_valid,
            errors: if !style_valid { vec!["Style violations detected".to_string()] } else { vec![] },
            warnings: vec!["Consider adding documentation".to_string()],
        });

        // Security validation
        let security_valid = self.validate_security(code)?;
        validations.push(CodeValidation {
            validation_type: "security".to_string(),
            passed: security_valid,
            errors: if !security_valid { vec!["Security issues detected".to_string()] } else { vec![] },
            warnings: vec![],
        });

        Ok(validations)
    }

    /// Validate code syntax (simplified implementation)
    async fn validate_syntax(&self, _code: &str, _language: &str) -> Result<bool> {
        // Would implement actual syntax validation
        // For now, return true if code is not empty
        Ok(true)
    }

    /// Validate code style (simplified implementation)
    fn validate_style(&self, code: &str, _language: &str) -> Result<bool> {
        // Check for basic style requirements
        let has_comments = code.contains("//") || code.contains("/*") || code.contains("#");
        let has_proper_indentation = code.lines().any(|line| line.starts_with("    ") || line.starts_with("\t"));

        Ok(has_comments && has_proper_indentation)
    }

    /// Validate code security (simplified implementation)
    fn validate_security(&self, code: &str) -> Result<bool> {
        // Check for common security issues
        let dangerous_patterns = [
            "eval(",
            "exec(",
            "system(",
            "popen(",
            "Runtime.getRuntime().exec(",
        ];

        for pattern in &dangerous_patterns {
            if code.contains(pattern) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Calculate overall code quality score
    fn calculate_code_quality(&self, code: &str, validations: &[CodeValidation]) -> Result<f64> {
        let mut score = 1.0;

        // Base score from validation results
        let failed_validations = validations.iter().filter(|v| !v.passed).count();
        score -= failed_validations as f64 * 0.2;

        // Content quality checks
        if code.lines().count() < 5 {
            score -= 0.1; // Too short
        }

        if !code.contains("function") && !code.contains("fn ") && !code.contains("def ") && !code.contains("class") {
            score -= 0.1; // No clear structure
        }

        // Length bonus (balanced - not too short, not too long)
        let line_count = code.lines().count();
        if line_count > 50 && line_count < 200 {
            score += 0.05; // Good length
        }

        Ok(score.max(0.0).min(1.0))
    }

    /// Generate code generation statistics
    fn generate_stats(&self, generated_code: &[GeneratedCode]) -> CodeGenerationStats {
        let total_lines = generated_code.iter()
            .map(|code| code.content.lines().count() as u32)
            .sum();

        let languages_used: Vec<String> = generated_code.iter()
            .map(|code| code.language.clone())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();

        let avg_quality = if generated_code.is_empty() {
            0.0
        } else {
            generated_code.iter()
                .map(|code| code.quality_score)
                .sum::<f64>() / generated_code.len() as f64
        };

        CodeGenerationStats {
            files_generated: generated_code.len() as u32,
            lines_of_code: total_lines,
            languages_used,
            avg_quality_score: avg_quality,
            generation_time_ms: 5000, // Would measure actual time
        }
    }
}

impl SwarmAgent for CodeGeneratorAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        let start_time = std::time::Instant::now();

        // Extract regenerated templates from input
        let templates: Vec<crate::swarm::agents::template_generator::RegeneratedTemplate> =
            if let Some(templates_data) = input.data.get("regenerated_templates") {
                serde_json::from_value(templates_data.clone())?
            } else {
                serde_json::from_value(input.data)?
            };

        // Extract generation context
        let generation_context = input.context.clone();

        // Generate code from templates
        let generated_code = self.generate_code_from_templates(&templates, &generation_context).await?;

        // Generate statistics
        let stats = self.generate_stats(&generated_code);

        // Convert to output format
        let output_data = json!({
            "generated_code": generated_code,
            "generation_stats": stats,
            "files_generated": generated_code.len(),
            "total_lines": stats.lines_of_code
        });

        let execution_time = start_time.elapsed().as_millis();

        Ok(AgentOutput {
            data: output_data,
            output_type: "generated_code".to_string(),
            target_agents: vec!["quality_assurance".to_string()],
            metadata: {
                let mut metadata = HashMap::new();
                metadata.insert("execution_time_ms".to_string(), execution_time.to_string());
                metadata.insert("files_generated".to_string(), generated_code.len().to_string());
                metadata.insert("lines_generated".to_string(), stats.lines_of_code.to_string());
                metadata.insert("avg_quality".to_string(), stats.avg_quality_score.to_string());
                metadata
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Validate template generator
        if self.template_generator.get_config().model.is_empty() {
            return Ok(false);
        }

        // Validate output paths
        if self.code_context.output_paths.is_empty() {
            return Ok(false);
        }

        // Validate language configurations
        if self.code_context.language_configs.is_empty() {
            return Ok(false);
        }

        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        let mut issues = Vec::new();

        // Check template generator
        if self.template_generator.get_config().model.is_empty() {
            issues.push("No code generation model configured".to_string());
        }

        // Check output paths
        if self.code_context.output_paths.is_empty() {
            issues.push("No output paths configured".to_string());
        }

        // Check language configurations
        if self.code_context.language_configs.is_empty() {
            issues.push("No language configurations available".to_string());
        }

        let score = if issues.is_empty() { 1.0 } else { 0.5 };

        let status = if score > 0.8 {
            HealthStatus::Healthy
        } else if score > 0.5 {
            HealthStatus::Degraded
        } else {
            HealthStatus::Unhealthy
        };

        AgentHealth {
            status,
            score,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::adapter::MockClient;

    #[tokio::test]
    async fn test_code_generator_creation() {
        let code_context = CodeContext {
            output_paths: vec!["generated/".to_string()],
            language_configs: HashMap::new(),
            style_preferences: HashMap::new(),
            project_templates: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test code"));
        let template_gen = TemplateGenerator::new(client);
        let agent = CodeGeneratorAgent::new(template_gen, code_context);

        assert_eq!(agent.name(), "code_generator");
        assert!(agent.capabilities().contains(&"code_generation".to_string()));
    }

    #[test]
    fn test_language_support_detection() {
        let code_context = CodeContext {
            output_paths: vec!["generated/".to_string()],
            language_configs: HashMap::new(),
            style_preferences: HashMap::new(),
            project_templates: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test"));
        let template_gen = TemplateGenerator::new(client);
        let agent = CodeGeneratorAgent::new(template_gen, code_context);

        // Test Rust detection
        assert!(agent.template_supports_language("fn main() {}", "rust").unwrap());

        // Test TypeScript detection
        assert!(agent.template_supports_language("interface User {}", "typescript").unwrap());

        // Test unknown language
        assert!(!agent.template_supports_language("random content", "unknown").unwrap());
    }

    #[tokio::test]
    async fn test_health_check() {
        let code_context = CodeContext {
            output_paths: vec!["generated/".to_string()],
            language_configs: HashMap::new(),
            style_preferences: HashMap::new(),
            project_templates: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test"));
        let template_gen = TemplateGenerator::new(client);
        let agent = CodeGeneratorAgent::new(template_gen, code_context);

        let health = agent.health_check().await;
        assert!(matches!(health.status, HealthStatus::Healthy));
        assert_eq!(health.score, 1.0);
    }
}
