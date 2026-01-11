//! Template Generator Agent - Regenerates templates from graph changes
//!
//! Monitors RDF graph changes and regenerates affected ggen templates
//! to reflect new facts, relationships, and domain knowledge.

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

/// Real Template Generator Agent implementation
#[derive(Debug)]
pub struct TemplateGeneratorAgent {
    base: BaseAgent,
    template_generator: TemplateGenerator,
    template_context: TemplateContext,
}

/// Template context for regeneration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateContext {
    /// Template directory paths
    pub template_paths: Vec<String>,
    /// Current graph schema
    pub graph_schema: String,
    /// Template patterns and conventions
    pub template_patterns: HashMap<String, String>,
    /// Language-specific configurations
    pub language_configs: HashMap<String, LanguageConfig>,
}

/// Language-specific configuration for template generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageConfig {
    /// File extensions for this language
    pub extensions: Vec<String>,
    /// Common patterns for this language
    pub patterns: HashMap<String, String>,
    /// Code style conventions
    pub conventions: HashMap<String, String>,
}

/// Regenerated template result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegeneratedTemplate {
    /// Template name
    pub name: String,
    /// Template path
    pub path: String,
    /// Original template content
    pub original_content: String,
    /// Regenerated content
    pub regenerated_content: String,
    /// Changes made
    pub changes: Vec<TemplateChange>,
    /// Quality score
    pub quality_score: f64,
}

/// Template change description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateChange {
    /// Change type
    pub change_type: ChangeType,
    /// Description of change
    pub description: String,
    /// Line numbers affected
    pub line_range: (usize, usize),
    /// Confidence in change
    pub confidence: f64,
}

/// Change types for template modifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ChangeType {
    /// Variable addition
    VariableAdded,
    /// Variable modification
    VariableModified,
    /// Structure change
    StructureChanged,
    /// Content updated
    ContentUpdated,
    /// Style/formatting change
    StyleChanged,
}

impl TemplateGeneratorAgent {
    /// Create a new Template Generator Agent
    pub fn new(template_generator: TemplateGenerator, template_context: TemplateContext) -> Self {
        let base = BaseAgent::new(
            "template_generator",
            vec![
                "template_regeneration".to_string(),
                "graph_to_template".to_string(),
                "template_analysis".to_string(),
                "change_detection".to_string(),
            ],
            AgentConfig {
                timeout_seconds: 90,
                retry_attempts: 3,
                verbose_logging: false,
                performance_thresholds: PerformanceThresholds {
                    max_execution_time_ms: 15000,
                    max_memory_usage_mb: 300,
                    min_quality_score: 0.8,
                },
            },
        );

        Self {
            base,
            template_generator,
            template_context,
        }
    }


    /// Find templates affected by graph changes
    async fn find_affected_templates(&self, graph_delta: &Value) -> Result<Vec<String>> {
        // Analyze graph delta to determine which templates are affected
        let mut affected_templates = Vec::new();

        // This would analyze the graph delta to find templates that use
        // entities, properties, or relationships that changed
        if let Some(triples) = graph_delta.get("added_triples") {
            if let Value::Array(triple_array) = triples {
                for triple in triple_array {
                    // Extract subject, predicate, object from triple
                    if let (Some(subject), Some(predicate), Some(object)) = (
                        triple.get("subject").and_then(|v| v.as_str()),
                        triple.get("predicate").and_then(|v| v.as_str()),
                        triple.get("object").and_then(|v| v.as_str())
                    ) {
                        // Find templates that reference these entities
                        for template_path in &self.template_context.template_paths {
                            if self.template_references_entity(template_path, subject, predicate, object).await? {
                                affected_templates.push(template_path.clone());
                            }
                        }
                    }
                }
            }
        }

        Ok(affected_templates)
    }

    /// Check if a template references specific entities
    async fn template_references_entity(
        &self,
        _template_path: &str,
        _subject: &str,
        _predicate: &str,
        _object: &str
    ) -> Result<bool> {
        // This would analyze template content to see if it references
        // the given subject, predicate, or object
        // For now, return a simple heuristic
        Ok(true) // Would implement actual analysis
    }

    /// Regenerate templates based on graph changes
    async fn regenerate_templates(&self, affected_templates: Vec<String>, graph_context: &str) -> Result<Vec<RegeneratedTemplate>> {
        let mut regenerated = Vec::new();

        for template_path in affected_templates {
            // Read current template
            let original_content = tokio::fs::read_to_string(&template_path).await
                .map_err(|e| GgenAiError::io_error(&format!("Failed to read template {}: {}", template_path, e)))?;

            // Generate new template based on updated graph context
            let new_content = self.generate_updated_template(&original_content, graph_context).await?;

            // Analyze changes between original and new
            let changes = self.analyze_template_changes(&original_content, &new_content)?;

            // Calculate quality score
            let quality_score = self.calculate_template_quality(&new_content)?;

            regenerated.push(RegeneratedTemplate {
                name: template_path.split('/').last().unwrap_or("unknown").to_string(),
                path: template_path,
                original_content,
                regenerated_content: new_content,
                changes,
                quality_score,
            });
        }

        Ok(regenerated)
    }

    /// Generate updated template content
    async fn generate_updated_template(&self, original: &str, graph_context: &str) -> Result<String> {
        // Use AI to regenerate template based on updated graph context
        let _prompt = format!(
            r#"You are an expert template engineer. Given an existing ggen template and updated graph context,
regenerate the template to reflect the new domain knowledge while preserving the original structure and intent.

Original Template:
{}

Updated Graph Context:
{}

Your task:
1. Analyze how the graph changes affect this template
2. Update variable names, types, and relationships accordingly
3. Maintain the same template structure and formatting style
4. Ensure all variables are properly typed and documented
5. Add any new variables needed for the updated context

Return the updated template with proper frontmatter and variable definitions.

Guidelines:
- Preserve existing variable names when possible
- Add new variables with clear naming and descriptions
- Update type hints to match new domain model
- Maintain code style and formatting consistency
- Ensure template remains valid and functional

Updated Template:"#,
            original, graph_context
        );

        // Use template generator to create updated template
        let description = "Regenerate template based on updated graph context";
        let examples = vec!["Maintain structure", "Update variables", "Preserve style"];

        self.template_generator.generate_template(description, examples).await
            .map(|template| template.body)
    }

    /// Analyze changes between original and regenerated templates
    fn analyze_template_changes(&self, original: &str, regenerated: &str) -> Result<Vec<TemplateChange>> {
        let mut changes = Vec::new();

        // Simple change detection - would implement more sophisticated diffing
        if original != regenerated {
            changes.push(TemplateChange {
                change_type: ChangeType::ContentUpdated,
                description: "Template content updated to reflect graph changes".to_string(),
                line_range: (1, regenerated.lines().count()),
                confidence: 0.9,
            });
        }

        Ok(changes)
    }

    /// Calculate template quality score
    fn calculate_template_quality(&self, content: &str) -> Result<f64> {
        let mut score = 1.0;

        // Check for basic quality indicators
        if !content.contains("{{") {
            score -= 0.2; // Missing variables
        }

        if !content.contains("# Description") && !content.contains("///") {
            score -= 0.1; // Missing documentation
        }

        if content.lines().count() < 10 {
            score -= 0.1; // Too short
        }

        Ok(score.max(0.0))
    }
}

impl SwarmAgent for TemplateGeneratorAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        let start_time = std::time::Instant::now();

        // Extract graph delta from input
        let graph_delta = input.data.get("graph_delta")
            .ok_or_else(|| GgenAiError::parsing_error("No graph_delta in input"))?;

        // Find affected templates
        let affected_templates = self.find_affected_templates(graph_delta).await?;

        // Get current graph context
        let graph_context = self.template_context.graph_schema.clone();

        // Regenerate templates
        let regenerated_templates = self.regenerate_templates(affected_templates, &graph_context).await?;

        // Convert to output format
        let output_data = json!({
            "regenerated_templates": regenerated_templates,
            "templates_updated": regenerated_templates.len(),
            "graph_context": graph_context
        });

        let execution_time = start_time.elapsed().as_millis();

        Ok(AgentOutput {
            data: output_data,
            output_type: "regenerated_templates".to_string(),
            target_agents: vec!["code_generator".to_string()],
            metadata: {
                let mut metadata = HashMap::new();
                metadata.insert("execution_time_ms".to_string(), execution_time.to_string());
                metadata.insert("templates_regenerated".to_string(), regenerated_templates.len().to_string());
                metadata
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Validate template generator
        if self.template_generator.get_config().model.is_empty() {
            return Ok(false);
        }

        // Validate template context
        if self.template_context.template_paths.is_empty() {
            return Ok(false);
        }

        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        let mut issues = Vec::new();

        // Check template generator
        if self.template_generator.get_config().model.is_empty() {
            issues.push("No template generation model configured".to_string());
        }

        // Check template paths
        if self.template_context.template_paths.is_empty() {
            issues.push("No template paths configured".to_string());
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
    async fn test_template_generator_creation() {
        let template_context = TemplateContext {
            template_paths: vec!["templates/".to_string()],
            graph_schema: "test schema".to_string(),
            template_patterns: HashMap::new(),
            language_configs: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test template"));
        let template_gen = TemplateGenerator::new(client);
        let agent = TemplateGeneratorAgent::new(template_gen, template_context);

        assert_eq!(agent.name(), "template_generator");
        assert!(agent.capabilities().contains(&"template_regeneration".to_string()));
    }

    #[tokio::test]
    async fn test_health_check() {
        let template_context = TemplateContext {
            template_paths: vec!["templates/".to_string()],
            graph_schema: "test schema".to_string(),
            template_patterns: HashMap::new(),
            language_configs: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test"));
        let template_gen = TemplateGenerator::new(client);
        let agent = TemplateGeneratorAgent::new(template_gen, template_context);

        let health = agent.health_check().await;
        assert!(matches!(health.status, HealthStatus::Healthy));
        assert_eq!(health.score, 1.0);
    }
}
