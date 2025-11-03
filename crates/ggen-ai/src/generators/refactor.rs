//! AI-powered code refactoring assistant

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::error_utils::{missing_closing_marker_error, ErrorContext};
use ggen_core::{MergeStrategy, ThreeWayMerger};
use std::sync::Arc;

/// Code refactoring suggestion
/// A refactoring suggestion with type, description, and impact assessment
#[derive(Debug, Clone)]
pub struct RefactoringSuggestion {
    /// Type of refactoring
    pub suggestion_type: SuggestionType,
    /// Description of the suggestion
    pub description: String,
    /// Suggested code changes
    pub suggested_code: String,
    /// Confidence score (0.0 to 1.0)
    pub confidence: f64,
    /// Reasoning behind the suggestion
    pub reasoning: String,
    /// Impact assessment
    pub impact: ImpactLevel,
}

/// Types of refactoring suggestions
#[derive(Debug, Clone)]
pub enum SuggestionType {
    /// Extract method
    ExtractMethod,
    /// Rename variable/function
    Rename,
    /// Simplify conditional logic
    SimplifyConditional,
    /// Remove code duplication
    RemoveDuplication,
    /// Improve error handling
    ImproveErrorHandling,
    /// Add type annotations
    AddTypeAnnotations,
    /// Optimize performance
    OptimizePerformance,
    /// Improve readability
    ImproveReadability,
    /// Add documentation
    AddDocumentation,
    /// Other refactoring
    Other(String),
}

/// Impact level of a refactoring suggestion
#[derive(Debug, Clone)]
pub enum ImpactLevel {
    /// Low impact - safe to apply
    Low,
    /// Medium impact - review carefully
    Medium,
    /// High impact - requires testing
    High,
}

/// AI-powered code refactoring assistant
#[derive(Debug)]
pub struct RefactorAssistant {
    client: Arc<dyn LlmClient>,
}

impl RefactorAssistant {
    /// Create a new refactoring assistant
    pub fn new(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Create a new refactoring assistant with custom config
    pub fn with_config(client: Arc<dyn LlmClient>, _config: LlmConfig) -> Self {
        Self { client }
    }

    /// Create a new refactoring assistant with a client
    pub fn with_client(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Suggest refactoring improvements for code
    pub async fn suggest_refactoring(
        &self, code: &str, context: &RefactoringContext,
    ) -> Result<Vec<RefactoringSuggestion>> {
        let prompt = self.build_refactoring_prompt(code, context)?;

        let response = self.client.complete(&prompt).await?;

        // Parse suggestions from response
        self.parse_suggestions(&response.content)
    }

    /// Suggest refactoring for specific patterns
    pub async fn suggest_pattern_refactoring(
        &self, code: &str, patterns: Vec<&str>,
    ) -> Result<Vec<RefactoringSuggestion>> {
        let context = RefactoringContext {
            language: "unknown".to_string(),
            framework: None,
            patterns: patterns.iter().map(|s| s.to_string()).collect(),
            focus_areas: vec!["pattern_improvement".to_string()],
            constraints: vec![],
        };

        self.suggest_refactoring(code, &context).await
    }

    /// Suggest refactoring for performance optimization
    pub async fn suggest_performance_refactoring(
        &self, code: &str, language: &str,
    ) -> Result<Vec<RefactoringSuggestion>> {
        let context = RefactoringContext {
            language: language.to_string(),
            framework: None,
            patterns: vec![],
            focus_areas: vec!["performance".to_string()],
            constraints: vec!["maintain_functionality".to_string()],
        };

        self.suggest_refactoring(code, &context).await
    }

    /// Suggest refactoring for readability improvement
    pub async fn suggest_readability_refactoring(
        &self, code: &str, language: &str,
    ) -> Result<Vec<RefactoringSuggestion>> {
        let context = RefactoringContext {
            language: language.to_string(),
            framework: None,
            patterns: vec![],
            focus_areas: vec!["readability".to_string()],
            constraints: vec!["maintain_functionality".to_string()],
        };

        self.suggest_refactoring(code, &context).await
    }

    /// Apply refactoring suggestions using three-way merge
    pub async fn apply_refactoring(
        &self, original_code: &str, suggestions: Vec<RefactoringSuggestion>,
        strategy: MergeStrategy,
    ) -> Result<String> {
        if suggestions.is_empty() {
            return Ok(original_code.to_string());
        }

        // Generate refactored code
        let refactored_code = self
            .generate_refactored_code(original_code, &suggestions)
            .await?;

        // Use three-way merge to apply changes
        let merger = ThreeWayMerger::new(strategy);
        let result = merger.merge(
            original_code,
            &refactored_code,
            original_code, // No manual changes for now
            std::path::Path::new("refactored_code"),
        )?;

        if result.has_conflicts {
            return Err(GgenAiError::validation("Refactoring resulted in conflicts"));
        }

        Ok(result.content)
    }

    /// Build refactoring prompt
    fn build_refactoring_prompt(&self, code: &str, context: &RefactoringContext) -> Result<String> {
        let mut prompt = String::new();

        prompt.push_str("You are an expert code refactoring assistant. Analyze the provided code ");
        prompt.push_str("and suggest specific, actionable refactoring improvements.\n\n");

        prompt.push_str("## Code to Refactor\n");
        prompt.push_str("```");
        prompt.push_str(&context.language);
        prompt.push('\n');
        prompt.push_str(code);
        prompt.push_str("\n```\n\n");

        if let Some(framework) = &context.framework {
            prompt.push_str(&format!("## Framework: {}\n\n", framework));
        }

        if !context.patterns.is_empty() {
            prompt.push_str("## Known Patterns\n");
            for pattern in &context.patterns {
                prompt.push_str(&format!("- {}\n", pattern));
            }
            prompt.push('\n');
        }

        if !context.focus_areas.is_empty() {
            prompt.push_str("## Focus Areas\n");
            for area in &context.focus_areas {
                prompt.push_str(&format!("- {}\n", area));
            }
            prompt.push('\n');
        }

        if !context.constraints.is_empty() {
            prompt.push_str("## Constraints\n");
            for constraint in &context.constraints {
                prompt.push_str(&format!("- {}\n", constraint));
            }
            prompt.push('\n');
        }

        prompt.push_str("## Refactoring Guidelines\n");
        prompt.push_str("1. Suggest specific, actionable improvements\n");
        prompt.push_str("2. Provide code examples for each suggestion\n");
        prompt.push_str("3. Include confidence scores (0.0 to 1.0)\n");
        prompt.push_str("4. Assess impact level (Low/Medium/High)\n");
        prompt.push_str("5. Explain the reasoning behind each suggestion\n");
        prompt.push_str("6. Focus on maintainability and readability\n");
        prompt.push_str("7. Consider performance implications\n");
        prompt.push_str("8. Ensure suggestions are safe to apply\n\n");

        prompt.push_str("## Output Format\n");
        prompt.push_str("Provide suggestions in the following JSON format:\n");
        prompt.push_str("```json\n");
        prompt.push_str("{\n");
        prompt.push_str("  \"suggestions\": [\n");
        prompt.push_str("    {\n");
        prompt.push_str("      \"type\": \"ExtractMethod\",\n");
        prompt.push_str(
            "      \"description\": \"Extract the validation logic into a separate method\",\n",
        );
        prompt.push_str("      \"suggested_code\": \"function validateInput(input) { ... }\",\n");
        prompt.push_str("      \"confidence\": 0.9,\n");
        prompt.push_str(
            "      \"reasoning\": \"The validation logic is repeated and can be centralized\",\n",
        );
        prompt.push_str("      \"impact\": \"Low\"\n");
        prompt.push_str("    }\n");
        prompt.push_str("  ]\n");
        prompt.push_str("}\n");
        prompt.push_str("```\n\n");

        prompt.push_str("Analyze the code and provide refactoring suggestions:\n\n");

        Ok(prompt)
    }

    /// Generate refactored code based on suggestions
    async fn generate_refactored_code(
        &self, original_code: &str, suggestions: &[RefactoringSuggestion],
    ) -> Result<String> {
        let mut prompt = String::new();

        prompt.push_str("You are an expert code refactoring assistant. Apply the provided refactoring suggestions ");
        prompt.push_str("to the original code and generate the improved version.\n\n");

        prompt.push_str("## Original Code\n");
        prompt.push_str("```\n");
        prompt.push_str(original_code);
        prompt.push_str("\n```\n\n");

        prompt.push_str("## Refactoring Suggestions\n");
        for (i, suggestion) in suggestions.iter().enumerate() {
            prompt.push_str(&format!(
                "{}. **{:?}**: {}\n",
                i + 1,
                suggestion.suggestion_type,
                suggestion.description
            ));
            prompt.push_str(&format!("   Reasoning: {}\n", suggestion.reasoning));
            prompt.push_str(&format!("   Suggested: {}\n", suggestion.suggested_code));
            prompt.push('\n');
        }

        prompt.push_str("## Requirements\n");
        prompt.push_str("1. Apply all suggestions that improve the code\n");
        prompt.push_str("2. Maintain the original functionality\n");
        prompt.push_str("3. Ensure the code is syntactically correct\n");
        prompt.push_str("4. Preserve comments and documentation\n");
        prompt.push_str("5. Follow best practices for the language\n\n");

        prompt.push_str("Generate the refactored code:\n\n");

        let response = self.client.complete(&prompt).await?;

        // Extract code from markdown code blocks if present
        let refactored_code = if response.content.contains("```") {
            let start = response
                .content
                .find("```")
                .ok_or_else(|| GgenAiError::validation("Could not find opening ``` marker"))?;
            let end = response.content.rfind("```").ok_or_else(|| {
                missing_closing_marker_error::<String>("```", "", ErrorContext::RefactorGeneration)
                    .unwrap_err()
            })?;
            response.content[start + 3..end].trim().to_string()
        } else {
            response.content.trim().to_string()
        };

        Ok(refactored_code)
    }

    /// Parse suggestions from LLM response
    fn parse_suggestions(&self, content: &str) -> Result<Vec<RefactoringSuggestion>> {
        // Try to parse as JSON first
        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(content) {
            if let Some(suggestions_array) = parsed.get("suggestions").and_then(|s| s.as_array()) {
                let mut suggestions = Vec::new();

                for suggestion_value in suggestions_array {
                    if let Ok(suggestion) = self.parse_suggestion_from_json(suggestion_value) {
                        suggestions.push(suggestion);
                    }
                }

                return Ok(suggestions);
            }
        }

        // Fallback to text parsing
        self.parse_suggestions_from_text(content)
    }

    /// Parse a single suggestion from JSON
    fn parse_suggestion_from_json(
        &self, value: &serde_json::Value,
    ) -> Result<RefactoringSuggestion> {
        let suggestion_type = value
            .get("type")
            .and_then(|t| t.as_str())
            .map(|s| self.parse_suggestion_type(s))
            .unwrap_or(SuggestionType::Other("Unknown".to_string()));

        let description = value
            .get("description")
            .and_then(|d| d.as_str())
            .unwrap_or("No description")
            .to_string();

        let suggested_code = value
            .get("suggested_code")
            .and_then(|c| c.as_str())
            .unwrap_or("")
            .to_string();

        let confidence = value
            .get("confidence")
            .and_then(|c| c.as_f64())
            .unwrap_or(0.5);

        let reasoning = value
            .get("reasoning")
            .and_then(|r| r.as_str())
            .unwrap_or("No reasoning provided")
            .to_string();

        let impact = value
            .get("impact")
            .and_then(|i| i.as_str())
            .map(|s| self.parse_impact_level(s))
            .unwrap_or(ImpactLevel::Medium);

        Ok(RefactoringSuggestion {
            suggestion_type,
            description,
            suggested_code,
            confidence,
            reasoning,
            impact,
        })
    }

    /// Parse suggestion type from string
    fn parse_suggestion_type(&self, s: &str) -> SuggestionType {
        match s.to_lowercase().as_str() {
            "extractmethod" | "extract_method" => SuggestionType::ExtractMethod,
            "rename" => SuggestionType::Rename,
            "simplifyconditional" | "simplify_conditional" => SuggestionType::SimplifyConditional,
            "removeduplication" | "remove_duplication" => SuggestionType::RemoveDuplication,
            "improveerrorhandling" | "improve_error_handling" => {
                SuggestionType::ImproveErrorHandling
            }
            "addtypeannotations" | "add_type_annotations" => SuggestionType::AddTypeAnnotations,
            "optimizeperformance" | "optimize_performance" => SuggestionType::OptimizePerformance,
            "improvereadability" | "improve_readability" => SuggestionType::ImproveReadability,
            "adddocumentation" | "add_documentation" => SuggestionType::AddDocumentation,
            _ => SuggestionType::Other(s.to_string()),
        }
    }

    /// Parse impact level from string
    fn parse_impact_level(&self, s: &str) -> ImpactLevel {
        match s.to_lowercase().as_str() {
            "low" => ImpactLevel::Low,
            "medium" => ImpactLevel::Medium,
            "high" => ImpactLevel::High,
            _ => ImpactLevel::Medium,
        }
    }

    /// Parse suggestions from text (fallback)
    fn parse_suggestions_from_text(&self, content: &str) -> Result<Vec<RefactoringSuggestion>> {
        // Simple text parsing implementation
        // Extract suggestions from markdown-style text format
        let mut suggestions = Vec::new();
        let lines: Vec<&str> = content.lines().collect();

        let mut i = 0;
        while i < lines.len() {
            let line = lines[i].trim();

            // Look for numbered suggestions (e.g., "1. **ExtractMethod**: Description")
            if line.starts_with(char::is_numeric) && line.contains("**") {
                if let Some(type_start) = line.find("**") {
                    if let Some(type_end) = line[type_start + 2..].find("**") {
                        let suggestion_type_str = &line[type_start + 2..type_start + 2 + type_end];
                        let description = line[type_start + 4 + type_end..]
                            .trim_start_matches(':')
                            .trim();

                        suggestions.push(RefactoringSuggestion {
                            suggestion_type: self.parse_suggestion_type(suggestion_type_str),
                            description: description.to_string(),
                            suggested_code: String::new(),
                            confidence: 0.7,
                            reasoning: String::new(),
                            impact: ImpactLevel::Medium,
                        });
                    }
                }
            }

            i += 1;
        }

        Ok(suggestions)
    }

    /// Get the LLM client
    pub fn client(&self) -> &Arc<dyn LlmClient> {
        &self.client
    }

    /// Get the current configuration from the client
    pub fn config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    // Note: set_config() removed - Arc<dyn LlmClient> doesn't support interior mutability
    // If config updates are needed, create a new RefactorAssistant instance with the new config
}

/// Context for refactoring analysis
#[derive(Debug, Clone)]
pub struct RefactoringContext {
    /// Programming language
    pub language: String,
    /// Framework being used
    pub framework: Option<String>,
    /// Known patterns in the codebase
    pub patterns: Vec<String>,
    /// Areas to focus on
    pub focus_areas: Vec<String>,
    /// Constraints to consider
    pub constraints: Vec<String>,
}

impl RefactoringContext {
    /// Create a new refactoring context
    pub fn new(language: String) -> Self {
        Self {
            language,
            framework: None,
            patterns: Vec::new(),
            focus_areas: Vec::new(),
            constraints: Vec::new(),
        }
    }

    /// Set the framework
    pub fn with_framework(mut self, framework: String) -> Self {
        self.framework = Some(framework);
        self
    }

    /// Add patterns
    pub fn with_patterns(mut self, patterns: Vec<String>) -> Self {
        self.patterns = patterns;
        self
    }

    /// Add focus areas
    pub fn with_focus_areas(mut self, focus_areas: Vec<String>) -> Self {
        self.focus_areas = focus_areas;
        self
    }

    /// Add constraints
    pub fn with_constraints(mut self, constraints: Vec<String>) -> Self {
        self.constraints = constraints;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;
    use crate::test_helpers::{
        create_refactor_assistant_with_response, create_refactor_test_assistant,
    };

    #[tokio::test]
    async fn test_refactor_assistant_creation() {
        let assistant = create_refactor_test_assistant();
        // Test passes if assistant was created successfully
        assert!(assistant.client.get_config().model.len() > 0);
    }

    #[tokio::test]
    async fn test_refactor_assistant_with_config() {
        let client = Arc::new(MockClient::with_response("Refactoring suggestions"));
        let assistant = RefactorAssistant::with_config(client, LlmConfig::default());

        // Test that the assistant was created successfully
        assert!(assistant.client.get_config().model.len() > 0);
    }

    #[tokio::test]
    async fn test_suggest_refactoring() {
        let assistant = create_refactor_test_assistant();

        let context = RefactoringContext::new("JavaScript".to_string());
        let result = assistant
            .suggest_refactoring(
                "function test() { if (input.length > 0) { return true; } }",
                &context,
            )
            .await;

        // This should succeed with parsed suggestions
        assert!(result.is_ok());
        let suggestions = result.expect("Failed to get refactoring suggestions");
        assert_eq!(suggestions.len(), 1);
        assert!(matches!(
            suggestions[0].suggestion_type,
            SuggestionType::ExtractMethod
        ));
    }

    #[tokio::test]
    async fn test_apply_refactoring() {
        let response = "function test() { return validate(input); }";
        let assistant = create_refactor_assistant_with_response(response);

        let suggestions = vec![RefactoringSuggestion {
            suggestion_type: SuggestionType::ExtractMethod,
            description: "Extract validation".to_string(),
            suggested_code: "function validate(input) { return input.length > 0; }".to_string(),
            confidence: 0.9,
            reasoning: "Repeated logic".to_string(),
            impact: ImpactLevel::Low,
        }];

        let result = assistant
            .apply_refactoring(
                "function test() { if (input.length > 0) { return true; } }",
                suggestions,
                MergeStrategy::GeneratedWins,
            )
            .await;

        // This should succeed with refactored code
        assert!(result.is_ok());
        let refactored = result.expect("Failed to apply refactoring");
        assert!(refactored.contains("validate"));
    }
}
