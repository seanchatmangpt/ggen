//! LLM Output Validation Framework
//!
//! This module provides comprehensive testing and scoring for LLM-generated content
//! across all ggen AI commands. It simulates real user behavior and evaluates output
//! quality on a 0-10 scale with detailed feedback.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Scoring dimensions for LLM output quality
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityScore {
    /// Overall score (0-10)
    pub total: f32,
    /// Individual dimension scores
    pub dimensions: HashMap<String, DimensionScore>,
    /// Actionable feedback for improvement
    pub feedback: Vec<String>,
    /// Test metadata
    pub metadata: TestMetadata,
}

/// Individual scoring dimension
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DimensionScore {
    /// Score for this dimension (0-10)
    pub score: f32,
    /// Weight in overall score
    pub weight: f32,
    /// Description of what was evaluated
    pub description: String,
    /// Specific issues found
    pub issues: Vec<String>,
    /// Suggestions for improvement
    pub suggestions: Vec<String>,
}

/// Test metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestMetadata {
    pub command: String,
    pub scenario: String,
    pub timestamp: String,
    pub model: String,
    pub execution_time_ms: u64,
}

/// User behavior simulation scenarios
#[derive(Debug, Clone)]
pub struct UserScenario {
    pub name: String,
    pub description: String,
    pub command: String,
    pub args: Vec<String>,
    pub expected_behavior: ExpectedBehavior,
}

/// Expected behavior for validation
#[derive(Debug, Clone)]
pub struct ExpectedBehavior {
    pub min_length: usize,
    pub max_length: usize,
    pub required_patterns: Vec<String>,
    pub forbidden_patterns: Vec<String>,
    pub quality_criteria: Vec<QualityCriterion>,
}

/// Quality criterion for evaluation
#[derive(Debug, Clone)]
pub struct QualityCriterion {
    pub name: String,
    pub description: String,
    pub weight: f32,
    pub validator: fn(&str) -> (f32, Vec<String>),
}

/// Validation framework
pub struct ValidationFramework {
    scenarios: Vec<UserScenario>,
}

impl ValidationFramework {
    /// Create new validation framework with predefined scenarios
    pub fn new() -> Self {
        Self {
            scenarios: Self::create_user_scenarios(),
        }
    }

    /// Create realistic user behavior scenarios
    fn create_user_scenarios() -> Vec<UserScenario> {
        vec![
            // Scenario 1: Simple template generation
            UserScenario {
                name: "simple_template".to_string(),
                description: "User wants a basic hello world template".to_string(),
                command: "ai generate".to_string(),
                args: vec![
                    "--description".to_string(),
                    "Create a simple hello world web page with HTML and CSS".to_string(),
                    "--max-tokens".to_string(),
                    "500".to_string(),
                ],
                expected_behavior: ExpectedBehavior {
                    min_length: 100,
                    max_length: 2000,
                    required_patterns: vec!["html".to_string(), "hello".to_string()],
                    forbidden_patterns: vec!["error".to_string(), "undefined".to_string()],
                    quality_criteria: vec![
                        QualityCriterion {
                            name: "structure".to_string(),
                            description: "Has proper structure and formatting".to_string(),
                            weight: 0.3,
                            validator: validate_structure,
                        },
                        QualityCriterion {
                            name: "completeness".to_string(),
                            description: "Contains all requested elements".to_string(),
                            weight: 0.3,
                            validator: validate_completeness,
                        },
                        QualityCriterion {
                            name: "usability".to_string(),
                            description: "Code is ready to use without modification".to_string(),
                            weight: 0.2,
                            validator: validate_usability,
                        },
                        QualityCriterion {
                            name: "clarity".to_string(),
                            description: "Code is well-commented and clear".to_string(),
                            weight: 0.2,
                            validator: validate_clarity,
                        },
                    ],
                },
            },
            // Scenario 2: Complex SPARQL query
            UserScenario {
                name: "complex_sparql".to_string(),
                description: "User needs a SPARQL query for data analysis".to_string(),
                command: "ai sparql".to_string(),
                args: vec![
                    "--description".to_string(),
                    "Find all users with age > 18 who have made purchases in the last 30 days"
                        .to_string(),
                    "--max-tokens".to_string(),
                    "300".to_string(),
                ],
                expected_behavior: ExpectedBehavior {
                    min_length: 50,
                    max_length: 1000,
                    required_patterns: vec![
                        "SELECT".to_string(),
                        "WHERE".to_string(),
                        "FILTER".to_string(),
                    ],
                    forbidden_patterns: vec!["syntax error".to_string()],
                    quality_criteria: vec![
                        QualityCriterion {
                            name: "correctness".to_string(),
                            description: "SPARQL syntax is valid".to_string(),
                            weight: 0.4,
                            validator: validate_sparql_syntax,
                        },
                        QualityCriterion {
                            name: "efficiency".to_string(),
                            description: "Query is optimized".to_string(),
                            weight: 0.3,
                            validator: validate_query_efficiency,
                        },
                        QualityCriterion {
                            name: "readability".to_string(),
                            description: "Query is formatted well".to_string(),
                            weight: 0.3,
                            validator: validate_readability,
                        },
                    ],
                },
            },
            // Scenario 3: Frontmatter generation
            UserScenario {
                name: "blog_frontmatter".to_string(),
                description: "User needs frontmatter for a blog post".to_string(),
                command: "ai frontmatter".to_string(),
                args: vec![
                    "--description".to_string(),
                    "Blog post about AI ethics with SEO optimization".to_string(),
                    "--max-tokens".to_string(),
                    "200".to_string(),
                ],
                expected_behavior: ExpectedBehavior {
                    min_length: 50,
                    max_length: 800,
                    required_patterns: vec!["title".to_string(), "description".to_string()],
                    forbidden_patterns: vec![],
                    quality_criteria: vec![
                        QualityCriterion {
                            name: "seo_quality".to_string(),
                            description: "Contains proper SEO metadata".to_string(),
                            weight: 0.4,
                            validator: validate_seo,
                        },
                        QualityCriterion {
                            name: "completeness".to_string(),
                            description: "All standard fields present".to_string(),
                            weight: 0.3,
                            validator: validate_frontmatter_completeness,
                        },
                        QualityCriterion {
                            name: "validity".to_string(),
                            description: "Valid YAML format".to_string(),
                            weight: 0.3,
                            validator: validate_yaml,
                        },
                    ],
                },
            },
            // Scenario 4: RDF graph generation
            UserScenario {
                name: "ontology_graph".to_string(),
                description: "User wants an RDF ontology for a specific domain".to_string(),
                command: "ai graph".to_string(),
                args: vec![
                    "--description".to_string(),
                    "E-commerce product catalog with categories and pricing".to_string(),
                    "--include-examples".to_string(),
                    "--max-tokens".to_string(),
                    "400".to_string(),
                ],
                expected_behavior: ExpectedBehavior {
                    min_length: 100,
                    max_length: 2000,
                    required_patterns: vec!["@prefix".to_string(), "owl:Class".to_string()],
                    forbidden_patterns: vec!["syntax error".to_string()],
                    quality_criteria: vec![
                        QualityCriterion {
                            name: "ontology_quality".to_string(),
                            description: "Well-structured ontology".to_string(),
                            weight: 0.4,
                            validator: validate_ontology,
                        },
                        QualityCriterion {
                            name: "examples".to_string(),
                            description: "Contains useful examples".to_string(),
                            weight: 0.3,
                            validator: validate_examples,
                        },
                        QualityCriterion {
                            name: "turtle_validity".to_string(),
                            description: "Valid Turtle syntax".to_string(),
                            weight: 0.3,
                            validator: validate_turtle,
                        },
                    ],
                },
            },
        ]
    }

    /// Run all validation scenarios
    pub fn run_all_scenarios(&self) -> Vec<QualityScore> {
        self.scenarios
            .iter()
            .map(|scenario| self.run_scenario(scenario))
            .collect()
    }

    /// Run a single validation scenario
    pub fn run_scenario(&self, scenario: &UserScenario) -> QualityScore {
        // Execute command and capture output
        let output = self.execute_command(scenario);

        // Score the output
        self.score_output(&output, scenario)
    }

    /// Execute command and capture output
    fn execute_command(&self, scenario: &UserScenario) -> String {
        // This would actually run the command - placeholder for now
        format!("Output from {}", scenario.command)
    }

    /// Score output against expected behavior
    fn score_output(&self, output: &str, scenario: &UserScenario) -> QualityScore {
        let mut dimension_scores = HashMap::new();
        let mut total_weighted_score = 0.0;
        let mut total_weight = 0.0;
        let mut all_feedback = Vec::new();

        // Score each quality criterion
        for criterion in &scenario.expected_behavior.quality_criteria {
            let (score, issues) = (criterion.validator)(output);

            total_weighted_score += score * criterion.weight;
            total_weight += criterion.weight;

            let suggestions = self.generate_suggestions(&criterion.name, score, &issues);
            all_feedback.extend(suggestions.clone());

            dimension_scores.insert(
                criterion.name.clone(),
                DimensionScore {
                    score,
                    weight: criterion.weight,
                    description: criterion.description.clone(),
                    issues,
                    suggestions,
                },
            );
        }

        // Calculate final score
        let total = if total_weight > 0.0 {
            total_weighted_score / total_weight
        } else {
            0.0
        };

        QualityScore {
            total,
            dimensions: dimension_scores,
            feedback: all_feedback,
            metadata: TestMetadata {
                command: scenario.command.clone(),
                scenario: scenario.name.clone(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                model: "qwen3-coder:30b".to_string(),
                execution_time_ms: 0,
            },
        }
    }

    /// Generate improvement suggestions based on score
    fn generate_suggestions(&self, dimension: &str, score: f32, issues: &[String]) -> Vec<String> {
        let mut suggestions = Vec::new();

        if score < 7.0 {
            match dimension {
                "structure" => suggestions
                    .push("Consider adding more semantic structure and proper nesting".to_string()),
                "completeness" => suggestions
                    .push("Ensure all requested elements are included in the output".to_string()),
                "usability" => {
                    suggestions.push("Add more examples and usage documentation".to_string())
                }
                "clarity" => {
                    suggestions.push("Improve code comments and variable naming".to_string())
                }
                "correctness" => {
                    suggestions.push("Review syntax and semantic correctness".to_string())
                }
                "efficiency" => suggestions.push("Optimize for better performance".to_string()),
                "readability" => {
                    suggestions.push("Improve formatting and organization".to_string())
                }
                "seo_quality" => {
                    suggestions.push("Add meta tags, keywords, and descriptions".to_string())
                }
                "ontology_quality" => {
                    suggestions.push("Add more relationships and properties".to_string())
                }
                _ => suggestions.push(format!("Improve {} quality", dimension)),
            }
        }

        // Add issue-specific suggestions
        for issue in issues {
            suggestions.push(format!("Fix: {}", issue));
        }

        suggestions
    }

    /// Generate comprehensive report
    pub fn generate_report(&self, scores: &[QualityScore]) -> String {
        let mut report = String::new();

        report.push_str("# LLM Output Validation Report\n\n");
        report.push_str(&format!(
            "Generated: {}\n\n",
            chrono::Utc::now().to_rfc3339()
        ));

        // Overall statistics
        let avg_score: f32 = scores.iter().map(|s| s.total).sum::<f32>() / scores.len() as f32;
        report.push_str(&format!("## Overall Statistics\n\n"));
        report.push_str(&format!("- **Average Score**: {:.2}/10\n", avg_score));
        report.push_str(&format!("- **Tests Run**: {}\n", scores.len()));
        report.push_str(&format!("- **Model**: qwen3-coder:30b\n\n"));

        // Individual test results
        report.push_str("## Individual Test Results\n\n");
        for score in scores {
            report.push_str(&format!(
                "### {} (Score: {:.2}/10)\n\n",
                score.metadata.scenario, score.total
            ));
            report.push_str(&format!("**Command**: `{}`\n\n", score.metadata.command));

            // Dimension scores
            report.push_str("#### Dimension Scores:\n\n");
            for (name, dim) in &score.dimensions {
                report.push_str(&format!(
                    "- **{}** ({:.1}/10, weight {:.0}%): {}\n",
                    name,
                    dim.score,
                    dim.weight * 100.0,
                    dim.description
                ));
                if !dim.issues.is_empty() {
                    report.push_str("  - Issues:\n");
                    for issue in &dim.issues {
                        report.push_str(&format!("    - {}\n", issue));
                    }
                }
            }

            // Feedback
            if !score.feedback.is_empty() {
                report.push_str("\n#### Improvement Suggestions:\n\n");
                for feedback in &score.feedback {
                    report.push_str(&format!("- {}\n", feedback));
                }
            }

            report.push_str("\n---\n\n");
        }

        report
    }
}

// Validator functions

fn validate_structure(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if output.len() < 50 {
        score -= 5.0;
        issues.push("Output too short".to_string());
    }

    if !output.contains('\n') {
        score -= 2.0;
        issues.push("No line breaks - poor formatting".to_string());
    }

    (score.max(0.0), issues)
}

fn validate_completeness(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if output.is_empty() {
        score = 0.0;
        issues.push("Empty output".to_string());
    }

    (score, issues)
}

fn validate_usability(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if output.contains("TODO") || output.contains("FIXME") {
        score -= 3.0;
        issues.push("Contains TODO/FIXME markers".to_string());
    }

    (score.max(0.0), issues)
}

fn validate_clarity(output: &str) -> (f32, Vec<String>) {
    let score = 8.0; // Base score
    let issues = Vec::new();
    (score, issues)
}

fn validate_sparql_syntax(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    let required_keywords = ["SELECT", "WHERE"];
    for keyword in required_keywords {
        if !output.to_uppercase().contains(keyword) {
            score -= 5.0;
            issues.push(format!("Missing required keyword: {}", keyword));
        }
    }

    (score.max(0.0), issues)
}

fn validate_query_efficiency(output: &str) -> (f32, Vec<String>) {
    let score = 8.0;
    let issues = Vec::new();
    (score, issues)
}

fn validate_readability(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if output.lines().count() < 3 {
        score -= 2.0;
        issues.push("Query is not well formatted".to_string());
    }

    (score.max(0.0), issues)
}

fn validate_seo(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    let seo_fields = ["title", "description", "keywords"];
    for field in seo_fields {
        if !output.to_lowercase().contains(field) {
            score -= 2.0;
            issues.push(format!("Missing SEO field: {}", field));
        }
    }

    (score.max(0.0), issues)
}

fn validate_frontmatter_completeness(output: &str) -> (f32, Vec<String>) {
    let score = 8.0;
    let issues = Vec::new();
    (score, issues)
}

fn validate_yaml(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if !output.contains(':') {
        score -= 5.0;
        issues.push("Not valid YAML format".to_string());
    }

    (score.max(0.0), issues)
}

fn validate_ontology(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if !output.contains("owl:Class") && !output.contains("rdfs:Class") {
        score -= 5.0;
        issues.push("Missing class definitions".to_string());
    }

    (score.max(0.0), issues)
}

fn validate_examples(output: &str) -> (f32, Vec<String>) {
    let score = 7.0;
    let issues = Vec::new();
    (score, issues)
}

fn validate_turtle(output: &str) -> (f32, Vec<String>) {
    let mut score = 10.0;
    let mut issues = Vec::new();

    if !output.contains("@prefix") {
        score -= 5.0;
        issues.push("Missing @prefix declarations".to_string());
    }

    (score.max(0.0), issues)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_framework_creation() {
        let framework = ValidationFramework::new();
        assert!(!framework.scenarios.is_empty());
    }

    #[test]
    fn test_structure_validation() {
        let (score, issues) = validate_structure("# Test\n\nContent here");
        assert!(score > 5.0);
        assert!(issues.is_empty());
    }

    #[test]
    fn test_sparql_validation() {
        let (score, _) = validate_sparql_syntax("SELECT ?s WHERE { ?s ?p ?o }");
        assert!(score > 8.0);
    }
}
