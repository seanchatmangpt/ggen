//! Executable validation runner
//!
//! This module executes the validation framework against real LLM outputs
//! and generates comprehensive scoring reports.

use std::fs;
use std::process::Command;
use std::time::Instant;

mod validation_framework;
use validation_framework::*;

/// Run real LLM commands and capture outputs
fn execute_real_command(command: &str, args: &[String]) -> (String, u64) {
    let start = Instant::now();

    let mut cmd = Command::new("cargo");
    cmd.arg("run").arg("--release").arg("--").arg(command);

    for arg in args {
        cmd.arg(arg);
    }

    let output = cmd.output().expect("Failed to execute command");
    let duration = start.elapsed().as_millis() as u64;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    (format!("{}\n{}", stdout, stderr), duration)
}

/// Enhanced validation framework that runs real commands
pub struct RealWorldValidator {
    #[allow(dead_code)]
    framework: ValidationFramework,
}

impl Default for RealWorldValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl RealWorldValidator {
    pub fn new() -> Self {
        Self {
            framework: ValidationFramework::new(),
        }
    }

    /// Run all scenarios with real LLM outputs
    pub fn run_real_validation(&self) -> Vec<QualityScore> {
        let scenarios = vec![
            (
                "simple_template",
                "ai generate",
                vec![
                    "--description",
                    "Create a simple hello world web page with HTML and CSS",
                    "--max-tokens",
                    "500",
                    "--output",
                    "/tmp/test_simple_template.txt",
                ],
            ),
            (
                "complex_sparql",
                "ai sparql",
                vec![
                    "--description",
                    "Find all users with age > 18 who have made purchases in the last 30 days",
                    "--max-tokens",
                    "300",
                    "--output",
                    "/tmp/test_sparql.txt",
                ],
            ),
            (
                "blog_frontmatter",
                "ai frontmatter",
                vec![
                    "--description",
                    "Blog post about AI ethics with SEO optimization",
                    "--max-tokens",
                    "200",
                    "--output",
                    "/tmp/test_frontmatter.txt",
                ],
            ),
            (
                "ontology_graph",
                "ai graph",
                vec![
                    "--description",
                    "E-commerce product catalog with categories and pricing",
                    "--include-examples",
                    "--max-tokens",
                    "400",
                    "--output",
                    "/tmp/test_ontology.ttl",
                ],
            ),
        ];

        let mut scores = Vec::new();

        for (name, command, args) in scenarios {
            println!("🧪 Running validation for: {}", name);

            let args_vec: Vec<String> = args.iter().map(|s| s.to_string()).collect();
            let (output, duration) = execute_real_command(command, &args_vec);

            // Read the output file if it exists
            let output_file = args_vec
                .iter()
                .position(|a| a == "--output")
                .and_then(|i| args_vec.get(i + 1))
                .and_then(|path| fs::read_to_string(path).ok())
                .unwrap_or_else(|| output.clone());

            let mut score = self.score_real_output(&output_file, name, command);
            score.metadata.execution_time_ms = duration;

            println!("   Score: {:.2}/10", score.total);
            scores.push(score);
        }

        scores
    }

    /// Score real LLM output with comprehensive criteria
    fn score_real_output(&self, output: &str, scenario: &str, command: &str) -> QualityScore {
        use std::collections::HashMap;

        let mut dimensions = HashMap::new();
        let mut feedback = Vec::new();

        // Dimension 1: Length appropriateness (0-10)
        let length_score = self.score_length(output);
        dimensions.insert(
            "length".to_string(),
            DimensionScore {
                score: length_score.0,
                weight: 0.1,
                description: "Output length is appropriate".to_string(),
                issues: length_score.1.clone(),
                suggestions: if length_score.0 < 7.0 {
                    vec!["Adjust max_tokens parameter to get better length".to_string()]
                } else {
                    vec![]
                },
            },
        );

        // Dimension 2: Format correctness (0-10)
        let format_score = self.score_format(output, command);
        dimensions.insert(
            "format".to_string(),
            DimensionScore {
                score: format_score.0,
                weight: 0.2,
                description: "Output format is correct".to_string(),
                issues: format_score.1.clone(),
                suggestions: if format_score.0 < 7.0 {
                    vec!["Improve prompt to specify exact format requirements".to_string()]
                } else {
                    vec![]
                },
            },
        );

        // Dimension 3: Content quality (0-10)
        let content_score = self.score_content_quality(output);
        dimensions.insert(
            "content_quality".to_string(),
            DimensionScore {
                score: content_score.0,
                weight: 0.25,
                description: "Content is high quality and relevant".to_string(),
                issues: content_score.1.clone(),
                suggestions: if content_score.0 < 7.0 {
                    vec!["Add more specific requirements to the prompt".to_string()]
                } else {
                    vec![]
                },
            },
        );

        // Dimension 4: Usability (0-10)
        let usability_score = self.score_usability(output);
        dimensions.insert(
            "usability".to_string(),
            DimensionScore {
                score: usability_score.0,
                weight: 0.2,
                description: "Output is immediately usable".to_string(),
                issues: usability_score.1.clone(),
                suggestions: if usability_score.0 < 7.0 {
                    vec!["Request more complete examples in prompt".to_string()]
                } else {
                    vec![]
                },
            },
        );

        // Dimension 5: Creativity/Insight (0-10)
        let creativity_score = self.score_creativity(output);
        dimensions.insert(
            "creativity".to_string(),
            DimensionScore {
                score: creativity_score.0,
                weight: 0.15,
                description: "Output shows creativity and insight".to_string(),
                issues: creativity_score.1.clone(),
                suggestions: if creativity_score.0 < 7.0 {
                    vec!["Increase temperature for more creative outputs".to_string()]
                } else {
                    vec![]
                },
            },
        );

        // Dimension 6: Professional quality (0-10)
        let professional_score = self.score_professional_quality(output);
        dimensions.insert(
            "professional".to_string(),
            DimensionScore {
                score: professional_score.0,
                weight: 0.1,
                description: "Output meets professional standards".to_string(),
                issues: professional_score.1.clone(),
                suggestions: if professional_score.0 < 7.0 {
                    vec!["Add quality requirements to prompt".to_string()]
                } else {
                    vec![]
                },
            },
        );

        // Calculate weighted total
        let total = dimensions.values().map(|d| d.score * d.weight).sum::<f32>()
            / dimensions.values().map(|d| d.weight).sum::<f32>();

        // Collect all feedback
        for (name, dim) in &dimensions {
            if dim.score < 7.0 {
                feedback.push(format!(
                    "⚠️  {} scored {:.1}/10 - needs improvement",
                    name, dim.score
                ));
                feedback.extend(dim.suggestions.clone());
            }
        }

        // Add overall feedback based on total score
        if total >= 9.0 {
            feedback.insert(0, "🌟 Excellent! Output quality is exceptional".to_string());
        } else if total >= 7.0 {
            feedback.insert(
                0,
                "✅ Good! Output quality is solid with minor improvements possible".to_string(),
            );
        } else if total >= 5.0 {
            feedback.insert(
                0,
                "⚠️  Fair - Output needs significant improvement".to_string(),
            );
        } else {
            feedback.insert(
                0,
                "❌ Poor - Output quality is below acceptable standards".to_string(),
            );
        }

        QualityScore {
            total,
            dimensions,
            feedback,
            metadata: TestMetadata {
                command: command.to_string(),
                scenario: scenario.to_string(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                model: "qwen3-coder:30b".to_string(),
                execution_time_ms: 0,
            },
        }
    }

    fn score_length(&self, output: &str) -> (f32, Vec<String>) {
        let len = output.len();
        let mut issues = Vec::new();

        let score = match len {
            0..=50 => {
                issues.push("Output too short".to_string());
                2.0
            }
            51..=100 => {
                issues.push("Output quite short".to_string());
                5.0
            }
            101..=500 => 9.0,
            501..=2000 => 10.0,
            2001..=5000 => 8.0,
            _ => {
                issues.push("Output might be too verbose".to_string());
                6.0
            }
        };

        (score, issues)
    }

    fn score_format(&self, output: &str, command: &str) -> (f32, Vec<String>) {
        let mut score: f32 = 10.0;
        let mut issues = Vec::new();

        if command.contains("sparql") {
            if !output.to_uppercase().contains("SELECT") {
                score -= 5.0;
                issues.push("Missing SELECT clause".to_string());
            }
            if !output.to_uppercase().contains("WHERE") {
                score -= 3.0;
                issues.push("Missing WHERE clause".to_string());
            }
        } else if command.contains("graph") {
            if !output.contains("@prefix") {
                score -= 4.0;
                issues.push("Missing @prefix declarations".to_string());
            }
        } else if command.contains("frontmatter")
            && !output.contains("title")
            && !output.contains("Title")
        {
            score -= 3.0;
            issues.push("Missing title field".to_string());
        }

        (score.max(0.0f32), issues)
    }

    fn score_content_quality(&self, output: &str) -> (f32, Vec<String>) {
        let mut score: f32 = 8.0; // Base score
        let mut issues = Vec::new();

        // Check for placeholder text
        if output.contains("TODO") || output.contains("FIXME") || output.contains("placeholder") {
            score -= 2.0;
            issues.push("Contains placeholder text".to_string());
        }

        // Check for errors
        if output.contains("error") || output.contains("Error") {
            score -= 3.0;
            issues.push("Contains error messages".to_string());
        }

        // Check for minimal effort
        if output.split_whitespace().count() < 20 {
            score -= 2.0;
            issues.push("Content seems minimal".to_string());
        }

        (score.max(0.0f32), issues)
    }

    fn score_usability(&self, output: &str) -> (f32, Vec<String>) {
        let mut score: f32 = 9.0;
        let mut issues = Vec::new();

        // Check for comments/documentation
        if !output.contains("#") && !output.contains("//") && !output.contains("/*") {
            score -= 1.0;
            issues.push("Missing comments or documentation".to_string());
        }

        // Check for examples
        if output.len() > 200 && !output.contains("example") && !output.contains("Example") {
            score -= 1.0;
            issues.push("Could benefit from examples".to_string());
        }

        (score.max(0.0f32), issues)
    }

    fn score_creativity(&self, output: &str) -> (f32, Vec<String>) {
        let mut score: f32 = 7.0; // Base score
        let mut issues = Vec::new();

        // Simple heuristics for creativity
        let unique_words: std::collections::HashSet<_> = output.split_whitespace().collect();

        if unique_words.len() < 10 {
            score -= 2.0;
            issues.push("Limited vocabulary used".to_string());
        } else if unique_words.len() > 50 {
            score += 2.0; // Bonus for rich vocabulary
        }

        (score.clamp(0.0f32, 10.0f32), issues)
    }

    fn score_professional_quality(&self, output: &str) -> (f32, Vec<String>) {
        let mut score: f32 = 8.0;
        let mut issues = Vec::new();

        // Check formatting
        if !output.contains('\n') {
            score -= 2.0;
            issues.push("Poor formatting - no line breaks".to_string());
        }

        // Check for proper capitalization
        let first_char = output.chars().next();
        if let Some(c) = first_char {
            if c.is_lowercase() && !output.starts_with('#') {
                score -= 1.0;
                issues.push("Should start with capital letter".to_string());
            }
        }

        (score.max(0.0f32), issues)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_real_validator_creation() {
        let _validator = RealWorldValidator::new();
        // Just ensure it creates successfully
    }
}
