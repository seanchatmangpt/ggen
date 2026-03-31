//! Template Validator Agent - Validates and fixes Tera template syntax errors
//!
//! This agent autonomously validates Tera templates for syntax correctness,
//! detects undefined variables, fixes filter issues, and cross-references
//! variables against SPARQL results to ensure template validity.

use crate::error::{GgenAiError, Result};
use crate::generators::TemplateValidator;
use crate::swarm::{
    AgentConfig, AgentHealth, AgentInput, AgentOutput, BaseAgent, HealthStatus,
    PerformanceThresholds, SwarmAgent, SwarmContext,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use tera::{Error as TeraError, Tera};
use tracing::{debug, info, warn};

/// Template Validator Agent implementation
#[derive(Debug)]
pub struct TemplateValidatorAgent {
    base: BaseAgent,
    sparql_results: Option<Value>,
    dry_run: bool,
    template_validator: TemplateValidator,
}

/// Template validation report with fixes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Template file path or identifier
    pub template_path: String,
    /// Whether template is valid
    pub is_valid: bool,
    /// Issues detected
    pub issues: Vec<TemplateIssue>,
    /// Fixes applied
    pub fixes: Vec<TemplateFix>,
    /// Validation timestamp
    pub timestamp: String,
    /// Quality score (0.0 - 1.0)
    pub quality_score: f64,
}

/// Template issue with severity and location
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateIssue {
    /// Issue type
    pub issue_type: IssueType,
    /// Issue severity
    pub severity: IssueSeverity,
    /// Line number (1-based)
    pub line_number: Option<usize>,
    /// Column number (1-based)
    pub column: Option<usize>,
    /// Issue description
    pub description: String,
    /// Context snippet
    pub context: Option<String>,
}

/// Template issue types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IssueType {
    /// Syntax error in template
    SyntaxError,
    /// Undefined variable reference
    UndefinedVariable,
    /// Invalid filter syntax
    InvalidFilter,
    /// Missing closing tag
    MissingClosingTag,
    /// Wrong variable name from SPARQL
    WrongVariableName,
    /// Type mismatch
    TypeMismatch,
    /// Unused variable
    UnusedVariable,
}

/// Issue severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum IssueSeverity {
    /// Information only
    Info,
    /// Warning - non-blocking
    Warning,
    /// Error - blocking
    Error,
    /// Critical - must fix
    Critical,
}

/// Template fix with application details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateFix {
    /// Fix type
    pub fix_type: FixType,
    /// Line number (1-based)
    pub line_number: usize,
    /// Original content
    pub original: String,
    /// Fixed content
    pub fixed: String,
    /// Fix description
    pub description: String,
    /// Confidence in fix (0.0 - 1.0)
    pub confidence: f64,
}

/// Template fix types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FixType {
    /// Remove undefined variable
    RemoveVariable,
    /// Mark variable as optional
    MarkOptional,
    /// Fix filter syntax
    FixFilter,
    /// Add missing closing tag
    AddClosingTag,
    /// Rename variable to match SPARQL
    RenameVariable,
    /// Cast to correct type
    CastType,
}

/// SPARQL variable mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlVariable {
    /// Variable name (with or without ? prefix)
    pub name: String,
    /// Variable type
    pub var_type: String,
    /// Whether variable is always present
    pub required: bool,
}

impl TemplateValidatorAgent {
    /// Create a new Template Validator Agent
    pub fn new(sparql_results: Option<Value>, dry_run: bool) -> Self {
        let base = BaseAgent::new(
            "template_validator",
            vec![
                "template_validation".to_string(),
                "syntax_checking".to_string(),
                "variable_validation".to_string(),
                "auto_fixing".to_string(),
                "sparql_cross_reference".to_string(),
            ],
            AgentConfig {
                timeout_seconds: 60,
                retry_attempts: 2,
                verbose_logging: false,
                performance_thresholds: PerformanceThresholds {
                    max_execution_time_ms: 10000,
                    max_memory_usage_mb: 200,
                    min_quality_score: 0.85,
                },
            },
        );

        let template_validator = TemplateValidator::new();

        Self {
            base,
            sparql_results,
            dry_run,
            template_validator,
        }
    }

    /// Validate and fix a template
    pub fn validate_and_fix(
        &self, template: &str, template_path: &str,
    ) -> Result<ValidationReport> {
        let mut issues = Vec::new();
        let mut fixes = Vec::new();

        // Step 1: Parse and validate syntax
        let syntax_issues = self.validate_syntax(template)?;
        issues.extend(syntax_issues);

        // Step 2: Extract variables from template
        let template_vars = self.extract_template_variables(template);

        // Step 3: Cross-reference with SPARQL results if available
        if let Some(sparql_results) = &self.sparql_results {
            let sparql_vars = self.extract_sparql_variables(sparql_results)?;
            let cross_ref_issues =
                self.cross_reference_variables(&template_vars, &sparql_vars, template);
            issues.extend(cross_ref_issues.0);
            fixes.extend(cross_ref_issues.1);
        }

        // Step 4: Check for undefined variables
        let undefined_issues = self.check_undefined_variables(&template_vars, template);
        issues.extend(undefined_issues.0);
        fixes.extend(undefined_issues.1);

        // Step 5: Validate filter syntax
        let filter_issues = self.validate_filters(template);
        issues.extend(filter_issues.0);
        fixes.extend(filter_issues.1);

        // Step 6: Calculate quality score
        let quality_score = self.calculate_quality_score(&issues);

        let is_valid = issues
            .iter()
            .all(|issue| issue.severity < IssueSeverity::Error);

        Ok(ValidationReport {
            template_path: template_path.to_string(),
            is_valid,
            issues,
            fixes,
            timestamp: chrono::Utc::now().to_rfc3339(),
            quality_score,
        })
    }

    /// Validate template syntax using Tera parser
    fn validate_syntax(&self, template: &str) -> Result<Vec<TemplateIssue>> {
        let mut issues = Vec::new();
        let mut tera = Tera::default();

        match tera.add_raw_template("validation_template", template) {
            Ok(_) => {
                debug!("Template syntax is valid");
            }
            Err(e) => {
                // Parse Tera error to extract line and column
                let (line, col, message) = self.parse_tera_error(&e);
                issues.push(TemplateIssue {
                    issue_type: IssueType::SyntaxError,
                    severity: IssueSeverity::Error,
                    line_number: line,
                    column: col,
                    description: message,
                    context: self.extract_context(template, line, 2),
                });
            }
        }

        Ok(issues)
    }

    /// Parse Tera error to extract location information
    fn parse_tera_error(&self, error: &TeraError) -> (Option<usize>, Option<usize>, String) {
        let message = error.to_string();

        // Try to extract line and column from error message
        // Tera errors typically include line numbers like "Error: line 5, column 10"
        let line = if let Some(pos) = message.find("line ") {
            let after = &message[pos + 5..];
            after.split(',').next().and_then(|s| s.trim().parse().ok())
        } else {
            None
        };

        let col = if let Some(pos) = message.find("column ") {
            let after = &message[pos + 7..];
            after
                .split(|c: char| !c.is_ascii_digit())
                .next()
                .and_then(|s| s.trim().parse().ok())
        } else {
            None
        };

        (line, col, message)
    }

    /// Extract context around a line for error reporting
    fn extract_context(
        &self, template: &str, line_number: Option<usize>, context_lines: usize,
    ) -> Option<String> {
        let line = line_number?;
        let lines: Vec<&str> = template.lines().collect();

        if line == 0 || line > lines.len() {
            return None;
        }

        let start = if line > context_lines {
            line - context_lines - 1
        } else {
            0
        };
        let end = (line + context_lines).min(lines.len());

        let context: Vec<String> = lines[start..end]
            .iter()
            .enumerate()
            .map(|(i, l)| {
                let num = start + i + 1;
                let marker = if num == line { " > " } else { "   " };
                format!("{}{:4}: {}", marker, num, l)
            })
            .collect();

        Some(context.join("\n"))
    }

    /// Extract variable names from Tera template
    fn extract_template_variables(&self, template: &str) -> HashSet<String> {
        let mut vars = HashSet::new();
        let mut chars = template.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '{' {
                if let Some(&'{') = chars.peek() {
                    chars.next();
                    let mut var_name = String::new();

                    while let Some(&c) = chars.peek() {
                        if c == '}' {
                            chars.next();
                            if let Some(&'}') = chars.peek() {
                                chars.next();
                                break;
                            }
                            var_name.push('}');
                            continue;
                        }
                        var_name.push(chars.next().unwrap());
                    }

                    if !var_name.is_empty() {
                        let var_clean = var_name
                            .trim()
                            .split('|')
                            .next()
                            .map(|s| s.trim())
                            .unwrap_or(var_name.trim())
                            .to_string();
                        vars.insert(var_clean);
                    }
                }
            }
        }

        vars
    }

    /// Extract variable names from SPARQL JSON results
    fn extract_sparql_variables(&self, sparql_results: &Value) -> Result<HashSet<SparqlVariable>> {
        let mut vars = HashSet::new();

        if let Some(results) = sparql_results.get("results") {
            if let Some(bindings) = results.get("bindings").and_then(|b| b.as_array()) {
                // Collect all variable names from bindings
                for binding in bindings {
                    if let Some(obj) = binding.as_object() {
                        for (key, value) in obj {
                            let var_type = value
                                .get("type")
                                .and_then(|t| t.as_str())
                                .unwrap_or("literal")
                                .to_string();

                            vars.insert(SparqlVariable {
                                name: key.clone(),
                                var_type,
                                required: true, // Assume required if present in bindings
                            });
                        }
                    }
                }
            }
        }

        // Also check for variables in head
        if let Some(head) = sparql_results.get("head") {
            if let Some(vars_array) = head.get("vars").and_then(|v| v.as_array()) {
                for var_name in vars_array {
                    if let Some(name) = var_name.as_str() {
                        vars.insert(SparqlVariable {
                            name: name.to_string(),
                            var_type: "unknown".to_string(),
                            required: false,
                        });
                    }
                }
            }
        }

        Ok(vars)
    }

    /// Cross-reference template variables with SPARQL results
    fn cross_reference_variables(
        &self, template_vars: &HashSet<String>, sparql_vars: &HashSet<SparqlVariable>,
        template: &str,
    ) -> (Vec<TemplateIssue>, Vec<TemplateFix>) {
        let mut issues = Vec::new();
        let mut fixes = Vec::new();

        let sparql_var_names: HashSet<String> =
            sparql_vars.iter().map(|v| v.name.clone()).collect();

        for template_var in template_vars {
            // Skip special Tera variables
            if template_var.starts_with("__tera") || template_var == "loop" {
                continue;
            }

            if !sparql_var_names.contains(template_var) {
                // Variable not found in SPARQL results
                let line = self.find_variable_line(template, template_var);

                issues.push(TemplateIssue {
                    issue_type: IssueType::UndefinedVariable,
                    severity: IssueSeverity::Warning,
                    line_number: line,
                    column: None,
                    description: format!(
                        "Variable '{{{{ {} }}}}' not found in SPARQL results. Available variables: {}",
                        template_var,
                        sparql_var_names.iter().collect::<Vec<_>>().join(", ")
                    ),
                    context: self.extract_context(template, line, 2),
                });

                // Suggest similar variable names
                if let Some(similar) = self.find_similar_variable(template_var, &sparql_var_names) {
                    fixes.push(TemplateFix {
                        fix_type: FixType::RenameVariable,
                        line_number: line.unwrap_or(1),
                        original: format!("{{{{{ }}}}}", template_var),
                        fixed: format!("{{{{{ }}}}}", similar),
                        description: format!(
                            "Rename '{}' to match SPARQL variable '{}'",
                            template_var, similar
                        ),
                        confidence: 0.8,
                    });
                } else {
                    // Mark as optional
                    fixes.push(TemplateFix {
                        fix_type: FixType::MarkOptional,
                        line_number: line.unwrap_or(1),
                        original: format!("{{{{{}}}}}", template_var),
                        fixed: format!("{{{{{}}}|default(value=\"\")}}", template_var),
                        description: format!(
                            "Mark '{}' as optional with default value",
                            template_var
                        ),
                        confidence: 0.6,
                    });
                }
            }
        }

        (issues, fixes)
    }

    /// Find the line number where a variable is used
    fn find_variable_line(&self, template: &str, variable: &str) -> Option<usize> {
        let search_pattern = format!("{{{{{}}}}}", variable);
        template
            .lines()
            .position(|line| line.contains(&search_pattern))
            .map(|pos| pos + 1) // Convert to 1-based
    }

    /// Find similar variable names using simple edit distance
    fn find_similar_variable(
        &self, template_var: &str, sparql_vars: &HashSet<String>,
    ) -> Option<String> {
        let mut best_match: Option<(String, usize)> = None;

        for sparql_var in sparql_vars {
            let distance = edit_distance(template_var, sparql_var);
            if distance <= 2 && distance < template_var.len() / 2 {
                if best_match.as_ref().map_or(true, |(_, d)| distance < *d) {
                    best_match = Some((sparql_var.clone(), distance));
                }
            }
        }

        best_match.map(|(name, _)| name)
    }

    /// Check for undefined variables and suggest fixes
    fn check_undefined_variables(
        &self, template_vars: &HashSet<String>, template: &str,
    ) -> (Vec<TemplateIssue>, Vec<TemplateFix>) {
        let mut issues = Vec::new();
        let mut fixes = Vec::new();

        // Known Tera built-in variables
        let builtins = ["loop", "super", "__tera_context", "__tera_static"];

        for var in template_vars {
            if builtins.contains(&var.as_str()) {
                continue;
            }

            // Check if variable is likely to be undefined
            // This is heuristic - in practice, variables might be defined at runtime
            let line = self.find_variable_line(template, var);

            // Warn about variables that look like they might be typos
            if var.contains('_') && var.chars().filter(|&c| c == '_').count() > 2 {
                issues.push(TemplateIssue {
                    issue_type: IssueType::UndefinedVariable,
                    severity: IssueSeverity::Info,
                    line_number: line,
                    column: None,
                    description: format!(
                        "Variable '{{{{ {} }}}}' has unusual naming (many underscores). Verify this is intentional.",
                        var
                    ),
                    context: self.extract_context(template, line, 2),
                });
            }
        }

        (issues, fixes)
    }

    /// Validate filter syntax and suggest fixes
    fn validate_filters(&self, template: &str) -> (Vec<TemplateIssue>, Vec<TemplateFix>) {
        let mut issues = Vec::new();
        let mut fixes = Vec::new();

        // Known Tera filters
        let known_filters = [
            "upper",
            "lower",
            "title",
            "trim",
            "trim_start",
            "trim_end",
            "truncate",
            "wordcount",
            "replace",
            "capitalize",
            "slugify",
            "first",
            "last",
            "nth",
            "length",
            "reverse",
            "sort",
            "unique",
            "join",
            "split",
            "default",
            "json_encode",
            "json_decode",
            "safe",
            "escape",
            "e",
            "int",
            "float",
            "bool",
            "string",
            "date",
            "filesizeformat",
            "abs",
            "round",
        ];

        let lines: Vec<&str> = template.lines().enumerate().collect();

        for (idx, line) in lines {
            // Find filter usages with pipe |
            let mut chars = line.chars().peekable();
            let mut pos = 0;

            while let Some(c) = chars.next() {
                pos += 1;
                if c == '|' {
                    // Extract filter name
                    let mut filter_name = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_whitespace() || c == '}' || c == '(' {
                            break;
                        }
                        filter_name.push(chars.next().unwrap());
                    }

                    if !filter_name.is_empty() && !known_filters.contains(&filter_name.as_str()) {
                        issues.push(TemplateIssue {
                            issue_type: IssueType::InvalidFilter,
                            severity: IssueSeverity::Warning,
                            line_number: Some(idx + 1),
                            column: Some(pos),
                            description: format!(
                                "Unknown filter '{}'. Known filters: {}",
                                filter_name,
                                known_filters.join(", ")
                            ),
                            context: Some(line.to_string()),
                        });

                        // Suggest similar filter
                        if let Some(similar) =
                            self.find_similar_filter(&filter_name, &known_filters)
                        {
                            fixes.push(TemplateFix {
                                fix_type: FixType::FixFilter,
                                line_number: idx + 1,
                                original: format!("|{}", filter_name),
                                fixed: format!("|{}", similar),
                                description: format!(
                                    "Replace unknown filter '{}' with '{}'",
                                    filter_name, similar
                                ),
                                confidence: 0.7,
                            });
                        }
                    }
                }
            }
        }

        (issues, fixes)
    }

    /// Find similar filter name using edit distance
    fn find_similar_filter(&self, filter: &str, known_filters: &[&str]) -> Option<String> {
        let mut best_match: Option<(String, usize)> = None;

        for &known in known_filters {
            let distance = edit_distance(filter, known);
            if distance <= 2 && distance < filter.len() / 2 {
                if best_match.as_ref().map_or(true, |(_, d)| distance < *d) {
                    best_match = Some((known.to_string(), distance));
                }
            }
        }

        best_match.map(|(name, _)| name)
    }

    /// Calculate quality score based on issues
    fn calculate_quality_score(&self, issues: &[TemplateIssue]) -> f64 {
        let mut score = 1.0;

        for issue in issues {
            match issue.severity {
                IssueSeverity::Critical => score -= 0.4,
                IssueSeverity::Error => score -= 0.3,
                IssueSeverity::Warning => score -= 0.1,
                IssueSeverity::Info => score -= 0.05,
            }
        }

        score.max(0.0)
    }

    /// Apply fixes to template (if not dry run)
    pub fn apply_fixes(&self, template: &str, fixes: &[TemplateFix]) -> Result<String> {
        if self.dry_run {
            info!("Dry run: skipping fix application");
            return Ok(template.to_string());
        }

        let mut result = template.to_string();
        let mut line_offset = 0;

        // Sort fixes by line number in descending order to avoid offset issues
        let mut sorted_fixes: Vec<_> = fixes.iter().collect();
        sorted_fixes.sort_by(|a, b| b.line_number.cmp(&a.line_number));

        for fix in sorted_fixes {
            let lines: Vec<&str> = result.lines().collect();
            if fix.line_number == 0 || fix.line_number > lines.len() {
                warn!("Invalid line number {} for fix, skipping", fix.line_number);
                continue;
            }

            let line_idx = fix.line_number - 1;
            let line = lines[line_idx];

            // Replace original with fixed content
            let new_line = line.replace(&fix.original, &fix.fixed);

            // Rebuild template with fixed line
            let before: String = lines[..line_idx].join("\n");
            let after: String = if line_idx + 1 < lines.len() {
                lines[line_idx + 1..].join("\n")
            } else {
                String::new()
            };

            result = format!("{}\n{}\n{}", before, new_line, after);
        }

        Ok(result)
    }
}

impl SwarmAgent for TemplateValidatorAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        let start_time = std::time::Instant::now();

        // Extract template from input
        let template = input
            .data
            .get("template")
            .and_then(|v| v.as_str())
            .ok_or_else(|| GgenAiError::parsing_error("No template in input"))?;

        let template_path = input
            .data
            .get("template_path")
            .and_then(|v| v.as_str())
            .unwrap_or("inline");

        // Validate and fix
        let report = self.validate_and_fix(template, template_path)?;

        // Apply fixes if not dry run
        let fixed_template = if !report.fixes.is_empty() && !self.dry_run {
            Some(self.apply_fixes(template, &report.fixes)?)
        } else {
            None
        };

        let execution_time = start_time.elapsed().as_millis();

        let output_data = json!({
            "validation_report": report,
            "fixed_template": fixed_template,
            "dry_run": self.dry_run,
        });

        Ok(AgentOutput {
            data: output_data,
            output_type: "template_validation".to_string(),
            target_agents: vec!["code_generator".to_string()],
            metadata: {
                let mut metadata = HashMap::new();
                metadata.insert("execution_time_ms".to_string(), execution_time.to_string());
                metadata.insert("issues_found".to_string(), report.issues.len().to_string());
                metadata.insert("fixes_applied".to_string(), report.fixes.len().to_string());
                metadata.insert(
                    "quality_score".to_string(),
                    report.quality_score.to_string(),
                );
                metadata
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Validate template validator
        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        AgentHealth {
            status: HealthStatus::Healthy,
            score: 1.0,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues: vec![],
        }
    }
}

/// Simple edit distance calculation for finding similar names
fn edit_distance(a: &str, b: &str) -> usize {
    let mut matrix = vec![vec![0; b.len() + 1]; a.len() + 1];

    for i in 0..=a.len() {
        matrix[i][0] = i;
    }
    for j in 0..=b.len() {
        matrix[0][j] = j;
    }

    for (i, ca) in a.chars().enumerate() {
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            matrix[i + 1][j + 1] = [
                matrix[i][j + 1] + 1, // deletion
                matrix[i + 1][j] + 1, // insertion
                matrix[i][j] + cost,  // substitution
            ]
            .iter()
            .min()
            .copied()
            .unwrap();
        }
    }

    matrix[a.len()][b.len()]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_edit_distance() {
        assert_eq!(edit_distance("kitten", "sitting"), 3);
        assert_eq!(edit_distance("foo", "bar"), 3);
        assert_eq!(edit_distance("test", "test"), 0);
        assert_eq!(edit_distance("var_name", "varname"), 1);
    }

    #[test]
    fn test_extract_template_variables() {
        let agent = TemplateValidatorAgent::new(None, false);
        let template = r#"
            {{ name }}
            {{ age|default(value=0) }}
            {{ user.email }}
            {% for item in items %}
                {{ item.value }}
            {% endfor %}
        "#;

        let vars = agent.extract_template_variables(template);
        assert!(vars.contains("name"));
        assert!(vars.contains("age"));
        assert!(vars.contains("user.email"));
        assert!(vars.contains("item.value"));
        assert!(vars.contains("items"));
    }

    #[test]
    fn test_validate_syntax_valid() {
        let agent = TemplateValidatorAgent::new(None, false);
        let template = "{{ name }} is {{ age }} years old.";
        let issues = agent.validate_syntax(template).unwrap();
        assert!(issues.is_empty());
    }

    #[test]
    fn test_validate_syntax_invalid() {
        let agent = TemplateValidatorAgent::new(None, false);
        let template = "{{ name }"; // Missing closing }}
        let issues = agent.validate_syntax(template).unwrap();
        assert!(!issues.is_empty());
        assert!(matches!(issues[0].issue_type, IssueType::SyntaxError));
    }

    #[test]
    fn test_validate_and_fix() {
        let agent = TemplateValidatorAgent::new(None, true);
        let template = "{{ name }} is {{ ag }} years old.";

        let report = agent.validate_and_fix(template, "test.tera").unwrap();
        assert!(!report.is_valid || report.quality_score < 1.0);
    }

    #[test]
    fn test_find_similar_variable() {
        let agent = TemplateValidatorAgent::new(None, false);
        let sparql_vars = HashSet::from([
            "user_name".to_string(),
            "user_email".to_string(),
            "user_age".to_string(),
        ]);

        let similar = agent.find_similar_variable("usr_name", &sparql_vars);
        assert_eq!(similar, Some("user_name".to_string()));
    }

    #[tokio::test]
    async fn test_agent_execute() {
        let agent = TemplateValidatorAgent::new(None, true);
        let context = SwarmContext {
            graph_state: String::new(),
            active_agents: vec![],
            metrics: Default::default(),
            config: Default::default(),
        };

        let input = AgentInput {
            data: json!({
                "template": "{{ name }} is {{ age }} years old.",
                "template_path": "test.tera"
            }),
            input_type: "template_validation".to_string(),
            source_agent: None,
            context: HashMap::new(),
        };

        let output = agent.execute(&context, input).await.unwrap();
        assert_eq!(output.output_type, "template_validation");

        let report = output.data.get("validation_report").unwrap();
        assert!(report.get("is_valid").unwrap().as_bool().unwrap());
    }
}
