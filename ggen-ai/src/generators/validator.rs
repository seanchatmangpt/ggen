//! Template validation and quality assessment system
//!
//! This module provides comprehensive validation and quality assessment for generated templates,
//! including syntax checking, best practices validation, and iterative improvement capabilities.

use crate::error::Result;
use ggen_core::Template;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Validation result with detailed feedback
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Overall validation status
    pub is_valid: bool,
    /// Quality score (0.0 to 1.0)
    pub quality_score: f64,
    /// Detailed validation issues
    pub issues: Vec<ValidationIssue>,
    /// Suggestions for improvement
    pub suggestions: Vec<String>,
    /// Metrics for quality assessment
    pub metrics: QualityMetrics,
}

/// Individual validation issue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationIssue {
    /// Issue type/category
    pub issue_type: IssueType,
    /// Severity level
    pub severity: Severity,
    /// Issue description
    pub description: String,
    /// Line number where issue occurs (if applicable)
    pub line_number: Option<usize>,
    /// Column number where issue occurs (if applicable)
    pub column_number: Option<usize>,
    /// Suggested fix
    pub suggestion: Option<String>,
}

/// Types of validation issues
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IssueType {
    /// YAML frontmatter issues
    YamlSyntax,
    /// Template syntax issues
    TemplateSyntax,
    /// RDF/SPARQL validation issues
    RdfValidation,
    /// Best practices violations
    BestPractice,
    /// Completeness issues
    Completeness,
    /// Security issues
    Security,
    /// Performance issues
    Performance,
    /// Style/consistency issues
    Style,
}

/// Severity levels for issues
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Severity {
    /// Critical errors that prevent usage
    Error,
    /// Warnings that should be addressed
    Warning,
    /// Information for improvement
    Info,
}

/// Quality metrics for assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    /// Completeness score (0.0 to 1.0)
    pub completeness: f64,
    /// Correctness score (0.0 to 1.0)
    pub correctness: f64,
    /// Maintainability score (0.0 to 1.0)
    pub maintainability: f64,
    /// Performance score (0.0 to 1.0)
    pub performance: f64,
    /// Security score (0.0 to 1.0)
    pub security: f64,
    /// Readability score (0.0 to 1.0)
    pub readability: f64,
}

/// Template validator for comprehensive validation
pub struct TemplateValidator {
    /// Validation rules and thresholds
    rules: ValidationRules,
}

/// Configuration for validation rules
#[derive(Debug, Clone)]
pub struct ValidationRules {
    /// Minimum quality score required
    pub min_quality_score: f64,
    /// Maximum allowed errors
    pub max_errors: usize,
    /// Maximum allowed warnings
    pub max_warnings: usize,
    /// Enable strict mode (more rigorous validation)
    pub strict_mode: bool,
    /// Custom validation rules
    pub custom_rules: HashMap<String, ValidationRule>,
}

/// Individual validation rule
pub struct ValidationRule {
    /// Rule name
    pub name: String,
    /// Rule description
    pub description: String,
    /// Whether rule is enabled
    pub enabled: bool,
    /// Rule implementation function
    pub validator: Box<dyn Fn(&Template) -> Result<Vec<ValidationIssue>> + Send + Sync>,
}

impl std::fmt::Debug for ValidationRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ValidationRule")
            .field("name", &self.name)
            .field("description", &self.description)
            .field("enabled", &self.enabled)
            .field("validator", &"<function>")
            .finish()
    }
}

impl Clone for ValidationRule {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            description: self.description.clone(),
            enabled: self.enabled,
            validator: Box::new(|_| Ok(vec![])), // Placeholder - cannot clone function
        }
    }
}

impl TemplateValidator {
    /// Create a new template validator with default rules
    pub fn new() -> Self {
        Self {
            rules: ValidationRules::default(),
        }
    }

    /// Create a validator with custom rules
    pub fn with_rules(rules: ValidationRules) -> Self {
        Self { rules }
    }

    /// Validate a template and return detailed results
    pub async fn validate_template(&self, template: &Template) -> Result<ValidationResult> {
        let mut issues = Vec::new();

        // Validate YAML frontmatter
        issues.extend(self.validate_yaml_frontmatter(template)?);

        // Validate template syntax
        issues.extend(self.validate_template_syntax(template)?);

        // Validate RDF/SPARQL content where applicable
        issues.extend(self.validate_rdf_content(template).await?);

        // Check best practices
        issues.extend(self.validate_best_practices(template)?);

        // Assess quality metrics
        let metrics = self.assess_quality(template, &issues)?;

        // Calculate overall quality score
        let quality_score = self.calculate_quality_score(&metrics, &issues);

        // Generate suggestions
        let suggestions = self.generate_suggestions(&issues);

        let is_valid = issues.iter()
            .filter(|issue| matches!(issue.severity, Severity::Error))
            .count() <= self.rules.max_errors;

        Ok(ValidationResult {
            is_valid,
            quality_score,
            issues,
            suggestions,
            metrics,
        })
    }

    /// Validate YAML frontmatter
    fn validate_yaml_frontmatter(&self, template: &Template) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // Check required fields
        if template.front.to.is_none() {
            issues.push(ValidationIssue {
                issue_type: IssueType::YamlSyntax,
                severity: Severity::Warning,
                description: "Template missing 'to' field in frontmatter".to_string(),
                line_number: None,
                column_number: None,
                suggestion: Some("Add 'to' field specifying output file path".to_string()),
            });
        }

        // Validate variable definitions
        for (var_name, _var_value) in &template.front.vars {
            if var_name.trim().is_empty() {
                issues.push(ValidationIssue {
                    issue_type: IssueType::YamlSyntax,
                    severity: Severity::Error,
                    description: "Empty variable name in frontmatter".to_string(),
                    line_number: None,
                    column_number: None,
                    suggestion: Some("Remove empty variable names".to_string()),
                });
            }
        }

        Ok(issues)
    }

    /// Validate template syntax using Tera
    fn validate_template_syntax(&self, template: &Template) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // Try to parse template with Tera
        match tera::Tera::new("templates/**/*") {
            Ok(mut tera) => {
                // Test template compilation
                let template_name = "validation_test";
                if let Err(e) = tera.add_raw_template(template_name, &template.body) {
                    issues.push(ValidationIssue {
                        issue_type: IssueType::TemplateSyntax,
                        severity: Severity::Error,
                        description: format!("Template syntax error: {}", e),
                        line_number: None,
                        column_number: None,
                        suggestion: Some("Fix template syntax errors".to_string()),
                    });
                }

                // Test template rendering with sample data
                let mut context = tera::Context::new();
                context.insert("name", "test");
                context.insert("description", "test description");

                if let Err(e) = tera.render(template_name, &context) {
                    issues.push(ValidationIssue {
                        issue_type: IssueType::TemplateSyntax,
                        severity: Severity::Warning,
                        description: format!("Template rendering error: {}", e),
                        line_number: None,
                        column_number: None,
                        suggestion: Some("Fix template variable usage".to_string()),
                    });
                }
            }
            Err(e) => {
                issues.push(ValidationIssue {
                    issue_type: IssueType::TemplateSyntax,
                    severity: Severity::Error,
                    description: format!("Failed to initialize template engine: {}", e),
                    line_number: None,
                    column_number: None,
                    suggestion: Some("Check template engine configuration".to_string()),
                });
            }
        }

        Ok(issues)
    }

    /// Validate RDF/SPARQL content where applicable
    async fn validate_rdf_content(&self, template: &Template) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // Check for RDF inline content
        if !template.front.rdf_inline.is_empty() {
            for rdf_content in &template.front.rdf_inline {
                // Validate RDF syntax
                if let Err(e) = ggen_core::Graph::new()?.insert_turtle(rdf_content) {
                    issues.push(ValidationIssue {
                        issue_type: IssueType::RdfValidation,
                        severity: Severity::Error,
                        description: format!("Invalid RDF syntax: {}", e),
                        line_number: None,
                        column_number: None,
                        suggestion: Some("Fix RDF syntax errors".to_string()),
                    });
                }
            }
        }

        // Check for SPARQL queries
        for (query_name, sparql_content) in &template.front.sparql {
            // Validate SPARQL syntax
            if let Err(e) = oxigraph::sparql::Query::parse(sparql_content, None) {
                issues.push(ValidationIssue {
                    issue_type: IssueType::RdfValidation,
                    severity: Severity::Error,
                    description: format!("Invalid SPARQL syntax in query '{}': {}", query_name, e),
                    line_number: None,
                    column_number: None,
                    suggestion: Some("Fix SPARQL syntax errors".to_string()),
                });
            }
        }

        Ok(issues)
    }

    /// Validate best practices
    fn validate_best_practices(&self, template: &Template) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // Check for hardcoded paths
        if template.body.contains("/tmp/") || template.body.contains("/var/") {
            issues.push(ValidationIssue {
                issue_type: IssueType::BestPractice,
                severity: Severity::Warning,
                description: "Template contains hardcoded absolute paths".to_string(),
                line_number: None,
                column_number: None,
                suggestion: Some("Use relative paths or template variables".to_string()),
            });
        }

        // Check for missing variable documentation
        let template_vars: Vec<&str> = template.front.vars.keys().map(|s| s.as_str()).collect();
        let documented_vars = self.extract_documented_variables(&template.body);

        for var in &template_vars {
            if !documented_vars.contains(&var.to_string()) {
                issues.push(ValidationIssue {
                    issue_type: IssueType::BestPractice,
                    severity: Severity::Info,
                    description: format!("Variable '{}' is not documented in template", var),
                    line_number: None,
                    column_number: None,
                    suggestion: Some(format!("Add documentation comment for variable {}", var)),
                });
            }
        }

        // Check for security issues
        if template.body.contains("eval(") || template.body.contains("exec(") {
            issues.push(ValidationIssue {
                issue_type: IssueType::Security,
                severity: Severity::Warning,
                description: "Template contains potentially unsafe code execution".to_string(),
                line_number: None,
                column_number: None,
                suggestion: Some("Remove or properly sanitize code execution".to_string()),
            });
        }

        Ok(issues)
    }

    /// Assess quality metrics
    fn assess_quality(&self, template: &Template, issues: &[ValidationIssue]) -> Result<QualityMetrics> {
        // Calculate completeness
        let completeness = self.assess_completeness(template)?;

        // Calculate correctness
        let error_count = issues.iter().filter(|i| matches!(i.severity, Severity::Error)).count();
        let correctness = if issues.is_empty() { 1.0 } else { 1.0 - (error_count as f64 * 0.1) };

        // Calculate maintainability
        let maintainability = self.assess_maintainability(template)?;

        // Calculate performance
        let performance = self.assess_performance(template)?;

        // Calculate security
        let security_issues = issues.iter().filter(|i| matches!(i.issue_type, IssueType::Security)).count();
        let security = if security_issues == 0 { 1.0 } else { 1.0 - (security_issues as f64 * 0.2) };

        // Calculate readability
        let readability = self.assess_readability(template)?;

        Ok(QualityMetrics {
            completeness,
            correctness,
            maintainability,
            performance,
            security,
            readability,
        })
    }

    /// Assess template completeness
    fn assess_completeness(&self, template: &Template) -> Result<f64> {
        let mut score = 0.0;
        let mut factors = 0;

        // Check if 'to' field is present
        if template.front.to.is_some() {
            score += 0.2;
        }
        factors += 1;

        // Check if variables are documented
        let documented_vars = self.extract_documented_variables(&template.body);
        let template_vars: Vec<&str> = template.front.vars.keys().map(|s| s.as_str()).collect();
        let documented_count = template_vars.iter().filter(|var| documented_vars.contains(&var.to_string())).count();
        if !template_vars.is_empty() {
            score += 0.15 * (documented_count as f32 / template_vars.len() as f32);
        }
        factors += 1;

        // Check if variables are documented
        if !template.front.vars.is_empty() {
            score += 0.2;
        }
        factors += 1;

        // Check if RDF content is present where expected
        if !template.front.rdf_inline.is_empty() || !template.front.rdf.is_empty() {
            score += 0.15;
        }
        factors += 1;

        // Check if template body is substantial
        if template.body.len() > 100 {
            score += 0.3;
        }
        factors += 1;

        Ok((score / factors as f32) as f64)
    }

    /// Assess maintainability
    fn assess_maintainability(&self, template: &Template) -> Result<f64> {
        let mut score: f32 = 1.0;

        // Penalize for excessive complexity
        let line_count = template.body.lines().count();
        if line_count > 500 {
            score -= 0.1;
        }

        // Check for good variable naming
        for var_name in template.front.vars.keys() {
            if var_name.contains('_') && var_name.len() > 3 {
                score += 0.05; // Bonus for descriptive names
            }
        }

        Ok((score.max(0.0).min(1.0)) as f64)
    }

    /// Assess performance
    fn assess_performance(&self, template: &Template) -> Result<f64> {
        let mut score: f32 = 1.0;

        // Check for inefficient patterns
        if template.body.contains("SELECT *") {
            score -= 0.1; // Penalize SELECT *
        }

        // Check for nested loops
        let nested_loops = template.body.matches("{% for %").count();
        if nested_loops > 3 {
            score -= 0.1; // Penalize excessive nesting
        }

        Ok((score.max(0.0).min(1.0)) as f64)
    }

    /// Assess readability
    fn assess_readability(&self, template: &Template) -> Result<f64> {
        let mut score: f32 = 1.0;

        // Check for comments
        if template.body.contains("#") || template.body.contains("//") {
            score += 0.1; // Bonus for comments
        }

        // Check for consistent indentation
        let lines: Vec<&str> = template.body.lines().collect();
        let indented_lines = lines.iter().filter(|line| line.starts_with("    ")).count();
        let total_lines = lines.len();

        if total_lines > 0 {
            let indentation_ratio = indented_lines as f64 / total_lines as f64;
            if indentation_ratio > 0.8 {
                score += 0.1; // Bonus for consistent indentation
            }
        }

        Ok((score.max(0.0).min(1.0)) as f64)
    }

    /// Extract documented variables from template body
    fn extract_documented_variables(&self, template_body: &str) -> Vec<String> {
        let mut variables = Vec::new();

        // Look for variable documentation patterns
        for line in template_body.lines() {
            if line.contains("{{") && line.contains("}}") {
                // Extract variable names from template syntax
                if let Some(start) = line.find("{{") {
                    if let Some(end) = line[start..].find("}}") {
                        let var_expr = &line[start + 2..start + end];
                        if let Some(pipe_pos) = var_expr.find('|') {
                            let var_name = var_expr[..pipe_pos].trim();
                            if !var_name.starts_with('"') && !var_name.starts_with('\'') {
                                variables.push(var_name.to_string());
                            }
                        } else {
                            let var_name = var_expr.trim();
                            if !var_name.starts_with('"') && !var_name.starts_with('\'') {
                                variables.push(var_name.to_string());
                            }
                        }
                    }
                }
            }
        }

        variables.sort();
        variables.dedup();
        variables
    }

    /// Calculate overall quality score
    fn calculate_quality_score(&self, metrics: &QualityMetrics, issues: &[ValidationIssue]) -> f64 {
        let base_score = (metrics.completeness + metrics.correctness + metrics.maintainability +
                         metrics.performance + metrics.security + metrics.readability) / 6.0;

        // Apply penalty for issues
        let error_penalty = issues.iter()
            .filter(|issue| matches!(issue.severity, Severity::Error))
            .count() as f64 * 0.1;

        let warning_penalty = issues.iter()
            .filter(|issue| matches!(issue.severity, Severity::Warning))
            .count() as f64 * 0.05;

        (base_score - error_penalty - warning_penalty).max(0.0).min(1.0)
    }

    /// Generate suggestions for improvement
    fn generate_suggestions(&self, issues: &[ValidationIssue]) -> Vec<String> {
        let mut suggestions = Vec::new();

        // Group issues by type for better suggestions
        let errors: Vec<_> = issues.iter().filter(|i| matches!(i.severity, Severity::Error)).collect();
        let warnings: Vec<_> = issues.iter().filter(|i| matches!(i.severity, Severity::Warning)).collect();

        if !errors.is_empty() {
            suggestions.push(format!("Fix {} critical errors before proceeding", errors.len()));
        }

        if !warnings.is_empty() {
            suggestions.push(format!("Address {} warnings to improve template quality", warnings.len()));
        }

        // Add specific suggestions based on issue types
        for issue in issues {
            if let Some(suggestion) = &issue.suggestion {
                suggestions.push(suggestion.clone());
            }
        }

        suggestions
    }

    /// Validate template iteratively with AI feedback
    pub async fn validate_with_feedback<F>(
        &self,
        template: &Template,
        feedback_fn: F,
    ) -> Result<ValidationResult>
    where
        F: Fn(&ValidationResult) -> Result<Option<String>>,
    {
        let mut current_template = template.clone();
        let mut iteration = 0;
        let max_iterations = 3;

        loop {
            iteration += 1;

            // Validate current template
            let result = self.validate_template(&current_template).await?;

            // Check if validation passes requirements
            if result.is_valid && result.quality_score >= self.rules.min_quality_score {
                return Ok(result);
            }

            // Check if we've exceeded max iterations
            if iteration >= max_iterations {
                return Ok(result);
            }

            // Get feedback for improvement
            if let Some(improvement) = feedback_fn(&result)? {
                // Apply improvement to template (this would require template modification logic)
                // For now, we'll just return the validation result
                println!("Iteration {}: {}", iteration, improvement);
            }

            break; // For now, we don't have template modification logic
        }

        Ok(self.validate_template(&current_template).await?)
    }
}

impl Default for ValidationRules {
    fn default() -> Self {
        Self {
            min_quality_score: 0.7,
            max_errors: 0,
            max_warnings: 5,
            strict_mode: false,
            custom_rules: HashMap::new(),
        }
    }
}

impl Default for TemplateValidator {
    fn default() -> Self {
        Self::new()
    }
}
