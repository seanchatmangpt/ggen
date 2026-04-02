//! Template validation example
//!
//! This example demonstrates template validation:
//! - Validating template syntax
//! - Checking template metadata
//! - Security validation
//! - Custom validation rules

use anyhow::{Context, Result};
use ggen_ai::{MockClient, TemplateValidator, ValidationIssue};
use ggen_core::Template;
use tracing::{info, warn, Level};
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    info!("Starting template validation example");

    // Example 1: Basic template validation
    example_basic_validation()?;

    // Example 2: Metadata validation
    example_metadata_validation()?;

    // Example 3: Security validation
    example_security_validation()?;

    // Example 4: Syntax validation
    example_syntax_validation()?;

    // Example 5: AI-powered validation
    example_ai_validation().await?;

    info!("All validation examples completed!");
    Ok(())
}

/// Example 1: Basic template validation
fn example_basic_validation() -> Result<()> {
    info!("=== Example 1: Basic Template Validation ===");

    // Valid template
    let valid_template = r#"---
name: valid-template
description: A valid template
version: 1.0.0
---
# {{ title }}

Content: {{ content }}
"#;

    match Template::from_str(valid_template) {
        Ok(template) => {
            info!("✓ Valid template parsed successfully");
            info!("  Name: {}", template.metadata.name);
            validate_template_structure(&template)?;
        }
        Err(e) => {
            warn!("✗ Template validation failed: {}", e);
        }
    }

    Ok(())
}

/// Example 2: Metadata validation
fn example_metadata_validation() -> Result<()> {
    info!("=== Example 2: Metadata Validation ===");

    let templates = vec![
        // Complete metadata
        (
            "complete",
            r#"---
name: complete-template
description: Template with complete metadata
version: 1.0.0
author: ggen-team
license: MIT
tags: [example, validation]
---
Content here
"#,
        ),
        // Minimal metadata (only required fields)
        (
            "minimal",
            r#"---
name: minimal-template
---
Content here
"#,
        ),
        // Missing name (invalid)
        (
            "missing-name",
            r#"---
description: Template missing name field
---
Content here
"#,
        ),
    ];

    for (label, template_str) in templates {
        info!("Validating {} metadata:", label);
        match Template::from_str(template_str) {
            Ok(template) => {
                let issues = validate_metadata(&template);
                if issues.is_empty() {
                    info!("  ✓ Metadata valid");
                } else {
                    warn!("  ✗ Metadata issues:");
                    for issue in issues {
                        warn!("    - {}", issue);
                    }
                }
            }
            Err(e) => {
                warn!("  ✗ Failed to parse: {}", e);
            }
        }
    }

    Ok(())
}

/// Example 3: Security validation
fn example_security_validation() -> Result<()> {
    info!("=== Example 3: Security Validation ===");

    let templates = vec![
        (
            "safe",
            r#"---
name: safe-template
---
Hello {{ user_name }}!
"#,
        ),
        (
            "unsafe-eval",
            r#"---
name: unsafe-template
---
{{ user_input | safe }}
eval({{ code }})
"#,
        ),
        (
            "sql-injection",
            r#"---
name: sql-template
---
SELECT * FROM users WHERE id = {{ user_id }}
"#,
        ),
    ];

    for (label, template_str) in templates {
        info!("Security check: {}", label);
        match Template::from_str(template_str) {
            Ok(template) => {
                let security_issues = check_security(&template);
                if security_issues.is_empty() {
                    info!("  ✓ No security issues detected");
                } else {
                    warn!("  ✗ Security issues found:");
                    for issue in security_issues {
                        warn!("    - {}", issue);
                    }
                }
            }
            Err(e) => {
                warn!("  ✗ Failed to parse: {}", e);
            }
        }
    }

    Ok(())
}

/// Example 4: Syntax validation
fn example_syntax_validation() -> Result<()> {
    info!("=== Example 4: Syntax Validation ===");

    let templates = vec![
        (
            "valid-syntax",
            r#"---
name: valid
---
{% if condition %}
  Valid content
{% endif %}
"#,
        ),
        (
            "unclosed-if",
            r#"---
name: unclosed
---
{% if condition %}
  Missing endif
"#,
        ),
        (
            "invalid-filter",
            r#"---
name: invalid-filter
---
{{ value | nonexistent_filter }}
"#,
        ),
        (
            "mismatched-tags",
            r#"---
name: mismatched
---
{% for item in items %}
{% endwhile %}
"#,
        ),
    ];

    for (label, template_str) in templates {
        info!("Syntax check: {}", label);
        match Template::from_str(template_str) {
            Ok(template) => {
                match validate_tera_syntax(&template) {
                    Ok(_) => info!("  ✓ Syntax valid"),
                    Err(e) => warn!("  ✗ Syntax error: {}", e),
                }
            }
            Err(e) => {
                warn!("  ✗ Parse error: {}", e);
            }
        }
    }

    Ok(())
}

/// Example 5: AI-powered validation
async fn example_ai_validation() -> Result<()> {
    info!("=== Example 5: AI-Powered Validation ===");

    // Create a mock validator response
    let validation_response = r#"[
        {
            "severity": "warning",
            "message": "Variable 'user_input' should be sanitized",
            "line": 5,
            "suggestion": "Use a sanitization filter"
        },
        {
            "severity": "info",
            "message": "Consider adding input validation",
            "line": 7,
            "suggestion": "Add type checking for {{ age }}"
        }
    ]"#;

    let mock_client = MockClient::with_response(validation_response);
    let validator = TemplateValidator::new(Box::new(mock_client));

    let template = Template::from_str(
        r#"---
name: ai-validated-template
---
User: {{ user_input }}
Age: {{ age }}
"#,
    )?;

    info!("Performing AI-powered validation...");
    info!("Mock validation response:");

    // In real usage, this would call the validator
    // let issues = validator.validate(&template).await?;

    // For demonstration, parse the mock response
    let mock_issues: Vec<serde_json::Value> = serde_json::from_str(validation_response)?;

    info!("Validation results:");
    for issue in mock_issues {
        let severity = issue["severity"].as_str().unwrap_or("unknown");
        let message = issue["message"].as_str().unwrap_or("no message");
        let line = issue["line"].as_i64().unwrap_or(0);
        let suggestion = issue["suggestion"].as_str().unwrap_or("no suggestion");

        info!(
            "  [{}] Line {}: {}",
            severity.to_uppercase(),
            line,
            message
        );
        info!("    Suggestion: {}", suggestion);
    }

    Ok(())
}

/// Validate template structure
fn validate_template_structure(template: &Template) -> Result<()> {
    // Check that template has content
    if template.content.trim().is_empty() {
        anyhow::bail!("Template content is empty");
    }

    // Check metadata name
    if template.metadata.name.is_empty() {
        anyhow::bail!("Template name is required");
    }

    info!("  ✓ Template structure valid");
    Ok(())
}

/// Validate template metadata
fn validate_metadata(template: &Template) -> Vec<String> {
    let mut issues = Vec::new();

    // Required fields
    if template.metadata.name.is_empty() {
        issues.push("Missing required field: name".to_string());
    }

    // Recommended fields
    if template.metadata.description.is_none() {
        issues.push("Missing recommended field: description".to_string());
    }

    if template.metadata.version.is_none() {
        issues.push("Missing recommended field: version".to_string());
    }

    // Name validation
    if !template.metadata.name.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
        issues.push("Template name should only contain alphanumeric characters, hyphens, and underscores".to_string());
    }

    // Version validation
    if let Some(version) = &template.metadata.version {
        if !is_valid_semver(version) {
            issues.push(format!("Invalid semver version: {}", version));
        }
    }

    issues
}

/// Check for security issues
fn check_security(template: &Template) -> Vec<String> {
    let mut issues = Vec::new();
    let content = &template.content;

    // Check for unsafe filter usage
    if content.contains("| safe") {
        issues.push(
            "Use of 'safe' filter detected - this may allow XSS attacks".to_string(),
        );
    }

    // Check for eval-like patterns
    if content.contains("eval(") {
        issues.push("Potential code injection: eval() detected".to_string());
    }

    // Check for SQL injection patterns
    if content.contains("SELECT") || content.contains("INSERT") || content.contains("UPDATE") {
        issues.push(
            "SQL keywords detected - ensure proper parameterization".to_string(),
        );
    }

    // Check for shell command execution
    if content.contains("system(") || content.contains("exec(") {
        issues.push("Shell command execution detected - potential security risk".to_string());
    }

    // Check for file operations
    if content.contains("open(") || content.contains("read(") || content.contains("write(") {
        issues.push("File operations detected - ensure proper access controls".to_string());
    }

    issues
}

/// Validate Tera template syntax
fn validate_tera_syntax(template: &Template) -> Result<()> {
    // This is a simplified check - in reality, Tera validates during rendering
    let content = &template.content;

    // Check for balanced control structures
    let if_count = content.matches("{% if").count();
    let endif_count = content.matches("{% endif %}").count();

    if if_count != endif_count {
        anyhow::bail!("Unbalanced if/endif blocks: {} if, {} endif", if_count, endif_count);
    }

    let for_count = content.matches("{% for").count();
    let endfor_count = content.matches("{% endfor %}").count();

    if for_count != endfor_count {
        anyhow::bail!(
            "Unbalanced for/endfor blocks: {} for, {} endfor",
            for_count,
            endfor_count
        );
    }

    // Check for unclosed variable tags
    let open_vars = content.matches("{{").count();
    let close_vars = content.matches("}}").count();

    if open_vars != close_vars {
        anyhow::bail!(
            "Unbalanced variable tags: {} opening, {} closing",
            open_vars,
            close_vars
        );
    }

    Ok(())
}

/// Check if version string is valid semver
fn is_valid_semver(version: &str) -> bool {
    // Simple semver check (not complete)
    let parts: Vec<&str> = version.split('.').collect();

    if parts.len() != 3 {
        return false;
    }

    parts.iter().all(|part| part.parse::<u32>().is_ok())
}

/// Custom validation rule example
struct CustomValidator {
    rules: Vec<Box<dyn Fn(&Template) -> Vec<String>>>,
}

impl CustomValidator {
    fn new() -> Self {
        Self { rules: Vec::new() }
    }

    fn add_rule(&mut self, rule: Box<dyn Fn(&Template) -> Vec<String>>) {
        self.rules.push(rule);
    }

    fn validate(&self, template: &Template) -> Vec<String> {
        let mut all_issues = Vec::new();

        for rule in &self.rules {
            let issues = rule(template);
            all_issues.extend(issues);
        }

        all_issues
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_validation() {
        let result = example_basic_validation();
        assert!(result.is_ok());
    }

    #[test]
    fn test_metadata_validation() {
        let template = Template::from_str(
            r#"---
name: test
description: Test template
version: 1.0.0
---
Content
"#,
        )
        .unwrap();

        let issues = validate_metadata(&template);
        assert!(issues.is_empty());
    }

    #[test]
    fn test_security_check() {
        let template = Template::from_str(
            r#"---
name: safe-template
---
{{ value }}
"#,
        )
        .unwrap();

        let issues = check_security(&template);
        assert!(issues.is_empty());
    }

    #[test]
    fn test_syntax_validation() {
        let template = Template::from_str(
            r#"---
name: test
---
{% if true %}
  content
{% endif %}
"#,
        )
        .unwrap();

        let result = validate_tera_syntax(&template);
        assert!(result.is_ok());
    }

    #[test]
    fn test_semver_validation() {
        assert!(is_valid_semver("1.0.0"));
        assert!(is_valid_semver("2.3.4"));
        assert!(!is_valid_semver("1.0"));
        assert!(!is_valid_semver("invalid"));
    }

    #[test]
    fn test_custom_validator() {
        let mut validator = CustomValidator::new();

        validator.add_rule(Box::new(|t: &Template| {
            if t.metadata.name.len() < 3 {
                vec!["Template name too short".to_string()]
            } else {
                vec![]
            }
        }));

        let template = Template::from_str(
            r#"---
name: ab
---
Content
"#,
        )
        .unwrap();

        let issues = validator.validate(&template);
        assert!(!issues.is_empty());
    }
}
