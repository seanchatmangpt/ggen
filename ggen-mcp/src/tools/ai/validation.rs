//! Template and code validation utilities
//!
//! This module provides validation functions for templates, checking for
//! syntax errors, security issues, and structural problems.

use crate::error::Result;
use serde_json::Value;

/// Validate template structure and content
pub fn validate_template(template: &str) -> Result<Vec<String>> {
    let mut errors = Vec::new();

    // Basic validation checks
    if template.is_empty() {
        errors.push("Template is empty".to_string());
        return Ok(errors);
    }

    // Check for common template syntax
    if !template.contains("{{") && !template.contains("{%") {
        tracing::warn!("Template may not contain template syntax (no braces or blocks found)");
    }

    // Check for balanced braces
    let open_braces = template.matches("{{").count();
    let close_braces = template.matches("}}").count();
    if open_braces != close_braces {
        errors.push(format!(
            "Unbalanced template braces: {} opening vs {} closing",
            open_braces, close_braces
        ));
    }

    // Check for balanced control blocks
    let open_blocks = template.matches("{%").count();
    let close_blocks = template.matches("%}").count();
    if open_blocks != close_blocks {
        errors.push(format!(
            "Unbalanced template blocks: {} opening vs {} closing",
            open_blocks, close_blocks
        ));
    }

    // Check for dangerous patterns
    if template.contains("eval(") || template.contains("exec(") {
        errors.push("Template contains potentially dangerous code execution".to_string());
    }

    if template.contains("system(") || template.contains("shell_exec(") {
        errors.push("Template contains system command execution".to_string());
    }

    // Check for SQL injection patterns
    if template.contains("DROP TABLE") || template.contains("DELETE FROM") {
        errors.push("Template contains potentially dangerous SQL operations".to_string());
    }

    // Validate JSON structure if template appears to be JSON
    if template.trim().starts_with('{') || template.trim().starts_with('[') {
        if let Err(e) = serde_json::from_str::<Value>(template) {
            tracing::debug!("Template is not valid JSON: {}", e);
            // This is not necessarily an error - template might contain Jinja2 syntax
        }
    }

    // Check for reasonable size
    if template.len() > 1_000_000 {
        errors.push("Template is very large (>1MB), may cause performance issues".to_string());
    }

    // Check for non-printable characters
    if template
        .chars()
        .any(|c| c.is_control() && c != '\n' && c != '\r' && c != '\t')
    {
        errors.push("Template contains unexpected control characters".to_string());
    }

    Ok(errors)
}
