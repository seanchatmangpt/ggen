//! Template module — re-exports from template_main for backwards compatibility.
//!
//! The canonical implementation lives in template_main.rs.
//! This module provides the stable public API.

use ggen_utils::error::Result;
use std::collections::HashSet;

pub use crate::template_types::{Frontmatter, Template};

/// Validate a Tera template for syntax correctness.
pub fn validate_template(
    template_content: &str,
) -> ggen_utils::error::Result<template_validation::TemplateValidationResult> {
    template_validation::validate_template(template_content)
}

/// Extract template variable names from a Tera template string (alias for template_validation::extract_variables).
pub fn extract_template_variables(template: &str) -> HashSet<String> {
    template_validation::extract_variables(template)
}

/// Extract variable names from SPARQL JSON results file.
pub fn extract_variables_from_sparql_results(results_path: &str) -> Result<HashSet<String>> {
    let content = std::fs::read_to_string(results_path).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to read SPARQL results: {}", e))
    })?;
    let json: serde_json::Value = serde_json::from_str(&content).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to parse SPARQL results JSON: {}", e))
    })?;

    let mut vars = HashSet::new();
    if let Some(bindings) = json
        .get("results")
        .and_then(|r| r.get("bindings"))
        .and_then(|b| b.as_array())
    {
        for binding in bindings {
            if let Some(obj) = binding.as_object() {
                for key in obj.keys() {
                    vars.insert(key.clone());
                }
            }
        }
    }
    Ok(vars)
}

pub mod template_validation {
    use ggen_utils::error::Result;
    use std::collections::HashSet;
    use tera::Tera;

    #[derive(Debug, Clone)]
    pub struct TemplateValidationResult {
        pub is_valid: bool,
        pub issues: Vec<TemplateIssue>,
    }

    #[derive(Debug, Clone)]
    pub enum TemplateIssue {
        SyntaxError(String),
        MissingVariable(String),
        UnusedVariable(String),
    }

    pub fn validate_template(template_content: &str) -> Result<TemplateValidationResult> {
        let mut issues = Vec::new();
        let mut tera = Tera::default();

        match tera.add_raw_template("test_template", template_content) {
            Ok(_) => {}
            Err(e) => {
                issues.push(TemplateIssue::SyntaxError(format!("{}", e)));
                return Ok(TemplateValidationResult {
                    is_valid: false,
                    issues,
                });
            }
        }

        let _used_vars = extract_variables(template_content);

        Ok(TemplateValidationResult {
            is_valid: true,
            issues,
        })
    }

    pub fn extract_variables(template: &str) -> HashSet<String> {
        let mut vars = HashSet::new();
        let mut chars = template.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '{' {
                if let Some(&'{') = chars.peek() {
                    chars.next();
                    let mut var_name = String::new();
                    // Consume characters until we hit the closing }}
                    while let Some(&c) = chars.peek() {
                        if c == '}' {
                            chars.next(); // consume first }
                            if let Some(&'}') = chars.peek() {
                                chars.next(); // consume second }
                                break;
                            }
                            // Not a closing }}, add the } back and continue
                            var_name.push('}');
                            continue;
                        }
                        var_name.push(chars.next().unwrap());
                    }
                    if !var_name.is_empty() {
                        // Trim whitespace and split by filter pipe
                        let var_clean = var_name
                            .trim()
                            .split('|')
                            .next()
                            .map(|s| s.trim())
                            .unwrap_or(var_name.trim());
                        vars.insert(var_clean.to_string());
                    }
                }
            }
        }

        vars
    }
}
