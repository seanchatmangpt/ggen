//! Simple Template Validator Test (without swarm dependency)

use ggen_ai::generators::validator::{TemplateValidator, ValidationResult, ValidationIssue, Severity, IssueType};
use ggen_ai::generators::template::Template;

fn main() {
    println!("Running Simple Template Validator Test...\n");

    let validator = TemplateValidator::new();

    // Test 1: Valid template
    println!("Test 1: Valid template");
    let valid_template = r#"---
to: "{{name}}.rs"
---
fn main() {
    println!("Hello, {}!", name);
}"#;

    match validator.validate_template(valid_template) {
        Ok(result) => {
            println!("  ✓ Validation passed: {}", result.valid);
            println!("  ✓ Issues: {}", result.issues.len());
        }
        Err(e) => {
            println!("  ✗ Error: {}", e);
        }
    }

    // Test 2: Invalid template (syntax error)
    println!("\nTest 2: Invalid template");
    let invalid_template = r#"---
to: "{{name}}.rs"
---
fn main() {
    println!("Hello, {}!", name)  // Missing semicolon
}"#;

    match validator.validate_template(invalid_template) {
        Ok(result) => {
            println!("  ✓ Validation result: {}", result.valid);
            println!("  ✓ Issues found: {}", result.issues.len());
            for issue in &result.issues {
                println!("    - {}: {} ({})", issue.issue_type, issue.message, issue.severity);
            }
        }
        Err(e) => {
            println!("  ✗ Error: {}", e);
        }
    }

    // Test 3: Template with undefined variables
    println!("\nTest 3: Template with undefined variables");
    let undefined_vars_template = r#"---
to: "{{name}}.rs"
---
fn main() {
    println!("Hello, {}!", name);
    println!("Email: {}", email);  // undefined variable
}"#;

    match validator.validate_template(undefined_vars_template) {
        Ok(result) => {
            println!("  ✓ Validation result: {}", result.valid);
            println!("  ✓ Issues found: {}", result.issues.len());
            for issue in &result.issues {
                println!("    - {}: {} ({})", issue.issue_type, issue.message, issue.severity);
            }
        }
        Err(e) => {
            println!("  ✗ Error: {}", e);
        }
    }

    // Test 4: Complex template with filters
    println!("\nTest 4: Complex template with filters");
    let complex_template = r#"---
to: "{{name|lower}}_controller.rs"
---
use std::io::{self, Write};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct {{name|title}} {
    {{#each fields}}
    pub {{this.name}}: {{this.type|default(value="String")}},
    {{/each}}
}"#;

    match validator.validate_template(complex_template) {
        Ok(result) => {
            println!("  ✓ Validation result: {}", result.valid);
            println!("  ✓ Issues found: {}", result.issues.len());
            for issue in &result.issues {
                println!("    - {}: {} ({})", issue.issue_type, issue.message, issue.severity);
            }
        }
        Err(e) => {
            println!("  ✗ Error: {}", e);
        }
    }

    // Test 5: JSON template format
    println!("\nTest 5: JSON template format");
    let json_template = r#"---
to: "{{model_name|lower}}.json"
---
{
    "model": "{{model_name|upper}}",
    "fields": [
        {{#each fields}}
        {
            "name": "{{this.name}}",
            "type": "{{this.type|default(value='string')}}"
        }
        {{#unless @last}},{{/unless}}
        {{/each}}
    ]
}"#;

    match validator.validate_template(json_template) {
        Ok(result) => {
            println!("  ✓ Validation result: {}", result.valid);
            println!("  ✓ Issues found: {}", result.issues.len());
            for issue in &result.issues {
                println!("    - {}: {} ({})", issue.issue_type, issue.message, issue.severity);
            }
        }
        Err(e) => {
            println!("  ✗ Error: {}", e);
        }
    }

    println!("\n🎉 Simple Template Validator Test completed!");
}