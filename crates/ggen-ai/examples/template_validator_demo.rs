//! Template Validator Agent Demonstration
//!
//! This example demonstrates the TemplateValidatorAgent's ability to:
//! - Validate Tera template syntax
//! - Detect undefined variables
//! - Cross-reference with SPARQL results
//! - Validate filter syntax
//! - Suggest and apply fixes

use ggen_ai::swarm::agents::template_validator::TemplateValidatorAgent;
use serde_json::json;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔍 Template Validator Agent Demonstration\n");
    println!("========================================\n");

    // Example 1: Valid template
    println!("📋 Example 1: Valid Template");
    println!("----------------------------\n");
    let agent = TemplateValidatorAgent::new(None, true);
    let valid_template = "{{ name }} is {{ age }} years old.";

    match agent.validate_and_fix(valid_template, "example1.tera") {
        Ok(report) => {
            println!(
                "✅ Template Status: {}",
                if report.is_valid { "VALID" } else { "INVALID" }
            );
            println!("📊 Quality Score: {:.2}", report.quality_score);
            println!("🔍 Issues Found: {}", report.issues.len());
            println!("🔧 Fixes Suggested: {}\n", report.fixes.len());
        }
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 2: Invalid syntax
    println!("📋 Example 2: Syntax Error");
    println!("-------------------------\n");
    let invalid_template = "{{ name }"; // Missing closing }}

    match agent.validate_and_fix(invalid_template, "example2.tera") {
        Ok(report) => {
            println!(
                "✅ Template Status: {}",
                if report.is_valid { "VALID" } else { "INVALID" }
            );
            println!("📊 Quality Score: {:.2}", report.quality_score);
            println!("🔍 Issues Found: {}", report.issues.len());

            for (i, issue) in report.issues.iter().enumerate() {
                println!("\n  Issue #{}:", i + 1);
                println!("    Type: {:?}", issue.issue_type);
                println!("    Severity: {:?}", issue.severity);
                println!("    Line: {:?}", issue.line_number);
                println!("    Description: {}", issue.description);
                if let Some(context) = &issue.context {
                    println!("    Context:\n{}", context);
                }
            }
            println!();
        }
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 3: Undefined variables with SPARQL cross-reference
    println!("📋 Example 3: SPARQL Cross-Reference");
    println!("-----------------------------------\n");

    let template_with_vars = r#"
User: {{ user_name }}
Email: {{ eml }}
Age: {{ age }}
"#;

    let sparql_results = json!({
        "head": {
            "vars": ["user_name", "email", "age"]
        },
        "results": {
            "bindings": [
                {
                    "user_name": {"type": "literal", "value": "Alice"},
                    "email": {"type": "literal", "value": "alice@example.com"},
                    "age": {"type": "literal", "value": "30"}
                }
            ]
        }
    });

    let agent_with_sparql = TemplateValidatorAgent::new(Some(sparql_results), true);

    match agent_with_sparql.validate_and_fix(template_with_vars, "example3.tera") {
        Ok(report) => {
            println!(
                "✅ Template Status: {}",
                if report.is_valid { "VALID" } else { "INVALID" }
            );
            println!("📊 Quality Score: {:.2}", report.quality_score);
            println!("🔍 Issues Found: {}", report.issues.len());
            println!("🔧 Fixes Suggested: {}\n", report.fixes.len());

            if !report.issues.is_empty() {
                println!("Issues Detected:");
                for (i, issue) in report.issues.iter().enumerate() {
                    println!("  {}. {:?} - {}", i + 1, issue.severity, issue.description);
                }
                println!();
            }

            if !report.fixes.is_empty() {
                println!("Suggested Fixes:");
                for (i, fix) in report.fixes.iter().enumerate() {
                    println!(
                        "  {}. Line {}: '{}' → '{}'",
                        i + 1,
                        fix.line_number,
                        fix.original,
                        fix.fixed
                    );
                    println!(
                        "     Reason: {} (confidence: {:.1})\n",
                        fix.description, fix.confidence
                    );
                }
            }
        }
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 4: Filter validation
    println!("📋 Example 4: Filter Validation");
    println!("------------------------------\n");

    let template_with_filters = r#"
Name: {{ name|upper }}
Email: {{ email|unknown_filter }}
Age: {{ age|default(value=0) }}
"#;

    match agent.validate_and_fix(template_with_filters, "example4.tera") {
        Ok(report) => {
            println!(
                "✅ Template Status: {}",
                if report.is_valid { "VALID" } else { "INVALID" }
            );
            println!("📊 Quality Score: {:.2}", report.quality_score);

            let filter_issues: Vec<_> = report
                .issues
                .iter()
                .filter(|i| {
                    matches!(
                        i.issue_type,
                        ggen_ai::swarm::agents::template_validator::IssueType::InvalidFilter
                    )
                })
                .collect();

            if !filter_issues.is_empty() {
                println!("\n🔍 Filter Issues:");
                for (i, issue) in filter_issues.iter().enumerate() {
                    println!(
                        "  {}. Line {:?}: {}",
                        i + 1,
                        issue.line_number,
                        issue.description
                    );
                }
                println!();
            }

            let filter_fixes: Vec<_> = report
                .fixes
                .iter()
                .filter(|f| {
                    matches!(
                        f.fix_type,
                        ggen_ai::swarm::agents::template_validator::FixType::FixFilter
                    )
                })
                .collect();

            if !filter_fixes.is_empty() {
                println!("🔧 Suggested Filter Fixes:");
                for (i, fix) in filter_fixes.iter().enumerate() {
                    println!(
                        "  {}. Replace '{}' with '{}'",
                        i + 1,
                        fix.original,
                        fix.fixed
                    );
                }
                println!();
            }
        }
        Err(e) => println!("❌ Error: {}\n", e),
    }

    // Example 5: Complex template with multiple issues
    println!("📋 Example 5: Complex Template");
    println!("-----------------------------\n");

    let complex_template = r#"
# User Profile

Name: {{ usr_name }}
Email: {{ email|lower }}
Bio: {{ bio|truncate(length=100) }}
Age: {{ ag }}

{% if usr_name %}
    Hello, {{ usr_name }}!
{% endif %}

{% for item in itms %}
    - {{ item.nam }}
{% endfor %}
"#;

    let complex_sparql = json!({
        "head": {
            "vars": ["user_name", "email", "bio", "age", "items"]
        },
        "results": {
            "bindings": [
                {
                    "user_name": {"type": "literal", "value": "Bob"},
                    "email": {"type": "literal", "value": "bob@example.com"},
                    "bio": {"type": "literal", "value": "A software engineer"},
                    "age": {"type": "literal", "value": "25"}
                }
            ]
        }
    });

    let agent_complex = TemplateValidatorAgent::new(Some(complex_sparql), true);

    match agent_complex.validate_and_fix(complex_template, "example5.tera") {
        Ok(report) => {
            println!(
                "✅ Template Status: {}",
                if report.is_valid { "VALID" } else { "INVALID" }
            );
            println!("📊 Quality Score: {:.2}", report.quality_score);
            println!("🔍 Total Issues: {}", report.issues.len());
            println!("🔧 Total Fixes: {}\n", report.fixes.len());

            // Group issues by severity
            use ggen_ai::sparql_validator::IssueSeverity;
            let critical = report
                .issues
                .iter()
                .filter(|i| i.severity == IssueSeverity::Critical)
                .count();
            let errors = report
                .issues
                .iter()
                .filter(|i| i.severity == IssueSeverity::Error)
                .count();
            let warnings = report
                .issues
                .iter()
                .filter(|i| i.severity == IssueSeverity::Warning)
                .count();
            let info = report
                .issues
                .iter()
                .filter(|i| i.severity == IssueSeverity::Info)
                .count();

            println!("Issue Breakdown:");
            println!("  🔴 Critical: {}", critical);
            println!("  🟠 Error: {}", errors);
            println!("  🟡 Warning: {}", warnings);
            println!("  🔵 Info: {}\n", info);

            if !report.fixes.is_empty() {
                println!("Summary of Fixes:");
                for (i, fix) in report.fixes.iter().take(5).enumerate() {
                    println!(
                        "  {}. {:?} at line {}",
                        i + 1,
                        fix.fix_type,
                        fix.line_number
                    );
                }
                if report.fixes.len() > 5 {
                    println!("  ... and {} more", report.fixes.len() - 5);
                }
                println!();
            }
        }
        Err(e) => println!("❌ Error: {}\n", e),
    }

    println!("========================================");
    println!("✨ Demonstration Complete!\n");

    Ok(())
}
