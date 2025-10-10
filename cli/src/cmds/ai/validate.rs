//! Validate templates

use clap::Args;
use ggen_utils::error::Result;
use anyhow;
use std::fs;

#[derive(Debug, Args)]
pub struct ValidateArgs {
    /// Template file to validate
    #[arg(short, long)]
    pub template: String,

    /// Strict validation mode
    #[arg(long)]
    pub strict: bool,
}

pub async fn run(args: &ValidateArgs) -> Result<()> {
    println!("Validating template: {}", args.template);

    // Load and parse template
    let content = fs::read_to_string(&args.template)?;
    let template = ggen_core::Template::parse(&content)?;

    // Create validator
    let validator = ggen_ai::TemplateValidator::new();

    // Validate template
    let result = validator.validate_template(&template).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("Validation Results:");
    println!("  Valid: {}", result.is_valid);
    println!("  Quality Score: {:.2}", result.quality_score);
    println!("  Issues Found: {}", result.issues.len());

    for issue in &result.issues {
        let severity = match issue.severity {
            ggen_ai::generators::validator::Severity::Error => "ERROR",
            ggen_ai::generators::validator::Severity::Warning => "WARNING",
            ggen_ai::generators::validator::Severity::Info => "INFO",
        };
        println!("  {}: {}", severity, issue.description);
    }

    if !result.suggestions.is_empty() {
        println!("\nSuggestions for improvement:");
        for suggestion in &result.suggestions {
            println!("  - {}", suggestion);
        }
    }

    Ok(())
}
