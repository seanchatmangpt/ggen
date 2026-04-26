//! Command-line interface for validation automation
//!
//! Provides a user-friendly CLI for running all validation approaches.

use crate::{ValidationApproach, ValidationAutomation};
use anyhow::Result;
use clap::Parser;
use colored::Colorize;
use std::path::PathBuf;

/// Validation Automation CLI
#[derive(Parser, Debug)]
#[command(name = "validation-automation")]
#[command(about = "Developer Experience & Quality of Life Automation for Validation", long_about = None)]
struct Args {
    /// Input to validate (code, query, etc.)
    #[arg(short, long, default_value = "SELECT * FROM {graph} WHERE ?s ?p ?o")]
    input: String,

    /// Number of inputs per agent
    #[arg(short, long, default_value_t = 100)]
    intensity: usize,

    /// Validation approach to run
    #[arg(short, long, value_enum)]
    approach: Option<ValidationApproach>,

    /// Generate reports (markdown, html, json, or all)
    #[arg(short, long)]
    report: Option<String>,

    /// Output directory for reports
    #[arg(short, long, default_value = ".")]
    output_dir: PathBuf,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

/// Run the CLI
pub async fn run() -> Result<()> {
    let args = Args::parse();

    // Initialize validation system
    let automation: ValidationAutomation = ValidationAutomation::new().await
        .map_err(|e| anyhow::anyhow!("Failed to initialize validation automation: {}", e))?;

    // Run validation
    let report: crate::report::ValidationReport = match args.approach {
        Some(approach) => {
            automation.run_validation(approach, &args.input, args.intensity).await
                .map_err(|e| anyhow::anyhow!("Validation failed: {}", e))?
        }
        None => automation.run_all_validations(&args.input, args.intensity).await
            .map_err(|e| anyhow::anyhow!("Validation failed: {}", e))?,
    };

    // Print summary
    report.print_summary();

    // Generate reports if requested
    if let Some(report_format) = args.report {
        generate_reports(&report, &report_format, &args.output_dir).await?;
    }

    Ok(())
}

/// Generate reports in specified format
async fn generate_reports(
    report: &crate::report::ValidationReport,
    format: &str,
    output_dir: &PathBuf,
) -> Result<()> {
    let formats: Vec<&str> = if format == "all" {
        vec!["markdown", "html", "json"]
    } else {
        vec![format]
    };

    for fmt in formats {
        let filename = match fmt {
            "markdown" => "VALIDATION_REPORT.md",
            "html" => "VALIDATION_REPORT.html",
            "json" => "VALIDATION_REPORT.json",
            _ => {
                eprintln!("{} Unknown report format: {}", "⚠️ ".yellow(), fmt);
                continue;
            }
        };

        let path = output_dir.join(filename);

        match fmt {
            "markdown" => report.generate_markdown(path.to_str().unwrap()).await?,
            "html" => report.generate_html(path.to_str().unwrap()).await?,
            "json" => report.generate_json(path.to_str().unwrap()).await?,
            _ => unreachable!(),
        }

        println!(
            "{} {}",
            "✅".green(),
            format!("Report generated: {}", path.display()).cyan()
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_args_parsing() {
        let args = Args {
            input: "test input".to_string(),
            intensity: 50,
            approach: Some(ValidationApproach::Consensus),
            report: Some("markdown".to_string()),
            output_dir: PathBuf::from("."),
            verbose: true,
        };

        assert_eq!(args.input, "test input");
        assert_eq!(args.intensity, 50);
        assert_eq!(args.approach, Some(ValidationApproach::Consensus));
    }
}
