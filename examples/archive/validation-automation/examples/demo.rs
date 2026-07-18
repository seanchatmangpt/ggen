//! Validation Automation Demo
//!
//! Demonstrates the DX/QoL improvements for validation approaches:
//! - Progress bars and visual feedback
//! - Colored terminal output
//! - Unified interface for all approaches
//! - Automatic report generation
//! - Error recovery and helpful suggestions

use validation_automation::ValidationAutomation;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize validation automation system
    println!("{} Validation Automation Demo", "🚀".bold());
    println!();

    let automation = ValidationAutomation::new().await?;

    // Example 1: Run all validation approaches
    println!("{}", "=== Example 1: Running All Approaches ===".bold());
    println!();

    let input = "SELECT * FROM {graph} WHERE ?s ?p ?o";
    let intensity = 100;

    let report = automation
        .run_all_validations(input, intensity)
        .await?;

    // Print summary
    report.print_summary();

    // Generate reports
    println!("{}", "=== Generating Reports ===".bold());
    println!();

    report.generate_markdown("VALIDATION_REPORT.md").await?;
    println!("✅ Markdown report generated: VALIDATION_REPORT.md");

    report.generate_html("VALIDATION_REPORT.html").await?;
    println!("✅ HTML report generated: VALIDATION_REPORT.html");

    report.generate_json("VALIDATION_REPORT.json").await?;
    println!("✅ JSON report generated: VALIDATION_REPORT.json");

    println!();

    // Example 2: Run specific approach
    println!("{}", "=== Example 2: Running Consensus Only ===".bold());
    println!();

    use validation_automation::ValidationApproach;

    let consensus_report = automation
        .run_validation(ValidationApproach::Consensus, input, intensity)
        .await?;

    consensus_report.print_summary();

    println!();
    println!("{}", "=== Demo Complete ===".bold());
    println!();
    println!("Try running: cargo run --example demo");

    Ok(())
}
