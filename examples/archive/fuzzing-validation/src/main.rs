//! Fuzzing-Based Validation Demo
//!
//! Demonstrates 7-agent fuzzing system for robustness validation

use fuzzing_validation::FuzzingValidationSystem;
use tracing_subscriber;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("🔬 Fuzzing-Based Validation Demo\n");

    // Create fuzzing validation system
    let system = FuzzingValidationSystem::new().await?;

    // Base input to fuzz (example SPARQL query)
    let base_input = "SELECT ?s ?p ?o WHERE { ?s ?p ?o FILTER(?s != <http://example.org/ignored>) }";

    println!("📝 Base Input:");
    println!("  {}\n", base_input);

    // Run fuzzing campaign
    println!("🚀 Running Fuzzing Campaign...\n");
    let report = system.run_fuzzing_campaign(base_input, 20).await?; // 20 inputs per agent

    // Display results
    println!("\n=== Fuzzing Results ===\n");

    println!("📊 Statistics:");
    println!("  Total Inputs: {}", report.result.total_inputs);
    println!("  Passed: {} ✅", report.result.passed);
    println!("  Crashes: {} 💥", report.result.crashes);
    println!("  Hangs: {} ⏱️", report.result.hangs);
    println!("  Incorrect: {} ❌", report.result.incorrect);
    println!("  Errors: {} ⚠️", report.result.errors);
    println!("  Unique Crashes: {} 🔍", report.result.unique_crashes);

    println!("\n📈 Metrics:");
    println!("  Error Rate: {:.1}%", report.result.error_rate() * 100.0);
    println!("  Crash Rate: {:.1}%", report.result.crash_rate() * 100.0);

    println!("\n🎯 Assessment:");
    println!("  {}", report.assessment);

    // Show crashes by fuzzer type
    println!("\n💥 Crashes by Fuzzer Type:");
    for (fuzzer_type, count) in report.result.crashes_by_type() {
        if count > 0 {
            println!("  {:?}: {} crashes", fuzzer_type, count);
        }
    }

    // Show top 5 recommendations
    if !report.recommendations.is_empty() {
        println!("\n💡 Top Recommendations:");
        for (i, rec) in report.recommendations.iter().take(5).enumerate() {
            println!("  {}. [{}] {}", i + 1, rec.priority, rec.issue);
            println!("     Suggestion: {}", rec.suggestion);
            println!();
        }
    }

    // Show sample failures
    println!("\n🔍 Sample Failures:");
    let failed_inputs = report.result.failed_inputs();
    for (i, input) in failed_inputs.iter().take(3).enumerate() {
        println!("  {}. {:?} - {:?}", i + 1, input.fuzzer_type, input.status);
        println!("     Input: {}", input.input.chars().take(60).collect::<String>());
        if let Some(details) = &input.failure_details {
            println!("     Details: {}", details);
        }
        println!();
    }

    println!("✅ Fuzzing Campaign Complete!");

    Ok(())
}
