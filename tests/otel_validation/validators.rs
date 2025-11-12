//! Validation logic for README capabilities
//!
//! High-level validators that run capability tests and collect results.

use super::*;
use capabilities::*;
use collectors::*;
use std::time::Instant;

/// Run all README capability validations
pub async fn validate_all_capabilities(ctx: &ValidationContext) -> Result<Vec<ValidationResult>> {
    let mut results = Vec::new();

    info!("Starting comprehensive README capability validation");

    // Core capabilities
    results.push(validate_quickstart(ctx).await?);
    results.push(validate_doctor(ctx).await?);
    results.push(validate_lifecycle(ctx).await?);
    results.push(validate_marketplace(ctx).await?);

    // AI capabilities
    results.push(validate_ai_generation(ctx).await?);

    // Integration capabilities
    results.push(validate_github(ctx).await?);

    // Performance validations
    results.push(validate_generation_performance(ctx).await?);
    results.push(validate_deterministic(ctx).await?);

    info!("Completed {} capability validations", results.len());

    Ok(results)
}

/// Validate critical path (most important capabilities)
pub async fn validate_critical_path(ctx: &ValidationContext) -> Result<Vec<ValidationResult>> {
    let mut results = Vec::new();

    info!("Validating critical path capabilities");

    // Critical path: quickstart -> doctor -> lifecycle -> generate
    results.push(validate_quickstart(ctx).await?);
    results.push(validate_doctor(ctx).await?);
    results.push(validate_lifecycle(ctx).await?);
    results.push(validate_generation_performance(ctx).await?);

    Ok(results)
}

/// Validate AI features
pub async fn validate_ai_features(ctx: &ValidationContext) -> Result<Vec<ValidationResult>> {
    let mut results = Vec::new();

    info!("Validating AI-powered features");

    results.push(validate_ai_generation(ctx).await?);

    Ok(results)
}

/// Validate performance SLOs
pub async fn validate_performance_slos(ctx: &ValidationContext) -> Result<Vec<ValidationResult>> {
    let mut results = Vec::new();

    info!("Validating performance SLOs");

    results.push(validate_generation_performance(ctx).await?);
    results.push(validate_deterministic(ctx).await?);

    Ok(results)
}

/// Generate comprehensive validation report
pub async fn generate_validation_report(ctx: &ValidationContext) -> Result<ValidationReport> {
    let start = Instant::now();

    // Run all validations
    let results = validate_all_capabilities(ctx).await?;

    let total_duration = start.elapsed().as_secs_f64() * 1000.0;

    Ok(ValidationReport::new(results, total_duration))
}

/// Validate and generate markdown report
pub async fn validate_and_report(ctx: &ValidationContext) -> Result<String> {
    let report = generate_validation_report(ctx).await?;

    // Print to console
    report.print_summary();

    // Generate markdown
    Ok(report.to_markdown())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_validate_critical_path() {
        let ctx = ValidationContext::new();
        let results = validate_critical_path(&ctx).await.unwrap();

        // Should have at least 4 critical capabilities
        assert!(results.len() >= 4);
    }

    #[tokio::test]
    async fn test_generate_report() {
        let ctx = ValidationContext::new();
        let report = generate_validation_report(&ctx).await.unwrap();

        // Report should have results
        assert!(!report.results.is_empty());

        // Markdown should be non-empty
        let md = report.to_markdown();
        assert!(md.contains("OpenTelemetry Validation Report"));
    }
}
