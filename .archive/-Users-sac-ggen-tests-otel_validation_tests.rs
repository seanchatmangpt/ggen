//! OpenTelemetry validation integration tests
//!
//! These tests validate that all README capabilities work correctly
//! using OpenTelemetry traces as proof.

mod otel_validation;

use anyhow::Result;
use otel_validation::*;

#[tokio::test]
async fn test_readme_quickstart_capability() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let result = capabilities::validate_quickstart(&ctx).await?;

    assert!(
        result.success,
        "Quickstart validation failed: {:?}",
        result.errors
    );

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_readme_doctor_capability() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let result = capabilities::validate_doctor(&ctx).await?;

    assert!(
        result.success,
        "Doctor validation failed: {:?}",
        result.errors
    );

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_readme_lifecycle_capability() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let result = capabilities::validate_lifecycle(&ctx).await?;

    assert!(
        result.success,
        "Lifecycle validation failed: {:?}",
        result.errors
    );

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_readme_marketplace_capability() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let result = capabilities::validate_marketplace(&ctx).await?;

    assert!(
        result.success,
        "Marketplace validation failed: {:?}",
        result.errors
    );

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_readme_generation_performance_slo() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let result = capabilities::validate_generation_performance(&ctx).await?;

    assert!(
        result.success,
        "Generation performance SLO failed: {:?}",
        result.errors
    );

    // Verify <3s SLO
    assert!(
        result.duration_ms < 3000.0,
        "Generation took {}ms (expected <3000ms)",
        result.duration_ms
    );

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_readme_deterministic_output() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let result = capabilities::validate_deterministic(&ctx).await?;

    assert!(
        result.success,
        "Deterministic output validation failed: {:?}",
        result.errors
    );

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_comprehensive_validation() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    let report = validators::generate_validation_report(&ctx).await?;

    // Print report
    report.print_summary();

    // At least 80% success rate required
    assert!(
        report.success_rate >= 80.0,
        "Validation success rate {}% is below 80%",
        report.success_rate
    );

    // All critical paths must pass
    for result in &report.results {
        if result.capability.contains("Quickstart")
            || result.capability.contains("Doctor")
            || result.capability.contains("Lifecycle")
        {
            assert!(
                result.success,
                "Critical capability '{}' failed",
                result.capability
            );
        }
    }

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_trace_collection() -> Result<()> {
    let ctx = ValidationContext::new();
    ctx.init()?;

    // Run a simple validation
    let _ = capabilities::validate_doctor(&ctx).await?;

    // Verify traces were collected
    let spans = ctx.collector.get_spans();
    assert!(!spans.is_empty(), "No traces were collected");

    ctx.shutdown();
    Ok(())
}

#[tokio::test]
async fn test_performance_metrics_validation() -> Result<()> {
    use collectors::PerformanceMetrics;

    // Test SLO validation
    let good_metrics = PerformanceMetrics {
        generation_time_ms: 2000.0,
        memory_usage_mb: 50.0,
        cpu_usage_percent: 60.0,
    };

    assert!(good_metrics.validate_slos().is_ok());

    let bad_metrics = PerformanceMetrics {
        generation_time_ms: 5000.0, // Exceeds 3s SLO
        memory_usage_mb: 150.0,     // Exceeds 100MB SLO
        cpu_usage_percent: 60.0,
    };

    assert!(bad_metrics.validate_slos().is_err());

    Ok(())
}
