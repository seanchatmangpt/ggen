//! Performance benchmarks for template linting
//! Ensures linting meets performance SLAs (<100ms)

use ggen_domain::template::lint::*;
use ggen_utils::error::Result;
use std::time::Instant;

/// Helper to create test fixture path
fn fixture_path(name: &str) -> String {
    format!("tests/fixtures/{}", name)
}

#[test]
fn bench_lint_valid_template() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let start = Instant::now();
    let _report = lint_template(&fixture_path("valid_template.toml"), &options)?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Linting valid template took {}ms (should be <100ms)",
        duration.as_millis()
    );

    Ok(())
}

#[test]
fn bench_lint_large_template() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let start = Instant::now();
    let _report = lint_template(&fixture_path("large_template.toml"), &options)?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Linting large template took {}ms (should be <100ms)",
        duration.as_millis()
    );

    Ok(())
}

#[test]
fn bench_lint_with_sparql() -> Result<()> {
    let options = LintOptions {
        check_sparql: true,
        check_schema: false,
    };

    let start = Instant::now();
    let _report = lint_template(&fixture_path("template_with_sparql.toml"), &options)?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Linting with SPARQL validation took {}ms (should be <100ms)",
        duration.as_millis()
    );

    Ok(())
}

#[test]
fn bench_lint_with_schema() -> Result<()> {
    let options = LintOptions {
        check_sparql: false,
        check_schema: true,
    };

    let start = Instant::now();
    let _report = lint_template(&fixture_path("template_with_rdf.toml"), &options)?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Linting with schema validation took {}ms (should be <100ms)",
        duration.as_millis()
    );

    Ok(())
}

#[test]
fn bench_lint_all_checks() -> Result<()> {
    let options = LintOptions {
        check_sparql: true,
        check_schema: true,
    };

    let start = Instant::now();
    let _report = lint_template(&fixture_path("template_with_sparql.toml"), &options)?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Linting with all checks took {}ms (should be <100ms)",
        duration.as_millis()
    );

    Ok(())
}

#[test]
fn bench_lint_multiple_iterations() -> Result<()> {
    let options = LintOptions {
        check_sparql: true,
        check_schema: true,
    };

    let iterations = 10;
    let start = Instant::now();

    for _ in 0..iterations {
        let _report = lint_template(&fixture_path("valid_template.toml"), &options)?;
    }

    let total_duration = start.elapsed();
    let avg_duration = total_duration.as_millis() / iterations;

    assert!(
        avg_duration < 100,
        "Average linting time over {} iterations: {}ms (should be <100ms)",
        iterations,
        avg_duration
    );

    Ok(())
}

#[tokio::test]
async fn bench_execute_lint_async() -> Result<()> {
    let input = LintInput {
        template: fixture_path("valid_template.toml"),
        check_sparql: true,
        check_schema: true,
    };

    let start = Instant::now();
    let _output = execute_lint(input).await?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Async lint execution took {}ms (should be <100ms)",
        duration.as_millis()
    );

    Ok(())
}

#[test]
fn bench_lint_concurrent_templates() -> Result<()> {
    let templates = vec![
        "valid_template.toml",
        "template_with_sparql.toml",
        "template_with_rdf.toml",
        "large_template.toml",
    ];

    let options = LintOptions {
        check_sparql: true,
        check_schema: true,
    };

    let start = Instant::now();

    for template in templates {
        let _report = lint_template(&fixture_path(template), &options)?;
    }

    let total_duration = start.elapsed();
    let avg_duration = total_duration.as_millis() / 4;

    assert!(
        avg_duration < 100,
        "Average linting time for 4 templates: {}ms (should be <100ms each)",
        avg_duration
    );

    Ok(())
}

#[test]
fn bench_lint_report_creation() {
    let iterations: u128 = 1000;
    let start = Instant::now();

    for i in 0..iterations {
        let _report = LintReport {
            errors: vec![LintError {
                line: Some(i as usize),
                message: format!("Error {}", i),
            }],
            warnings: vec![LintWarning {
                line: Some(i as usize),
                message: format!("Warning {}", i),
            }],
        };
    }

    let duration = start.elapsed();
    let avg_duration_micros = duration.as_micros() / iterations;

    assert!(
        avg_duration_micros < 100,
        "Average report creation time: {}μs (should be <100μs)",
        avg_duration_micros
    );
}

#[test]
fn bench_lint_error_accumulation() -> Result<()> {
    // Test performance when accumulating many errors
    let options = LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let start = Instant::now();
    let report = lint_template(&fixture_path("invalid_template.toml"), &options)?;
    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 100,
        "Linting with error accumulation took {}ms (should be <100ms)",
        duration.as_millis()
    );

    assert!(report.errors.len() > 0, "Should have accumulated errors");

    Ok(())
}
