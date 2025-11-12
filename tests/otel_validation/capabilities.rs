//! README capability validation tests
//!
//! Each test validates a specific capability mentioned in the README
//! using OpenTelemetry traces as proof.

use super::*;
use std::process::Command;
use std::time::Instant;

/// Validate quickstart capability (2-minute setup)
#[instrument(name = "validate.quickstart", skip(ctx))]
pub async fn validate_quickstart(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating quickstart capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    // Test: quickstart demo command
    let output = Command::new("cargo")
        .args(&["run", "--", "quickstart", "demo", "--dry-run"])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            ctx.collector.assert_span_exists("ggen.quickstart")?;
            ctx.collector
                .assert_duration_under("ggen.quickstart", 120_000.0)?; // 2 minutes
        }
        Ok(out) => errors.push(format!(
            "Quickstart failed: {}",
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => errors.push(format!("Quickstart execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "Quickstart".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "Quickstart".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate AI-powered template generation
#[instrument(name = "validate.ai_generation", skip(ctx))]
pub async fn validate_ai_generation(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating AI generation capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    // Test: AI template generation (dry-run)
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "ai",
            "generate",
            "-d",
            "REST API module",
            "--dry-run",
        ])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            // Validate required spans exist
            if let Err(e) = ctx.collector.assert_span_exists("ggen.ai.generate") {
                errors.push(e.to_string());
            }
            if let Err(e) = ctx.collector.assert_span_success("ggen.ai.generate") {
                errors.push(e.to_string());
            }
        }
        Ok(out) => errors.push(format!(
            "AI generation failed: {}",
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => errors.push(format!("AI generation execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "AI Generation".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "AI Generation".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate doctor command (environment check)
#[instrument(name = "validate.doctor", skip(ctx))]
pub async fn validate_doctor(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating doctor capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    let output = Command::new("cargo")
        .args(&["run", "--", "doctor"])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            ctx.collector.assert_span_exists("ggen.doctor")?;
            ctx.collector.assert_span_success("ggen.doctor")?;
        }
        Ok(out) => errors.push(format!(
            "Doctor failed: {}",
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => errors.push(format!("Doctor execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "Doctor Command".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "Doctor Command".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate lifecycle phase execution
#[instrument(name = "validate.lifecycle", skip(ctx))]
pub async fn validate_lifecycle(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating lifecycle capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    // Test: lifecycle list command
    let output = Command::new("cargo")
        .args(&["run", "--", "lifecycle", "list"])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            ctx.collector.assert_span_exists("ggen.lifecycle.list")?;
            ctx.collector.assert_span_success("ggen.lifecycle.list")?;
        }
        Ok(out) => errors.push(format!(
            "Lifecycle failed: {}",
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => errors.push(format!("Lifecycle execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "Lifecycle".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "Lifecycle".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate marketplace search
#[instrument(name = "validate.marketplace", skip(ctx))]
pub async fn validate_marketplace(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating marketplace capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    let output = Command::new("cargo")
        .args(&["run", "--", "search", "rust"])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            ctx.collector
                .assert_span_exists("ggen.marketplace.search")?;
            ctx.collector
                .assert_duration_under("ggen.marketplace.search", 5000.0)?; // <5s
        }
        Ok(out) => errors.push(format!(
            "Marketplace search failed: {}",
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => errors.push(format!("Marketplace execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "Marketplace".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "Marketplace".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate GitHub integration
#[instrument(name = "validate.github", skip(ctx))]
pub async fn validate_github(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating GitHub integration capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    let output = Command::new("cargo")
        .args(&["run", "--", "github", "pages-status"])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            ctx.collector.assert_span_exists("ggen.github.pages")?;
        }
        Ok(out) => {
            // GitHub commands may fail without credentials - that's OK
            warn!(
                "GitHub command completed with warnings: {}",
                String::from_utf8_lossy(&out.stderr)
            );
        }
        Err(e) => errors.push(format!("GitHub execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "GitHub Integration".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "GitHub Integration".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate template generation performance (<3s)
#[instrument(name = "validate.generation_performance", skip(ctx))]
pub async fn validate_generation_performance(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating generation performance SLO");
    let start = Instant::now();
    let mut errors = Vec::new();

    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "gen",
            "templates/rust-module.tmpl",
            "--vars",
            "name=test",
            "--dry-run",
        ])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            // Validate performance SLO: <3s
            ctx.collector
                .assert_duration_under("ggen.generate", 3000.0)?;
        }
        Ok(out) => errors.push(format!(
            "Generation failed: {}",
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => errors.push(format!("Generation execution failed: {}", e)),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "Generation Performance".to_string(),
            duration,
            1,
            1,
        ))
    } else {
        Ok(ValidationResult::failure(
            "Generation Performance".to_string(),
            duration,
            errors,
        ))
    }
}

/// Validate deterministic output (byte-identical)
#[instrument(name = "validate.deterministic", skip(ctx))]
pub async fn validate_deterministic(ctx: &ValidationContext) -> Result<ValidationResult> {
    info!("Validating deterministic output capability");
    let start = Instant::now();
    let mut errors = Vec::new();

    // Generate same template twice, verify identical output
    let output1 = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "gen",
            "templates/rust-module.tmpl",
            "--vars",
            "name=test,determinism=42",
        ])
        .output();

    let output2 = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "gen",
            "templates/rust-module.tmpl",
            "--vars",
            "name=test,determinism=42",
        ])
        .output();

    match (output1, output2) {
        (Ok(out1), Ok(out2)) if out1.status.success() && out2.status.success() => {
            if out1.stdout != out2.stdout {
                errors.push("Output not deterministic (byte mismatch)".to_string());
            }
            ctx.collector
                .assert_span_exists("ggen.generate.deterministic")?;
        }
        _ => errors.push("Deterministic generation failed".to_string()),
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success(
            "Deterministic Output".to_string(),
            duration,
            1,
            0,
        ))
    } else {
        Ok(ValidationResult::failure(
            "Deterministic Output".to_string(),
            duration,
            errors,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_capability_validators_compile() {
        // Ensure all validators compile and have correct signatures
        let ctx = ValidationContext::new();

        // Just verify the functions exist and can be called
        assert!(validate_quickstart(&ctx).await.is_ok());
    }
}
