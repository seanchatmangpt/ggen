//! Integration tests for the cargo publish validator script.
//!
//! These tests verify the validator script works correctly with the cleanroom project.

use std::path::PathBuf;
use std::process::Command;

/// Get the path to the validator script
fn validator_script_path() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("bin");
    path.push("validate-crate");
    path
}

/// Get the project root path
fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

#[test]
fn test_validator_exists() {
    let script = validator_script_path();
    assert!(
        script.exists(),
        "Validator script should exist at {:?}",
        script
    );
}

#[test]
fn test_validator_is_executable() {
    let script = validator_script_path();
    let metadata = std::fs::metadata(&script)
        .unwrap_or_else(|e| panic!("Failed to get metadata for {:?}: {}", script, e));

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = metadata.permissions().mode();
        assert!(
            mode & 0o111 != 0,
            "Validator script should be executable: mode={:o}",
            mode
        );
    }
}

#[test]
#[ignore] // This test runs the actual validator and may take time
fn test_validator_help() {
    let script = validator_script_path();
    let output = Command::new(&script)
        .arg("--help")
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    assert!(
        output.status.success(),
        "Validator --help should succeed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("cargo publish validator"),
        "Help should mention cargo publish validator"
    );
    assert!(stdout.contains("OPTIONS"), "Help should show options");
}

#[test]
#[ignore] // This test runs the actual validator and may take significant time
fn test_validator_on_cleanroom_project() {
    let script = validator_script_path();
    let output = Command::new(&script)
        .arg("-v") // Verbose mode
        .arg("-t")
        .arg("30") // 30 second timeout for safety
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Print output for debugging
    println!("=== Validator Output ===");
    println!("{}", stdout);
    if !stderr.is_empty() {
        println!("=== Validator Errors ===");
        println!("{}", stderr);
    }

    // Check that key validation steps ran
    assert!(
        stdout.contains("Validating Cargo.toml"),
        "Should run Cargo.toml validation"
    );
    assert!(
        stdout.contains("Running cargo check"),
        "Should run cargo check"
    );
    assert!(
        stdout.contains("Validation Summary"),
        "Should show validation summary"
    );

    // Note: We don't assert on exit code because the project may have warnings/issues
    // The important thing is that the validator runs to completion
}

#[test]
#[ignore] // This test runs the actual validator
fn test_validator_generates_report() {
    use std::fs;

    let script = validator_script_path();
    let report_path = project_root().join("test-validation-report.json");

    // Clean up any existing report
    let _ = fs::remove_file(&report_path);

    let output = Command::new(&script)
        .arg("-o")
        .arg(&report_path)
        .arg("-t")
        .arg("30")
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    // Check that report was generated
    assert!(
        report_path.exists(),
        "Validation report should be generated at {:?}",
        report_path
    );

    // Parse and validate report structure
    let report_content =
        fs::read_to_string(&report_path).unwrap_or_else(|e| panic!("Failed to read report: {}", e));

    let report: serde_json::Value = serde_json::from_str(&report_content)
        .unwrap_or_else(|e| panic!("Failed to parse report JSON: {}", e));

    // Verify report structure
    assert!(
        report.get("timestamp").is_some(),
        "Report should have timestamp"
    );
    assert!(
        report.get("project_root").is_some(),
        "Report should have project_root"
    );
    assert!(
        report.get("duration_seconds").is_some(),
        "Report should have duration"
    );
    assert!(report.get("status").is_some(), "Report should have status");
    assert!(report.get("checks").is_some(), "Report should have checks");
    assert!(
        report.get("ready_to_publish").is_some(),
        "Report should have ready_to_publish"
    );
    assert!(
        report.get("validations").is_some(),
        "Report should have validations"
    );

    // Clean up
    let _ = fs::remove_file(&report_path);

    // Print report for debugging (if test fails)
    if !output.status.success() {
        println!("=== Validation Report ===");
        println!("{}", serde_json::to_string_pretty(&report).unwrap());
    }
}

#[test]
#[ignore] // This test checks performance
fn test_validator_completes_in_time() {
    use std::time::Instant;

    let script = validator_script_path();
    let start = Instant::now();

    let output = Command::new(&script)
        .arg("-t")
        .arg("10") // 10 second timeout
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    let duration = start.elapsed();

    println!("Validator completed in {:.2}s", duration.as_secs_f64());

    // Should complete within 15 seconds (10s target + 5s buffer)
    assert!(
        duration.as_secs() < 15,
        "Validator should complete within 15 seconds, took {}s",
        duration.as_secs()
    );

    // Print summary if it failed
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        println!("=== Validator Output ===");
        println!("{}", stdout);
    }
}

#[test]
fn test_validator_invalid_timeout() {
    let script = validator_script_path();
    let output = Command::new(&script)
        .arg("-t")
        .arg("not-a-number")
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    // Should fail with error exit code
    assert!(
        !output.status.success(),
        "Validator should fail with invalid timeout"
    );
}

#[test]
#[ignore] // This test checks specific validations
fn test_validator_checks_cargo_toml() {
    let script = validator_script_path();
    let output = Command::new(&script)
        .arg("-v")
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should check Cargo.toml fields
    assert!(
        stdout.contains("Validating Cargo.toml") || stdout.contains("Cargo.toml validation"),
        "Should validate Cargo.toml"
    );
}

#[test]
#[ignore] // This test checks README detection
fn test_validator_checks_readme() {
    let script = validator_script_path();
    let output = Command::new(&script)
        .arg("-v")
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should check for README
    assert!(
        stdout.contains("README") || stdout.contains("readme"),
        "Should check for README file"
    );

    // Cleanroom project has README.md, so should pass
    if output.status.success() {
        assert!(
            stdout.contains("README found") || stdout.contains("PASS"),
            "Should find README.md in cleanroom project"
        );
    }
}

#[test]
#[ignore] // This test checks license detection
fn test_validator_checks_license() {
    let script = validator_script_path();
    let output = Command::new(&script)
        .arg("-v")
        .current_dir(project_root())
        .output()
        .unwrap_or_else(|e| panic!("Failed to run validator: {}", e));

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should check for LICENSE
    assert!(
        stdout.contains("LICENSE") || stdout.contains("license") || stdout.contains("License"),
        "Should check for LICENSE file"
    );

    // Cleanroom project has LICENSE, so should pass
    if output.status.success() {
        assert!(
            stdout.contains("License file found") || stdout.contains("PASS"),
            "Should find LICENSE in cleanroom project"
        );
    }
}
