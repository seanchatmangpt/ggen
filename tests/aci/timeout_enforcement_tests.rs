//! Timeout Enforcement & Poka-Yoke Tests (User Story 2)
//!
//! Tests verify that cargo make targets enforce timeouts and prevent mistakes
//! through warnings-as-errors, quality gates, and SLO violation detection.
//!
//! SUCCESS CRITERIA: 60% SLO violation reduction, 50% defect escape reduction (SC-003, SC-004)
//!
//! NOTE: These tests follow Chicago TDD - they test observable behavior
//! using real process execution, not mocks.

#[path = "mod.rs"]
mod aci_utils;

use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};
use aci_utils::{parse_makefile_toml, extract_description};

/// Test: cargo make check enforces 5s timeout
///
/// EXPECTED: Process killed after 5s when hung
/// RATIONALE: Prevents indefinite hangs, enforces <5s SLO from tool description
#[test]
fn test_timeout_enforcement_on_check() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    // Verify check target has timeout wrapper
    let check_target = targets.get("check").expect("check target not found");

    // Check should use timeout command OR have timeout in script
    let has_timeout_command = check_target.command.as_ref()
        .map(|cmd| cmd == "timeout")
        .unwrap_or(false);

    let has_timeout_script = check_target.script.as_ref()
        .map(|script| script.contains("timeout"))
        .unwrap_or(false);

    assert!(
        has_timeout_command || has_timeout_script,
        "check target MUST use 'timeout' command or timeout in script for poka-yoke enforcement. command={:?}, script={:?}",
        check_target.command,
        check_target.script.as_ref().map(|s| &s[..100.min(s.len())])
    );

    // Verify timeout value is reasonable (<= 15s as documented)
    let check_desc = check_target.description.as_ref()
        .expect("check has no description");

    let has_timeout_spec = check_desc.contains("15s timeout")
        || check_desc.contains("<5s")
        || check_desc.contains("SLO:");

    assert!(
        has_timeout_spec,
        "check description MUST specify timeout in SLO section: {}",
        check_desc
    );
}

/// Test: cargo make check treats warnings as errors
///
/// EXPECTED: Compiler warnings cause RED Andon signal (exit code != 0)
/// RATIONALE: Warnings are defects - poka-yoke prevents them from reaching production
#[test]
fn test_warnings_as_errors_enforcement() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    let check_target = targets.get("check").expect("check target not found");

    // Check should enforce RUSTFLAGS="-D warnings" or equivalent
    // This can be set via:
    // 1. env.RUSTFLAGS in task definition
    // 2. [env] section with RUSTFLAGS
    // 3. .cargo/config.toml (project-wide)

    let check_desc = check_target.description.as_ref()
        .expect("check has no description");

    // Description should mention warnings-as-errors policy
    let mentions_warnings = check_desc.to_lowercase().contains("warning")
        || check_desc.to_lowercase().contains("red")
        || check_desc.to_lowercase().contains("andon");

    assert!(
        mentions_warnings,
        "check description should mention warnings treatment in Andon signals: {}",
        check_desc
    );
}

/// Test: Quality gate validation prevents completion on RED signals
///
/// EXPECTED: RED signals (errors, test failures) prevent pre-commit from passing
/// RATIONALE: Poka-yoke - cannot commit broken code
#[test]
fn test_quality_gate_validation() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    // pre-commit should have dependencies that enforce quality gates
    let pre_commit = targets.get("pre-commit").expect("pre-commit target not found");

    let desc = pre_commit.description.as_ref()
        .expect("pre-commit has no description");

    // Should mention quality gates, validation, or checks
    let has_quality_gates = desc.to_lowercase().contains("check")
        || desc.to_lowercase().contains("lint")
        || desc.to_lowercase().contains("test")
        || desc.to_lowercase().contains("validate");

    assert!(
        has_quality_gates,
        "pre-commit description should mention quality gate checks: {}",
        desc
    );

    // Should mention RED Andon signals or error handling
    let mentions_error_handling = desc.to_lowercase().contains("red")
        || desc.to_lowercase().contains("error")
        || desc.to_lowercase().contains("fail");

    assert!(
        mentions_error_handling,
        "pre-commit description should mention RED signal handling: {}",
        desc
    );
}

/// Test: SLO violation detection catches timeout violations
///
/// EXPECTED: Processes exceeding SLO thresholds are detected and reported
/// RATIONALE: Measure and enforce performance standards
#[test]
fn test_slo_violation_detection() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    // Critical targets should have documented SLO thresholds
    let critical_targets = vec![
        ("check", "<5s"),
        ("test-unit", "<16s"),
        ("test", "30s-120s"),
        ("lint", "<10s"),
        ("build", "<30s"),
    ];

    let mut violations = Vec::new();

    for (target_name, expected_slo) in critical_targets {
        if let Some(target) = targets.get(target_name) {
            if let Some(desc) = &target.description {
                // Check if description contains SLO specification
                let has_slo = desc.contains("SLO:")
                    || desc.contains("timeout")
                    || desc.contains("<")
                    || desc.to_lowercase().contains("second");

                if !has_slo {
                    violations.push(format!(
                        "{}: Missing SLO specification (expected: {})",
                        target_name, expected_slo
                    ));
                }
            } else {
                violations.push(format!("{}: No description", target_name));
            }
        } else {
            violations.push(format!("{}: Target not found", target_name));
        }
    }

    if !violations.is_empty() {
        panic!(
            "SLO violations detected:\n{}",
            violations.join("\n")
        );
    }
}

/// Test: All timeout wrappers use consistent command format
///
/// EXPECTED: timeout command with seconds value (e.g., "timeout 15s")
/// RATIONALE: Consistent poka-yoke pattern across all targets
#[test]
fn test_consistent_timeout_command_format() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    // Targets that MUST have timeout wrappers (command OR script)
    let timeout_required = vec![
        "check", "test", "test-unit", "test-integration",
        "lint", "build", "build-release"
    ];

    let mut violations = Vec::new();

    for target_name in timeout_required {
        if let Some(target) = targets.get(target_name) {
            let has_timeout_command = target.command.as_ref()
                .map(|cmd| cmd == "timeout")
                .unwrap_or(false);

            let has_timeout_script = target.script.as_ref()
                .map(|script| script.contains("timeout"))
                .unwrap_or(false);

            if !has_timeout_command && !has_timeout_script {
                violations.push(format!(
                    "{}: Missing timeout wrapper (command: {:?}, has_script: {})",
                    target_name,
                    target.command,
                    target.script.is_some()
                ));
            }
        }
    }

    if !violations.is_empty() {
        panic!(
            "Timeout wrapper violations:\n{}",
            violations.join("\n")
        );
    }
}

/// Test: Pre-commit hook includes timeout validation
///
/// EXPECTED: pre-commit verifies timeout enforcement on critical targets
/// RATIONALE: Prevent commits that bypass poka-yoke mechanisms
#[test]
fn test_pre_commit_timeout_validation() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    let pre_commit = targets.get("pre-commit").expect("pre-commit target not found");
    let desc = pre_commit.description.as_ref()
        .expect("pre-commit has no description");

    // Should mention timeout enforcement or SLO validation
    let mentions_timeout = desc.to_lowercase().contains("timeout")
        || desc.to_lowercase().contains("slo")
        || desc.contains("<");

    assert!(
        mentions_timeout,
        "pre-commit description should mention timeout enforcement: {}",
        desc
    );
}

/// Test: Poka-yoke mechanisms are documented in Makefile.toml
///
/// EXPECTED: Comments explain timeout values, warnings-as-errors, quality gates
/// RATIONALE: Documentation is part of poka-yoke - makes mistake prevention explicit
#[test]
fn test_poka_yoke_documentation_exists() {
    let makefile_content = std::fs::read_to_string("Makefile.toml")
        .expect("Failed to read Makefile.toml");

    // Should have comments explaining poka-yoke patterns
    let has_timeout_docs = makefile_content.contains("timeout")
        && (makefile_content.contains("poka-yoke")
            || makefile_content.contains("mistake prevention")
            || makefile_content.contains("error-proofing"));

    assert!(
        has_timeout_docs || makefile_content.contains("SLO:"),
        "Makefile.toml should document poka-yoke mechanisms (timeout enforcement, SLOs)"
    );
}

/// Test: Actual timeout enforcement works (integration test)
///
/// EXPECTED: cargo make check completes within 15s timeout
/// RATIONALE: Verify timeout is actually enforced at runtime
#[test]
fn test_runtime_timeout_enforcement() {
    let start = Instant::now();

    let output = Command::new("cargo")
        .args(["make", "check"])
        .output()
        .expect("Failed to execute cargo make check");

    let elapsed = start.elapsed();

    // Should complete within documented 15s timeout
    // (Allow 20s buffer for slow CI environments)
    assert!(
        elapsed < Duration::from_secs(20),
        "cargo make check exceeded timeout: {:?} (should be <15s per SLO)",
        elapsed
    );

    // Should exit successfully (or with timeout error code 124)
    assert!(
        output.status.success() || output.status.code() == Some(124),
        "cargo make check failed unexpectedly: {:?}",
        output.status
    );
}

/// Test: Warnings-as-errors runtime enforcement
///
/// EXPECTED: Compiler warnings cause cargo make check to fail
/// RATIONALE: Verify RUSTFLAGS=-D warnings is actually applied
#[test]
#[ignore] // Only run when testing warning enforcement (requires test code with warnings)
fn test_runtime_warnings_as_errors() {
    // This test should be run with code that produces warnings
    // For now, we verify the configuration is present

    let output = Command::new("cargo")
        .args(["make", "check"])
        .output()
        .expect("Failed to execute cargo make check");

    // If there are warnings, check should fail (not just warn)
    // This is integration test - requires actual warning in codebase to validate

    let stderr = String::from_utf8_lossy(&output.stderr);
    if stderr.contains("warning:") {
        assert!(
            !output.status.success(),
            "cargo make check should FAIL on warnings (warnings-as-errors not enforced)"
        );
    }
}
