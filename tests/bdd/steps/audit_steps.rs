use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for `ggen audit` noun-verb commands
///
/// Covers audit operations:
/// - audit hazard: Generate hazard reports
/// - audit performance: Performance analysis
/// - audit security: Security vulnerability checks

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have a project with dependencies$")]
fn have_project_with_dependencies(world: &mut GgenWorld) {
    // Create a mock ggen.lock file with dependencies
    let lockfile_content = r#"{
        "packages": {
            "io.ggen.rust.cli-subcommand": {
                "version": "0.2.0",
                "sha256": "abc123def456",
                "installed": true
            },
            "io.ggen.python.web-api": {
                "version": "1.0.0", 
                "sha256": "def456ghi789",
                "installed": true
            }
        }
    }"#;

    let lockfile_path = world.project_dir.join("ggen.lock");
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r"^I have vulnerable dependencies$")]
fn have_vulnerable_dependencies(world: &mut GgenWorld) {
    // Create a mock lockfile with known vulnerable packages
    let lockfile_content = r#"{
        "packages": {
            "io.ggen.vulnerable.package": {
                "version": "0.1.0",
                "sha256": "vulnerable123",
                "installed": true,
                "vulnerabilities": ["CVE-2023-1234"]
            }
        }
    }"#;

    let lockfile_path = world.project_dir.join("ggen.lock");
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write vulnerable lockfile");
}

#[given(regex = r"^I have performance-critical code$")]
fn have_performance_critical_code(world: &mut GgenWorld) {
    // Create mock source files that would trigger performance warnings
    let source_content = r#"// Performance-critical code
fn slow_function() {
    for i in 0..1000000 {
        // Simulate expensive operation
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}"#;

    let source_path = world.project_dir.join("src/main.rs");
    fs::create_dir_all(source_path.parent().unwrap()).expect("Failed to create src directory");
    fs::write(&source_path, source_content).expect("Failed to write performance-critical code");
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen audit hazard"$"#)]
fn run_audit_hazard(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("audit")
        .arg("hazard")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run audit hazard");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen audit performance"$"#)]
fn run_audit_performance(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("audit")
        .arg("performance")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run audit performance");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen audit security"$"#)]
fn run_audit_security(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("audit")
        .arg("security")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run audit security");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen audit (.+)"$"#)]
fn run_audit_command(world: &mut GgenWorld, subcommand: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("audit")
        .arg(&subcommand)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run audit command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^I should see hazard report$")]
fn should_see_hazard_report(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("hazard")
            || stdout.contains("Hazard")
            || stdout.contains("risk")
            || stdout.contains("Risk")
            || stdout.contains("report")
            || stdout.contains("Report"),
        "Expected to see hazard report, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see performance analysis$")]
fn should_see_performance_analysis(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("performance")
            || stdout.contains("Performance")
            || stdout.contains("slow")
            || stdout.contains("optimization")
            || stdout.contains("bottleneck")
            || stdout.contains("timing"),
        "Expected to see performance analysis, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see security vulnerabilities$")]
fn should_see_security_vulnerabilities(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("security")
            || stdout.contains("Security")
            || stdout.contains("vulnerability")
            || stdout.contains("Vulnerability")
            || stdout.contains("CVE")
            || stdout.contains("exploit")
            || stdout.contains("threat"),
        "Expected to see security vulnerabilities, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see no vulnerabilities$")]
fn should_see_no_vulnerabilities(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("No vulnerabilities")
            || stdout.contains("no vulnerabilities")
            || stdout.contains("clean")
            || stdout.contains("secure")
            || stdout.is_empty(),
        "Expected to see no vulnerabilities, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see risk assessment$")]
fn should_see_risk_assessment(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("risk")
            || stdout.contains("Risk")
            || stdout.contains("assessment")
            || stdout.contains("Assessment")
            || stdout.contains("severity")
            || stdout.contains("Severity")
            || stdout.contains("critical")
            || stdout.contains("high")
            || stdout.contains("medium")
            || stdout.contains("low"),
        "Expected to see risk assessment, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see recommendations$")]
fn should_see_recommendations(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("recommendation")
            || stdout.contains("Recommendation")
            || stdout.contains("suggestion")
            || stdout.contains("Suggestion")
            || stdout.contains("fix")
            || stdout.contains("Fix")
            || stdout.contains("improve")
            || stdout.contains("Improve"),
        "Expected to see recommendations, but got: {}",
        stdout
    );
}

#[then(regex = r"^the report should be in JSON format$")]
fn report_should_be_json_format(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    serde_json::from_str::<serde_json::Value>(&stdout)
        .unwrap_or_else(|e| panic!("Report should be valid JSON: {}\nOutput: {}", e, stdout));
}

#[then(regex = r"^the report should be in HTML format$")]
fn report_should_be_html_format(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("<html")
            || stdout.contains("<HTML")
            || stdout.contains("<body")
            || stdout.contains("<head")
            || stdout.contains("<!DOCTYPE"),
        "Expected to see HTML format, but got: {}",
        stdout
    );
}

#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut GgenWorld) {
    assert!(
        world.last_command_succeeded(),
        "Command failed with exit code: {}\nStderr: {}",
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr()
    );
}

#[then(regex = r"^the command should fail$")]
fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

#[then(regex = r#"^I should see "([^"]+)" in output$"#)]
fn should_see_in_output(world: &mut GgenWorld, expected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains(&expected) || stderr.contains(&expected),
        "Expected to see '{}' in output, but got:\nStdout: {}\nStderr: {}",
        expected,
        stdout,
        stderr
    );
}

#[then(regex = r#"^I should see "([^"]+)" in stderr$"#)]
fn should_see_in_stderr(world: &mut GgenWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected to see '{}' in stderr, but got: {}",
        expected,
        stderr
    );
}
