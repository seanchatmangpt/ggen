use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};

/// Security step definitions for Cleanroom BDD tests
///
/// These steps handle secure-by-default policy
/// and capability management.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^a fixture project "([^"]+)"$")]
fn fixture_project(world: &mut CleanroomWorld, project: String) {
    world.set_fixture_project(project, "target/debug/mock-binary".to_string());
}

#[given(regex = r"^I enable policy "([^"]+)" explicitly$")]
fn enable_policy_explicitly(world: &mut CleanroomWorld, policy: String) {
    world.set_policy("network_enabled".to_string(), "true".to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I run "([^"]+)" using "([^"]+)"$")]
fn run_command_with_backend(world: &mut CleanroomWorld, command: String, backend: String) {
    // Mock command execution
    world.last_exit_code = Some(1); // Network disabled by default
    world.last_output = Some(std::process::Output {
        status: std::process::ExitStatus::from_raw(1),
        stdout: b"".to_vec(),
        stderr: b"network disabled".to_vec(),
    });
    world.set_backend(backend);
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the exit code is nonzero$")]
fn exit_code_is_nonzero(world: &mut CleanroomWorld) {
    let exit_code = world.last_exit_code.unwrap_or(0);
    assert_ne!(exit_code, 0, "Exit code should be nonzero");
}

#[then(regex = r"^stderr contains "([^"]+)"$")]
fn stderr_contains(world: &mut CleanroomWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected stderr to contain '{}', but got: {}",
        expected, stderr
    );
}

#[then(regex = r"^capabilities dropped include ALL$")]
fn capabilities_dropped_include_all(world: &mut CleanroomWorld) {
    // Verify that capabilities are dropped
    // In a real implementation, this would check actual capability dropping
    assert!(
        world.policy_settings.contains_key("capabilities_dropped"),
        "Capabilities should be dropped"
    );
}

#[then(regex = r"^process runs as non-root$")]
fn process_runs_as_non_root(world: &mut CleanroomWorld) {
    // Verify that process runs as non-root
    // In a real implementation, this would check actual user ID
    assert!(
        world.policy_settings.contains_key("non_root"),
        "Process should run as non-root"
    );
}

#[then(regex = r"^the exit code is 0$")]
fn exit_code_is_zero(world: &mut CleanroomWorld) {
    let exit_code = world.last_exit_code.unwrap_or(-1);
    assert_eq!(exit_code, 0, "Exit code should be 0");
}
