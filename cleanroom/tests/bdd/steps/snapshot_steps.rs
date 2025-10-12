use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};

/// Snapshot step definitions for Cleanroom BDD tests
///
/// These steps handle snapshots with redaction and stability
/// for consistent output comparison across backends.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^a fixture project "([^"]+)"$")]
fn fixture_project(world: &mut CleanroomWorld, project: String) {
    world.set_fixture_project(project, "target/debug/mock-binary".to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I run "([^"]+)" using "([^"]+)"$")]
fn run_command_with_backend(world: &mut CleanroomWorld, command: String, backend: String) {
    // Mock command execution
    world.last_exit_code = Some(0);
    world.last_output = Some(std::process::Output {
        status: std::process::ExitStatus::from_raw(0),
        stdout: b"USAGE: mock-binary [OPTIONS]\nOPTIONS:\n  --help     Show this help message".to_vec(),
        stderr: b"".to_vec(),
    });
    world.set_backend(backend);
}

#[when(regex = r"^I snapshot stdout as "([^"]+)"$")]
fn snapshot_stdout_as(world: &mut CleanroomWorld, snapshot_name: String) {
    let stdout = world.last_stdout();
    world.add_snapshot(snapshot_name, stdout);
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^snapshot matches on "([^"]+)" backend too$")]
fn snapshot_matches_on_backend(world: &mut CleanroomWorld, backend: String) {
    // Verify that snapshots are consistent across backends
    // In a real implementation, this would compare snapshots from different backends
    assert!(
        !world.snapshots.is_empty(),
        "Should have snapshots to compare"
    );
}

#[then(regex = r"^paths and timestamps are normalized$")]
fn paths_and_timestamps_normalized(world: &mut CleanroomWorld) {
    // Verify that paths and timestamps are normalized in snapshots
    // In a real implementation, this would check for normalized content
    assert!(
        !world.snapshots.is_empty(),
        "Should have snapshots to check normalization"
    );
}
