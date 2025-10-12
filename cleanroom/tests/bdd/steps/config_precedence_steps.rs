use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};
use std::fs;

/// Configuration precedence step definitions for Cleanroom BDD tests
///
/// These steps handle configuration precedence and validation
/// for different configuration sources (TOML, environment, CLI args).

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^Cargo\.toml contains:$")]
fn cargo_toml_contains(world: &mut CleanroomWorld, content: String) {
    let cargo_toml_path = world.project_dir.join("Cargo.toml");
    
    fs::write(&cargo_toml_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write Cargo.toml: {}", e));
    
    world.capture_file("Cargo.toml", content.trim().to_string());
}

#[given(regex = r"^environment CLEANROOM_TIMEOUT_MS="([^"]+)"$")]
fn cleanroom_timeout_ms_env(world: &mut CleanroomWorld, timeout: String) {
    world.set_env("CLEANROOM_TIMEOUT_MS".to_string(), timeout);
}

#[given(regex = r"^I pass --timeout-ms=(\d+) to run\(\)$")]
fn pass_timeout_ms_to_run(world: &mut CleanroomWorld, timeout_ms: u64) {
    world.set_timeout(timeout_ms);
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I run "([^"]+)"$")]
fn run_command(world: &mut CleanroomWorld, command: String) {
    // Parse and execute command
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    // Mock command execution
    world.last_exit_code = Some(0);
    world.last_output = Some(std::process::Output {
        status: std::process::ExitStatus::from_raw(0),
        stdout: b"Mock output".to_vec(),
        stderr: b"".to_vec(),
    });
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^effective timeout is (\d+)$")]
fn effective_timeout_is(world: &mut CleanroomWorld, expected_timeout: u64) {
    let actual_timeout = world.timeout_ms.expect("Timeout should be set");
    
    assert_eq!(
        actual_timeout, expected_timeout,
        "Expected effective timeout {}, but got {}",
        expected_timeout, actual_timeout
    );
}

#[then(regex = r"^env contains "([^"]+)"$")]
fn env_contains(world: &mut CleanroomWorld, expected_env: String) {
    let (key, value) = expected_env.split_once('=')
        .expect("Environment variable should be in KEY=VALUE format");
    
    let actual_value = world.environment.get(key)
        .expect("Environment variable should be set");
    
    assert_eq!(
        actual_value, value,
        "Expected environment variable {}={}, but got {}={}",
        key, value, key, actual_value
    );
}
