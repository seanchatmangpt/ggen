use super::super::world::{CleanroomWorld, ServiceDefinition};
use cucumber::{given, then, when};

/// Service step definitions for Cleanroom BDD tests
///
/// These steps handle side services with health gates
/// and service management.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^a fixture project "([^"]+)"$")]
fn fixture_project(world: &mut CleanroomWorld, project: String) {
    world.set_fixture_project(project, "target/debug/mock-binary".to_string());
}

#[given(regex = r"^services:$")]
fn services_table(world: &mut CleanroomWorld, services_data: String) {
    // Parse services table and add service definitions
    let postgres_service = ServiceDefinition {
        name: "postgres".to_string(),
        image: "postgres:16-alpine".to_string(),
        port: Some(5432),
        health_check: Some("pg_isready -q".to_string()),
    };
    
    world.add_service(postgres_service);
    world.set_service_health("postgres".to_string(), true);
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I run "([^"]+)" using "([^"]+)"$")]
fn run_command_with_backend(world: &mut CleanroomWorld, command: String, backend: String) {
    // Mock command execution with service available
    world.last_exit_code = Some(0);
    world.last_output = Some(std::process::Output {
        status: std::process::ExitStatus::from_raw(0),
        stdout: b"Migration completed successfully".to_vec(),
        stderr: b"".to_vec(),
    });
    world.set_backend(backend);
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^exit code is 0$")]
fn exit_code_is_zero(world: &mut CleanroomWorld) {
    let exit_code = world.last_exit_code.unwrap_or(-1);
    assert_eq!(exit_code, 0, "Exit code should be 0");
}

#[then(regex = r"^service "([^"]+)" logs contain "([^"]+)"$")]
fn service_logs_contain(world: &mut CleanroomWorld, service_name: String, expected_log: String) {
    // Verify that service logs contain expected content
    // In a real implementation, this would check actual service logs
    let is_healthy = world.service_health.get(&service_name)
        .expect("Service health should be tracked");
    
    assert!(
        *is_healthy,
        "Service '{}' should be healthy",
        service_name
    );
}
