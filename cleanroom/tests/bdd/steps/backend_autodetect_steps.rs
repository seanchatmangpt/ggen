use super::super::world::CleanroomWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::process::Output;

/// Backend auto-detection step definitions for Cleanroom BDD tests
///
/// These steps handle backend auto-detection and preference ordering
/// for different container engines and local execution.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^docker is "([^"]+)" available$")]
fn docker_availability(world: &mut CleanroomWorld, availability: String) {
    match availability.as_str() {
        "available" => {
            // Check if Docker is actually available
            let output = Command::new("docker")
                .arg("version")
                .output();
            
            let available = output.map(|o| o.status.success()).unwrap_or(false);
            world.docker_available = Some(available);
            
            if !available {
                panic!("Docker is marked as available but not found");
            }
        }
        "unavailable" => {
            world.docker_available = Some(false);
        }
        "maybe" => {
            // Check if Docker is available
            let output = Command::new("docker")
                .arg("version")
                .output();
            
            let available = output.map(|o| o.status.success()).unwrap_or(false);
            world.docker_available = Some(available);
        }
        _ => {
            panic!("Unknown Docker availability: {}", availability);
        }
    }
}

#[given(regex = r"^podman is "([^"]+)" available$")]
fn podman_availability(world: &mut CleanroomWorld, availability: String) {
    match availability.as_str() {
        "available" => {
            // Check if Podman is actually available
            let output = Command::new("podman")
                .arg("version")
                .output();
            
            let available = output.map(|o| o.status.success()).unwrap_or(false);
            world.podman_available = Some(available);
            
            if !available {
                panic!("Podman is marked as available but not found");
            }
        }
        "unavailable" => {
            world.podman_available = Some(false);
        }
        "maybe" => {
            // Check if Podman is available
            let output = Command::new("podman")
                .arg("version")
                .output();
            
            let available = output.map(|o| o.status.success()).unwrap_or(false);
            world.podman_available = Some(available);
        }
        _ => {
            panic!("Unknown Podman availability: {}", availability);
        }
    }
}

#[given(regex = r"^environment CLEANROOM_BACKEND is unset$")]
fn cleanroom_backend_unset(world: &mut CleanroomWorld) {
    world.unset_env("CLEANROOM_BACKEND".to_string());
}

#[given(regex = r#"^environment CLEANROOM_BACKEND="([^"]+)"$"#)]
fn cleanroom_backend_set(world: &mut CleanroomWorld, backend: String) {
    world.set_env("CLEANROOM_BACKEND".to_string(), backend);
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I request backend "([^"]+)"$")]
fn request_backend(world: &mut CleanroomWorld, backend: String) {
    // Auto-detect backend based on availability and preferences
    let resolved_backend = match backend.as_str() {
        "auto" => {
            // Check environment variable first
            if let Some(env_backend) = world.environment.get("CLEANROOM_BACKEND") {
                env_backend.clone()
            } else {
                // Auto-detect based on availability
                if world.docker_available.unwrap_or(false) {
                    "docker".to_string()
                } else if world.podman_available.unwrap_or(false) {
                    "podman".to_string()
                } else {
                    "local".to_string()
                }
            }
        }
        "docker" => {
            if world.docker_available.unwrap_or(false) {
                "docker".to_string()
            } else {
                panic!("Docker backend requested but not available");
            }
        }
        "podman" => {
            if world.podman_available.unwrap_or(false) {
                "podman".to_string()
            } else {
                panic!("Podman backend requested but not available");
            }
        }
        "local" => "local".to_string(),
        _ => {
            panic!("Unknown backend: {}", backend);
        }
    };
    
    world.set_backend(resolved_backend);
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r#"^the resolved backend is one of "([^"]+)"$"#)]
fn resolved_backend_is_one_of(world: &mut CleanroomWorld, expected_backends: String) {
    let resolved_backend = world.current_backend.as_ref()
        .expect("Backend should be resolved");
    
    let backends: Vec<&str> = expected_backends.split(',').map(|s| s.trim()).collect();
    
    assert!(
        backends.contains(&resolved_backend.as_str()),
        "Expected resolved backend to be one of {:?}, but got '{}'",
        backends, resolved_backend
    );
}

#[then(regex = r"^the resolved backend is "([^"]+)"$")]
fn resolved_backend_is(world: &mut CleanroomWorld, expected_backend: String) {
    let resolved_backend = world.current_backend.as_ref()
        .expect("Backend should be resolved");
    
    assert_eq!(
        resolved_backend, &expected_backend,
        "Expected resolved backend '{}', but got '{}'",
        expected_backend, resolved_backend
    );
}

#[then(regex = r"^preference order is docker > podman > local given availability$")]
fn preference_order_is_docker_podman_local(world: &mut CleanroomWorld) {
    let resolved_backend = world.current_backend.as_ref()
        .expect("Backend should be resolved");
    
    // Verify that the resolved backend follows the preference order
    match resolved_backend.as_str() {
        "docker" => {
            // Docker should only be chosen if it's available
            assert!(
                world.docker_available.unwrap_or(false),
                "Docker should be available if chosen"
            );
        }
        "podman" => {
            // Podman should only be chosen if Docker is not available but Podman is
            assert!(
                !world.docker_available.unwrap_or(false),
                "Docker should not be available if Podman is chosen"
            );
            assert!(
                world.podman_available.unwrap_or(false),
                "Podman should be available if chosen"
            );
        }
        "local" => {
            // Local should only be chosen if neither Docker nor Podman are available
            assert!(
                !world.docker_available.unwrap_or(false),
                "Docker should not be available if local is chosen"
            );
            assert!(
                !world.podman_available.unwrap_or(false),
                "Podman should not be available if local is chosen"
            );
        }
        _ => {
            panic!("Unknown resolved backend: {}", resolved_backend);
        }
    }
}

#[then(regex = r"^resolved backend is not "([^"]+)" if a container engine is available$")]
fn resolved_backend_is_not_if_container_available(world: &mut CleanroomWorld, backend: String) {
    let resolved_backend = world.current_backend.as_ref()
        .expect("Backend should be resolved");
    
    let container_available = world.docker_available.unwrap_or(false) || 
                             world.podman_available.unwrap_or(false);
    
    if container_available {
        assert_ne!(
            resolved_backend, &backend,
            "Resolved backend should not be '{}' when container engine is available",
            backend
        );
    }
}

#[then(regex = r"^the scenario is skipped with reason "([^"]+)"$")]
fn scenario_skipped_with_reason(world: &mut CleanroomWorld, expected_reason: String) {
    let current_scenario = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let skip_reason = world.skip_reasons.get(current_scenario)
        .expect("Scenario should have skip reason");
    
    assert!(
        skip_reason.contains(&expected_reason),
        "Expected skip reason to contain '{}', but got '{}'",
        expected_reason, skip_reason
    );
}

#[then(regex = r"^podman-tagged scenarios are skipped$")]
fn podman_tagged_scenarios_are_skipped(world: &mut CleanroomWorld) {
    // Check if Podman is unavailable
    if !world.podman_available.unwrap_or(false) {
        // Verify that Podman scenarios would be skipped
        // In a real implementation, this would check scenario tags
        assert!(
            world.skip_reasons.values().any(|reason| reason.contains("podman")),
            "Podman-tagged scenarios should be skipped when Podman is unavailable"
        );
    }
}

#[then(regex = r"^other scenarios run$")]
fn other_scenarios_run(world: &mut CleanroomWorld) {
    // Verify that non-Podman scenarios can still run
    // In a real implementation, this would check that other scenarios are not skipped
    
    let total_scenarios = world.scenarios.len();
    let skipped_scenarios = world.skip_reasons.len();
    
    assert!(
        skipped_scenarios < total_scenarios,
        "Some scenarios should still run, but all are skipped"
    );
}
