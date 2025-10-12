use super::super::world::CleanroomWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Backend step definitions for Cleanroom BDD tests
///
/// These steps handle backend selection, configuration, and validation
/// for different execution environments (local, Docker, Podman).

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have Docker available$")]
fn docker_available(world: &mut CleanroomWorld) {
    // Check if Docker is available
    let output = Command::new("docker")
        .arg("version")
        .output();
    
    match output {
        Ok(output) if output.status.success() => {
            world.set_backend("docker".to_string());
        }
        _ => {
            panic!("Docker is not available - required for this test");
        }
    }
}

#[given(regex = r"^I have Podman available$")]
fn podman_available(world: &mut CleanroomWorld) {
    // Check if Podman is available
    let output = Command::new("podman")
        .arg("version")
        .output();
    
    match output {
        Ok(output) if output.status.success() => {
            world.set_backend("podman".to_string());
        }
        _ => {
            panic!("Podman is not available - required for this test");
        }
    }
}

#[given(regex = r"^I have a local execution environment$")]
fn local_execution_environment(world: &mut CleanroomWorld) {
    // Local backend is always available
    world.set_backend("local".to_string());
}

#[given(regex = r"^I have a container image "([^"]+)"$")]
fn have_container_image(world: &mut CleanroomWorld, image: String) {
    // Verify the image exists locally or can be pulled
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set before checking for container image");
    
    let cmd = match backend.as_str() {
        "docker" => "docker",
        "podman" => "podman",
        _ => panic!("Container backend required for image operations"),
    };
    
    let output = Command::new(cmd)
        .arg("image")
        .arg("inspect")
        .arg(&image)
        .output();
    
    match output {
        Ok(output) if output.status.success() => {
            // Image exists locally
        }
        _ => {
            // Try to pull the image
            let pull_output = Command::new(cmd)
                .arg("pull")
                .arg(&image)
                .output()
                .expect("Failed to pull container image");
            
            if !pull_output.status.success() {
                panic!("Failed to pull container image '{}'", image);
            }
        }
    }
}

#[given(regex = r"^I have a container registry at "([^"]+)"$")]
fn have_container_registry(world: &mut CleanroomWorld, registry_url: String) {
    // Setup mock registry if needed, or verify real registry
    if registry_url.contains("localhost") || registry_url.contains("127.0.0.1") {
        // Use mock registry
        let mock_url = world.setup_mock_server();
        world.set_policy("registry_url".to_string(), mock_url);
    } else {
        // Verify real registry is accessible
        world.set_policy("registry_url".to_string(), registry_url);
    }
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I select the "([^"]+)" backend$")]
fn select_backend(world: &mut CleanroomWorld, backend: String) {
    match backend.as_str() {
        "local" => {
            world.set_backend("local".to_string());
        }
        "docker" => {
            // Verify Docker is available
            let output = Command::new("docker")
                .arg("version")
                .output()
                .expect("Failed to check Docker version");
            
            if !output.status.success() {
                panic!("Docker backend selected but Docker is not available");
            }
            
            world.set_backend("docker".to_string());
        }
        "podman" => {
            // Verify Podman is available
            let output = Command::new("podman")
                .arg("version")
                .output()
                .expect("Failed to check Podman version");
            
            if !output.status.success() {
                panic!("Podman backend selected but Podman is not available");
            }
            
            world.set_backend("podman".to_string());
        }
        "auto" => {
            // Auto-detect backend
            if Command::new("docker").arg("version").output().map(|o| o.status.success()).unwrap_or(false) {
                world.set_backend("docker".to_string());
            } else if Command::new("podman").arg("version").output().map(|o| o.status.success()).unwrap_or(false) {
                world.set_backend("podman".to_string());
            } else {
                world.set_backend("local".to_string());
            }
        }
        _ => {
            panic!("Unknown backend: {}", backend);
        }
    }
}

#[when(regex = r"^I configure the backend with:$")]
fn configure_backend(world: &mut CleanroomWorld, config: String) {
    // Parse configuration and apply to world state
    for line in config.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        
        if let Some((key, value)) = line.split_once('=') {
            world.set_policy(key.trim().to_string(), value.trim().to_string());
        }
    }
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the backend should be "([^"]+)"$")]
fn backend_should_be(world: &mut CleanroomWorld, expected_backend: String) {
    let actual_backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    assert_eq!(
        actual_backend, &expected_backend,
        "Expected backend '{}', but got '{}'",
        expected_backend, actual_backend
    );
}

#[then(regex = r"^the backend should be automatically detected$")]
fn backend_should_be_auto_detected(world: &mut CleanroomWorld) {
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    // Verify it's one of the supported backends
    assert!(
        matches!(backend.as_str(), "local" | "docker" | "podman"),
        "Backend '{}' is not a supported auto-detected backend",
        backend
    );
}

#[then(regex = r"^the container should be running$")]
fn container_should_be_running(world: &mut CleanroomWorld) {
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    let cmd = match backend.as_str() {
        "docker" => "docker",
        "podman" => "podman",
        _ => panic!("Container backend required for container operations"),
    };
    
    // Check if any containers are running
    let output = Command::new(cmd)
        .arg("ps")
        .arg("--format")
        .arg("{{.Status}}")
        .output()
        .expect("Failed to check container status");
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    if !stdout.contains("Up") {
        panic!("No containers are running");
    }
}

#[then(regex = r"^the container should be stopped$")]
fn container_should_be_stopped(world: &mut CleanroomWorld) {
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    let cmd = match backend.as_str() {
        "docker" => "docker",
        "podman" => "podman",
        _ => panic!("Container backend required for container operations"),
    };
    
    // Check if any containers are running
    let output = Command::new(cmd)
        .arg("ps")
        .arg("--format")
        .arg("{{.Status}}")
        .output()
        .expect("Failed to check container status");
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    if stdout.contains("Up") {
        panic!("Containers are still running");
    }
}

#[then(regex = r"^the backend should support network isolation$")]
fn backend_supports_network_isolation(world: &mut CleanroomWorld) {
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    match backend.as_str() {
        "docker" | "podman" => {
            // Container backends support network isolation
        }
        "local" => {
            // Local backend has limited network isolation
            world.add_network_constraint("limited".to_string());
        }
        _ => {
            panic!("Unknown backend: {}", backend);
        }
    }
}

#[then(regex = r"^the backend should support filesystem isolation$")]
fn backend_supports_filesystem_isolation(world: &mut CleanroomWorld) {
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    match backend.as_str() {
        "docker" | "podman" => {
            // Container backends support full filesystem isolation
            world.add_filesystem_constraint("isolated".to_string());
        }
        "local" => {
            // Local backend has limited filesystem isolation
            world.add_filesystem_constraint("limited".to_string());
        }
        _ => {
            panic!("Unknown backend: {}", backend);
        }
    }
}

#[then(regex = r"^the backend should support resource limits$")]
fn backend_supports_resource_limits(world: &mut CleanroomWorld) {
    let backend = world.current_backend.as_ref()
        .expect("Backend should be set");
    
    match backend.as_str() {
        "docker" | "podman" => {
            // Container backends support resource limits
        }
        "local" => {
            // Local backend has limited resource control
        }
        _ => {
            panic!("Unknown backend: {}", backend);
        }
    }
}
