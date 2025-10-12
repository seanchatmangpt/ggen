use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};
use std::fs;

/// Policy step definitions for Cleanroom BDD tests
///
/// These steps handle policy configuration, constraints, and validation
/// for security, isolation, and resource management.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have a security policy with:$")]
fn security_policy(world: &mut CleanroomWorld, policy_config: String) {
    // Parse policy configuration
    for line in policy_config.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        
        if let Some((key, value)) = line.split_once(':') {
            let key = key.trim().to_string();
            let value = value.trim().to_string();
            
            match key.as_str() {
                "network" => {
                    world.add_network_constraint(value);
                }
                "filesystem" => {
                    world.add_filesystem_constraint(value);
                }
                "memory_limit" | "cpu_limit" | "timeout" => {
                    world.set_policy(key, value);
                }
                _ => {
                    world.set_policy(key, value);
                }
            }
        }
    }
}

#[given(regex = r"^I have network isolation enabled$")]
fn network_isolation_enabled(world: &mut CleanroomWorld) {
    world.add_network_constraint("isolated".to_string());
}

#[given(regex = r"^I have filesystem isolation enabled$")]
fn filesystem_isolation_enabled(world: &mut CleanroomWorld) {
    world.add_filesystem_constraint("isolated".to_string());
}

#[given(regex = r"^I have resource limits configured$")]
fn resource_limits_configured(world: &mut CleanroomWorld) {
    world.set_policy("memory_limit".to_string(), "512MB".to_string());
    world.set_policy("cpu_limit".to_string(), "1.0".to_string());
    world.set_policy("timeout".to_string(), "30s".to_string());
}

#[given(regex = r"^I have a policy file "([^"]+)" with:$")]
fn policy_file(world: &mut CleanroomWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    
    // Create parent directories if needed
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create parent directories");
    }
    
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write policy file {}: {}", filename, e));
    
    // Capture the policy file
    world.capture_file(&filename, content.trim().to_string());
}

#[given(regex = r"^I have allowed network ports: "([^"]+)"$")]
fn allowed_network_ports(world: &mut CleanroomWorld, ports: String) {
    world.set_policy("allowed_ports".to_string(), ports);
}

#[given(regex = r"^I have blocked network access$")]
fn blocked_network_access(world: &mut CleanroomWorld) {
    world.add_network_constraint("blocked".to_string());
}

#[given(regex = r"^I have read-only filesystem access$")]
fn readonly_filesystem_access(world: &mut CleanroomWorld) {
    world.add_filesystem_constraint("readonly".to_string());
}

#[given(regex = r"^I have writable filesystem access$")]
fn writable_filesystem_access(world: &mut CleanroomWorld) {
    world.add_filesystem_constraint("writable".to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I apply the policy "([^"]+)"$")]
fn apply_policy(world: &mut CleanroomWorld, policy_name: String) {
    // Load and apply policy from file
    let policy_file = world.project_dir.join(&policy_name);
    
    if !policy_file.exists() {
        panic!("Policy file '{}' does not exist", policy_name);
    }
    
    let content = fs::read_to_string(&policy_file)
        .unwrap_or_else(|e| panic!("Failed to read policy file '{}': {}", policy_name, e));
    
    // Parse and apply policy
    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        
        if let Some((key, value)) = line.split_once('=') {
            world.set_policy(key.trim().to_string(), value.trim().to_string());
        }
    }
}

#[when(regex = r"^I set the memory limit to "([^"]+)"$")]
fn set_memory_limit(world: &mut CleanroomWorld, limit: String) {
    world.set_policy("memory_limit".to_string(), limit);
}

#[when(regex = r"^I set the CPU limit to "([^"]+)"$")]
fn set_cpu_limit(world: &mut CleanroomWorld, limit: String) {
    world.set_policy("cpu_limit".to_string(), limit);
}

#[when(regex = r"^I set the timeout to "([^"]+)"$")]
fn set_timeout(world: &mut CleanroomWorld, timeout: String) {
    world.set_policy("timeout".to_string(), timeout);
}

#[when(regex = r"^I enable network isolation$")]
fn enable_network_isolation(world: &mut CleanroomWorld) {
    world.add_network_constraint("isolated".to_string());
}

#[when(regex = r"^I disable network isolation$")]
fn disable_network_isolation(world: &mut CleanroomWorld) {
    // Remove network isolation constraint
    world.network_constraints.retain(|c| c != "isolated");
}

#[when(regex = r"^I enable filesystem isolation$")]
fn enable_filesystem_isolation(world: &mut CleanroomWorld) {
    world.add_filesystem_constraint("isolated".to_string());
}

#[when(regex = r"^I disable filesystem isolation$")]
fn disable_filesystem_isolation(world: &mut CleanroomWorld) {
    // Remove filesystem isolation constraint
    world.filesystem_constraints.retain(|c| c != "isolated");
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the policy should be active$")]
fn policy_should_be_active(world: &mut CleanroomWorld) {
    // Verify that policy settings are applied
    assert!(
        !world.policy_settings.is_empty() || 
        !world.network_constraints.is_empty() || 
        !world.filesystem_constraints.is_empty(),
        "No policy settings are active"
    );
}

#[then(regex = r"^the network should be isolated$")]
fn network_should_be_isolated(world: &mut CleanroomWorld) {
    assert!(
        world.network_constraints.contains(&"isolated".to_string()),
        "Network isolation should be enabled"
    );
}

#[then(regex = r"^the filesystem should be isolated$")]
fn filesystem_should_be_isolated(world: &mut CleanroomWorld) {
    assert!(
        world.filesystem_constraints.contains(&"isolated".to_string()),
        "Filesystem isolation should be enabled"
    );
}

#[then(regex = r"^the memory limit should be "([^"]+)"$")]
fn memory_limit_should_be(world: &mut CleanroomWorld, expected_limit: String) {
    let actual_limit = world.policy_settings.get("memory_limit")
        .expect("Memory limit should be set");
    
    assert_eq!(
        actual_limit, &expected_limit,
        "Expected memory limit '{}', but got '{}'",
        expected_limit, actual_limit
    );
}

#[then(regex = r"^the CPU limit should be "([^"]+)"$")]
fn cpu_limit_should_be(world: &mut CleanroomWorld, expected_limit: String) {
    let actual_limit = world.policy_settings.get("cpu_limit")
        .expect("CPU limit should be set");
    
    assert_eq!(
        actual_limit, &expected_limit,
        "Expected CPU limit '{}', but got '{}'",
        expected_limit, actual_limit
    );
}

#[then(regex = r"^the timeout should be "([^"]+)"$")]
fn timeout_should_be(world: &mut CleanroomWorld, expected_timeout: String) {
    let actual_timeout = world.policy_settings.get("timeout")
        .expect("Timeout should be set");
    
    assert_eq!(
        actual_timeout, &expected_timeout,
        "Expected timeout '{}', but got '{}'",
        expected_timeout, actual_timeout
    );
}

#[then(regex = r"^the network access should be blocked$")]
fn network_access_should_be_blocked(world: &mut CleanroomWorld) {
    assert!(
        world.network_constraints.contains(&"blocked".to_string()),
        "Network access should be blocked"
    );
}

#[then(regex = r"^the filesystem should be read-only$")]
fn filesystem_should_be_readonly(world: &mut CleanroomWorld) {
    assert!(
        world.filesystem_constraints.contains(&"readonly".to_string()),
        "Filesystem should be read-only"
    );
}

#[then(regex = r"^the filesystem should be writable$")]
fn filesystem_should_be_writable(world: &mut CleanroomWorld) {
    assert!(
        world.filesystem_constraints.contains(&"writable".to_string()),
        "Filesystem should be writable"
    );
}

#[then(regex = r"^the allowed ports should be "([^"]+)"$")]
fn allowed_ports_should_be(world: &mut CleanroomWorld, expected_ports: String) {
    let actual_ports = world.policy_settings.get("allowed_ports")
        .expect("Allowed ports should be set");
    
    assert_eq!(
        actual_ports, &expected_ports,
        "Expected allowed ports '{}', but got '{}'",
        expected_ports, actual_ports
    );
}

#[then(regex = r"^the policy should be validated$")]
fn policy_should_be_validated(world: &mut CleanroomWorld) {
    // Check that policy settings are consistent
    let has_network_constraints = !world.network_constraints.is_empty();
    let has_filesystem_constraints = !world.filesystem_constraints.is_empty();
    let has_resource_limits = world.policy_settings.contains_key("memory_limit") ||
                             world.policy_settings.contains_key("cpu_limit") ||
                             world.policy_settings.contains_key("timeout");
    
    // At least one type of constraint should be active
    assert!(
        has_network_constraints || has_filesystem_constraints || has_resource_limits,
        "Policy should have at least one constraint active"
    );
}

#[then(regex = r"^the policy should be enforced$")]
fn policy_should_be_enforced(world: &mut CleanroomWorld) {
    // Verify that policy enforcement is active
    // This would typically involve checking that the policy is actually
    // being applied to the execution environment
    
    // For now, just verify that policy settings exist
    assert!(
        !world.policy_settings.is_empty() || 
        !world.network_constraints.is_empty() || 
        !world.filesystem_constraints.is_empty(),
        "Policy should be enforced but no constraints are active"
    );
}
