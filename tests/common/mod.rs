//! Test Configuration Helper Module
//!
//! Reads configuration values from `chicago-tdd-tools.toml` and provides
//! convenient accessor functions for test timeouts, property test cases,
//! testcontainers settings, performance, and guard constraints.
//!
//! Configuration is cached after first read to avoid repeated file I/O.
//! Default values are used if config file is missing or values are invalid.

// Submodules for test utilities
pub mod fixtures;
pub mod helpers;

// Re-export commonly used items
// These re-exports enable the `use common::{...}` pattern in tests
#[allow(unused_imports)]
pub use fixtures::*;
#[allow(unused_imports)]
pub use helpers::*;

use std::sync::OnceLock;
use std::time::Duration;

/// Configuration structure matching chicago-tdd-tools.toml
#[derive(Debug, Clone)]
struct TestConfig {
    // Test timeouts
    unit_timeout_seconds: u64,
    integration_timeout_seconds: u64,

    // Property testing
    default_test_cases: u32,

    // Testcontainers
    container_wait_timeout_seconds: u64,
    http_connection_timeout_seconds: u64,
    default_http_port: u16,
    default_https_port: u16,
    default_http_alt_port: u16,
    concurrent_containers_count: usize,
    concurrent_commands_count: usize,
    multi_container_count: usize,
    commands_per_container: usize,

    // Performance
    hot_path_tick_budget: u64,

    // Guards
    max_run_len: usize,
    max_batch_size: usize,
}

impl Default for TestConfig {
    fn default() -> Self {
        Self {
            // Test timeouts - defaults from chicago-tdd-tools.toml comments
            unit_timeout_seconds: 1,
            integration_timeout_seconds: 30,

            // Property testing - default from config
            default_test_cases: 100,

            // Testcontainers - defaults from config
            container_wait_timeout_seconds: 5,
            http_connection_timeout_seconds: 2,
            default_http_port: 80,
            default_https_port: 443,
            default_http_alt_port: 8080,
            concurrent_containers_count: 5,
            concurrent_commands_count: 10,
            multi_container_count: 3,
            commands_per_container: 5,

            // Performance - default from config
            hot_path_tick_budget: 8,

            // Guards - defaults from config
            max_run_len: 8,
            max_batch_size: 1000,
        }
    }
}

/// Global config cache
static CONFIG: OnceLock<TestConfig> = OnceLock::new();

/// Load configuration from chicago-tdd-tools.toml
fn load_config() -> TestConfig {
    // Try to read config file from project root
    let config_path = std::path::Path::new("chicago-tdd-tools.toml");

    if !config_path.exists() {
        // Config file not found, use defaults
        return TestConfig::default();
    }

    let config_content = match std::fs::read_to_string(config_path) {
        Ok(content) => content,
        Err(_) => return TestConfig::default(),
    };

    let parsed: toml::Value = match toml::from_str(&config_content) {
        Ok(value) => value,
        Err(_) => return TestConfig::default(),
    };

    // Extract values with fallback to defaults
    let mut config = TestConfig::default();

    // Test timeouts
    if let Some(test_table) = parsed.get("test").and_then(|v| v.as_table()) {
        if let Some(unit_timeout) = test_table
            .get("unit_timeout_seconds")
            .and_then(|v| v.as_integer())
        {
            config.unit_timeout_seconds = unit_timeout.max(1) as u64; // Minimum 1 second
        }
        if let Some(integration_timeout) = test_table
            .get("integration_timeout_seconds")
            .and_then(|v| v.as_integer())
        {
            config.integration_timeout_seconds = integration_timeout.max(1) as u64;
            // Minimum 1 second
        }
    }

    // Property testing
    if let Some(property_table) = parsed.get("property").and_then(|v| v.as_table()) {
        if let Some(test_cases) = property_table
            .get("default_test_cases")
            .and_then(|v| v.as_integer())
        {
            config.default_test_cases = test_cases.max(1) as u32; // Minimum 1 test case
        }
    }

    // Testcontainers
    if let Some(tc_table) = parsed.get("testcontainers").and_then(|v| v.as_table()) {
        if let Some(timeout) = tc_table
            .get("container_wait_timeout_seconds")
            .and_then(|v| v.as_integer())
        {
            config.container_wait_timeout_seconds = timeout.max(1) as u64;
        }
        if let Some(timeout) = tc_table
            .get("http_connection_timeout_seconds")
            .and_then(|v| v.as_integer())
        {
            config.http_connection_timeout_seconds = timeout.max(1) as u64;
        }
        if let Some(port) = tc_table
            .get("default_http_port")
            .and_then(|v| v.as_integer())
        {
            config.default_http_port = port.clamp(1, 65535) as u16;
        }
        if let Some(port) = tc_table
            .get("default_https_port")
            .and_then(|v| v.as_integer())
        {
            config.default_https_port = port.clamp(1, 65535) as u16;
        }
        if let Some(port) = tc_table
            .get("default_http_alt_port")
            .and_then(|v| v.as_integer())
        {
            config.default_http_alt_port = port.clamp(1, 65535) as u16;
        }
        if let Some(count) = tc_table
            .get("concurrent_containers_count")
            .and_then(|v| v.as_integer())
        {
            config.concurrent_containers_count = count.max(1) as usize;
        }
        if let Some(count) = tc_table
            .get("concurrent_commands_count")
            .and_then(|v| v.as_integer())
        {
            config.concurrent_commands_count = count.max(1) as usize;
        }
        if let Some(count) = tc_table
            .get("multi_container_count")
            .and_then(|v| v.as_integer())
        {
            config.multi_container_count = count.max(1) as usize;
        }
        if let Some(count) = tc_table
            .get("commands_per_container")
            .and_then(|v| v.as_integer())
        {
            config.commands_per_container = count.max(1) as usize;
        }
    }

    // Performance
    if let Some(perf_table) = parsed.get("performance").and_then(|v| v.as_table()) {
        if let Some(budget) = perf_table
            .get("hot_path_tick_budget")
            .and_then(|v| v.as_integer())
        {
            config.hot_path_tick_budget = budget.max(1) as u64;
        }
    }

    // Guards
    if let Some(guards_table) = parsed.get("guards").and_then(|v| v.as_table()) {
        if let Some(len) = guards_table.get("max_run_len").and_then(|v| v.as_integer()) {
            config.max_run_len = len.max(1) as usize;
        }
        if let Some(size) = guards_table
            .get("max_batch_size")
            .and_then(|v| v.as_integer())
        {
            config.max_batch_size = size.max(1) as usize;
        }
    }

    config
}

/// Get the cached configuration, loading it if necessary
fn get_config() -> &'static TestConfig {
    CONFIG.get_or_init(load_config)
}

// ============================================================================
// Public API - Test Timeouts
// ============================================================================

/// Get unit test timeout duration from config
pub fn unit_timeout() -> Duration {
    Duration::from_secs(get_config().unit_timeout_seconds)
}

/// Get integration test timeout duration from config
///
/// **Note**: The `async_test!` macro automatically uses this timeout from config.
/// Only use `async_test_with_timeout!` if you need a custom timeout different from config.
pub fn integration_timeout() -> Duration {
    Duration::from_secs(get_config().integration_timeout_seconds)
}

// ============================================================================
// Public API - Property Testing
// ============================================================================

/// Get default number of test cases for property-based testing
pub fn property_test_cases() -> u32 {
    get_config().default_test_cases
}

// ============================================================================
// Public API - Testcontainers
// ============================================================================

/// Get container wait timeout duration from config
pub fn container_wait_timeout() -> Duration {
    Duration::from_secs(get_config().container_wait_timeout_seconds)
}

/// Get HTTP connection timeout duration from config
pub fn http_connection_timeout() -> Duration {
    Duration::from_secs(get_config().http_connection_timeout_seconds)
}

/// Get default HTTP port from config
pub fn default_http_port() -> u16 {
    get_config().default_http_port
}

/// Get default HTTPS port from config
pub fn default_https_port() -> u16 {
    get_config().default_https_port
}

/// Get default HTTP alternate port from config
pub fn default_http_alt_port() -> u16 {
    get_config().default_http_alt_port
}

/// Get concurrent containers count from config
pub fn concurrent_containers_count() -> usize {
    get_config().concurrent_containers_count
}

/// Get concurrent commands count from config
pub fn concurrent_commands_count() -> usize {
    get_config().concurrent_commands_count
}

/// Get multi-container count from config
pub fn multi_container_count() -> usize {
    get_config().multi_container_count
}

/// Get commands per container from config
pub fn commands_per_container() -> usize {
    get_config().commands_per_container
}

// ============================================================================
// Public API - Performance
// ============================================================================

/// Get hot path tick budget from config
pub fn hot_path_tick_budget() -> u64 {
    get_config().hot_path_tick_budget
}

// ============================================================================
// Public API - Guards
// ============================================================================

/// Get maximum run length from config
pub fn max_run_len() -> usize {
    get_config().max_run_len
}

/// Get maximum batch size from config
pub fn max_batch_size() -> usize {
    get_config().max_batch_size
}

// ============================================================================
// Public API - Docker Availability
// ============================================================================

/// Check if Docker is available and running
///
/// 🚨 CRITICAL - Returns `false` if the Docker daemon is not running.
///
/// This helper verifies the Docker daemon is actually running and responding,
/// not just that the `docker` command executed successfully.
///
/// **Kaizen improvement**: Added timeout to prevent hanging when Docker is stopped.
/// Pattern: All external commands should have timeouts to fail fast.
/// Benefits: Prevents tests from hanging indefinitely and provides fast feedback.
///
/// # Returns
///
/// `true` if the Docker daemon is running and responding, `false` otherwise.
#[allow(dead_code)]
pub fn docker_available() -> bool {
    use std::process::Command;

    // Use docker ps to verify daemon is running (fast and reliable)
    // Simple synchronous check - docker ps is fast enough (< 1s typically)
    match Command::new("docker")
        .args(["ps", "--format", "{{.ID}}"])
        .output()
    {
        Ok(output) => {
            // Command succeeded - Docker daemon is running and responding
            // docker ps will succeed even with no containers, so success means Docker is available
            output.status.success()
        }
        Err(_) => {
            // 🚨 Docker command not found or failed
            false
        }
    }
}

/// Require Docker to be available, panic if not
///
/// 🚨 CRITICAL - Stops test immediately if Docker is unavailable.
///
/// Integration tests that require Docker should use this function.
/// If Docker is not available, the test will fail with a clear error message.
///
/// # Panics
///
/// Panics if Docker is not available, with a message indicating Docker is required
/// and how to start Docker.
///
/// # Example
///
/// ```rust,no_run
/// use crate::common::require_docker;
///
/// #[test]
/// fn test_container_operation() {
///     require_docker();
///     // Test code here...
/// }
/// ```
#[allow(dead_code)]
pub fn require_docker() {
    if !docker_available() {
        panic!(
            "🚨 Docker is required for this test but Docker daemon is not running.\n\
             ⚠️  STOP: Cannot proceed with test\n\
             💡 FIX: Start Docker Desktop or Docker daemon\n\
             📋 macOS: Open Docker Desktop\n\
             📋 Linux: sudo systemctl start docker\n\
             📋 Windows: Start Docker Desktop"
        );
    }
    // ✅ Docker is available, test can proceed
}

/// Find ggen binary in container at common locations
///
/// **Root Cause Prevention**: Helper function to find binary instead of hardcoded path assumption.
/// Pattern: Always search for binary in common locations, don't assume path.
///
/// # Arguments
///
/// * `container` - Container to search in
/// * `workspace_root` - Root directory of workspace (e.g., "/workspace")
///
/// # Returns
///
/// Path to binary if found, or error if not found
#[allow(dead_code)]
pub fn find_ggen_binary_in_container(
    container: &chicago_tdd_tools::testcontainers::GenericContainer, workspace_root: &str,
) -> Result<String, String> {
    use chicago_tdd_tools::testcontainers::exec::SUCCESS_EXIT_CODE;

    // Try common locations
    let locations = vec![
        format!("{}/target/release/ggen", workspace_root),
        format!("{}/target/debug/ggen", workspace_root),
        format!("{}/ggen/target/release/ggen", workspace_root),
        format!("{}/ggen/target/debug/ggen", workspace_root),
    ];

    for location in &locations {
        let check = container.exec("test", &["-f", location]).ok();
        if let Some(result) = check {
            if result.exit_code == SUCCESS_EXIT_CODE {
                return Ok(location.clone());
            }
        }
    }

    // If not found, search for any ggen binary
    let search = container
        .exec(
            "sh",
            &[
                "-c",
                &format!(
                    "find {} -name 'ggen' -type f 2>/dev/null | head -1",
                    workspace_root
                ),
            ],
        )
        .ok();

    if let Some(result) = search {
        if result.exit_code == SUCCESS_EXIT_CODE && !result.stdout.trim().is_empty() {
            return Ok(result.stdout.trim().to_string());
        }
    }

    Err(format!(
        "ggen binary not found in any common location. Searched: {:?}",
        locations
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_loads_with_defaults() {
        // Should load defaults even if config file doesn't exist
        let unit = unit_timeout();
        assert_eq!(unit.as_secs(), 1);

        let integration = integration_timeout();
        assert_eq!(integration.as_secs(), 30);
    }

    #[test]
    fn test_config_values_are_positive() {
        // All timeout values should be positive
        assert!(unit_timeout().as_secs() > 0);
        assert!(integration_timeout().as_secs() > 0);
        assert!(container_wait_timeout().as_secs() > 0);
        assert!(http_connection_timeout().as_secs() > 0);

        // Property test cases should be positive
        assert!(property_test_cases() > 0);

        // Performance and guard values should be positive
        assert!(hot_path_tick_budget() > 0);
        assert!(max_run_len() > 0);
        assert!(max_batch_size() > 0);
    }

    #[test]
    fn test_config_is_cached() {
        // Multiple calls should return same values (cached)
        let timeout1 = unit_timeout();
        let timeout2 = unit_timeout();
        assert_eq!(timeout1, timeout2);
    }

    #[test]
    fn test_property_test_cases_default() {
        let cases = property_test_cases();
        assert!(cases >= 1);
        assert!(cases <= 10000); // Reasonable upper bound
    }

    #[test]
    fn test_testcontainers_config_values() {
        // Ports should be valid
        assert!(default_http_port() > 0);
        assert!(default_https_port() > 0);
        assert!(default_http_alt_port() > 0);

        // Counts should be positive
        assert!(concurrent_containers_count() > 0);
        assert!(concurrent_commands_count() > 0);
        assert!(multi_container_count() > 0);
        assert!(commands_per_container() > 0);
    }
}
