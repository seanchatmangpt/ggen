//! Unit tests for cleanroom testing framework modules
//!
//! These tests verify individual module functionality in isolation.

use cleanroom::{
    CleanroomConfig, Policy, SecurityLevel, NetworkPolicy,
    ResourceLimits, DeterministicManager, CoverageTracker,
    SnapshotManager, TracingManager, TestReport,
    PostgresContainer, RedisContainer, GenericContainer,
    ContainerWrapper, ServiceContainer,
    CleanroomError, ErrorKind,
};
use std::time::Duration;
use uuid::Uuid;

/// Test CleanroomConfig functionality
#[test]
fn test_cleanroom_config() {
    // Test default configuration
    let config = CleanroomConfig::default();
    assert!(config.enable_singleton_containers);
    assert_eq!(config.container_startup_timeout, Duration::from_secs(30));
    assert_eq!(config.test_execution_timeout, Duration::from_secs(300));
    assert!(config.enable_deterministic_execution);
    assert!(config.enable_coverage_tracking);
    assert!(config.enable_snapshot_testing);
    assert!(config.enable_tracing);
    
    // Test configuration validation
    assert!(config.validate().is_ok());
    
    // Test invalid configuration
    let mut invalid_config = CleanroomConfig::default();
    invalid_config.max_concurrent_containers = 0;
    assert!(invalid_config.validate().is_err());
}

/// Test Policy functionality
#[test]
fn test_policy() {
    // Test default policy
    let policy = Policy::default();
    assert_eq!(policy.security_level, SecurityLevel::Standard);
    assert!(policy.allows_network());
    
    // Test locked policy
    let locked_policy = Policy::locked();
    assert_eq!(locked_policy.security_level, SecurityLevel::Locked);
    assert!(!locked_policy.allows_network());
    
    // Test custom policy
    let custom_policy = Policy::with_security_level(SecurityLevel::Strict)
        .with_network_isolation(false);
    assert_eq!(custom_policy.security_level, SecurityLevel::Strict);
    assert!(custom_policy.allows_network());
    
    // Test policy summary
    let summary = custom_policy.summary();
    assert!(summary.contains("Policy Summary"));
    assert!(summary.contains("Security Level"));
}

/// Test ResourceLimits functionality
#[test]
fn test_resource_limits() {
    // Test default limits
    let limits = ResourceLimits::new();
    assert_eq!(limits.max_memory_mb, 1024);
    assert_eq!(limits.max_cpu_percent, 100.0);
    assert_eq!(limits.max_disk_mb, 2048);
    
    // Test custom limits
    let custom_limits = ResourceLimits::new()
        .with_max_memory_mb(512)
        .with_max_cpu_percent(50.0)
        .with_max_disk_mb(1024);
    
    assert_eq!(custom_limits.max_memory_mb, 512);
    assert_eq!(custom_limits.max_cpu_percent, 50.0);
    assert_eq!(custom_limits.max_disk_mb, 1024);
    
    // Test limit validation
    assert!(custom_limits.validate().is_ok());
    
    // Test invalid limits
    let invalid_limits = ResourceLimits::new()
        .with_max_memory_mb(0)
        .with_max_cpu_percent(-10.0);
    
    assert!(invalid_limits.validate().is_err());
}

/// Test DeterministicManager functionality
#[test]
fn test_deterministic_manager() {
    let manager = DeterministicManager::new();
    
    // Test seed setting
    let seed = 12345u64;
    manager.set_seed(seed);
    assert_eq!(manager.get_current_seed(), seed);
    
    // Test deterministic generation
    let value1 = manager.generate_deterministic_value("test_key");
    let value2 = manager.generate_deterministic_value("test_key");
    assert_eq!(value1, value2);
    
    // Test different keys produce different values
    let value3 = manager.generate_deterministic_value("different_key");
    assert_ne!(value1, value3);
}

/// Test CoverageTracker functionality
#[test]
fn test_coverage_tracker() {
    let tracker = CoverageTracker::new();
    
    // Test initial state
    let report = tracker.get_report();
    assert_eq!(report.total_tests, 0);
    assert_eq!(report.covered_lines, 0);
    assert_eq!(report.coverage_percentage, 0.0);
    
    // Test test execution tracking
    tracker.start_test("test1");
    tracker.record_line_execution("test1", 10);
    tracker.record_line_execution("test1", 20);
    tracker.end_test("test1", true);
    
    let report = tracker.get_report();
    assert_eq!(report.total_tests, 1);
    assert_eq!(report.passed_tests, 1);
    assert_eq!(report.failed_tests, 0);
    assert_eq!(report.covered_lines, 2);
}

/// Test SnapshotManager functionality
#[test]
fn test_snapshot_manager() {
    let manager = SnapshotManager::new();
    
    // Test snapshot creation
    let test_data = serde_json::json!({
        "key": "value",
        "number": 42
    });
    
    let snapshot_id = manager.create_snapshot("test_snapshot", &test_data);
    assert!(!snapshot_id.is_nil());
    
    // Test snapshot verification
    let is_valid = manager.verify_snapshot("test_snapshot", &test_data);
    assert!(is_valid);
    
    // Test snapshot retrieval
    let retrieved_snapshot = manager.get_snapshot("test_snapshot");
    assert!(retrieved_snapshot.is_some());
    
    // Test invalid snapshot
    let invalid_data = serde_json::json!({
        "key": "different_value"
    });
    
    let is_invalid = manager.verify_snapshot("test_snapshot", &invalid_data);
    assert!(!is_invalid);
}

/// Test TracingManager functionality
#[test]
fn test_tracing_manager() {
    let manager = TracingManager::new();
    
    // Test trace creation
    let trace_id = manager.start_trace("test_trace");
    assert!(!trace_id.is_nil());
    
    // Test trace logging
    manager.log_trace_event(&trace_id, "test_event", "test_data");
    
    // Test trace completion
    manager.end_trace(&trace_id);
    
    // Test trace retrieval
    let traces = manager.get_traces();
    assert!(!traces.is_empty());
    
    // Test trace filtering
    let filtered_traces = manager.get_traces_by_name("test_trace");
    assert!(!filtered_traces.is_empty());
}

/// Test TestReport functionality
#[test]
fn test_test_report() {
    let report = TestReport::new();
    
    // Test initial state
    assert!(!report.session_id.is_nil());
    assert_eq!(report.test_summary.total_tests, 0);
    assert_eq!(report.test_summary.passed_tests, 0);
    assert_eq!(report.test_summary.failed_tests, 0);
    
    // Test test execution tracking
    report.record_test_execution("test1", true, Duration::from_millis(100));
    report.record_test_execution("test2", false, Duration::from_millis(200));
    
    assert_eq!(report.test_summary.total_tests, 2);
    assert_eq!(report.test_summary.passed_tests, 1);
    assert_eq!(report.test_summary.failed_tests, 1);
    
    // Test report serialization
    let json_report = report.to_json().unwrap();
    assert!(json_report.contains("session_id"));
    assert!(json_report.contains("test_summary"));
}

/// Test PostgresContainer functionality
#[test]
fn test_postgres_container() {
    let container = PostgresContainer::new("postgres:15");
    
    // Test basic properties
    assert_eq!(container.image(), "postgres:15");
    assert_eq!(container.container_type(), "postgres");
    
    // Test environment variables
    let container_with_env = container
        .with_env("POSTGRES_PASSWORD", "test")
        .with_env("POSTGRES_DB", "testdb");
    
    // Test port configuration
    let container_with_port = container_with_env.with_port(5432);
    assert_eq!(container_with_port.port(), Some(5432));
    
    // Test connection string generation
    let connection_string = container_with_port.connection_string();
    assert!(connection_string.contains("postgresql://"));
    assert!(connection_string.contains("testdb"));
}

/// Test RedisContainer functionality
#[test]
fn test_redis_container() {
    let container = RedisContainer::new("redis:7");
    
    // Test basic properties
    assert_eq!(container.image(), "redis:7");
    assert_eq!(container.container_type(), "redis");
    
    // Test port configuration
    let container_with_port = container.with_port(6379);
    assert_eq!(container_with_port.port(), Some(6379));
    
    // Test Redis URL generation
    let redis_url = container_with_port.redis_url();
    assert!(redis_url.starts_with("redis://"));
}

/// Test GenericContainer functionality
#[test]
fn test_generic_container() {
    let container = GenericContainer::new("nginx:latest");
    
    // Test basic properties
    assert_eq!(container.image(), "nginx:latest");
    assert_eq!(container.container_type(), "generic");
    
    // Test environment variables
    let container_with_env = container
        .with_env("NGINX_PORT", "8080")
        .with_env("NGINX_HOST", "localhost");
    
    // Test port configuration
    let container_with_port = container_with_env.with_port(8080);
    assert_eq!(container_with_port.port(), Some(8080));
    
    // Test command configuration
    let container_with_cmd = container_with_port.with_command("nginx", &["-g", "daemon off;"]);
    assert_eq!(container_with_cmd.command(), Some(("nginx", vec!["-g", "daemon off;"])));
}

/// Test CleanroomError functionality
#[test]
fn test_cleanroom_error() {
    // Test error creation
    let error = CleanroomError::new(ErrorKind::ValidationError, "Test error message");
    assert_eq!(error.kind(), ErrorKind::ValidationError);
    assert_eq!(error.message(), "Test error message");
    
    // Test error conversion
    let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
    let cleanroom_error = CleanroomError::from(io_error);
    assert_eq!(cleanroom_error.kind(), ErrorKind::IoError);
    
    // Test error chaining
    let chained_error = CleanroomError::chain_error(error, "Additional context");
    assert!(chained_error.message().contains("Additional context"));
    assert!(chained_error.message().contains("Test error message"));
}

/// Test error kind functionality
#[test]
fn test_error_kind() {
    // Test error kind display
    assert_eq!(format!("{}", ErrorKind::ValidationError), "ValidationError");
    assert_eq!(format!("{}", ErrorKind::IoError), "IoError");
    assert_eq!(format!("{}", ErrorKind::ContainerError), "ContainerError");
    
    // Test error kind from string
    assert_eq!(ErrorKind::from_str("ValidationError"), Ok(ErrorKind::ValidationError));
    assert_eq!(ErrorKind::from_str("IoError"), Ok(ErrorKind::IoError));
    assert_eq!(ErrorKind::from_str("InvalidError"), Err("Invalid error kind"));
}

/// Test configuration serialization
#[test]
fn test_config_serialization() {
    let config = CleanroomConfig::default();
    
    // Test JSON serialization
    let json = serde_json::to_string(&config).unwrap();
    assert!(json.contains("enable_singleton_containers"));
    assert!(json.contains("container_startup_timeout"));
    
    // Test JSON deserialization
    let deserialized_config: CleanroomConfig = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_config.enable_singleton_containers, config.enable_singleton_containers);
    assert_eq!(deserialized_config.container_startup_timeout, config.container_startup_timeout);
    
    // Test TOML serialization
    let toml = toml::to_string(&config).unwrap();
    assert!(toml.contains("enable_singleton_containers"));
    assert!(toml.contains("container_startup_timeout"));
    
    // Test TOML deserialization
    let deserialized_config: CleanroomConfig = toml::from_str(&toml).unwrap();
    assert_eq!(deserialized_config.enable_singleton_containers, config.enable_singleton_containers);
}

/// Test policy serialization
#[test]
fn test_policy_serialization() {
    let policy = Policy::with_security_level(SecurityLevel::Strict)
        .with_network_isolation(false);
    
    // Test JSON serialization
    let json = serde_json::to_string(&policy).unwrap();
    assert!(json.contains("security_level"));
    assert!(json.contains("network"));
    
    // Test JSON deserialization
    let deserialized_policy: Policy = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_policy.security_level, policy.security_level);
    assert_eq!(deserialized_policy.network.enable_network_isolation, policy.network.enable_network_isolation);
}

/// Test resource limits serialization
#[test]
fn test_resource_limits_serialization() {
    let limits = ResourceLimits::new()
        .with_max_memory_mb(512)
        .with_max_cpu_percent(50.0)
        .with_max_disk_mb(1024);
    
    // Test JSON serialization
    let json = serde_json::to_string(&limits).unwrap();
    assert!(json.contains("max_memory_mb"));
    assert!(json.contains("max_cpu_percent"));
    assert!(json.contains("max_disk_mb"));
    
    // Test JSON deserialization
    let deserialized_limits: ResourceLimits = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_limits.max_memory_mb, limits.max_memory_mb);
    assert_eq!(deserialized_limits.max_cpu_percent, limits.max_cpu_percent);
    assert_eq!(deserialized_limits.max_disk_mb, limits.max_disk_mb);
}

/// Test deterministic manager serialization
#[test]
fn test_deterministic_manager_serialization() {
    let manager = DeterministicManager::new();
    manager.set_seed(12345);
    
    // Test JSON serialization
    let json = serde_json::to_string(&manager).unwrap();
    assert!(json.contains("seed"));
    assert!(json.contains("12345"));
    
    // Test JSON deserialization
    let deserialized_manager: DeterministicManager = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_manager.get_current_seed(), 12345);
}

/// Test coverage tracker serialization
#[test]
fn test_coverage_tracker_serialization() {
    let tracker = CoverageTracker::new();
    tracker.start_test("test1");
    tracker.record_line_execution("test1", 10);
    tracker.end_test("test1", true);
    
    // Test JSON serialization
    let json = serde_json::to_string(&tracker).unwrap();
    assert!(json.contains("total_tests"));
    assert!(json.contains("covered_lines"));
    
    // Test JSON deserialization
    let deserialized_tracker: CoverageTracker = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_tracker.get_report().total_tests, 1);
    assert_eq!(deserialized_tracker.get_report().covered_lines, 1);
}

/// Test snapshot manager serialization
#[test]
fn test_snapshot_manager_serialization() {
    let manager = SnapshotManager::new();
    let test_data = serde_json::json!({"key": "value"});
    let snapshot_id = manager.create_snapshot("test_snapshot", &test_data);
    
    // Test JSON serialization
    let json = serde_json::to_string(&manager).unwrap();
    assert!(json.contains("snapshots"));
    
    // Test JSON deserialization
    let deserialized_manager: SnapshotManager = serde_json::from_str(&json).unwrap();
    assert!(deserialized_manager.get_snapshot("test_snapshot").is_some());
}

/// Test tracing manager serialization
#[test]
fn test_tracing_manager_serialization() {
    let manager = TracingManager::new();
    let trace_id = manager.start_trace("test_trace");
    manager.log_trace_event(&trace_id, "test_event", "test_data");
    manager.end_trace(&trace_id);
    
    // Test JSON serialization
    let json = serde_json::to_string(&manager).unwrap();
    assert!(json.contains("traces"));
    
    // Test JSON deserialization
    let deserialized_manager: TracingManager = serde_json::from_str(&json).unwrap();
    assert!(!deserialized_manager.get_traces().is_empty());
}

/// Test test report serialization
#[test]
fn test_test_report_serialization() {
    let report = TestReport::new();
    report.record_test_execution("test1", true, Duration::from_millis(100));
    
    // Test JSON serialization
    let json = report.to_json().unwrap();
    assert!(json.contains("session_id"));
    assert!(json.contains("test_summary"));
    
    // Test TOML serialization
    let toml = report.to_toml().unwrap();
    assert!(toml.contains("session_id"));
    assert!(toml.contains("test_summary"));
}
