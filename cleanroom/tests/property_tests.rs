//! Property tests for cleanroom testing framework
//!
//! These tests verify deterministic behavior and property-based testing
//! using proptest for comprehensive coverage.

use proptest::prelude::*;
use cleanroom::{
    CleanroomConfig, Policy, SecurityLevel, NetworkPolicy,
    ResourceLimits, DeterministicManager, CoverageTracker,
    SnapshotManager, TracingManager, TestReport,
    PostgresContainer, RedisContainer, GenericContainer,
    CleanroomError, ErrorKind,
};
use std::time::Duration;
use uuid::Uuid;

/// Property test for CleanroomConfig validation
proptest! {
    #[test]
    fn test_config_validation_property(
        enable_singleton in any::<bool>(),
        startup_timeout_secs in 1..300u64,
        execution_timeout_secs in 1..600u64,
        max_containers in 1..100usize,
        enable_deterministic in any::<bool>(),
        enable_coverage in any::<bool>(),
        enable_snapshots in any::<bool>(),
        enable_tracing in any::<bool>(),
    ) {
        let config = CleanroomConfig {
            enable_singleton_containers: enable_singleton,
            container_startup_timeout: Duration::from_secs(startup_timeout_secs),
            test_execution_timeout: Duration::from_secs(execution_timeout_secs),
            max_concurrent_containers: max_containers,
            enable_deterministic_execution: enable_deterministic,
            enable_coverage_tracking: enable_coverage,
            enable_snapshot_testing: enable_snapshots,
            enable_tracing: enable_tracing,
            ..CleanroomConfig::default()
        };
        
        // Config should always validate with reasonable values
        prop_assert!(config.validate().is_ok());
        
        // Timeouts should be positive
        prop_assert!(config.container_startup_timeout > Duration::from_secs(0));
        prop_assert!(config.test_execution_timeout > Duration::from_secs(0));
        
        // Max containers should be positive
        prop_assert!(config.max_concurrent_containers > 0);
    }
}

/// Property test for Policy security levels
proptest! {
    #[test]
    fn test_policy_security_levels_property(
        security_level in prop::sample::select(&[
            SecurityLevel::Permissive,
            SecurityLevel::Standard,
            SecurityLevel::Strict,
            SecurityLevel::Locked,
        ]),
        enable_network_isolation in any::<bool>(),
        enable_port_scanning in any::<bool>(),
        enable_file_system_isolation in any::<bool>(),
    ) {
        let policy = Policy {
            security_level: security_level.clone(),
            network: NetworkPolicy {
                enable_network_isolation,
                enable_port_scanning,
                enable_file_system_isolation,
            },
            ..Policy::default()
        };
        
        // Policy should always be valid
        prop_assert!(policy.validate().is_ok());
        
        // Security level should match
        prop_assert_eq!(policy.security_level, security_level);
        
        // Network isolation should be consistent
        prop_assert_eq!(policy.network.enable_network_isolation, enable_network_isolation);
        
        // Summary should contain security level
        let summary = policy.summary();
        prop_assert!(summary.contains("Security Level"));
    }
}

/// Property test for ResourceLimits validation
proptest! {
    #[test]
    fn test_resource_limits_property(
        max_memory_mb in 1..4096u32,
        max_cpu_percent in 1.0..100.0f64,
        max_disk_mb in 1..8192u32,
        max_network_mb in 1..1024u32,
    ) {
        let limits = ResourceLimits {
            max_memory_mb: max_memory_mb as usize,
            max_cpu_percent,
            max_disk_mb: max_disk_mb as usize,
            max_network_mb: max_network_mb as usize,
            ..ResourceLimits::default()
        };
        
        // Limits should always validate with positive values
        prop_assert!(limits.validate().is_ok());
        
        // All values should be positive
        prop_assert!(limits.max_memory_mb > 0);
        prop_assert!(limits.max_cpu_percent > 0.0);
        prop_assert!(limits.max_disk_mb > 0);
        prop_assert!(limits.max_network_mb > 0);
        
        // CPU percentage should not exceed 100%
        prop_assert!(limits.max_cpu_percent <= 100.0);
    }
}

/// Property test for DeterministicManager behavior
proptest! {
    #[test]
    fn test_deterministic_manager_property(
        seed in any::<u64>(),
        key1 in "[a-zA-Z0-9_]{1,50}",
        key2 in "[a-zA-Z0-9_]{1,50}",
    ) {
        let manager = DeterministicManager::new();
        manager.set_seed(seed);
        
        // Same seed should produce same results
        let value1 = manager.generate_deterministic_value(&key1);
        let value2 = manager.generate_deterministic_value(&key1);
        prop_assert_eq!(value1, value2);
        
        // Different keys should produce different values
        let value3 = manager.generate_deterministic_value(&key2);
        prop_assert_ne!(value1, value3);
        
        // Seed should be preserved
        prop_assert_eq!(manager.get_current_seed(), seed);
    }
}

/// Property test for CoverageTracker behavior
proptest! {
    #[test]
    fn test_coverage_tracker_property(
        test_names in prop::collection::vec("[a-zA-Z0-9_]{1,20}", 1..10),
        line_numbers in prop::collection::vec(1..1000usize, 1..20),
    ) {
        let tracker = CoverageTracker::new();
        
        for (i, test_name) in test_names.iter().enumerate() {
            tracker.start_test(test_name);
            
            // Record some line executions
            for line_num in &line_numbers[..std::cmp::min(line_numbers.len(), 5)] {
                tracker.record_line_execution(test_name, *line_num);
            }
            
            // End test (alternate between success and failure)
            let success = i % 2 == 0;
            tracker.end_test(test_name, success);
        }
        
        let report = tracker.get_report();
        
        // Total tests should match input
        prop_assert_eq!(report.total_tests, test_names.len());
        
        // Passed tests should be half (rounded up)
        prop_assert_eq!(report.passed_tests, (test_names.len() + 1) / 2);
        
        // Failed tests should be half (rounded down)
        prop_assert_eq!(report.failed_tests, test_names.len() / 2);
        
        // Coverage should be calculated correctly
        prop_assert!(report.coverage_percentage >= 0.0);
        prop_assert!(report.coverage_percentage <= 100.0);
    }
}

/// Property test for SnapshotManager behavior
proptest! {
    #[test]
    fn test_snapshot_manager_property(
        snapshot_names in prop::collection::vec("[a-zA-Z0-9_]{1,20}", 1..10),
        data_keys in prop::collection::vec("[a-zA-Z0-9_]{1,10}", 1..5),
        data_values in prop::collection::vec("[a-zA-Z0-9_]{1,20}", 1..5),
    ) {
        let manager = SnapshotManager::new();
        
        for snapshot_name in &snapshot_names {
            // Create test data
            let mut test_data = serde_json::Map::new();
            for (key, value) in data_keys.iter().zip(data_values.iter()) {
                test_data.insert(key.clone(), serde_json::Value::String(value.clone()));
            }
            let test_data = serde_json::Value::Object(test_data);
            
            // Create snapshot
            let snapshot_id = manager.create_snapshot(snapshot_name, &test_data);
            prop_assert!(!snapshot_id.is_nil());
            
            // Verify snapshot
            let is_valid = manager.verify_snapshot(snapshot_name, &test_data);
            prop_assert!(is_valid);
            
            // Retrieve snapshot
            let retrieved_snapshot = manager.get_snapshot(snapshot_name);
            prop_assert!(retrieved_snapshot.is_some());
        }
        
        // All snapshots should be retrievable
        for snapshot_name in &snapshot_names {
            let snapshot = manager.get_snapshot(snapshot_name);
            prop_assert!(snapshot.is_some());
        }
    }
}

/// Property test for TracingManager behavior
proptest! {
    #[test]
    fn test_tracing_manager_property(
        trace_names in prop::collection::vec("[a-zA-Z0-9_]{1,20}", 1..10),
        event_names in prop::collection::vec("[a-zA-Z0-9_]{1,20}", 1..5),
        event_data in prop::collection::vec("[a-zA-Z0-9_]{1,50}", 1..5),
    ) {
        let manager = TracingManager::new();
        
        for trace_name in &trace_names {
            // Start trace
            let trace_id = manager.start_trace(trace_name);
            prop_assert!(!trace_id.is_nil());
            
            // Log events
            for (event_name, event_data) in event_names.iter().zip(event_data.iter()) {
                manager.log_trace_event(&trace_id, event_name, event_data);
            }
            
            // End trace
            manager.end_trace(&trace_id);
        }
        
        // All traces should be retrievable
        let traces = manager.get_traces();
        prop_assert_eq!(traces.len(), trace_names.len());
        
        // Traces should be filterable by name
        for trace_name in &trace_names {
            let filtered_traces = manager.get_traces_by_name(trace_name);
            prop_assert!(!filtered_traces.is_empty());
        }
    }
}

/// Property test for TestReport behavior
proptest! {
    #[test]
    fn test_test_report_property(
        test_names in prop::collection::vec("[a-zA-Z0-9_]{1,20}", 1..20),
        execution_times in prop::collection::vec(1..1000u64, 1..20),
    ) {
        let report = TestReport::new();
        
        for (i, test_name) in test_names.iter().enumerate() {
            let success = i % 2 == 0;
            let execution_time = Duration::from_millis(execution_times[i % execution_times.len()]);
            
            report.record_test_execution(test_name, success, execution_time);
        }
        
        // Total tests should match input
        prop_assert_eq!(report.test_summary.total_tests, test_names.len());
        
        // Passed tests should be half (rounded up)
        prop_assert_eq!(report.test_summary.passed_tests, (test_names.len() + 1) / 2);
        
        // Failed tests should be half (rounded down)
        prop_assert_eq!(report.test_summary.failed_tests, test_names.len() / 2);
        
        // Average execution time should be calculated
        prop_assert!(report.test_summary.average_execution_time > Duration::from_millis(0));
        
        // Report should be serializable
        let json_report = report.to_json();
        prop_assert!(json_report.is_ok());
        
        let toml_report = report.to_toml();
        prop_assert!(toml_report.is_ok());
    }
}

/// Property test for container configuration
proptest! {
    #[test]
    fn test_container_configuration_property(
        image in "[a-zA-Z0-9_/:.-]{1,50}",
        port in 1..65535u16,
        env_key in "[a-zA-Z0-9_]{1,20}",
        env_value in "[a-zA-Z0-9_]{1,50}",
    ) {
        // Test PostgresContainer
        let postgres_container = PostgresContainer::new(&image)
            .with_port(port)
            .with_env(&env_key, &env_value);
        
        prop_assert_eq!(postgres_container.image(), image);
        prop_assert_eq!(postgres_container.port(), Some(port));
        prop_assert_eq!(postgres_container.container_type(), "postgres");
        
        // Test RedisContainer
        let redis_container = RedisContainer::new(&image)
            .with_port(port)
            .with_env(&env_key, &env_value);
        
        prop_assert_eq!(redis_container.image(), image);
        prop_assert_eq!(redis_container.port(), Some(port));
        prop_assert_eq!(redis_container.container_type(), "redis");
        
        // Test GenericContainer
        let generic_container = GenericContainer::new(&image)
            .with_port(port)
            .with_env(&env_key, &env_value);
        
        prop_assert_eq!(generic_container.image(), image);
        prop_assert_eq!(generic_container.port(), Some(port));
        prop_assert_eq!(generic_container.container_type(), "generic");
    }
}

/// Property test for error handling
proptest! {
    #[test]
    fn test_error_handling_property(
        error_message in "[a-zA-Z0-9_ ]{1,100}",
        error_kind in prop::sample::select(&[
            ErrorKind::ValidationError,
            ErrorKind::IoError,
            ErrorKind::ContainerError,
            ErrorKind::PolicyError,
            ErrorKind::ResourceError,
            ErrorKind::DeterministicError,
            ErrorKind::CoverageError,
            ErrorKind::SnapshotError,
            ErrorKind::TracingError,
            ErrorKind::ReportError,
        ]),
    ) {
        let error = CleanroomError::new(error_kind.clone(), &error_message);
        
        // Error should preserve kind and message
        prop_assert_eq!(error.kind(), error_kind);
        prop_assert_eq!(error.message(), error_message);
        
        // Error should be displayable
        let error_string = format!("{}", error);
        prop_assert!(error_string.contains(&error_message));
        
        // Error should be convertible to string
        let error_string = error.to_string();
        prop_assert!(error_string.contains(&error_message));
    }
}

/// Property test for UUID generation
proptest! {
    #[test]
    fn test_uuid_generation_property(
        count in 1..100usize,
    ) {
        let mut uuids = Vec::new();
        
        for _ in 0..count {
            let uuid = Uuid::new_v4();
            uuids.push(uuid);
        }
        
        // All UUIDs should be unique
        for i in 0..uuids.len() {
            for j in (i + 1)..uuids.len() {
                prop_assert_ne!(uuids[i], uuids[j]);
            }
        }
        
        // All UUIDs should be valid
        for uuid in &uuids {
            prop_assert!(!uuid.is_nil());
        }
    }
}

/// Property test for duration operations
proptest! {
    #[test]
    fn test_duration_operations_property(
        duration1_ms in 1..10000u64,
        duration2_ms in 1..10000u64,
    ) {
        let duration1 = Duration::from_millis(duration1_ms);
        let duration2 = Duration::from_millis(duration2_ms);
        
        // Duration addition should be associative
        let sum1 = duration1 + duration2;
        let sum2 = duration2 + duration1;
        prop_assert_eq!(sum1, sum2);
        
        // Duration should be positive
        prop_assert!(duration1 > Duration::from_millis(0));
        prop_assert!(duration2 > Duration::from_millis(0));
        
        // Duration should be comparable
        if duration1_ms > duration2_ms {
            prop_assert!(duration1 > duration2);
        } else if duration1_ms < duration2_ms {
            prop_assert!(duration1 < duration2);
        } else {
            prop_assert_eq!(duration1, duration2);
        }
    }
}

/// Property test for JSON serialization round-trip
proptest! {
    #[test]
    fn test_json_serialization_round_trip_property(
        config in any::<CleanroomConfig>(),
    ) {
        // Serialize to JSON
        let json = serde_json::to_string(&config);
        prop_assert!(json.is_ok());
        
        // Deserialize from JSON
        let deserialized_config: Result<CleanroomConfig, _> = serde_json::from_str(&json.unwrap());
        prop_assert!(deserialized_config.is_ok());
        
        let deserialized_config = deserialized_config.unwrap();
        
        // Round-trip should preserve values
        prop_assert_eq!(deserialized_config.enable_singleton_containers, config.enable_singleton_containers);
        prop_assert_eq!(deserialized_config.container_startup_timeout, config.container_startup_timeout);
        prop_assert_eq!(deserialized_config.test_execution_timeout, config.test_execution_timeout);
        prop_assert_eq!(deserialized_config.max_concurrent_containers, config.max_concurrent_containers);
        prop_assert_eq!(deserialized_config.enable_deterministic_execution, config.enable_deterministic_execution);
        prop_assert_eq!(deserialized_config.enable_coverage_tracking, config.enable_coverage_tracking);
        prop_assert_eq!(deserialized_config.enable_snapshot_testing, config.enable_snapshot_testing);
        prop_assert_eq!(deserialized_config.enable_tracing, config.enable_tracing);
    }
}

/// Property test for TOML serialization round-trip
proptest! {
    #[test]
    fn test_toml_serialization_round_trip_property(
        config in any::<CleanroomConfig>(),
    ) {
        // Serialize to TOML
        let toml = toml::to_string(&config);
        prop_assert!(toml.is_ok());
        
        // Deserialize from TOML
        let deserialized_config: Result<CleanroomConfig, _> = toml::from_str(&toml.unwrap());
        prop_assert!(deserialized_config.is_ok());
        
        let deserialized_config = deserialized_config.unwrap();
        
        // Round-trip should preserve values
        prop_assert_eq!(deserialized_config.enable_singleton_containers, config.enable_singleton_containers);
        prop_assert_eq!(deserialized_config.container_startup_timeout, config.container_startup_timeout);
        prop_assert_eq!(deserialized_config.test_execution_timeout, config.test_execution_timeout);
        prop_assert_eq!(deserialized_config.max_concurrent_containers, config.max_concurrent_containers);
        prop_assert_eq!(deserialized_config.enable_deterministic_execution, config.enable_deterministic_execution);
        prop_assert_eq!(deserialized_config.enable_coverage_tracking, config.enable_coverage_tracking);
        prop_assert_eq!(deserialized_config.enable_snapshot_testing, config.enable_snapshot_testing);
        prop_assert_eq!(deserialized_config.enable_tracing, config.enable_tracing);
    }
}
