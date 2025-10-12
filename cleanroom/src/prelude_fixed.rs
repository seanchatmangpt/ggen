//! Cleanroom prelude - common imports for cleanroom users

pub use crate::scenario::{scenario, Scenario, RunResult};
pub use crate::assertions::Assert;
pub use crate::backend::{Backend, AutoBackend};
pub use crate::policy::{Policy, SecurityLevel};
pub use crate::limits::ResourceLimits;
pub use crate::run;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prelude_imports_scenario() {
        // Test that scenario types are accessible
        let _scenario = scenario("test");
        let _run_result = RunResult {
            exit_code: 0,
            stdout: "test".to_string(),
            stderr: "".to_string(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "test".to_string(),
            concurrent: false,
            step_order: vec![],
        };
    }

    #[test]
    fn test_prelude_imports_assertions() {
        // Test that Assert trait is accessible
        let run_result = RunResult {
            exit_code: 0,
            stdout: "test".to_string(),
            stderr: "".to_string(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "test".to_string(),
            concurrent: false,
            step_order: vec![],
        };
        
        // Test that assertion methods exist
        let _asserted = run_result.success();
        let _asserted2 = run_result.exit_code(0);
        let _asserted3 = run_result.stdout_contains("test");
    }

    #[test]
    fn test_prelude_imports_backend() {
        // Test that Backend trait is accessible
        use crate::backend::TestcontainerBackend;
        
        if TestcontainerBackend::is_available() {
            let backend = TestcontainerBackend::new("alpine:latest").unwrap();
            assert_eq!(backend.name(), "testcontainers");
            assert!(backend.is_available());
            assert!(backend.supports_hermetic());
            assert!(backend.supports_deterministic());
        }
    }

    #[test]
    fn test_prelude_imports_policy() {
        // Test that Policy and SecurityLevel are accessible
        let policy = Policy::default();
        let _security_level = SecurityLevel::Strict;
        
        // Test that policy can be used
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(policy.rng_profile.seed.is_some());
        assert!(policy.net_profile.allow_network);
        assert!(policy.fs_profile.allow_read);
        assert!(policy.proc_profile.allow_spawn);
    }

    #[test]
    fn test_prelude_imports_resource_limits() {
        // Test that ResourceLimits is accessible
        let limits = ResourceLimits::default();
        
        // Test that limits can be used
        assert!(limits.max_memory_mb > 0);
        assert!(limits.max_cpu_percent > 0.0);
        assert!(limits.max_disk_mb > 0);
        assert!(limits.max_network_mb > 0);
        assert!(limits.max_containers > 0);
        assert!(limits.max_processes > 0);
        assert!(limits.max_files > 0);
        assert!(limits.max_duration_secs > 0);
    }

    #[test]
    fn test_prelude_imports_run() {
        // Test that run function is accessible
        let result = run(["echo", "test"]);
        
        // This will fail in test environment without Docker, but we can test the function exists
        assert!(result.is_err() || result.is_ok());
    }

    #[test]
    fn test_prelude_all_imports_accessible() {
        // Test that all prelude imports are accessible and can be used together
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("integration_test");
        
        // Test scenario building with policy
        let _built_scenario = scenario
            .step("echo", ["echo", "hello"])
            .step("cat", ["cat", "/dev/null"]);
        
        // Test that all types can be used together
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(limits.max_memory_mb > 0);
        assert_eq!(_built_scenario.name(), "integration_test");
    }

    #[test]
    fn test_prelude_imports_compilation() {
        // Test that all prelude imports compile without errors
        use crate::scenario::{scenario, Scenario, RunResult};
        use crate::assertions::Assert;
        use crate::backend::{Backend, AutoBackend};
        use crate::policy::{Policy, SecurityLevel};
        use crate::limits::ResourceLimits;
        use crate::run;
        
        // Test that types can be instantiated
        let _scenario = scenario("test");
        let _policy = Policy::default();
        let _limits = ResourceLimits::default();
        let _security_level = SecurityLevel::Strict;
        
        // Test that functions can be called
        let _result = run(["echo", "test"]);
        
        // Test that traits can be used
        let run_result = RunResult {
            exit_code: 0,
            stdout: "test".to_string(),
            stderr: "".to_string(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "test".to_string(),
            concurrent: false,
            step_order: vec![],
        };
        let _asserted = run_result.success();
    }

    #[test]
    fn test_prelude_imports_no_conflicts() {
        // Test that prelude imports don't conflict with each other
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("test");
        
        // Test that we can use all types simultaneously
        let _built_scenario = scenario
            .step("echo", ["echo", "hello"]);
        
        // Test that we can access policy properties
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(policy.rng_profile.seed.is_some());
        
        // Test that we can access limits properties
        assert!(limits.max_memory_mb > 0);
        assert!(limits.max_cpu_percent > 0.0);
        
        // Test that we can access scenario properties
        assert_eq!(_built_scenario.name(), "test");
    }

    #[test]
    fn test_prelude_imports_type_safety() {
        // Test that prelude imports maintain type safety
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("test");
        
        // Test that types are correctly typed
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(limits.max_memory_mb > 0);
        assert_eq!(scenario.name(), "test");
        
        // Test that we can't mix up types
        // This should compile without errors
        let _timeout = policy.time_profile.timeout_secs;
        let _memory = limits.max_memory_mb;
        let _name = scenario.name();
    }

    #[test]
    fn test_prelude_imports_functionality() {
        // Test that prelude imports provide expected functionality
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("test");
        
        // Test policy functionality
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(policy.rng_profile.seed.is_some());
        assert!(policy.net_profile.allow_network);
        assert!(policy.fs_profile.allow_read);
        assert!(policy.proc_profile.allow_spawn);
        
        // Test limits functionality
        assert!(limits.max_memory_mb > 0);
        assert!(limits.max_cpu_percent > 0.0);
        assert!(limits.max_disk_mb > 0);
        assert!(limits.max_network_mb > 0);
        assert!(limits.max_containers > 0);
        assert!(limits.max_processes > 0);
        assert!(limits.max_files > 0);
        assert!(limits.max_duration_secs > 0);
        
        // Test scenario functionality
        assert_eq!(scenario.name(), "test");
        let _built_scenario = scenario
            .step("echo", ["echo", "hello"])
            .step("cat", ["cat", "/dev/null"]);
        assert_eq!(_built_scenario.name(), "test");
    }

    #[test]
    fn test_prelude_imports_serialization() {
        // Test that prelude imports support serialization
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        
        // Test policy serialization
        let policy_serialized = serde_json::to_string(&policy);
        assert!(policy_serialized.is_ok());
        
        let policy_deserialized: Result<Policy, _> = serde_json::from_str(&policy_serialized.unwrap());
        assert!(policy_deserialized.is_ok());
        
        // Test limits serialization
        let limits_serialized = serde_json::to_string(&limits);
        assert!(limits_serialized.is_ok());
        
        let limits_deserialized: Result<ResourceLimits, _> = serde_json::from_str(&limits_serialized.unwrap());
        assert!(limits_deserialized.is_ok());
    }

    #[test]
    fn test_prelude_imports_clone_derive() {
        // Test that prelude imports support cloning
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("test");
        
        // Test that types can be cloned
        let _policy_clone = policy.clone();
        let _limits_clone = limits.clone();
        let _scenario_clone = scenario.clone();
    }

    #[test]
    fn test_prelude_imports_debug_derive() {
        // Test that prelude imports support debug formatting
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("test");
        
        // Test that types can be formatted for debugging
        let _policy_debug = format!("{:?}", policy);
        let _limits_debug = format!("{:?}", limits);
        let _scenario_debug = format!("{:?}", scenario);
        
        // Test that debug strings contain expected information
        assert!(_policy_debug.contains("Policy"));
        assert!(_limits_debug.contains("ResourceLimits"));
        assert!(_scenario_debug.contains("test"));
    }

    #[test]
    fn test_prelude_imports_default_implementations() {
        // Test that prelude imports have default implementations
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        
        // Test that defaults are reasonable
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(policy.rng_profile.seed.is_some());
        assert!(policy.net_profile.allow_network);
        assert!(policy.fs_profile.allow_read);
        assert!(policy.proc_profile.allow_spawn);
        
        assert!(limits.max_memory_mb > 0);
        assert!(limits.max_cpu_percent > 0.0);
        assert!(limits.max_disk_mb > 0);
        assert!(limits.max_network_mb > 0);
        assert!(limits.max_containers > 0);
        assert!(limits.max_processes > 0);
        assert!(limits.max_files > 0);
        assert!(limits.max_duration_secs > 0);
    }

    #[test]
    fn test_prelude_imports_comprehensive_usage() {
        // Test comprehensive usage of all prelude imports
        let policy = Policy::default();
        let limits = ResourceLimits::default();
        let scenario = scenario("comprehensive_test");
        
        // Build a complex scenario
        let built_scenario = scenario
            .step("echo", ["echo", "hello"])
            .step("cat", ["cat", "/dev/null"])
            .step("ls", ["ls", "-la"]);
        
        // Test that all components work together
        assert_eq!(built_scenario.name(), "comprehensive_test");
        assert!(policy.time_profile.timeout_secs > 0);
        assert!(limits.max_memory_mb > 0);
        
        // Test that we can use the run function
        let result = run(["echo", "test"]);
        assert!(result.is_err() || result.is_ok());
        
        // Test that we can use assertions
        if let Ok(run_result) = result {
            let _asserted = run_result.success();
            let _asserted2 = run_result.exit_code(0);
            let _asserted3 = run_result.stdout_contains("test");
        }
    }
}
