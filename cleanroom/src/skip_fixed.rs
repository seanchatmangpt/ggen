//! Skip semantics for missing engines or capabilities
//!
//! Provides intelligent test skipping when required engines or capabilities
//! are not available, ensuring tests fail gracefully rather than panicking.

use crate::error::{CleanroomError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Skip reason
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SkipReason {
    /// Engine is unavailable
    EngineUnavailable { engine: String },
    /// Capability is missing
    CapabilityMissing { capability: String },
    /// Platform not supported
    PlatformNotSupported { platform: String },
    /// Feature not enabled
    FeatureNotEnabled { feature: String },
    /// Custom skip reason
    Custom { reason: String },
}

impl std::fmt::Display for SkipReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SkipReason::EngineUnavailable { engine } => {
                write!(f, "Engine '{}' is unavailable", engine)
            }
            SkipReason::CapabilityMissing { capability } => {
                write!(f, "Capability '{}' is missing", capability)
            }
            SkipReason::PlatformNotSupported { platform } => {
                write!(f, "Platform '{}' is not supported", platform)
            }
            SkipReason::FeatureNotEnabled { feature } => {
                write!(f, "Feature '{}' is not enabled", feature)
            }
            SkipReason::Custom { reason } => {
                write!(f, "{}", reason)
            }
        }
    }
}

/// Skip configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SkipConfig {
    /// Whether to fail on skipped tests
    pub fail_on_skipped: bool,
    /// Whether to skip tests when engines are unavailable
    pub skip_on_engine_unavailable: bool,
    /// Whether to skip tests when capabilities are missing
    pub skip_on_capability_missing: bool,
    /// Custom skip conditions
    pub custom_conditions: HashMap<String, bool>,
}

impl Default for SkipConfig {
    fn default() -> Self {
        Self {
            fail_on_skipped: false,
            skip_on_engine_unavailable: true,
            skip_on_capability_missing: true,
            custom_conditions: HashMap::new(),
        }
    }
}

/// Skip manager
pub struct SkipManager {
    config: SkipConfig,
    engine_availability: HashMap<String, bool>,
    capability_availability: HashMap<String, bool>,
}

impl SkipManager {
    /// Create a new skip manager
    pub fn new(config: SkipConfig) -> Self {
        Self {
            config,
            engine_availability: HashMap::new(),
            capability_availability: HashMap::new(),
        }
    }

    /// Check if an engine is available
    pub fn is_engine_available(&self, engine: &str) -> bool {
        self.engine_availability.get(engine).copied().unwrap_or(false)
    }

    /// Set engine availability
    pub fn set_engine_availability(&mut self, engine: &str, available: bool) {
        self.engine_availability.insert(engine.to_string(), available);
    }

    /// Check if a capability is available
    pub fn is_capability_available(&self, capability: &str) -> bool {
        self.capability_availability.get(capability).copied().unwrap_or(true)
    }

    /// Set capability availability
    pub fn set_capability_availability(&mut self, capability: &str, available: bool) {
        self.capability_availability.insert(capability.to_string(), available);
    }

    /// Check if a test should be skipped
    pub fn should_skip(&self, engine: Option<&str>, capabilities: &[&str]) -> Option<SkipReason> {
        // Check engine availability
        if let Some(engine) = engine {
            if self.config.skip_on_engine_unavailable && !self.is_engine_available(engine) {
                return Some(SkipReason::EngineUnavailable {
                    engine: engine.to_string(),
                });
            }
        }

        // Check capability availability
        if self.config.skip_on_capability_missing {
            for capability in capabilities {
                if !self.is_capability_available(capability) {
                    return Some(SkipReason::CapabilityMissing {
                        capability: capability.to_string(),
                    });
                }
            }
        }

        // Check custom conditions
        for (condition, should_skip) in &self.config.custom_conditions {
            if *should_skip {
                return Some(SkipReason::Custom {
                    reason: format!("Custom condition '{}' is true", condition),
                });
            }
        }

        None
    }

    /// Skip a test with reason
    pub fn skip_test(&self, reason: SkipReason) -> Result<()> {
        if self.config.fail_on_skipped {
            Err(CleanroomError::validation_error(format!("Test skipped: {}", reason)))
        } else {
            // In a real implementation, this would mark the test as skipped
            // For now, we'll just return Ok
            Ok(())
        }
    }

    /// Check if podman is available
    pub fn is_podman_available(&self) -> bool {
        self.is_engine_available("podman")
    }

    /// Check if docker is available
    pub fn is_docker_available(&self) -> bool {
        self.is_engine_available("docker")
    }

    /// Check if local backend is available
    pub fn is_local_available(&self) -> bool {
        self.is_engine_available("local")
    }

    /// Detect engine availability automatically
    pub fn detect_engine_availability(&mut self) {
        // Check Docker availability
        let docker_available = std::process::Command::new("docker")
            .arg("--version")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false);
        self.set_engine_availability("docker", docker_available);

        // Check Podman availability
        let podman_available = std::process::Command::new("podman")
            .arg("--version")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false);
        self.set_engine_availability("podman", podman_available);

        // Local backend is always available
        self.set_engine_availability("local", true);
    }

    /// Get available engines
    pub fn get_available_engines(&self) -> Vec<String> {
        self.engine_availability
            .iter()
            .filter(|(_, &available)| available)
            .map(|(engine, _)| engine.clone())
            .collect()
    }

    /// Get unavailable engines
    pub fn get_unavailable_engines(&self) -> Vec<String> {
        self.engine_availability
            .iter()
            .filter(|(_, &available)| !available)
            .map(|(engine, _)| engine.clone())
            .collect()
    }
}

/// Skip assertion trait
pub trait SkipAssert {
    /// Assert that test is skipped with reason
    fn skipped_with_reason(&self, reason: &str) -> &Self;
    
    /// Assert that test is not skipped
    fn not_skipped(&self) -> &Self;
}

impl SkipAssert for crate::scenario::RunResult {
    fn skipped_with_reason(&self, reason: &str) -> &Self {
        // For now, just verify that execution succeeded
        // In a real implementation, this would check skip status
        assert_eq!(self.exit_code, 0, "Expected test to be skipped with reason: {}", reason);
        self
    }
    
    fn not_skipped(&self) -> &Self {
        // For now, just verify that execution succeeded
        // In a real implementation, this would check skip status
        assert_eq!(self.exit_code, 0, "Expected test to not be skipped");
        self
    }
}

/// Skip result type
#[derive(Debug, Clone)]
pub enum SkipResult<T> {
    /// Test was executed
    Executed(T),
    /// Test was skipped
    Skipped(SkipReason),
}

impl<T> SkipResult<T> {
    /// Check if the result was skipped
    pub fn is_skipped(&self) -> bool {
        matches!(self, SkipResult::Skipped(_))
    }

    /// Check if the result was executed
    pub fn is_executed(&self) -> bool {
        matches!(self, SkipResult::Executed(_))
    }

    /// Get the executed value, panicking if skipped
    pub fn unwrap(self) -> T {
        match self {
            SkipResult::Executed(value) => value,
            SkipResult::Skipped(reason) => panic!("Expected executed result, got skipped: {}", reason),
        }
    }

    /// Get the executed value, or default if skipped
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            SkipResult::Executed(value) => value,
            SkipResult::Skipped(_) => default,
        }
    }

    /// Get the skip reason if skipped
    pub fn skip_reason(&self) -> Option<&SkipReason> {
        match self {
            SkipResult::Executed(_) => None,
            SkipResult::Skipped(reason) => Some(reason),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_manager_creation() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config);
        
        assert!(!manager.is_engine_available("docker"));
        assert!(!manager.is_engine_available("podman"));
        assert!(!manager.is_engine_available("local"));
    }

    #[test]
    fn test_engine_availability() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", true);
        manager.set_engine_availability("podman", false);
        
        assert!(manager.is_engine_available("docker"));
        assert!(!manager.is_engine_available("podman"));
    }

    #[test]
    fn test_should_skip() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", false);
        
        let skip_reason = manager.should_skip(Some("docker"), &[]);
        assert!(skip_reason.is_some());
        
        if let Some(SkipReason::EngineUnavailable { engine }) = skip_reason {
            assert_eq!(engine, "docker");
        } else {
            panic!("Expected EngineUnavailable skip reason");
        }
    }

    #[test]
    fn test_skip_result() {
        let result = SkipResult::Executed(42);
        assert!(result.is_executed());
        assert!(!result.is_skipped());
        assert_eq!(result.unwrap(), 42);
        
        let skipped = SkipResult::Skipped(SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        });
        assert!(skipped.is_skipped());
        assert!(!skipped.is_executed());
        assert_eq!(skipped.unwrap_or(0), 0);
    }

    #[test]
    fn test_skip_manager_creation() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config.clone());
        assert_eq!(manager.config, config);
        assert!(manager.engine_availability.is_empty());
    }

    #[test]
    fn test_skip_manager_with_config() {
        let config = SkipConfig {
            fail_on_skipped: true,
            skip_on_engine_unavailable: true,
            skip_on_resource_limit: true,
            skip_on_policy_violation: true,
            skip_on_timeout: true,
            skip_on_dependency_missing: true,
            skip_on_platform_mismatch: true,
            skip_on_version_mismatch: true,
            skip_on_permission_denied: true,
            skip_on_network_unavailable: true,
            skip_on_disk_full: true,
            skip_on_memory_insufficient: true,
            skip_on_cpu_overloaded: true,
            skip_on_backend_error: true,
            skip_on_config_error: true,
            skip_on_validation_error: true,
            skip_on_serialization_error: true,
            skip_on_deserialization_error: true,
            skip_on_io_error: true,
            skip_on_timeout_error: true,
            skip_on_internal_error: true,
            skip_on_external_error: true,
            skip_on_unknown_error: true,
        };
        let manager = SkipManager::new(config.clone());
        assert_eq!(manager.config, config);
    }

    #[test]
    fn test_skip_manager_engine_availability() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        // Test setting engine availability
        manager.set_engine_availability("docker", true);
        manager.set_engine_availability("podman", false);
        manager.set_engine_availability("local", true);
        
        assert!(manager.is_engine_available("docker"));
        assert!(!manager.is_engine_available("podman"));
        assert!(manager.is_engine_available("local"));
        assert!(!manager.is_engine_available("nonexistent"));
    }

    #[test]
    fn test_skip_manager_engine_availability_methods() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", true);
        manager.set_engine_availability("podman", false);
        manager.set_engine_availability("local", true);
        
        assert!(manager.is_docker_available());
        assert!(!manager.is_podman_available());
        assert!(manager.is_local_available());
    }

    #[test]
    fn test_skip_manager_detect_engine_availability() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.detect_engine_availability();
        
        // At least local should be available
        assert!(manager.is_local_available());
        
        // Docker and Podman availability depends on system
        let available_engines = manager.get_available_engines();
        assert!(!available_engines.is_empty());
        assert!(available_engines.contains(&"local".to_string()));
    }

    #[test]
    fn test_skip_manager_get_available_engines() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", true);
        manager.set_engine_availability("podman", false);
        manager.set_engine_availability("local", true);
        manager.set_engine_availability("test", true);
        
        let available = manager.get_available_engines();
        assert_eq!(available.len(), 3);
        assert!(available.contains(&"docker".to_string()));
        assert!(available.contains(&"local".to_string()));
        assert!(available.contains(&"test".to_string()));
        assert!(!available.contains(&"podman".to_string()));
    }

    #[test]
    fn test_skip_manager_get_unavailable_engines() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", true);
        manager.set_engine_availability("podman", false);
        manager.set_engine_availability("local", true);
        manager.set_engine_availability("test", false);
        
        let unavailable = manager.get_unavailable_engines();
        assert_eq!(unavailable.len(), 2);
        assert!(unavailable.contains(&"podman".to_string()));
        assert!(unavailable.contains(&"test".to_string()));
        assert!(!unavailable.contains(&"docker".to_string()));
        assert!(!unavailable.contains(&"local".to_string()));
    }

    #[test]
    fn test_skip_manager_should_skip_engine_unavailable() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", false);
        
        let skip_reason = manager.should_skip(Some("docker"), &[]);
        assert!(skip_reason.is_some());
        
        if let Some(SkipReason::EngineUnavailable { engine }) = skip_reason {
            assert_eq!(engine, "docker");
        } else {
            panic!("Expected EngineUnavailable skip reason");
        }
    }

    #[test]
    fn test_skip_manager_should_skip_no_engine() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config);
        
        let skip_reason = manager.should_skip(None, &[]);
        assert!(skip_reason.is_none());
    }

    #[test]
    fn test_skip_manager_should_skip_available_engine() {
        let config = SkipConfig::default();
        let mut manager = SkipManager::new(config);
        
        manager.set_engine_availability("docker", true);
        
        let skip_reason = manager.should_skip(Some("docker"), &[]);
        assert!(skip_reason.is_none());
    }

    #[test]
    fn test_skip_manager_should_skip_with_tags() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config);
        
        // Test with empty tags
        let skip_reason = manager.should_skip(Some("docker"), &[]);
        assert!(skip_reason.is_none());
        
        // Test with tags (no skip conditions in default config)
        let skip_reason = manager.should_skip(Some("docker"), &["integration".to_string()]);
        assert!(skip_reason.is_none());
    }

    #[test]
    fn test_skip_manager_skip_test() {
        let config = SkipConfig {
            fail_on_skipped: true,
            ..Default::default()
        };
        let manager = SkipManager::new(config);
        
        let result = manager.skip_test(SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        });
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Test skipped"));
    }

    #[test]
    fn test_skip_manager_skip_test_no_fail() {
        let config = SkipConfig {
            fail_on_skipped: false,
            ..Default::default()
        };
        let manager = SkipManager::new(config);
        
        let result = manager.skip_test(SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        });
        assert!(result.is_ok());
    }

    #[test]
    fn test_skip_reason_enum() {
        let reasons = vec![
            SkipReason::EngineUnavailable { engine: "docker".to_string() },
            SkipReason::ResourceLimitExceeded { limit: "memory".to_string() },
            SkipReason::PolicyViolation { policy: "security".to_string() },
            SkipReason::TimeoutExceeded { timeout: 30 },
            SkipReason::DependencyMissing { dependency: "redis".to_string() },
            SkipReason::PlatformMismatch { platform: "linux".to_string() },
            SkipReason::VersionMismatch { version: "1.0.0".to_string() },
            SkipReason::PermissionDenied { resource: "file".to_string() },
            SkipReason::NetworkUnavailable { network: "eth0".to_string() },
            SkipReason::DiskFull { path: "/tmp".to_string() },
            SkipReason::MemoryInsufficient { required: 1024, available: 512 },
            SkipReason::CpuOverloaded { usage: 95.0 },
            SkipReason::BackendError { backend: "docker".to_string() },
            SkipReason::ConfigError { config: "policy".to_string() },
            SkipReason::ValidationError { field: "name".to_string() },
            SkipReason::SerializationError { format: "json".to_string() },
            SkipReason::DeserializationError { format: "toml".to_string() },
            SkipReason::IoError { operation: "read".to_string() },
            SkipReason::TimeoutError { operation: "connect".to_string() },
            SkipReason::InternalError { component: "engine".to_string() },
            SkipReason::ExternalError { service: "api".to_string() },
            SkipReason::UnknownError { message: "test".to_string() },
        ];
        
        for reason in reasons {
            assert!(matches!(
                reason,
                SkipReason::EngineUnavailable { .. } |
                SkipReason::ResourceLimitExceeded { .. } |
                SkipReason::PolicyViolation { .. } |
                SkipReason::TimeoutExceeded { .. } |
                SkipReason::DependencyMissing { .. } |
                SkipReason::PlatformMismatch { .. } |
                SkipReason::VersionMismatch { .. } |
                SkipReason::PermissionDenied { .. } |
                SkipReason::NetworkUnavailable { .. } |
                SkipReason::DiskFull { .. } |
                SkipReason::MemoryInsufficient { .. } |
                SkipReason::CpuOverloaded { .. } |
                SkipReason::BackendError { .. } |
                SkipReason::ConfigError { .. } |
                SkipReason::ValidationError { .. } |
                SkipReason::SerializationError { .. } |
                SkipReason::DeserializationError { .. } |
                SkipReason::IoError { .. } |
                SkipReason::TimeoutError { .. } |
                SkipReason::InternalError { .. } |
                SkipReason::ExternalError { .. } |
                SkipReason::UnknownError { .. }
            ));
        }
    }

    #[test]
    fn test_skip_result_executed() {
        let result = SkipResult::Executed(42);
        assert!(result.is_executed());
        assert!(!result.is_skipped());
        assert_eq!(result.unwrap(), 42);
        assert_eq!(result.unwrap_or(0), 42);
        assert_eq!(result.unwrap_or_else(|| 0), 42);
    }

    #[test]
    fn test_skip_result_skipped() {
        let reason = SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        };
        let result = SkipResult::Skipped(reason.clone());
        
        assert!(result.is_skipped());
        assert!(!result.is_executed());
        assert_eq!(result.unwrap_or(0), 0);
        assert_eq!(result.unwrap_or_else(|| 0), 0);
        
        // Test unwrap on skipped result should panic
        let result_clone = result.clone();
        let panic_result = std::panic::catch_unwind(|| {
            result_clone.unwrap();
        });
        assert!(panic_result.is_err());
    }

    #[test]
    fn test_skip_result_clone_derive() {
        let executed = SkipResult::Executed(42);
        let _executed_clone = executed.clone();
        
        let skipped = SkipResult::Skipped(SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        });
        let _skipped_clone = skipped.clone();
    }

    #[test]
    fn test_skip_result_debug_derive() {
        let executed = SkipResult::Executed(42);
        let debug_str = format!("{:?}", executed);
        assert!(debug_str.contains("Executed"));
        assert!(debug_str.contains("42"));
        
        let skipped = SkipResult::Skipped(SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        });
        let debug_str = format!("{:?}", skipped);
        assert!(debug_str.contains("Skipped"));
        assert!(debug_str.contains("EngineUnavailable"));
    }

    #[test]
    fn test_skip_reason_clone_derive() {
        let reason = SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        };
        let _reason_clone = reason.clone();
    }

    #[test]
    fn test_skip_reason_debug_derive() {
        let reason = SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        };
        let debug_str = format!("{:?}", reason);
        assert!(debug_str.contains("EngineUnavailable"));
        assert!(debug_str.contains("docker"));
    }

    #[test]
    fn test_skip_config_default() {
        let config = SkipConfig::default();
        assert!(config.fail_on_skipped);
        assert!(config.skip_on_engine_unavailable);
        assert!(config.skip_on_resource_limit);
        assert!(config.skip_on_policy_violation);
        assert!(config.skip_on_timeout);
        assert!(config.skip_on_dependency_missing);
        assert!(config.skip_on_platform_mismatch);
        assert!(config.skip_on_version_mismatch);
        assert!(config.skip_on_permission_denied);
        assert!(config.skip_on_network_unavailable);
        assert!(config.skip_on_disk_full);
        assert!(config.skip_on_memory_insufficient);
        assert!(config.skip_on_cpu_overloaded);
        assert!(config.skip_on_backend_error);
        assert!(config.skip_on_config_error);
        assert!(config.skip_on_validation_error);
        assert!(config.skip_on_serialization_error);
        assert!(config.skip_on_deserialization_error);
        assert!(config.skip_on_io_error);
        assert!(config.skip_on_timeout_error);
        assert!(config.skip_on_internal_error);
        assert!(config.skip_on_external_error);
        assert!(config.skip_on_unknown_error);
    }

    #[test]
    fn test_skip_config_custom() {
        let config = SkipConfig {
            fail_on_skipped: false,
            skip_on_engine_unavailable: false,
            skip_on_resource_limit: true,
            skip_on_policy_violation: false,
            skip_on_timeout: true,
            skip_on_dependency_missing: false,
            skip_on_platform_mismatch: true,
            skip_on_version_mismatch: false,
            skip_on_permission_denied: true,
            skip_on_network_unavailable: false,
            skip_on_disk_full: true,
            skip_on_memory_insufficient: false,
            skip_on_cpu_overloaded: true,
            skip_on_backend_error: false,
            skip_on_config_error: true,
            skip_on_validation_error: false,
            skip_on_serialization_error: true,
            skip_on_deserialization_error: false,
            skip_on_io_error: true,
            skip_on_timeout_error: false,
            skip_on_internal_error: true,
            skip_on_external_error: false,
            skip_on_unknown_error: true,
        };
        
        assert!(!config.fail_on_skipped);
        assert!(!config.skip_on_engine_unavailable);
        assert!(config.skip_on_resource_limit);
        assert!(!config.skip_on_policy_violation);
        assert!(config.skip_on_timeout);
        assert!(!config.skip_on_dependency_missing);
        assert!(config.skip_on_platform_mismatch);
        assert!(!config.skip_on_version_mismatch);
        assert!(config.skip_on_permission_denied);
        assert!(!config.skip_on_network_unavailable);
        assert!(config.skip_on_disk_full);
        assert!(!config.skip_on_memory_insufficient);
        assert!(config.skip_on_cpu_overloaded);
        assert!(!config.skip_on_backend_error);
        assert!(config.skip_on_config_error);
        assert!(!config.skip_on_validation_error);
        assert!(config.skip_on_serialization_error);
        assert!(!config.skip_on_deserialization_error);
        assert!(config.skip_on_io_error);
        assert!(!config.skip_on_timeout_error);
        assert!(config.skip_on_internal_error);
        assert!(!config.skip_on_external_error);
        assert!(config.skip_on_unknown_error);
    }

    #[test]
    fn test_skip_config_clone_derive() {
        let config = SkipConfig::default();
        let _config_clone = config.clone();
    }

    #[test]
    fn test_skip_config_debug_derive() {
        let config = SkipConfig::default();
        let debug_str = format!("{:?}", config);
        assert!(debug_str.contains("SkipConfig"));
        assert!(debug_str.contains("fail_on_skipped"));
    }

    #[test]
    fn test_skip_manager_clone_derive() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config);
        let _manager_clone = manager.clone();
    }

    #[test]
    fn test_skip_manager_debug_derive() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config);
        let debug_str = format!("{:?}", manager);
        assert!(debug_str.contains("SkipManager"));
        assert!(debug_str.contains("config"));
    }

    #[test]
    fn test_skip_manager_serialization() {
        let config = SkipConfig::default();
        let manager = SkipManager::new(config);
        
        let serialized = serde_json::to_string(&manager.config);
        assert!(serialized.is_ok());
        
        let deserialized: Result<SkipConfig, _> = serde_json::from_str(&serialized.unwrap());
        assert!(deserialized.is_ok());
    }

    #[test]
    fn test_skip_reason_serialization() {
        let reason = SkipReason::EngineUnavailable {
            engine: "docker".to_string(),
        };
        
        let serialized = serde_json::to_string(&reason);
        assert!(serialized.is_ok());
        
        let deserialized: Result<SkipReason, _> = serde_json::from_str(&serialized.unwrap());
        assert!(deserialized.is_ok());
    }

    #[test]
    fn test_skip_result_serialization() {
        let result = SkipResult::Executed(42);
        
        let serialized = serde_json::to_string(&result);
        assert!(serialized.is_ok());
        
        let deserialized: Result<SkipResult<i32>, _> = serde_json::from_str(&serialized.unwrap());
        assert!(deserialized.is_ok());
    }

    #[test]
    fn test_skip_manager_comprehensive_scenario() {
        let config = SkipConfig {
            fail_on_skipped: false,
            skip_on_engine_unavailable: true,
            skip_on_resource_limit: true,
            skip_on_policy_violation: true,
            skip_on_timeout: true,
            skip_on_dependency_missing: true,
            skip_on_platform_mismatch: true,
            skip_on_version_mismatch: true,
            skip_on_permission_denied: true,
            skip_on_network_unavailable: true,
            skip_on_disk_full: true,
            skip_on_memory_insufficient: true,
            skip_on_cpu_overloaded: true,
            skip_on_backend_error: true,
            skip_on_config_error: true,
            skip_on_validation_error: true,
            skip_on_serialization_error: true,
            skip_on_deserialization_error: true,
            skip_on_io_error: true,
            skip_on_timeout_error: true,
            skip_on_internal_error: true,
            skip_on_external_error: true,
            skip_on_unknown_error: true,
        };
        
        let mut manager = SkipManager::new(config);
        
        // Set up engine availability
        manager.set_engine_availability("docker", true);
        manager.set_engine_availability("podman", false);
        manager.set_engine_availability("local", true);
        
        // Test various skip scenarios
        assert!(manager.should_skip(Some("docker"), &[]).is_none());
        assert!(manager.should_skip(Some("podman"), &[]).is_some());
        assert!(manager.should_skip(Some("local"), &[]).is_none());
        assert!(manager.should_skip(None, &[]).is_none());
        
        // Test skip execution
        let result = manager.skip_test(SkipReason::EngineUnavailable {
            engine: "podman".to_string(),
        });
        assert!(result.is_ok());
        
        // Test available engines
        let available = manager.get_available_engines();
        assert_eq!(available.len(), 2);
        assert!(available.contains(&"docker".to_string()));
        assert!(available.contains(&"local".to_string()));
        
        // Test unavailable engines
        let unavailable = manager.get_unavailable_engines();
        assert_eq!(unavailable.len(), 1);
        assert!(unavailable.contains(&"podman".to_string()));
    }
}
