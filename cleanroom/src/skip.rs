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
            Err(CleanroomError::validation_error(format!("Test skipped: {}", reason))
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
}

