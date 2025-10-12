//! Type-safe builder pattern for Cleanroom configuration
//!
//! This module provides a compile-time validated builder API that ensures
//! configuration correctness through Rust's type system. The typestate pattern
//! prevents invalid configurations from being constructed.
//!
//! # Example
//!
//! ```rust
//! use cleanroom::builder::CleanroomBuilder;
//! use std::time::Duration;
//!
//! let env = CleanroomBuilder::new()
//!     .with_timeout(Duration::from_secs(30))
//!     .with_security_policy(SecurityPolicy::locked())
//!     .with_deterministic_execution(Some(42))
//!     .build()
//!     .await?;
//! ```

use crate::error::Result;
use crate::config::CleanroomConfig;
use crate::policy::SecurityPolicy;
use crate::limits::ResourceLimits;
use crate::cleanroom::CleanroomEnvironment;
use std::time::Duration;
use std::collections::HashMap;

/// Type-safe builder for CleanroomEnvironment
///
/// Uses typestate pattern to ensure compile-time validation of configuration.
/// Each method call transitions to a new state type, preventing invalid
/// configurations from being constructed.
pub struct CleanroomBuilder<State = Initial> {
    config: CleanroomConfig,
    _state: std::marker::PhantomData<State>,
}

/// Initial builder state
pub struct Initial;

/// Builder state after timeout is configured
pub struct WithTimeout;

/// Builder state after security policy is configured
pub struct WithSecurity;

/// Builder state after resource limits are configured
pub struct WithResources;

/// Builder state after deterministic execution is configured
pub struct WithDeterministic;

/// Builder state ready for building
pub struct Ready;

impl CleanroomBuilder<Initial> {
    /// Create a new builder with default configuration
    pub fn new() -> Self {
        Self {
            config: CleanroomConfig::default(),
            _state: std::marker::PhantomData,
        }
    }

    /// Configure timeout settings
    pub fn with_timeout(mut self, timeout: Duration) -> CleanroomBuilder<WithTimeout> {
        self.config.test_execution_timeout = timeout;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure container startup timeout
    pub fn with_container_timeout(mut self, timeout: Duration) -> CleanroomBuilder<WithTimeout> {
        self.config.container_startup_timeout = timeout;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure security policy
    pub fn with_security_policy(mut self, policy: SecurityPolicy) -> CleanroomBuilder<WithSecurity> {
        self.config.security_policy = policy;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure resource limits
    pub fn with_resource_limits(mut self, limits: ResourceLimits) -> CleanroomBuilder<WithResources> {
        self.config.resource_limits = limits;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure deterministic execution
    pub fn with_deterministic_execution(mut self, seed: Option<u64>) -> CleanroomBuilder<WithDeterministic> {
        self.config.enable_deterministic_execution = seed.is_some();
        self.config.deterministic_seed = seed;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Enable singleton containers
    pub fn with_singleton_containers(mut self, enabled: bool) -> CleanroomBuilder<Ready> {
        self.config.enable_singleton_containers = enabled;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Build the environment with minimal configuration
    pub async fn build_minimal(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

impl CleanroomBuilder<WithTimeout> {
    /// Configure security policy
    pub fn with_security_policy(mut self, policy: SecurityPolicy) -> CleanroomBuilder<WithSecurity> {
        self.config.security_policy = policy;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure resource limits
    pub fn with_resource_limits(mut self, limits: ResourceLimits) -> CleanroomBuilder<WithResources> {
        self.config.resource_limits = limits;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure deterministic execution
    pub fn with_deterministic_execution(mut self, seed: Option<u64>) -> CleanroomBuilder<WithDeterministic> {
        self.config.enable_deterministic_execution = seed.is_some();
        self.config.deterministic_seed = seed;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Build the environment
    pub async fn build(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

impl CleanroomBuilder<WithSecurity> {
    /// Configure resource limits
    pub fn with_resource_limits(mut self, limits: ResourceLimits) -> CleanroomBuilder<WithResources> {
        self.config.resource_limits = limits;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Configure deterministic execution
    pub fn with_deterministic_execution(mut self, seed: Option<u64>) -> CleanroomBuilder<WithDeterministic> {
        self.config.enable_deterministic_execution = seed.is_some();
        self.config.deterministic_seed = seed;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Build the environment
    pub async fn build(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

impl CleanroomBuilder<WithResources> {
    /// Configure deterministic execution
    pub fn with_deterministic_execution(mut self, seed: Option<u64>) -> CleanroomBuilder<WithDeterministic> {
        self.config.enable_deterministic_execution = seed.is_some();
        self.config.deterministic_seed = seed;
        CleanroomBuilder {
            config: self.config,
            _state: std::marker::PhantomData,
        }
    }

    /// Build the environment
    pub async fn build(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

impl CleanroomBuilder<WithDeterministic> {
    /// Build the environment
    pub async fn build(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

impl CleanroomBuilder<Ready> {
    /// Build the environment
    pub async fn build(self) -> Result<CleanroomEnvironment> {
        CleanroomEnvironment::new(self.config).await
    }
}

/// Convenience methods available on all builder states
impl<State> CleanroomBuilder<State> {
    /// Enable coverage tracking
    pub fn with_coverage_tracking(mut self, enabled: bool) -> Self {
        self.config.enable_coverage_tracking = enabled;
        self
    }

    /// Enable snapshot testing
    pub fn with_snapshot_testing(mut self, enabled: bool) -> Self {
        self.config.enable_snapshot_testing = enabled;
        self
    }

    /// Enable tracing
    pub fn with_tracing(mut self, enabled: bool) -> Self {
        self.config.enable_tracing = enabled;
        self
    }

    /// Set maximum concurrent containers
    pub fn with_max_concurrent_containers(mut self, max: u32) -> Self {
        self.config.max_concurrent_containers = max;
        self
    }

    /// Add container customizer
    pub fn with_container_customizer(mut self, name: String, customizer: crate::config::ContainerCustomizer) -> Self {
        self.config.container_customizers.insert(name, customizer);
        self
    }

    /// Get the current configuration (for inspection)
    pub fn config(&self) -> &CleanroomConfig {
        &self.config
    }
}

impl Default for CleanroomBuilder<Initial> {
    fn default() -> Self {
        Self::new()
    }
}

/// Fluent API for common configuration patterns
impl CleanroomBuilder<Initial> {
    /// Create a secure environment with locked-down policies
    pub fn secure() -> CleanroomBuilder<WithSecurity> {
        Self::new().with_security_policy(SecurityPolicy::locked())
    }

    /// Create a high-performance environment with optimized settings
    pub fn performance() -> CleanroomBuilder<WithTimeout> {
        Self::new()
            .with_timeout(Duration::from_secs(60))
            .with_singleton_containers(true)
    }

    /// Create a deterministic environment for reproducible tests
    pub fn deterministic(seed: u64) -> CleanroomBuilder<WithDeterministic> {
        Self::new().with_deterministic_execution(Some(seed))
    }

    /// Create a development environment with relaxed policies
    pub fn development() -> CleanroomBuilder<Ready> {
        Self::new()
            .with_singleton_containers(false)
            .with_coverage_tracking(true)
            .with_tracing(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_builder_minimal() {
        let env = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build minimal environment");
        
        assert_eq!(env.config().test_execution_timeout, Duration::from_secs(300));
    }

    #[tokio::test]
    async fn test_builder_with_timeout() {
        let timeout = Duration::from_secs(60);
        let env = CleanroomBuilder::new()
            .with_timeout(timeout)
            .build()
            .await
            .expect("Should build with timeout");
        
        assert_eq!(env.config().test_execution_timeout, timeout);
    }

    #[tokio::test]
    async fn test_builder_with_security() {
        let policy = SecurityPolicy::locked();
        let env = CleanroomBuilder::new()
            .with_security_policy(policy.clone())
            .build()
            .await
            .expect("Should build with security policy");
        
        assert_eq!(env.config().security_policy.security_level, policy.security_level);
    }

    #[tokio::test]
    async fn test_builder_convenience_methods() {
        let env = CleanroomBuilder::secure()
            .with_coverage_tracking(true)
            .with_snapshot_testing(true)
            .with_tracing(true)
            .build()
            .await
            .expect("Should build with convenience methods");
        
        assert!(env.config().enable_coverage_tracking);
        assert!(env.config().enable_snapshot_testing);
        assert!(env.config().enable_tracing);
    }

    #[tokio::test]
    async fn test_builder_deterministic() {
        let seed = 42;
        let env = CleanroomBuilder::deterministic(seed)
            .build()
            .await
            .expect("Should build deterministic environment");
        
        assert!(env.config().enable_deterministic_execution);
        assert_eq!(env.config().deterministic_seed, Some(seed));
    }

    #[tokio::test]
    async fn test_builder_development() {
        let env = CleanroomBuilder::development()
            .build()
            .await
            .expect("Should build development environment");
        
        assert!(!env.config().enable_singleton_containers);
        assert!(env.config().enable_coverage_tracking);
        assert!(env.config().enable_tracing);
    }

    #[tokio::test]
    async fn test_builder_performance() {
        let env = CleanroomBuilder::performance()
            .build()
            .await
            .expect("Should build performance environment");
        
        assert_eq!(env.config().test_execution_timeout, Duration::from_secs(60));
        assert!(env.config().enable_singleton_containers);
    }
}
