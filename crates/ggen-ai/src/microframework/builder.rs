//! Builder pattern for orchestrator configuration
//!
//! Provides a fluent API for configuring the orchestrator.

use super::agents::MicroAgent;
use super::orchestrator::AgentOrchestrator;
use super::MicroframeworkConfig;
use crate::error::{GgenAiError, Result};
use std::time::Duration;

/// Builder for AgentOrchestrator
#[derive(Debug, Clone)]
pub struct OrchestratorBuilder {
    /// Configuration being built
    config: MicroframeworkConfig,
    /// Agents to register
    agents: Vec<Box<dyn FnOnce(&AgentOrchestrator) + Send>>,
}

impl OrchestratorBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: MicroframeworkConfig::default(),
            agents: Vec::new(),
        }
    }

    /// Set maximum concurrent agents (1-10)
    pub fn max_agents(mut self, max: usize) -> Self {
        self.config.max_agents = max.min(10).max(1);
        self
    }

    /// Enable work stealing
    pub fn enable_work_stealing(mut self) -> Self {
        self.config.enable_work_stealing = true;
        self
    }

    /// Disable work stealing
    pub fn disable_work_stealing(mut self) -> Self {
        self.config.enable_work_stealing = false;
        self
    }

    /// Enable circuit breaker
    pub fn enable_circuit_breaker(mut self) -> Self {
        self.config.enable_circuit_breaker = true;
        self
    }

    /// Disable circuit breaker
    pub fn disable_circuit_breaker(mut self) -> Self {
        self.config.enable_circuit_breaker = false;
        self
    }

    /// Enable backpressure handling
    pub fn enable_backpressure(mut self) -> Self {
        self.config.enable_backpressure = true;
        self
    }

    /// Disable backpressure handling
    pub fn disable_backpressure(mut self) -> Self {
        self.config.enable_backpressure = false;
        self
    }

    /// Enable progress tracking
    pub fn enable_progress(mut self) -> Self {
        self.config.enable_progress_tracking = true;
        self
    }

    /// Disable progress tracking
    pub fn disable_progress(mut self) -> Self {
        self.config.enable_progress_tracking = false;
        self
    }

    /// Enable metrics collection
    pub fn enable_metrics(mut self) -> Self {
        self.config.enable_metrics = true;
        self
    }

    /// Disable metrics collection
    pub fn disable_metrics(mut self) -> Self {
        self.config.enable_metrics = false;
        self
    }

    /// Set default task timeout
    pub fn default_timeout(mut self, timeout: Duration) -> Self {
        self.config.default_timeout_secs = timeout.as_secs();
        self
    }

    /// Set default timeout in seconds
    pub fn default_timeout_secs(mut self, secs: u64) -> Self {
        self.config.default_timeout_secs = secs;
        self
    }

    /// Use high-performance configuration
    pub fn high_performance(mut self) -> Self {
        self.config = MicroframeworkConfig::high_performance();
        self
    }

    /// Use development configuration
    pub fn development(mut self) -> Self {
        self.config = MicroframeworkConfig::development();
        self
    }

    /// Use custom configuration
    pub fn with_config(mut self, config: MicroframeworkConfig) -> Self {
        self.config = config;
        self
    }

    /// Build the orchestrator
    pub fn build(self) -> Result<AgentOrchestrator> {
        // Validate configuration
        if self.config.max_agents == 0 || self.config.max_agents > 10 {
            return Err(GgenAiError::invalid_input(
                "max_agents must be between 1 and 10",
            ));
        }

        let orchestrator = AgentOrchestrator::with_config(self.config);

        Ok(orchestrator)
    }
}

impl Default for OrchestratorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Quick builder functions for common configurations
impl AgentOrchestrator {
    /// Create with max performance settings
    pub fn max_performance() -> Result<Self> {
        OrchestratorBuilder::new().high_performance().build()
    }

    /// Create for development/testing
    pub fn development() -> Result<Self> {
        OrchestratorBuilder::new().development().build()
    }

    /// Create with specific agent count
    pub fn with_agents(count: usize) -> Result<Self> {
        OrchestratorBuilder::new().max_agents(count).build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builder_defaults() {
        let orchestrator = OrchestratorBuilder::new().build().unwrap();
        let stats = orchestrator.statistics();
        assert_eq!(stats.registered_agents, 0);
    }

    #[test]
    fn test_builder_max_agents() {
        let builder = OrchestratorBuilder::new().max_agents(5);
        assert_eq!(builder.config.max_agents, 5);
    }

    #[test]
    fn test_builder_max_agents_clamped() {
        let builder = OrchestratorBuilder::new().max_agents(100);
        assert_eq!(builder.config.max_agents, 10);

        let builder = OrchestratorBuilder::new().max_agents(0);
        assert_eq!(builder.config.max_agents, 1);
    }

    #[test]
    fn test_builder_chain() {
        let orchestrator = OrchestratorBuilder::new()
            .max_agents(8)
            .enable_circuit_breaker()
            .enable_backpressure()
            .disable_progress()
            .default_timeout_secs(30)
            .build()
            .unwrap();

        assert!(orchestrator.statistics().registered_agents == 0);
    }

    #[test]
    fn test_high_performance() {
        let orchestrator = AgentOrchestrator::max_performance().unwrap();
        assert!(orchestrator.statistics().registered_agents == 0);
    }

    #[test]
    fn test_development() {
        let orchestrator = AgentOrchestrator::development().unwrap();
        assert!(orchestrator.statistics().registered_agents == 0);
    }
}
