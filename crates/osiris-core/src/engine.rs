//! OSIRIS Engine Implementation
//!
//! Main engine orchestrating all OSIRIS components

use crate::{OSIRISConfig};
use crate::domain::{Domain, DomainRegistry, DomainStatus};
use crate::domains::{LifePattern, PatternRegistry};
use crate::health::{HealthMonitor, HealthStatus};
use crate::autonomic::{AutonomicStateMachine, AutonomicState};
use crate::circuit_breaker::{JidokaCircuitBreaker, CircuitBreakerConfig};
use crate::signals::{OSIRISSignal, SignalHandler, SignalLevel};
use crate::patterns::{PatternCategory, PatternStatus};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

/// OSIRIS Engine - The main orchestrator
impl crate::OSIRISEngine {
    /// Create a new OSIRIS engine with custom configuration
    pub async fn with_config(config: OSIRISConfig) -> Result<Self, Box<dyn std::error::Error>> {
        info!("Initializing OSIRIS Engine with custom configuration");

        let domains = Arc::new(RwLock::new(DomainRegistry::new(config.max_domains)));
        let circuit_breakers = Arc::new(RwLock::new(HashMap::new()));
        let health_monitor = Arc::new(HealthMonitor::with_interval(config.health_check_interval_ms));
        let patterns = Arc::new(RwLock::new(PatternRegistry::new()));

        // Register core components for health monitoring
        health_monitor.register_component("engine").await;
        health_monitor.register_component("autonomic_state").await;
        health_monitor.register_component("circuit_breakers").await;

        info!("OSIRIS Engine initialized successfully");

        Ok(Self {
            config,
            domains,
            circuit_breakers,
            health_monitor,
            patterns,
        })
    }

    /// Initialize the OSIRIS system with default domains
    pub async fn initialize(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!("Initializing OSIRIS System");

        // Register core domains
        self.register_core_domains().await?;

        // Register core patterns
        self.register_core_patterns().await?;

        // Start health monitoring
        self.start_health_monitoring().await?;

        // Initialize autonomic state
        let mut state = AutonomicStateMachine::new();
        state.transition_to(crate::AutonomicState::Monitoring, "System initialized".to_string()).await?;

        info!("OSIRIS System initialized successfully");
        Ok(())
    }

    /// Register core domains
    async fn register_core_domains(&self) -> Result<(), Box<dyn std::error::Error>> {
        let mut domains = vec![
            Domain::with_description(
                "health-domain".to_string(),
                "Health Management".to_string(),
                "Manages system health and wellness patterns".to_string(),
            ),
            Domain::with_description(
                "optimization-domain".to_string(),
                "Process Optimization".to_string(),
                "Optimizes business processes and workflows".to_string(),
            ),
            Domain::with_description(
                "quality-domain".to_string(),
                "Quality Assurance".to_string(),
                "Ensures quality standards are met".to_string(),
            ),
            Domain::with_description(
                "safety-domain".to_string(),
                "Safety Management".to_string(),
                "Maintains safety protocols and standards".to_string(),
            ),
        ];

        for domain in domains {
            if let Err(e) = self.register_domain(domain).await {
                warn!("Failed to register domain: {}", e);
            }
        }

        Ok(())
    }

    /// Register core patterns
    async fn register_core_patterns(&self) -> Result<(), Box<dyn std::error::Error>> {
        let mut patterns = vec![
            LifePattern::with_fields(
                "health-monitor".to_string(),
                "Health Monitor".to_string(),
                "Monitors overall system health and wellness".to_string(),
                crate::patterns::PatternCategory::Health,
                "1.0.0".to_string(),
                json!({
                    "interval_ms": 30000,
                    "metrics": ["memory", "cpu", "disk", "network"]
                }),
            ),
            LifePattern::with_fields(
                "process-optimizer".to_string(),
                "Process Optimizer".to_string(),
                "Optimizes business processes for efficiency".to_string(),
                crate::patterns::PatternCategory::Optimization,
                "1.0.0".to_string(),
                json!({
                    "optimization_threshold": 0.8,
                    "max_iterations": 100
                }),
            ),
            LifePattern::with_fields(
                "quality-checker".to_string(),
                "Quality Checker".to_string(),
                "Ensures quality standards are maintained".to_string(),
                crate::patterns::PatternCategory::Quality,
                "1.0.0".to_string(),
                json!({
                    "quality_threshold": 0.95,
                    "auto_correct": true
                }),
            ),
            LifePattern::with_fields(
                "safety-guardian".to_string(),
                "Safety Guardian".to_string(),
                "Monitors and maintains safety protocols".to_string(),
                crate::patterns::PatternCategory::Safety,
                "1.0.0".to_string(),
                json!({
                    "safety_threshold": 0.99,
                    "immediate_intervention": true
                }),
            ),
        ];

        for pattern in patterns {
            if let Err(e) = self.register_pattern(pattern).await {
                warn!("Failed to register pattern: {}", e);
            }
        }

        Ok(())
    }

    /// Execute a pattern within a specific domain
    pub async fn execute_pattern_in_domain(
        &self,
        domain_id: &str,
        pattern_id: &str,
        input: Value,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        info!(
            "Executing pattern {} in domain {}",
            pattern_id, domain_id
        );

        // Get domain and check if it can execute patterns
        let domain = self.domains.read().await.get(domain_id)
            .ok_or_else(|| format!("Domain {} not found", domain_id))?;

        if !domain.can_execute() {
            return Err(format!("Domain {} cannot execute patterns", domain_id).into());
        }

        // Get circuit breaker for domain
        let circuit_breakers = self.circuit_breakers.read().await;
        let breaker = circuit_breakers.get(domain_id)
            .ok_or_else(|| format!("Circuit breaker not found for domain {}", domain_id))?;

        // Execute pattern with circuit breaker protection
        breaker.execute(|| async move {
            // Pattern execution would be implemented here
            // For now, return a success response
            Ok(json!({
                "status": "success",
                "domain": domain_id,
                "pattern": pattern_id,
                "input": input,
                "result": "Pattern executed successfully",
                "timestamp": chrono::Utc::now().to_rfc3339()
            }))
        }).await
    }

    /// Create a circuit breaker for a domain
    pub async fn create_circuit_breaker(
        &self,
        domain_id: &str,
        failure_threshold: usize,
        recovery_timeout_ms: u64,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut circuit_breakers = self.circuit_breakers.write().await;

        let breaker = JidokaCircuitBreaker::new(failure_threshold, recovery_timeout_ms);
        circuit_breakers.insert(domain_id.to_string(), breaker);

        // Register circuit breaker for health monitoring
        self.health_monitor.register_component(&format!("circuit_breaker_{}", domain_id)).await;

        info!(
            "Circuit breaker created for domain {} (threshold: {}, timeout: {}ms)",
            domain_id, failure_threshold, recovery_timeout_ms
        );

        Ok(())
    }

    /// Get overall system health
    pub async fn get_system_health(&self) -> Value {
        let health_status = self.health_monitor.check_health().await;
        let domains = self.domains.read().await.list().await;
        let patterns = self.patterns.read().await.list().await;
        let circuit_breakers = self.circuit_breakers.read().await;

        let circuit_breaker_stats: HashMap<String, Value> = circuit_breakers
            .iter()
            .filter_map(|(id, breaker)| {
                tokio::task::block_in_place(|| {
                    let stats = breaker.stats();
                    serde_json::to_value(stats)
                        .ok()
                        .map(|v| (id.clone(), v))
                })
            })
            .collect();

        json!({
            "overall_health": health_status.to_string(),
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "domains": {
                "total": domains.len(),
                "active": domains.iter().filter(|d| d.is_active()).count(),
                "details": domains
            },
            "patterns": {
                "total": patterns.len(),
                "active": patterns.iter().filter(|p| p.status == crate::patterns::PatternStatus::Active).count(),
                "details": patterns
            },
            "circuit_breakers": circuit_breaker_stats
        })
    }

    /// Emit an OSIRIS signal
    pub async fn emit_signal(&self, signal: OSIRISSignal) -> Result<(), Box<dyn std::error::Error>> {
        info!("Emitting OSIRIS Signal: {}", signal);

        // Log the signal
        match signal.level {
            SignalLevel::Info => debug!("Info signal: {}", signal),
            SignalLevel::Warning => warn!("Warning signal: {}", signal),
            SignalLevel::Critical => error!("Critical signal: {}", signal),
        }

        // Process the signal
        if let Err(e) = SignalHandler::process(signal).await {
            error!("Failed to process signal: {}", e);
            return Err(e.into());
        }

        Ok(())
    }

    /// Get autonomic state
    pub async fn get_autonomic_state(&self) -> crate::AutonomicState {
        // In a full implementation, this would return the current state
        // For now, return a default state
        crate::AutonomicState::Monitoring
    }

    /// Set autonomic state
    pub async fn set_autonomic_state(&self, state: crate::AutonomicState, reason: String) -> Result<(), Box<dyn std::error::Error>> {
        // In a full implementation, this would transition the state
        info!("Setting autonomic state to {:?}: {}", state, reason);

        // Emit state transition signal
        let signal = OSIRISSignal::new(
            "state_transition",
            format!("State changed to {:?}", state),
            SignalLevel::Info,
        );

        self.emit_signal(signal).await?;

        Ok(())
    }

    /// Get system statistics
    pub async fn get_system_stats(&self) -> Value {
        let domains = self.domains.read().await;
        let patterns = self.patterns.read().await;
        let circuit_breakers = self.circuit_breakers.read().await;

        let total_pattern_executions = patterns
            .list()
            .await
            .iter()
            .map(|p| p.stats.success_count + p.stats.failure_count)
            .sum::<u64>();

        json!({
            "domains": {
                "total": domains.count().await,
                "active": domains.get_active_domains().await.len(),
                "executable": domains.get_executable_domains().await.len()
            },
            "patterns": {
                "total": patterns.count().await,
                "active": patterns.get_active_patterns().await.len(),
                "total_executions": total_pattern_executions
            },
            "circuit_breakers": {
                "total": circuit_breakers.len(),
                "states": circuit_breakers
                    .iter()
                    .map(|(id, breaker)| {
                        (id.clone(), breaker.state().await.to_string())
                    })
                    .collect::<HashMap<String, String>>()
            },
            "health": self.health_monitor.check_health().await.to_string(),
            "timestamp": chrono::Utc::now().to_rfc3339()
        })
    }

    /// Shutdown the OSIRIS system gracefully
    pub async fn shutdown(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!("Shutting down OSIRIS System");

        // Transition to shutdown state
        self.set_autonomic_state(
            crate::AutonomicState::Shutdown,
            "System shutdown initiated".to_string()
        ).await?;

        // Stop health monitoring
        // Note: HealthMonitor doesn't have a stop method in the current implementation

        // Emit shutdown signal
        let signal = OSIRISSignal::new(
            "system_shutdown",
            "OSIRIS System is shutting down",
            SignalLevel::Info,
        );

        self.emit_signal(signal).await?;

        info!("OSIRIS System shutdown completed");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_osiris_engine_creation() {
        let config = OSIRISConfig::default();
        let engine = crate::OSIRISEngine::new(config).await;
        assert!(engine.is_ok());
    }

    #[tokio::test]
    async fn test_osiris_engine_with_config() {
        let config = OSIRISConfig {
            max_domains: 50,
            circuit_breaker_threshold: 3,
            health_check_interval_ms: 60000,
            pattern_timeout_ms: 10000,
            signal_buffer_size: 500,
        };

        let engine = crate::OSIRISEngine::with_config(config).await;
        assert!(engine.is_ok());
    }

    #[tokio::test]
    async fn test_circuit_breaker_creation() {
        let config = OSIRISConfig::default();
        let engine = crate::OSIRISEngine::new(config).await.unwrap();

        let result = engine.create_circuit_breaker("test-domain", 3, 5000).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_system_health() {
        let config = OSIRISConfig::default();
        let engine = crate::OSIRISEngine::new(config).await.unwrap();

        let health = engine.get_system_health().await;
        assert!(health.is_object());
        assert!(health["overall_health"].is_string());
    }

    #[tokio::test]
    async fn test_system_stats() {
        let config = OSIRISConfig::default();
        let engine = crate::OSIRISEngine::new(config).await.unwrap();

        let stats = engine.get_system_stats().await;
        assert!(stats.is_object());
        assert!(stats["domains"].is_object());
        assert!(stats["patterns"].is_object());
    }
}