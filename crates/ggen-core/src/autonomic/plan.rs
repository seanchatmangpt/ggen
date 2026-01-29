//! Plan component of the MAPE-K autonomic loop
//!
//! This module implements recovery strategy selection and planning based on
//! failure analysis. It provides cost-based ranking of candidate strategies
//! and generates comprehensive recovery plans with rollback capabilities.
//!
//! ## Overview
//!
//! The planner selects optimal recovery strategies by:
//! - Identifying candidate strategies for detected failures
//! - Estimating recovery costs (time, resources, risk)
//! - Ranking strategies by cost-benefit analysis
//! - Creating detailed recovery plans with preconditions
//! - Generating rollback plans for safe failure recovery
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::autonomic::plan::{ResiliencePlanner, PlannerConfig, AnalysisResult, FailureType};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let config = PlannerConfig::default();
//! let planner = ResiliencePlanner::new(config)?;
//!
//! let analysis = AnalysisResult {
//!     failure_type: FailureType::NodeCrash,
//!     severity: 0.8,
//!     affected_nodes: vec!["node-1".to_string()],
//!     root_cause: "OOM error".to_string(),
//! };
//!
//! let plan = planner.plan(&analysis)?;
//! println!("Selected strategy: {:?}", plan.strategy);
//! println!("Estimated recovery time: {:?}", plan.estimated_recovery_time);
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use std::time::Duration;

// Import types from knowledge_base module for MAPE-K integration
use super::knowledge_base::FailureType;

// ============================================================================
// Core Types
// ============================================================================

/// Configuration for the resilience planner
#[derive(Debug, Clone)]
pub struct PlannerConfig {
    /// Maximum acceptable recovery time
    pub max_recovery_time: Duration,
    /// Minimum confidence threshold for strategies
    pub min_confidence: f64,
    /// Weight for recovery time in cost calculation
    pub time_weight: f64,
    /// Weight for resource usage in cost calculation
    pub resource_weight: f64,
    /// Weight for risk in cost calculation
    pub risk_weight: f64,
}

impl Default for PlannerConfig {
    fn default() -> Self {
        Self {
            max_recovery_time: Duration::from_secs(300), // 5 minutes
            min_confidence: 0.7,
            time_weight: 0.4,
            resource_weight: 0.3,
            risk_weight: 0.3,
        }
    }
}

/// Resilience planner that selects optimal recovery strategies
pub struct ResiliencePlanner {
    strategy_selector: StrategySelector,
    cost_estimator: RecoveryCostEstimator,
    config: PlannerConfig,
}

impl ResiliencePlanner {
    /// Create a new resilience planner with the given configuration
    pub fn new(config: PlannerConfig) -> Result<Self> {
        // Validate configuration
        if config.min_confidence < 0.0 || config.min_confidence > 1.0 {
            return Err(Error::invalid_input(
                "Confidence threshold must be between 0.0 and 1.0",
            ));
        }

        let total_weight = config.time_weight + config.resource_weight + config.risk_weight;
        if (total_weight - 1.0).abs() > 0.01 {
            return Err(Error::invalid_input(
                "Cost weights must sum to 1.0",
            ));
        }

        Ok(Self {
            strategy_selector: StrategySelector::new(),
            cost_estimator: RecoveryCostEstimator::new(config.clone()),
            config,
        })
    }

    /// Generate a recovery plan based on failure analysis
    pub fn plan(&self, analysis: &AnalysisResult) -> Result<RecoveryPlan> {
        // Get candidate strategies for this failure type
        let candidates = self.get_candidate_strategies(&analysis.failure_type)?;

        if candidates.is_empty() {
            return Err(Error::new(&format!(
                "No recovery strategies available for failure type: {:?}",
                analysis.failure_type
            )));
        }

        // Rank strategies by cost
        let ranked = self.rank_strategies(candidates, analysis)?;

        // Select the best strategy
        let (best_strategy, best_cost) = ranked
            .into_iter()
            .next()
            .ok_or_else(|| Error::new("Failed to rank strategies"))?;

        // Calculate confidence based on cost and severity
        let confidence = self.calculate_confidence(&best_cost, analysis.severity);

        if confidence < self.config.min_confidence {
            return Err(Error::new(&format!(
                "Best strategy confidence ({:.2}) below threshold ({:.2})",
                confidence, self.config.min_confidence
            )));
        }

        // Create rollback plan
        let rollback_plan = self.create_rollback_plan(&best_strategy)?;

        // Extract preconditions
        let preconditions = self.extract_preconditions(&best_strategy, analysis)?;

        Ok(RecoveryPlan {
            strategy: best_strategy,
            estimated_recovery_time: best_cost.total_time,
            rollback_plan,
            preconditions,
            confidence,
        })
    }

    /// Get candidate recovery strategies for a failure type
    fn get_candidate_strategies(&self, failure_type: &FailureType) -> Result<Vec<RecoveryStrategy>> {
        self.strategy_selector.select_candidates(failure_type)
    }

    /// Rank strategies by estimated recovery cost
    fn rank_strategies(
        &self,
        strategies: Vec<RecoveryStrategy>,
        _analysis: &AnalysisResult,
    ) -> Result<Vec<(RecoveryStrategy, RecoveryCost)>> {
        let mut ranked: Vec<(RecoveryStrategy, RecoveryCost)> = Vec::new();

        for strategy in strategies {
            let cost = self.cost_estimator.estimate(&strategy)?;

            // Filter out strategies that exceed max recovery time
            if cost.total_time <= self.config.max_recovery_time {
                ranked.push((strategy, cost));
            }
        }

        // Sort by normalized cost (lower is better)
        ranked.sort_by(|a, b| {
            a.1.normalized_cost()
                .partial_cmp(&b.1.normalized_cost())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(ranked)
    }

    /// Create a rollback plan for the given strategy
    fn create_rollback_plan(&self, strategy: &RecoveryStrategy) -> Result<RollbackPlan> {
        let steps = match strategy {
            RecoveryStrategy::SupervisorRestart { node_id, restart_type: _ } => {
                vec![
                    RollbackStep::SaveState {
                        target: node_id.clone(),
                    },
                    RollbackStep::RestoreFromCheckpoint {
                        checkpoint_id: format!("pre-restart-{}", node_id),
                    },
                ]
            }
            RecoveryStrategy::QuorumReconfiguration { excluded_nodes, new_quorum_size } => {
                vec![
                    RollbackStep::SaveState {
                        target: "quorum-config".to_string(),
                    },
                    RollbackStep::RestoreConfiguration {
                        config_key: "quorum.nodes".to_string(),
                        previous_value: format!("excluded: {:?}, size: {}", excluded_nodes, new_quorum_size),
                    },
                ]
            }
            RecoveryStrategy::BackendFailover { from, to: _ } => {
                vec![
                    RollbackStep::SaveState {
                        target: "backend-routing".to_string(),
                    },
                    RollbackStep::RestoreConfiguration {
                        config_key: "backend.active".to_string(),
                        previous_value: format!("{:?}", from),
                    },
                ]
            }
            RecoveryStrategy::LoadShedding { percentage: _, priority_threshold: _ } => {
                vec![RollbackStep::RestoreConfiguration {
                    config_key: "load_shedding.enabled".to_string(),
                    previous_value: "false".to_string(),
                }]
            }
            RecoveryStrategy::GracefulDegradation { disabled_features } => {
                vec![RollbackStep::RestoreConfiguration {
                    config_key: "features.enabled".to_string(),
                    previous_value: format!("all except: {:?}", disabled_features),
                }]
            }
        };

        Ok(RollbackPlan {
            steps,
            timeout: Duration::from_secs(60),
        })
    }

    /// Extract preconditions for executing a strategy
    fn extract_preconditions(
        &self,
        strategy: &RecoveryStrategy,
        _analysis: &AnalysisResult,
    ) -> Result<Vec<Precondition>> {
        let mut preconditions = Vec::new();

        match strategy {
            RecoveryStrategy::SupervisorRestart { node_id, .. } => {
                preconditions.push(Precondition::NodeAvailable {
                    node_id: node_id.clone(),
                });
                preconditions.push(Precondition::QuorumHealthy {
                    min_nodes: 2,
                });
            }
            RecoveryStrategy::QuorumReconfiguration { excluded_nodes: _, new_quorum_size } => {
                preconditions.push(Precondition::SufficientNodes {
                    required: *new_quorum_size,
                });
            }
            RecoveryStrategy::BackendFailover { from: _, to } => {
                preconditions.push(Precondition::BackendHealthy {
                    backend: to.clone(),
                });
            }
            RecoveryStrategy::LoadShedding { .. } => {
                preconditions.push(Precondition::MetricsAvailable);
            }
            RecoveryStrategy::GracefulDegradation { disabled_features } => {
                preconditions.push(Precondition::FeatureConfigValid {
                    features: disabled_features.clone(),
                });
            }
        }

        Ok(preconditions)
    }

    /// Calculate confidence score for a strategy
    fn calculate_confidence(&self, cost: &RecoveryCost, severity: f64) -> f64 {
        // Higher cost or severity reduces confidence
        let cost_factor = 1.0 - cost.normalized_cost();
        let severity_factor = 1.0 - severity;

        // Weighted average
        (cost_factor * 0.6 + severity_factor * 0.4).max(0.0).min(1.0)
    }
}

/// Recovery plan with strategy, timing, and preconditions
#[derive(Debug, Clone)]
pub struct RecoveryPlan {
    pub strategy: RecoveryStrategy,
    pub estimated_recovery_time: Duration,
    pub rollback_plan: RollbackPlan,
    pub preconditions: Vec<Precondition>,
    pub confidence: f64,
}

/// Recovery strategies for different failure scenarios
#[derive(Debug, Clone, PartialEq)]
pub enum RecoveryStrategy {
    /// Restart a node using supervisor
    SupervisorRestart {
        node_id: String,
        restart_type: RestartType,
    },
    /// Reconfigure quorum membership
    QuorumReconfiguration {
        excluded_nodes: Vec<String>,
        new_quorum_size: usize,
    },
    /// Failover to alternative backend
    BackendFailover {
        from: BackendType,
        to: BackendType,
    },
    /// Shed load to reduce resource pressure
    LoadShedding {
        percentage: f64,
        priority_threshold: Priority,
    },
    /// Gracefully degrade by disabling features
    GracefulDegradation {
        disabled_features: Vec<String>,
    },
}

/// Type of supervisor restart
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RestartType {
    /// Quick restart (minimal state preservation)
    Quick,
    /// Full restart (complete state restoration)
    Full,
    /// Cold restart (clean slate)
    Cold,
}

/// Backend types for failover
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BackendType {
    Primary,
    Secondary,
    Cache,
    Fallback,
}

/// Priority levels for load shedding
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    Low,
    Medium,
    High,
    Critical,
}

/// Rollback plan for safe recovery
#[derive(Debug, Clone)]
pub struct RollbackPlan {
    pub steps: Vec<RollbackStep>,
    pub timeout: Duration,
}

/// Individual rollback steps
#[derive(Debug, Clone)]
pub enum RollbackStep {
    SaveState { target: String },
    RestoreFromCheckpoint { checkpoint_id: String },
    RestoreConfiguration { config_key: String, previous_value: String },
}

/// Preconditions that must be met before executing a strategy
#[derive(Debug, Clone)]
pub enum Precondition {
    NodeAvailable { node_id: String },
    QuorumHealthy { min_nodes: usize },
    SufficientNodes { required: usize },
    BackendHealthy { backend: BackendType },
    MetricsAvailable,
    FeatureConfigValid { features: Vec<String> },
}

// ============================================================================
// Strategy Selection
// ============================================================================

/// Selects candidate recovery strategies based on failure type
struct StrategySelector;

impl StrategySelector {
    fn new() -> Self {
        Self
    }

    fn select_candidates(&self, failure_type: &FailureType) -> Result<Vec<RecoveryStrategy>> {
        let strategies = match failure_type {
            FailureType::ContainerFailure => {
                vec![
                    RecoveryStrategy::SupervisorRestart {
                        node_id: "crashed-container".to_string(),
                        restart_type: RestartType::Full,
                    },
                    RecoveryStrategy::QuorumReconfiguration {
                        excluded_nodes: vec!["crashed-container".to_string()],
                        new_quorum_size: 2,
                    },
                ]
            }
            FailureType::NetworkFailure => {
                vec![
                    RecoveryStrategy::QuorumReconfiguration {
                        excluded_nodes: vec!["partitioned-node".to_string()],
                        new_quorum_size: 2,
                    },
                    RecoveryStrategy::GracefulDegradation {
                        disabled_features: vec!["distributed-consensus".to_string()],
                    },
                ]
            }
            FailureType::ResourceExhaustion => {
                vec![
                    RecoveryStrategy::LoadShedding {
                        percentage: 0.3,
                        priority_threshold: Priority::Medium,
                    },
                    RecoveryStrategy::SupervisorRestart {
                        node_id: "overloaded-node".to_string(),
                        restart_type: RestartType::Quick,
                    },
                ]
            }
            FailureType::ServiceDegradation => {
                vec![
                    RecoveryStrategy::LoadShedding {
                        percentage: 0.2,
                        priority_threshold: Priority::Low,
                    },
                    RecoveryStrategy::GracefulDegradation {
                        disabled_features: vec!["non-critical".to_string()],
                    },
                ]
            }
            FailureType::DataCorruption => {
                vec![
                    RecoveryStrategy::BackendFailover {
                        from: BackendType::Primary,
                        to: BackendType::Secondary,
                    },
                    RecoveryStrategy::GracefulDegradation {
                        disabled_features: vec!["write-operations".to_string()],
                    },
                ]
            }
            FailureType::ConfigurationError => {
                vec![
                    RecoveryStrategy::SupervisorRestart {
                        node_id: "misconfigured-service".to_string(),
                        restart_type: RestartType::Cold,
                    },
                    RecoveryStrategy::GracefulDegradation {
                        disabled_features: vec!["experimental-config".to_string()],
                    },
                ]
            }
            FailureType::DependencyFailure => {
                vec![
                    RecoveryStrategy::BackendFailover {
                        from: BackendType::Primary,
                        to: BackendType::Fallback,
                    },
                    RecoveryStrategy::LoadShedding {
                        percentage: 0.4,
                        priority_threshold: Priority::Medium,
                    },
                ]
            }
            FailureType::Unknown => {
                vec![RecoveryStrategy::GracefulDegradation {
                    disabled_features: vec!["experimental".to_string()],
                }]
            }
        };

        Ok(strategies)
    }
}

// ============================================================================
// Cost Estimation
// ============================================================================

/// Estimates recovery cost for strategies
struct RecoveryCostEstimator {
    config: PlannerConfig,
}

impl RecoveryCostEstimator {
    fn new(config: PlannerConfig) -> Self {
        Self { config }
    }

    /// Estimate the cost of executing a recovery strategy
    pub fn estimate(&self, strategy: &RecoveryStrategy) -> Result<RecoveryCost> {
        let (total_time, resource_usage, risk) = match strategy {
            RecoveryStrategy::SupervisorRestart { restart_type, .. } => {
                let time = match restart_type {
                    RestartType::Quick => Duration::from_secs(5),
                    RestartType::Full => Duration::from_secs(30),
                    RestartType::Cold => Duration::from_secs(60),
                };
                (time, 0.2, 0.1)
            }
            RecoveryStrategy::QuorumReconfiguration { .. } => {
                (Duration::from_secs(15), 0.4, 0.3)
            }
            RecoveryStrategy::BackendFailover { .. } => {
                (Duration::from_secs(10), 0.3, 0.2)
            }
            RecoveryStrategy::LoadShedding { percentage, .. } => {
                (Duration::from_secs(2), *percentage, 0.4)
            }
            RecoveryStrategy::GracefulDegradation { disabled_features } => {
                let time = Duration::from_secs(1 + disabled_features.len() as u64);
                let resource = 0.1 * disabled_features.len() as f64;
                (time, resource, 0.5)
            }
        };

        Ok(RecoveryCost {
            total_time,
            resource_usage,
            risk,
            time_weight: self.config.time_weight,
            resource_weight: self.config.resource_weight,
            risk_weight: self.config.risk_weight,
        })
    }
}

/// Estimated cost of a recovery strategy
#[derive(Debug, Clone)]
pub struct RecoveryCost {
    pub total_time: Duration,
    pub resource_usage: f64,
    pub risk: f64,
    time_weight: f64,
    resource_weight: f64,
    risk_weight: f64,
}

impl RecoveryCost {
    /// Calculate normalized cost (0.0 = best, 1.0 = worst)
    fn normalized_cost(&self) -> f64 {
        // Normalize time to [0, 1] range (assuming max 300s)
        let time_norm = (self.total_time.as_secs_f64() / 300.0).min(1.0);

        // Resource usage and risk are already in [0, 1]
        let resource_norm = self.resource_usage.min(1.0);
        let risk_norm = self.risk.min(1.0);

        // Weighted sum
        time_norm * self.time_weight +
        resource_norm * self.resource_weight +
        risk_norm * self.risk_weight
    }
}

// ============================================================================
// Analysis Types (from Monitor/Analyze components)
// ============================================================================

/// Result of failure analysis (from Analyze component)
///
/// This is a simplified view of the analyze module's AnalysisResult,
/// adapted for planning purposes with additional context fields.
#[derive(Debug, Clone)]
pub struct AnalysisResult {
    pub failure_type: FailureType,
    pub severity: f64,
    pub affected_nodes: Vec<String>,
    pub root_cause: String,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Test 1: Plan creation with valid configuration
    // ========================================================================
    #[test]
    fn test_planner_creation_with_valid_config() {
        // Arrange
        let config = PlannerConfig::default();

        // Act
        let result = ResiliencePlanner::new(config);

        // Assert
        assert!(result.is_ok());
    }

    // ========================================================================
    // Test 2: Plan creation fails with invalid confidence threshold
    // ========================================================================
    #[test]
    fn test_planner_creation_fails_invalid_confidence() {
        // Arrange
        let mut config = PlannerConfig::default();
        config.min_confidence = 1.5; // Invalid: > 1.0

        // Act
        let result = ResiliencePlanner::new(config);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("between 0.0 and 1.0"));
    }

    // ========================================================================
    // Test 3: Plan creation fails with invalid cost weights
    // ========================================================================
    #[test]
    fn test_planner_creation_fails_invalid_weights() {
        // Arrange
        let mut config = PlannerConfig::default();
        config.time_weight = 0.5;
        config.resource_weight = 0.3;
        config.risk_weight = 0.3; // Sum = 1.1, invalid

        // Act
        let result = ResiliencePlanner::new(config);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("must sum to 1.0"));
    }

    // ========================================================================
    // Test 4: Plan generation for container failure selects supervisor restart
    // ========================================================================
    #[test]
    fn test_plan_container_failure_selects_supervisor_restart() {
        // Arrange
        let config = PlannerConfig::default();
        let planner = ResiliencePlanner::new(config).expect("Failed to create planner");

        let analysis = AnalysisResult {
            failure_type: FailureType::ContainerFailure,
            severity: 0.6,
            affected_nodes: vec!["node-1".to_string()],
            root_cause: "OOM error".to_string(),
        };

        // Act
        let plan = planner.plan(&analysis).expect("Failed to create plan");

        // Assert
        match plan.strategy {
            RecoveryStrategy::SupervisorRestart { .. } => {
                // Expected strategy selected
                assert!(plan.estimated_recovery_time > Duration::from_secs(0));
                assert!(plan.confidence >= 0.0 && plan.confidence <= 1.0);
                assert!(!plan.preconditions.is_empty());
            }
            _ => panic!("Expected SupervisorRestart strategy"),
        }
    }

    // ========================================================================
    // Test 5: Plan generation for data corruption selects failover
    // ========================================================================
    #[test]
    fn test_plan_data_corruption_selects_failover() {
        // Arrange
        let config = PlannerConfig::default();
        let planner = ResiliencePlanner::new(config).expect("Failed to create planner");

        let analysis = AnalysisResult {
            failure_type: FailureType::DataCorruption,
            severity: 0.7,
            affected_nodes: vec!["backend-1".to_string()],
            root_cause: "Checksum mismatch".to_string(),
        };

        // Act
        let plan = planner.plan(&analysis).expect("Failed to create plan");

        // Assert
        match plan.strategy {
            RecoveryStrategy::BackendFailover { from, to } => {
                assert_eq!(from, BackendType::Primary);
                assert_eq!(to, BackendType::Secondary);
                assert!(!plan.rollback_plan.steps.is_empty());
            }
            RecoveryStrategy::GracefulDegradation { .. } => {
                // Also acceptable for data corruption
                assert!(!plan.rollback_plan.steps.is_empty());
            }
            _ => panic!("Expected BackendFailover or GracefulDegradation strategy"),
        }
    }

    // ========================================================================
    // Test 6: Rollback plan contains state preservation steps
    // ========================================================================
    #[test]
    fn test_rollback_plan_contains_state_preservation() {
        // Arrange
        let config = PlannerConfig::default();
        let planner = ResiliencePlanner::new(config).expect("Failed to create planner");

        let analysis = AnalysisResult {
            failure_type: FailureType::ContainerFailure,
            severity: 0.5,
            affected_nodes: vec!["node-1".to_string()],
            root_cause: "Process crash".to_string(),
        };

        // Act
        let plan = planner.plan(&analysis).expect("Failed to create plan");

        // Assert
        assert!(!plan.rollback_plan.steps.is_empty());
        let has_save_step = plan.rollback_plan.steps.iter().any(|step| {
            matches!(step, RollbackStep::SaveState { .. })
        });
        assert!(has_save_step, "Rollback plan should contain state save step");
    }

    // ========================================================================
    // Test 7: Recovery cost calculation produces valid normalized cost
    // ========================================================================
    #[test]
    fn test_recovery_cost_normalized_calculation() {
        // Arrange
        let config = PlannerConfig::default();
        let cost = RecoveryCost {
            total_time: Duration::from_secs(30),
            resource_usage: 0.5,
            risk: 0.3,
            time_weight: config.time_weight,
            resource_weight: config.resource_weight,
            risk_weight: config.risk_weight,
        };

        // Act
        let normalized = cost.normalized_cost();

        // Assert
        assert!(normalized >= 0.0 && normalized <= 1.0,
                "Normalized cost should be in [0, 1] range, got: {}", normalized);
    }

    // ========================================================================
    // Test 8: Strategy selector returns appropriate strategies for failure types
    // ========================================================================
    #[test]
    fn test_strategy_selector_returns_appropriate_strategies() {
        // Arrange
        let selector = StrategySelector::new();
        let failure_types = vec![
            FailureType::ContainerFailure,
            FailureType::NetworkFailure,
            FailureType::ResourceExhaustion,
            FailureType::ServiceDegradation,
            FailureType::DataCorruption,
            FailureType::ConfigurationError,
            FailureType::DependencyFailure,
            FailureType::Unknown,
        ];

        // Act & Assert
        for failure_type in failure_types {
            let strategies = selector.select_candidates(&failure_type)
                .expect("Failed to select strategies");

            assert!(!strategies.is_empty(),
                    "Should have strategies for {:?}", failure_type);

            // Verify strategies are appropriate for failure type
            match failure_type {
                FailureType::ContainerFailure => {
                    assert!(strategies.iter().any(|s| matches!(s, RecoveryStrategy::SupervisorRestart { .. })));
                }
                FailureType::DataCorruption | FailureType::DependencyFailure => {
                    assert!(strategies.iter().any(|s| matches!(s, RecoveryStrategy::BackendFailover { .. })));
                }
                FailureType::ResourceExhaustion | FailureType::ServiceDegradation => {
                    assert!(strategies.iter().any(|s| matches!(s, RecoveryStrategy::LoadShedding { .. })));
                }
                _ => {
                    // Other failure types should have at least one strategy
                    assert!(!strategies.is_empty());
                }
            }
        }
    }
}
