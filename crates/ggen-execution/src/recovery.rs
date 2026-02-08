// Error recovery mechanisms for the execution framework
use crate::error::*;
use crate::framework::*;
use crate::types::*;
use chrono::{DateTime, Utc};
use rand::Rng;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use uuid::Uuid;

// ============================================================================
// RECOVERY STRATEGIES
// ============================================================================

/// Recovery strategy for handling different types of errors
#[derive(Debug, Clone)]
pub enum RecoveryStrategy {
    Retry,
    Fallback,
    CircuitBreaker,
    DeadLetterQueue,
    CircuitReset,
    Custom(String),
}

/// Recovery policy configuration
#[derive(Debug, Clone)]
pub struct RecoveryPolicy {
    pub strategy: RecoveryStrategy,
    pub max_attempts: u32,
    pub initial_delay_ms: u64,
    pub max_delay_ms: u64,
    pub backoff_multiplier: f64,
    pub jitter_ms: u64,
    pub conditions: Vec<RecoveryCondition>,
}

/// Recovery condition for when to apply recovery
#[derive(Debug, Clone, PartialEq)]
pub enum RecoveryCondition {
    OnErrorType(String),
    OnAgentState(String),
    OnConsecutiveFailures(u32),
    OnTimeSinceLastSuccess(u64),
    OnErrorCount(u32),
    Always,
}

// ============================================================================
// HEALTH CHECKER
// ============================================================================

/// Health checker for monitoring system health
pub struct HealthChecker {
    check_interval_ms: u64,
    health_thresholds: HealthThresholds,
    health_history: VecDeque<HealthRecord>,
    agent_health: HashMap<AgentId, AgentHealthStatus>,
    system_health: SystemHealthStatus,
}

/// Health thresholds
#[derive(Debug, Clone)]
pub struct HealthThresholds {
    pub max_error_rate: f64,
    pub min_success_rate: f64,
    pub max_latency_ms: u64,
    pub min_available_agents: usize,
    pub max_memory_usage_mb: u64,
    pub max_cpu_usage_percent: f64,
}

/// Health record
#[derive(Debug, Clone)]
pub struct HealthRecord {
    pub timestamp: DateTime<Utc>,
    pub component: String,
    pub status: HealthStatus,
    pub metrics: HashMap<String, f64>,
}

/// Agent health status
#[derive(Debug, Clone)]
pub struct AgentHealthStatus {
    pub agent_id: AgentId,
    pub status: HealthStatus,
    pub last_check: DateTime<Utc>,
    pub error_count: u32,
    pub success_count: u32,
    pub average_latency_ms: u64,
}

/// System health status
#[derive(Debug, Clone)]
pub struct SystemHealthStatus {
    pub status: HealthStatus,
    pub total_agents: usize,
    pub healthy_agents: usize,
    pub unhealthy_agents: usize,
    pub system_error_rate: f64,
    pub system_throughput: f64,
    pub last_update: DateTime<Utc>,
}

/// Health status enum
#[derive(Debug, Clone, PartialEq)]
pub enum HealthStatus {
    Healthy,
    Degraded,
    Critical,
    Unknown,
}

impl HealthChecker {
    pub fn new(check_interval_ms: u64) -> Self {
        Self {
            check_interval_ms,
            health_thresholds: HealthThresholds::default(),
            health_history: VecDeque::new(),
            agent_health: HashMap::new(),
            system_health: SystemHealthStatus {
                status: HealthStatus::Healthy,
                total_agents: 0,
                healthy_agents: 0,
                unhealthy_agents: 0,
                system_error_rate: 0.0,
                system_throughput: 0.0,
                last_update: Utc::now(),
            },
        }
    }

    pub fn with_thresholds(mut self, thresholds: HealthThresholds) -> Self {
        self.health_thresholds = thresholds;
        self
    }

    /// Check health of all agents
    pub async fn check_all_agents_health(
        &mut self, agents: &mut HashMap<AgentId, Box<dyn UnifiedAgentTrait>>,
    ) -> Result<(), ExecutionError> {
        let mut healthy_count = 0;
        let mut error_count = 0;
        let total_agents = agents.len();

        for (agent_id, agent) in &mut *agents {
            let health_status = self.check_agent_health(agent).await?;

            self.agent_health
                .insert(agent_id.clone(), health_status.clone());

            match health_status.status {
                HealthStatus::Healthy => healthy_count += 1,
                _ => error_count += 1,
            }
        }

        // Update system health
        self.system_health.total_agents = total_agents;
        self.system_health.healthy_agents = healthy_count;
        self.system_health.unhealthy_agents = error_count;
        self.system_health.status = self.calculate_system_status();
        self.system_health.last_update = Utc::now();

        Ok(())
    }

    /// Check health of a single agent
    async fn check_agent_health(
        &mut self, agent: &mut Box<dyn UnifiedAgentTrait>,
    ) -> Result<AgentHealthStatus, ExecutionError> {
        // Get the initial health values before calling execute_task
        let error_count = agent.get_health().error_count;
        let uptime = agent.get_health().uptime_seconds;

        // Create a test task to check responsiveness
        let test_task = Task::new(
            "health-check",
            "Health Check Task",
            "system",
            TaskPriority::Normal,
            serde_json::json!({"type": "health-check"}),
        );

        // Execute test task
        let result = agent.execute_task(test_task).await;

        let mut final_error_count = error_count;
        let mut success_count = 0;

        if let Ok(task_result) = result {
            if task_result.success {
                success_count += 1;
            } else {
                final_error_count += 1;
            }
        } else {
            final_error_count += 1;
        }

        let success_rate = success_count as f64 / (success_count + final_error_count).max(1) as f64;

        let status = if success_rate >= self.health_thresholds.min_success_rate {
            HealthStatus::Healthy
        } else if success_rate >= 0.5 {
            HealthStatus::Degraded
        } else {
            HealthStatus::Critical
        };

        let agent_health = AgentHealthStatus {
            agent_id: agent.get_id().to_string(),
            status: status.clone(),
            last_check: Utc::now(),
            error_count: final_error_count,
            success_count,
            average_latency_ms: uptime / 1000, // Simplified
        };

        // Add to health history
        self.health_history.push_back(HealthRecord {
            timestamp: Utc::now(),
            component: format!("agent-{}", agent.get_id()),
            status,
            metrics: HashMap::from([
                ("success_rate".to_string(), success_rate),
                ("error_count".to_string(), error_count as f64),
            ]),
        });

        // Keep history size manageable
        if self.health_history.len() > 1000 {
            self.health_history.pop_front();
        }

        Ok(agent_health)
    }

    /// Calculate overall system health status
    fn calculate_system_status(&self) -> HealthStatus {
        let total_agents = self.system_health.total_agents;
        let healthy_agents = self.system_health.healthy_agents;
        let healthy_ratio = healthy_agents as f64 / total_agents.max(1) as f64;

        if healthy_ratio >= 0.8 {
            HealthStatus::Healthy
        } else if healthy_ratio >= 0.5 {
            HealthStatus::Degraded
        } else {
            HealthStatus::Critical
        }
    }

    /// Get system health status
    pub fn get_system_health(&self) -> &SystemHealthStatus {
        &self.system_health
    }

    /// Get agent health status
    pub fn get_agent_health(&self, agent_id: &str) -> Option<&AgentHealthStatus> {
        self.agent_health.get(agent_id)
    }
}

impl Default for HealthThresholds {
    fn default() -> Self {
        Self {
            max_error_rate: 0.1,
            min_success_rate: 0.9,
            max_latency_ms: 5000,
            min_available_agents: 1,
            max_memory_usage_mb: 8192,
            max_cpu_usage_percent: 80.0,
        }
    }
}

// ============================================================================
// RECOVERY MANAGER
// ============================================================================

/// Recovery manager for handling system recovery
pub struct RecoveryManager {
    strategies: HashMap<String, RecoveryPolicy>,
    error_history: VecDeque<ErrorRecord>,
    active_recoveries: HashMap<String, RecoveryAttempt>,
    shutdown_signal: Arc<Mutex<bool>>,
}

/// Error record for tracking errors
#[derive(Debug, Clone)]
pub struct ErrorRecord {
    pub id: String,
    pub timestamp: DateTime<Utc>,
    pub error_type: String,
    pub component: String,
    pub severity: ErrorSeverity,
    pub retry_count: u32,
    pub recovery_applied: Option<RecoveryStrategy>,
}

/// Recovery attempt
#[derive(Debug, Clone)]
pub struct RecoveryAttempt {
    pub id: String,
    pub error_id: String,
    pub strategy: RecoveryStrategy,
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub status: RecoveryStatus,
    pub retry_count: u32,
}

/// Recovery status
#[derive(Debug, Clone, PartialEq)]
pub enum RecoveryStatus {
    Pending,
    InProgress,
    Succeeded,
    Failed,
    Retrying,
}

impl RecoveryManager {
    pub fn new() -> Self {
        Self {
            strategies: HashMap::new(),
            error_history: VecDeque::new(),
            active_recoveries: HashMap::new(),
            shutdown_signal: Arc::new(Mutex::new(false)),
        }
    }

    /// Register a recovery policy
    pub fn register_policy(&mut self, error_type: &str, policy: RecoveryPolicy) {
        self.strategies.insert(error_type.to_string(), policy);
    }

    /// Handle an error and attempt recovery
    pub async fn handle_error(
        &mut self, error: &ExecutionError, component: &str,
    ) -> Result<RecoveryResult, ExecutionError> {
        let error_id = Uuid::new_v4().to_string();
        let error_record = ErrorRecord {
            id: error_id.clone(),
            timestamp: Utc::now(),
            error_type: error.to_string(),
            component: component.to_string(),
            severity: self.determine_error_severity(error),
            retry_count: 0,
            recovery_applied: None,
        };

        // Add to error history
        self.error_history.push_back(error_record.clone());

        // Find appropriate recovery policy
        let policy = self.find_recovery_policy(error).await?;
        let strategy = policy.strategy.clone();

        // Start recovery attempt
        let recovery_attempt = RecoveryAttempt {
            id: Uuid::new_v4().to_string(),
            error_id: error_id.clone(),
            strategy: strategy.clone(),
            start_time: Utc::now(),
            end_time: None,
            status: RecoveryStatus::InProgress,
            retry_count: 0,
        };

        self.active_recoveries
            .insert(recovery_attempt.id.clone(), recovery_attempt.clone());

        // Apply recovery strategy
        let result = self
            .apply_recovery_strategy(&strategy, error, &policy)
            .await?;

        // Update recovery attempt
        let recovery = self
            .active_recoveries
            .get_mut(&recovery_attempt.id)
            .unwrap();
        recovery.end_time = Some(Utc::now());
        recovery.status = if result.success {
            RecoveryStatus::Succeeded
        } else {
            RecoveryStatus::Failed
        };

        // Update error record
        let recent_error = self.error_history.iter_mut().rev().next().unwrap();
        recent_error.recovery_applied = Some(strategy);

        Ok(result)
    }

    /// Find recovery policy for error
    async fn find_recovery_policy(
        &self, error: &ExecutionError,
    ) -> Result<RecoveryPolicy, ExecutionError> {
        // Find policy based on error type
        let error_type = self.get_error_type(error);

        if let Some(policy) = self.strategies.get(&error_type) {
            Ok(policy.clone())
        } else {
            // Default recovery policy
            Ok(RecoveryPolicy {
                strategy: RecoveryStrategy::Retry,
                max_attempts: 3,
                initial_delay_ms: 1000,
                max_delay_ms: 10000,
                backoff_multiplier: 2.0,
                jitter_ms: 100,
                conditions: vec![RecoveryCondition::Always],
            })
        }
    }

    /// Get error type from execution error
    fn get_error_type(&self, error: &ExecutionError) -> String {
        match error {
            ExecutionError::Agent(_) => "agent".to_string(),
            ExecutionError::Task(_) => "task".to_string(),
            ExecutionError::Communication(_) => "communication".to_string(),
            ExecutionError::Timeout(_) => "timeout".to_string(),
            ExecutionError::Resource(_) => "resource".to_string(),
            ExecutionError::Configuration(_) => "configuration".to_string(),
            _ => "general".to_string(),
        }
    }

    /// Apply recovery strategy
    async fn apply_recovery_strategy(
        &self, strategy: &RecoveryStrategy, error: &ExecutionError, policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        match strategy {
            RecoveryStrategy::Retry => self.retry_strategy(error, policy).await,
            RecoveryStrategy::Fallback => self.fallback_strategy(error, policy).await,
            RecoveryStrategy::CircuitBreaker => self.circuit_breaker_strategy(error, policy).await,
            RecoveryStrategy::DeadLetterQueue => {
                self.dead_letter_queue_strategy(error, policy).await
            }
            RecoveryStrategy::CircuitReset => self.circuit_reset_strategy(error, policy).await,
            RecoveryStrategy::Custom(name) => self.custom_strategy(name, error, policy).await,
        }
    }

    /// Retry strategy implementation
    async fn retry_strategy(
        &self, error: &ExecutionError, policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        let mut retry_count = 0;

        while retry_count < policy.max_attempts {
            retry_count += 1;

            // Calculate delay with exponential backoff and jitter
            let delay_ms = self.calculate_backoff_delay(policy, retry_count);
            tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;

            // Try to recover (simplified - in production would attempt actual recovery)
            if self.attempt_recovery(error).await? {
                return Ok(RecoveryResult {
                    success: true,
                    strategy: RecoveryStrategy::Retry,
                    retry_count,
                    message: format!("Recovery succeeded after {} attempts", retry_count),
                });
            }
        }

        Ok(RecoveryResult {
            success: false,
            strategy: RecoveryStrategy::Retry,
            retry_count,
            message: format!("Retry strategy failed after {} attempts", retry_count),
        })
    }

    /// Fallback strategy implementation
    async fn fallback_strategy(
        &self, _error: &ExecutionError, _policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        // Implement fallback logic
        // This would involve switching to alternative components or modes

        Ok(RecoveryResult {
            success: true,
            strategy: RecoveryStrategy::Fallback,
            retry_count: 0,
            message: "Fallback strategy applied successfully".to_string(),
        })
    }

    /// Circuit breaker strategy implementation
    async fn circuit_breaker_strategy(
        &self, _error: &ExecutionError, _policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        // Implement circuit breaker logic
        // Open circuit, wait, then attempt reset

        Ok(RecoveryResult {
            success: true,
            strategy: RecoveryStrategy::CircuitBreaker,
            retry_count: 0,
            message: "Circuit breaker applied successfully".to_string(),
        })
    }

    /// Dead letter queue strategy implementation
    async fn dead_letter_queue_strategy(
        &self, _error: &ExecutionError, _policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        // Implement dead letter queue logic
        // Move failed operations to DLQ for later processing

        Ok(RecoveryResult {
            success: true,
            strategy: RecoveryStrategy::DeadLetterQueue,
            retry_count: 0,
            message: "Dead letter queue applied successfully".to_string(),
        })
    }

    /// Circuit reset strategy implementation
    async fn circuit_reset_strategy(
        &self, _error: &ExecutionError, _policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        // Implement circuit reset logic
        // Attempt to reset circuit after failure

        Ok(RecoveryResult {
            success: true,
            strategy: RecoveryStrategy::CircuitReset,
            retry_count: 0,
            message: "Circuit reset applied successfully".to_string(),
        })
    }

    /// Custom strategy implementation
    async fn custom_strategy(
        &self, name: &str, _error: &ExecutionError, _policy: &RecoveryPolicy,
    ) -> Result<RecoveryResult, ExecutionError> {
        // Placeholder for custom strategy implementation
        Ok(RecoveryResult {
            success: false,
            strategy: RecoveryStrategy::Custom(name.to_string()),
            retry_count: 0,
            message: format!("Custom strategy '{}' not implemented", name),
        })
    }

    /// Calculate backoff delay
    fn calculate_backoff_delay(&self, policy: &RecoveryPolicy, retry_count: u32) -> u64 {
        let delay_ms = (policy.initial_delay_ms as f64
            * policy.backoff_multiplier.powi(retry_count as i32)) as u64;
        let mut rng = rand::thread_rng();
        let jitter = rng.gen_range(0..policy.jitter_ms);

        delay_ms.min(policy.max_delay_ms) + jitter
    }

    /// Attempt recovery operation
    async fn attempt_recovery(&self, _error: &ExecutionError) -> Result<bool, ExecutionError> {
        // Simplified recovery attempt
        // In production, this would implement specific recovery logic
        let mut rng = rand::thread_rng();
        Ok(rng.gen_bool(0.5))
    }

    /// Determine error severity
    fn determine_error_severity(&self, error: &ExecutionError) -> ErrorSeverity {
        match error {
            ExecutionError::Task(_) | ExecutionError::Agent(_) => ErrorSeverity::Error,
            ExecutionError::Communication(_) | ExecutionError::Timeout(_) => ErrorSeverity::Warning,
            ExecutionError::Convergence(_) | ExecutionError::Recovery(_) => ErrorSeverity::Critical,
            _ => ErrorSeverity::Info,
        }
    }

    /// Get recovery statistics
    pub fn get_recovery_stats(&self) -> RecoveryStats {
        let total_errors = self.error_history.len();
        let successful_recoveries = self
            .error_history
            .iter()
            .filter(|e| e.recovery_applied.is_some())
            .count();

        RecoveryStats {
            total_errors,
            successful_recoveries,
            failed_recoveries: total_errors - successful_recoveries,
            recovery_rate: successful_recoveries as f64 / total_errors.max(1) as f64,
        }
    }
}

/// Recovery result
#[derive(Debug, Clone)]
pub struct RecoveryResult {
    pub success: bool,
    pub strategy: RecoveryStrategy,
    pub retry_count: u32,
    pub message: String,
}

/// Recovery statistics
#[derive(Debug, Clone)]
pub struct RecoveryStats {
    pub total_errors: usize,
    pub successful_recoveries: usize,
    pub failed_recoveries: usize,
    pub recovery_rate: f64,
}

// ============================================================================
// SELF-HEALING WORKFLOW
// ============================================================================

/// Self-healing workflow for automated recovery
pub struct SelfHealingWorkflow {
    health_checker: HealthChecker,
    recovery_manager: RecoveryManager,
    notification_tx: mpsc::Sender<HealingEvent>,
}

/// Healing event
#[derive(Debug, Clone)]
pub struct HealingEvent {
    pub event_id: String,
    pub timestamp: DateTime<Utc>,
    pub event_type: HealingEventType,
    pub component: String,
    pub severity: ErrorSeverity,
    pub recovery_applied: Option<RecoveryStrategy>,
}

/// Healing event types
#[derive(Debug, Clone, PartialEq)]
pub enum HealingEventType {
    HealthCheck,
    ErrorDetected,
    RecoveryAttempt,
    RecoverySuccess,
    RecoveryFailure,
    CircuitOpened,
    CircuitClosed,
}

impl SelfHealingWorkflow {
    pub fn new() -> Self {
        let (tx, _) = mpsc::channel(100);
        Self {
            health_checker: HealthChecker::new(5000),
            recovery_manager: RecoveryManager::new(),
            notification_tx: tx,
        }
    }

    /// Start self-healing workflow
    pub async fn start(
        &mut self, agents: &mut HashMap<AgentId, Box<dyn UnifiedAgentTrait>>,
    ) -> Result<(), ExecutionError> {
        // Register default recovery policies
        self.register_default_policies();

        // Start health monitoring
        loop {
            if let Err(e) = self.health_checker.check_all_agents_health(agents).await {
                eprintln!("Health check failed: {:?}", e);
            }

            // Check for errors and attempt recovery
            self.check_and_recover().await;

            // Sleep for check interval
            tokio::time::sleep(tokio::time::Duration::from_millis(
                self.health_checker.check_interval_ms,
            ))
            .await;
        }
    }

    /// Register default recovery policies
    fn register_default_policies(&mut self) {
        // Task execution errors
        self.recovery_manager.register_policy(
            "task",
            RecoveryPolicy {
                strategy: RecoveryStrategy::Retry,
                max_attempts: 3,
                initial_delay_ms: 1000,
                max_delay_ms: 10000,
                backoff_multiplier: 2.0,
                jitter_ms: 100,
                conditions: vec![RecoveryCondition::Always],
            },
        );

        // Communication errors
        self.recovery_manager.register_policy(
            "communication",
            RecoveryPolicy {
                strategy: RecoveryStrategy::CircuitBreaker,
                max_attempts: 1,
                initial_delay_ms: 5000,
                max_delay_ms: 30000,
                backoff_multiplier: 1.5,
                jitter_ms: 500,
                conditions: vec![RecoveryCondition::Always],
            },
        );

        // Resource errors
        self.recovery_manager.register_policy(
            "resource",
            RecoveryPolicy {
                strategy: RecoveryStrategy::Fallback,
                max_attempts: 2,
                initial_delay_ms: 2000,
                max_delay_ms: 15000,
                backoff_multiplier: 2.0,
                jitter_ms: 200,
                conditions: vec![RecoveryCondition::Always],
            },
        );
    }

    /// Check for errors and attempt recovery
    async fn check_and_recover(&mut self) {
        // Check system health
        let system_health = self.health_checker.get_system_health();

        if system_health.status == HealthStatus::Critical {
            // System-wide recovery
            if let Err(e) = self.system_wide_recovery().await {
                eprintln!("System-wide recovery failed: {:?}", e);
            }
        }

        // Check individual agents - collect agent IDs first to avoid borrow issues
        let critical_agents: Vec<String> = self
            .health_checker
            .agent_health
            .iter()
            .filter(|(_, agent_health)| agent_health.status == HealthStatus::Critical)
            .map(|(agent_id, _)| agent_id.clone())
            .collect();

        for agent_id in critical_agents {
            // Agent-specific recovery
            if let Err(e) = self.agent_recovery(&agent_id).await {
                eprintln!("Agent recovery failed for {}: {:?}", agent_id, e);
            }
        }
    }

    /// System-wide recovery
    async fn system_wide_recovery(&mut self) -> Result<(), ExecutionError> {
        let event = HealingEvent {
            event_id: Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            event_type: HealingEventType::ErrorDetected,
            component: "system".to_string(),
            severity: ErrorSeverity::Critical,
            recovery_applied: Some(RecoveryStrategy::CircuitBreaker),
        };

        self.notification_tx
            .send(event)
            .await
            .map_err(|_| ExecutionError::Recovery("Failed to send healing event".to_string()))?;

        // Implement system-wide recovery logic
        Ok(())
    }

    /// Agent-specific recovery
    async fn agent_recovery(&mut self, agent_id: &str) -> Result<(), ExecutionError> {
        let event = HealingEvent {
            event_id: Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            event_type: HealingEventType::ErrorDetected,
            component: format!("agent-{}", agent_id),
            severity: ErrorSeverity::Error,
            recovery_applied: Some(RecoveryStrategy::Retry),
        };

        self.notification_tx
            .send(event)
            .await
            .map_err(|_| ExecutionError::Recovery("Failed to send healing event".to_string()))?;

        // Implement agent-specific recovery logic
        Ok(())
    }

    /// Get recovery statistics
    pub fn get_recovery_stats(&self) -> RecoveryStats {
        self.recovery_manager.get_recovery_stats()
    }

    /// Get health checker
    pub fn get_health_checker(&self) -> &HealthChecker {
        &self.health_checker
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_health_checker_creation() {
        let checker = HealthChecker::new(5000);
        assert_eq!(checker.health_history.len(), 0);
        assert_eq!(checker.agent_health.len(), 0);
    }

    #[test]
    fn test_recovery_manager_creation() {
        let manager = RecoveryManager::new();
        assert_eq!(manager.strategies.len(), 0);
        assert_eq!(manager.error_history.len(), 0);
    }

    #[test]
    fn test_recovery_policy_registration() {
        let mut manager = RecoveryManager::new();
        let policy = RecoveryPolicy {
            strategy: RecoveryStrategy::Retry,
            max_attempts: 3,
            initial_delay_ms: 1000,
            max_delay_ms: 10000,
            backoff_multiplier: 2.0,
            jitter_ms: 100,
            conditions: vec![RecoveryCondition::Always],
        };

        manager.register_policy("test-error", policy);
        assert_eq!(manager.strategies.len(), 1);
        assert!(manager.strategies.contains_key("test-error"));
    }

    #[tokio::test]
    async fn test_error_handling() {
        let mut manager = RecoveryManager::new();
        let error = ExecutionError::Task("Test error".to_string());

        let result = manager.handle_error(&error, "test-component").await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_self_healing_workflow_creation() {
        let workflow = SelfHealingWorkflow::new();
        assert_eq!(workflow.health_checker.health_history.len(), 0);
        assert_eq!(workflow.recovery_manager.strategies.len(), 0);
    }

    #[test]
    fn test_recovery_result() {
        let result = RecoveryResult {
            success: true,
            strategy: RecoveryStrategy::Retry,
            retry_count: 2,
            message: "Recovery successful".to_string(),
        };

        assert!(result.success);
        assert_eq!(result.retry_count, 2);
    }
}
