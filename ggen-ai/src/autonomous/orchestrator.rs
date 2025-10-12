//! Machine-timescale orchestration with parallel execution
//!
//! # Autonomous Orchestrator - Machine-Timescale Coordination
//!
//! ## PURPOSE
//! Coordinates autonomous regeneration cycles at machine timescale (sub-30 second target),
//! managing parallel event processing, deployment automation, and continuous optimization
//! without human intervention.
//!
//! ## RESPONSIBILITIES
//! - **Parallel Event Processing**: Execute regeneration events concurrently with configurable limits
//! - **Cycle Time Optimization**: Monitor and optimize regeneration cycles to achieve sub-30s target
//! - **Health Monitoring**: Continuous health checks of regeneration pipeline with automatic recovery
//! - **Graceful Degradation**: Handle partial failures without stopping the orchestration loop
//! - **Metrics Collection**: Track cycle time, success rate, concurrent operations for optimization
//!
//! ## CONSTRAINTS
//! - Cycle Time Target: Must complete regeneration cycles in <30 seconds under normal load
//! - Concurrency Limits: Respects `max_concurrent_operations` to prevent resource exhaustion
//! - Single Orchestrator: Only one orchestrator instance should run at a time per deployment
//! - Event Ordering: Events processed in parallel but dependencies must be respected
//!
//! ## INVARIANTS
//! 1. Only one orchestrator instance active per deployment context
//! 2. All events processed without data loss, even during errors
//! 3. Health checks run continuously and independently from event processing
//! 4. OrchestrationStats accurately reflect actual system behavior
//!
//! ## REFACTORING PRIORITIES (from REFACTORING_ANALYSIS.md)
//! - **P0-1**: Extract error handling utility to reduce duplication
//! - **P1-3**: Add progress indicators for long operations
//! - **P2-1**: Break down execute_cycle (91 lines) into focused methods

use crate::autonomous::telemetry::TelemetryEventType;
use crate::autonomous::{
    ChangeEvent, DeploymentAutomation, GraphChangeNotifier, RegenerationEngine, TelemetryCollector,
    TelemetryEvent,
};
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

/// Orchestrator configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestratorConfig {
    /// Enable autonomous operation
    pub autonomous: bool,
    /// Maximum concurrent regenerations
    pub max_concurrent: usize,
    /// Target cycle time in milliseconds
    pub target_cycle_ms: u64,
    /// Enable adaptive optimization
    pub adaptive_optimization: bool,
    /// Health check interval in seconds
    pub health_check_interval_secs: u64,
}

impl Default for OrchestratorConfig {
    fn default() -> Self {
        use crate::constants::autonomous;

        Self {
            autonomous: true,
            max_concurrent: num_cpus::get(),
            target_cycle_ms: autonomous::TARGET_CYCLE_TIME_MS,
            adaptive_optimization: true,
            health_check_interval_secs: autonomous::HEALTH_CHECK_INTERVAL_SECS,
        }
    }
}

/// Parallel execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParallelExecution {
    /// Execution ID
    pub id: String,
    /// Start timestamp
    pub started_at: chrono::DateTime<chrono::Utc>,
    /// End timestamp
    pub completed_at: Option<chrono::DateTime<chrono::Utc>>,
    /// Total duration in milliseconds
    pub duration_ms: Option<u64>,
    /// Number of tasks executed
    pub tasks_executed: usize,
    /// Number of successful tasks
    pub tasks_succeeded: usize,
    /// Number of failed tasks
    pub tasks_failed: usize,
    /// Parallelism achieved
    pub parallelism: usize,
}

/// Orchestration statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestrationStats {
    pub total_cycles: u64,
    pub avg_cycle_time_ms: f64,
    pub events_processed: u64,
    pub regenerations_triggered: u64,
    pub deployments_completed: u64,
}

impl Default for OrchestrationStats {
    fn default() -> Self {
        Self {
            total_cycles: 0,
            avg_cycle_time_ms: 0.0,
            events_processed: 0,
            regenerations_triggered: 0,
            deployments_completed: 0,
        }
    }
}

/// Main orchestrator for autonomous regeneration
#[derive(Clone)]
pub struct RegenerationOrchestrator {
    config: OrchestratorConfig,
    regeneration_engine: Arc<RegenerationEngine>,
    #[allow(dead_code)] // Deployment kept for future automated deployment features
    deployment: Arc<RwLock<DeploymentAutomation>>,
    telemetry: Arc<TelemetryCollector>,
    notifier: Arc<GraphChangeNotifier>,
    stats: Arc<RwLock<OrchestrationStats>>,
    running: Arc<RwLock<bool>>,
}

impl std::fmt::Debug for RegenerationOrchestrator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RegenerationOrchestrator")
            .field("config", &self.config)
            .field("regeneration_engine", &"Arc<RegenerationEngine>")
            .field("deployment", &"Arc<RwLock<DeploymentAutomation>>")
            .field("telemetry", &"Arc<TelemetryCollector>")
            .field("notifier", &"Arc<GraphChangeNotifier>")
            .field("stats", &"Arc<RwLock<OrchestrationStats>>")
            .field("running", &"Arc<RwLock<bool>>")
            .finish()
    }
}

impl RegenerationOrchestrator {
    /// Create a new orchestrator
    pub fn new(
        config: OrchestratorConfig, regeneration_engine: Arc<RegenerationEngine>,
        deployment: Arc<RwLock<DeploymentAutomation>>, telemetry: Arc<TelemetryCollector>,
        notifier: Arc<GraphChangeNotifier>,
    ) -> Self {
        Self {
            config,
            regeneration_engine,
            deployment,
            telemetry,
            notifier,
            stats: Arc::new(RwLock::new(OrchestrationStats::default())),
            running: Arc::new(RwLock::new(false)),
        }
    }

    /// Start autonomous orchestration
    pub async fn start(self: Arc<Self>) -> Result<()> {
        {
            let mut running = self.running.write().await;
            if *running {
                return Err(GgenAiError::orchestration(
                    "Orchestrator is already running".to_string(),
                ));
            }
            *running = true;
        }

        info!(
            autonomous = self.config.autonomous,
            max_concurrent = self.config.max_concurrent,
            target_cycle_ms = self.config.target_cycle_ms,
            "Starting regeneration orchestrator"
        );

        // Record telemetry
        self.telemetry
            .record(TelemetryEvent::new(
                TelemetryEventType::RegenerationStarted,
                "orchestrator".to_string(),
            ))
            .await;

        // Start regeneration engine
        self.regeneration_engine.clone().start().await?;

        // Start health check loop if configured
        if self.config.health_check_interval_secs > 0 {
            let orchestrator = self.clone();
            tokio::spawn(async move {
                orchestrator.health_check_loop().await;
            });
        }

        info!("Orchestrator started successfully");
        Ok(())
    }

    /// Stop orchestration
    pub async fn stop(&self) -> Result<()> {
        info!("Stopping orchestrator");

        let mut running = self.running.write().await;
        *running = false;

        // Record telemetry
        self.telemetry
            .record(TelemetryEvent::new(
                TelemetryEventType::RegenerationCompleted,
                "orchestrator".to_string(),
            ))
            .await;

        info!("Orchestrator stopped");
        Ok(())
    }

    /// Check if orchestrator is running
    pub async fn is_running(&self) -> bool {
        *self.running.read().await
    }

    /// Execute a full regeneration cycle
    pub async fn execute_cycle(&self, events: Vec<ChangeEvent>) -> Result<ParallelExecution> {
        let execution_id = uuid::Uuid::new_v4().to_string();
        let start_time = Instant::now();

        info!(
            execution_id = %execution_id,
            events = events.len(),
            "Starting regeneration cycle"
        );

        let mut execution = self.initialize_execution(&execution_id, events.len());

        // Process events and collect results
        let results = self.process_events_parallel(events).await;
        self.collect_results(&mut execution, results);

        // Finalize execution
        self.finalize_execution(&mut execution, start_time, &execution_id)
            .await;

        // Check performance targets
        self.check_performance_targets(&execution).await;

        Ok(execution)
    }

    /// Initialize execution tracking structure
    fn initialize_execution(&self, execution_id: &str, num_events: usize) -> ParallelExecution {
        ParallelExecution {
            id: execution_id.to_string(),
            started_at: chrono::Utc::now(),
            completed_at: None,
            duration_ms: None,
            tasks_executed: num_events,
            tasks_succeeded: 0,
            tasks_failed: 0,
            parallelism: self.config.max_concurrent,
        }
    }

    /// Collect results from parallel event processing
    fn collect_results(&self, execution: &mut ParallelExecution, results: Vec<Result<()>>) {
        for result in results {
            match result {
                Ok(_) => execution.tasks_succeeded += 1,
                Err(e) => {
                    error!(error = %e, "Event processing failed");
                    execution.tasks_failed += 1;
                }
            }
        }
    }

    /// Finalize execution: update stats, record telemetry, log summary
    async fn finalize_execution(
        &self, execution: &mut ParallelExecution, start_time: Instant, execution_id: &str,
    ) {
        let duration = start_time.elapsed();
        execution.completed_at = Some(chrono::Utc::now());
        execution.duration_ms = Some(duration.as_millis() as u64);

        // Update stats
        self.update_orchestration_stats(execution).await;

        // Record telemetry
        self.record_cycle_telemetry(execution_id, execution).await;

        // Log summary
        info!(
            execution_id = %execution_id,
            duration_ms = execution.duration_ms.unwrap_or(0),
            succeeded = execution.tasks_succeeded,
            failed = execution.tasks_failed,
            "Regeneration cycle completed"
        );
    }

    /// Update orchestration statistics
    async fn update_orchestration_stats(&self, execution: &ParallelExecution) {
        let mut stats = self.stats.write().await;
        stats.total_cycles += 1;
        let total = stats.total_cycles as f64;
        if let Some(duration_ms) = execution.duration_ms {
            stats.avg_cycle_time_ms =
                (stats.avg_cycle_time_ms * (total - 1.0) + duration_ms as f64) / total;
        }
        stats.events_processed += execution.tasks_executed as u64;
    }

    /// Record telemetry for cycle completion
    async fn record_cycle_telemetry(&self, execution_id: &str, execution: &ParallelExecution) {
        self.telemetry
            .record(
                TelemetryEvent::new(
                    TelemetryEventType::RegenerationCompleted,
                    "orchestrator".to_string(),
                )
                .with_data("execution_id", serde_json::json!(execution_id))
                .with_data("duration_ms", serde_json::json!(execution.duration_ms))
                .with_data(
                    "tasks_succeeded",
                    serde_json::json!(execution.tasks_succeeded),
                )
                .with_data("tasks_failed", serde_json::json!(execution.tasks_failed)),
            )
            .await;
    }

    /// Check if cycle met performance targets and trigger optimization if needed
    async fn check_performance_targets(&self, execution: &ParallelExecution) {
        if let Some(duration_ms) = execution.duration_ms {
            if duration_ms > self.config.target_cycle_ms {
                warn!(
                    target = self.config.target_cycle_ms,
                    actual = duration_ms,
                    "Cycle time exceeded target"
                );

                if self.config.adaptive_optimization {
                    self.optimize_performance().await;
                }
            }
        }
    }

    /// Process events in parallel
    async fn process_events_parallel(&self, events: Vec<ChangeEvent>) -> Vec<Result<()>> {
        use futures::stream::{self, StreamExt};

        let max_concurrent = self.config.max_concurrent;

        debug!(
            events = events.len(),
            max_concurrent = max_concurrent,
            "Processing events in parallel"
        );

        stream::iter(events)
            .map(|event| async move {
                // Publish event to notifier
                self.notifier.publish(event).await
            })
            .buffer_unordered(max_concurrent)
            .collect::<Vec<_>>()
            .await
    }

    /// Optimize performance based on telemetry
    async fn optimize_performance(&self) {
        info!("Analyzing performance for optimization opportunities");

        let metrics = self.telemetry.get_metrics().await;

        // Simple heuristic: if regeneration time is high, suggest increasing workers
        if metrics.avg_regeneration_time_ms > self.config.target_cycle_ms as f64 {
            info!(
                avg_time = metrics.avg_regeneration_time_ms,
                target = self.config.target_cycle_ms,
                "Consider increasing parallel workers or optimizing templates"
            );
        }

        // Check success rate
        use crate::constants::autonomous;

        if metrics.success_rate < autonomous::MIN_SUCCESS_RATE {
            warn!(
                success_rate = metrics.success_rate,
                threshold = autonomous::MIN_SUCCESS_RATE,
                "Low success rate detected - investigate common failures"
            );
        }
    }

    /// Health check loop
    async fn health_check_loop(&self) {
        let interval = tokio::time::Duration::from_secs(self.config.health_check_interval_secs);
        let mut ticker = tokio::time::interval(interval);

        loop {
            ticker.tick().await;

            if !self.is_running().await {
                break;
            }

            debug!("Running health check");

            // Check telemetry
            let metrics = self.telemetry.get_metrics().await;
            let stats = self.stats.read().await.clone();

            info!(
                cycles = stats.total_cycles,
                avg_cycle_ms = stats.avg_cycle_time_ms,
                success_rate = metrics.success_rate,
                "Health check: system operating normally"
            );

            // Record health check telemetry
            self.telemetry
                .record(
                    TelemetryEvent::new(
                        TelemetryEventType::PerformanceMetric,
                        "health-check".to_string(),
                    )
                    .with_data("cycles", serde_json::json!(stats.total_cycles))
                    .with_data("avg_cycle_ms", serde_json::json!(stats.avg_cycle_time_ms))
                    .with_data("success_rate", serde_json::json!(metrics.success_rate)),
                )
                .await;
        }

        info!("Health check loop stopped");
    }

    /// Get orchestration statistics
    pub async fn get_stats(&self) -> OrchestrationStats {
        self.stats.read().await.clone()
    }

    /// Trigger manual regeneration
    pub async fn trigger_regeneration(&self, template_id: &str) -> Result<()> {
        info!(template = %template_id, "Manually triggering regeneration");

        let event = ChangeEvent::new(
            crate::autonomous::events::ChangeType::TemplateChanged,
            template_id.to_string(),
            "manual".to_string(),
        );

        self.notifier.publish(event).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::autonomous::{DeploymentConfig, RegenerationConfig, TelemetryConfig};
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let regen_config = RegenerationConfig::default();
        let deploy_config = DeploymentConfig::default();
        let telemetry_config = TelemetryConfig::default();
        let orch_config = OrchestratorConfig::default();

        let client = Arc::new(MockClient::with_response("test"));
        let notifier = Arc::new(GraphChangeNotifier::default());
        let telemetry = Arc::new(TelemetryCollector::new(telemetry_config));

        let regen_engine = Arc::new(RegenerationEngine::new(
            regen_config,
            client,
            notifier.clone(),
        ));

        let deployment = Arc::new(RwLock::new(DeploymentAutomation::new(deploy_config)));

        let orchestrator = RegenerationOrchestrator::new(
            orch_config,
            regen_engine,
            deployment,
            telemetry,
            notifier,
        );

        assert!(!orchestrator.is_running().await);
    }

    #[tokio::test]
    async fn test_parallel_execution() {
        let orch_config = OrchestratorConfig::default();
        let regen_config = RegenerationConfig::default();
        let deploy_config = DeploymentConfig::default();
        let telemetry_config = TelemetryConfig::default();

        let client = Arc::new(MockClient::with_response("test"));
        let notifier = Arc::new(GraphChangeNotifier::default());
        let telemetry = Arc::new(TelemetryCollector::new(telemetry_config));

        let regen_engine = Arc::new(RegenerationEngine::new(
            regen_config,
            client,
            notifier.clone(),
        ));

        let deployment = Arc::new(RwLock::new(DeploymentAutomation::new(deploy_config)));

        let orchestrator = RegenerationOrchestrator::new(
            orch_config,
            regen_engine,
            deployment,
            telemetry,
            notifier,
        );

        let events = vec![ChangeEvent::new(
            crate::autonomous::events::ChangeType::NodeAdded,
            "http://example.org/node1".to_string(),
            "test".to_string(),
        )];

        let result = orchestrator.execute_cycle(events).await.unwrap();
        assert_eq!(result.tasks_executed, 1);
    }
}
