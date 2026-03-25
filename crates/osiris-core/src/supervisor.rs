//! Supervisor Pattern - Joe Armstrong's Erlang-style fault tolerance for Rust
//!
//! Implements the supervisor pattern for automatic component restart and recovery.
//! Based on Erlang/OTP's supervision tree model.

use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tracing::{debug, error, info, warn};

use crate::error::{OSIRISError, Result};
use crate::signals::{OSIRISSignal, SignalLevel};

/// Restart strategy for supervised components
#[derive(Debug, Clone)]
pub enum RestartStrategy {
    /// Transient: restart only on abnormal termination
    Transient {
        /// Maximum retry attempts (None = unlimited)
        max_retries: Option<u32>,
        /// Backoff strategy between retries
        backoff: BackoffStrategy,
    },
    /// Permanent: always restart, even on normal termination
    Permanent {
        /// Maximum retry attempts (None = unlimited)
        max_retries: Option<u32>,
        /// Backoff strategy between retries
        backoff: BackoffStrategy,
    },
    /// Temporary: try a few times then give up
    Temporary {
        /// Maximum retry attempts before giving up
        max_retries: u32,
        /// Backoff strategy between retries
        backoff: BackoffStrategy,
    },
}

impl Default for RestartStrategy {
    fn default() -> Self {
        Self::Transient {
            max_retries: Some(5),
            backoff: BackoffStrategy::default(),
        }
    }
}

/// Backoff strategy for retry timing
#[derive(Debug, Clone)]
pub enum BackoffStrategy {
    /// No delay between retries
    None,
    /// Fixed delay between retries
    Fixed { delay_ms: u64 },
    /// Exponential backoff: delay * multiplier^attempt
    Exponential {
        initial_delay_ms: u64,
        multiplier: f64,
        max_delay_ms: u64,
    },
}

impl Default for BackoffStrategy {
    fn default() -> Self {
        Self::Exponential {
            initial_delay_ms: 100,
            multiplier: 2.0,
            max_delay_ms: 30000,
        }
    }
}

impl BackoffStrategy {
    /// Calculate delay for the given attempt number
    pub fn delay_for_attempt(&self, attempt: u32) -> Duration {
        match self {
            BackoffStrategy::None => Duration::ZERO,
            BackoffStrategy::Fixed { delay_ms } => Duration::from_millis(*delay_ms),
            BackoffStrategy::Exponential {
                initial_delay_ms,
                multiplier,
                max_delay_ms,
            } => {
                let delay = (*initial_delay_ms as f64) * multiplier.powi(attempt as i32);
                let delay_ms = delay.min(*max_delay_ms as f64) as u64;
                Duration::from_millis(delay_ms)
            }
        }
    }
}

/// Trait for components that can be supervised and restarted
#[async_trait]
pub trait Restartable: Send + Sync {
    /// Identifier for this component
    fn id(&self) -> &str;

    /// Start or restart the component
    async fn start(&mut self) -> Result<()>;

    /// Stop the component
    async fn stop(&mut self) -> Result<()>;

    /// Check if component is still healthy/running
    async fn is_healthy(&self) -> bool;

    /// Get current restart count
    fn restart_count(&self) -> u32 {
        0
    }
}

/// Child process information
#[derive(Debug, Clone)]
struct ChildSpec {
    /// Component ID
    id: String,
    /// Restart strategy
    strategy: RestartStrategy,
    /// Current restart count
    restart_count: u32,
    /// Last restart timestamp
    last_restart: Option<chrono::DateTime<chrono::Utc>>,
    /// Current state
    state: ChildState,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ChildState {
    Starting,
    Running,
    Restarting,
    Dead,
}

/// Supervised child process handle
pub struct ChildHandle {
    id: String,
    join_handle: JoinHandle<()>,
}

impl ChildHandle {
    /// Get the child's ID
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Check if the child task is finished
    pub fn is_finished(&self) -> bool {
        self.join_handle.is_finished()
    }
}

/// Supervisor: monitors children and restarts them on failure
pub struct SupervisorTree {
    /// Children being supervised
    children: Arc<RwLock<std::collections::HashMap<String, ChildSpec>>>,
    /// Active task handles
    handles: Arc<RwLock<std::collections::HashMap<String, ChildHandle>>>,
    /// Supervisor configuration
    config: SupervisorConfig,
}

/// Supervisor configuration
#[derive(Debug, Clone)]
pub struct SupervisorConfig {
    /// Supervision strategy
    pub strategy: SupervisionStrategy,
    /// Intensity: max restarts
    pub intensity: u32,
    /// Period in seconds for intensity check
    pub period: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum SupervisionStrategy {
    /// One for one: restart only failed child
    OneForOne,
    /// One for all: restart all children if one fails
    OneForAll,
    /// Rest for one: restart failed and all later children
    RestForOne,
}

impl Default for SupervisorConfig {
    fn default() -> Self {
        Self {
            strategy: SupervisionStrategy::OneForOne,
            intensity: 5,
            period: 60,
        }
    }
}

impl SupervisorTree {
    /// Create a new supervisor
    pub fn new(config: SupervisorConfig) -> Self {
        Self {
            children: Arc::new(RwLock::new(std::collections::HashMap::new())),
            handles: Arc::new(RwLock::new(std::collections::HashMap::new())),
            config,
        }
    }

    /// Create a supervisor with default config (OneForOne, 5 intensity, 60s period)
    pub fn default_tree() -> Self {
        Self::new(SupervisorConfig::default())
    }

    /// Register a child component with a restart strategy
    pub async fn register_child(&self, id: String, strategy: RestartStrategy) -> Result<()> {
        let mut children = self.children.write().await;

        if children.contains_key(&id) {
            return Err(OSIRISError::ConfigurationError(format!(
                "Child {} already registered",
                &id
            )));
        }

        info!("Registered child component for supervision: {}", &id);
        children.insert(
            id,
            ChildSpec {
                id: String::new(), // Will be set later
                strategy,
                restart_count: 0,
                last_restart: None,
                state: ChildState::Starting,
            },
        );
        Ok(())
    }

    /// Spawn a monitored task with automatic restart
    pub async fn spawn_monitored<F>(
        &self, id: &str, init_fn: F, strategy: RestartStrategy,
    ) -> Result<()>
    where
        F: Fn() -> tokio::task::JoinHandle<Result<()>> + Send + Sync + 'static,
    {
        let id_str = id.to_string();
        self.register_child(id_str.clone(), strategy).await?;

        let children = Arc::clone(&self.children);
        let handles = Arc::clone(&self.handles);
        let config = self.config.clone();
        let handles_clone = Arc::clone(&handles);

        let supervision_task = tokio::spawn(async move {
            Self::run_supervision_loop(id_str, Box::new(init_fn), children, handles_clone, config)
                .await;
        });

        let mut handles_guard = handles.write().await;
        handles_guard.insert(
            id.to_string(),
            ChildHandle {
                id: id.to_string(),
                join_handle: supervision_task,
            },
        );

        Ok(())
    }

    /// Run supervision loop for a child
    async fn run_supervision_loop(
        id: String, init_fn: Box<dyn Fn() -> tokio::task::JoinHandle<Result<()>> + Send>,
        children: Arc<RwLock<std::collections::HashMap<String, ChildSpec>>>,
        _handles: Arc<RwLock<std::collections::HashMap<String, ChildHandle>>>,
        _config: SupervisorConfig,
    ) {
        loop {
            // Get current strategy
            let strategy = {
                let children_guard = children.read().await;
                children_guard.get(&id).map(|spec| spec.strategy.clone())
            };

            let Some(strategy) = strategy else {
                warn!("Child {} removed from supervision", id);
                break;
            };

            // Start the task
            info!("Starting child task: {}", id);
            let task = init_fn();

            // Wait for task completion
            let result = task.await;

            // Update state based on result
            let should_restart = {
                let mut children_guard = children.write().await;
                if let Some(spec) = children_guard.get_mut(&id) {
                    let is_abnormal_failure = result.is_err();

                    match &strategy {
                        RestartStrategy::Transient {
                            max_retries,
                            backoff,
                        } => {
                            // Restart only on abnormal termination
                            if is_abnormal_failure {
                                let should_retry = if let Some(max) = max_retries {
                                    spec.restart_count < *max
                                } else {
                                    true
                                };

                                if should_retry {
                                    spec.restart_count += 1;
                                    spec.last_restart = Some(chrono::Utc::now());
                                    spec.state = ChildState::Restarting;

                                    let delay = backoff.delay_for_attempt(spec.restart_count - 1);
                                    info!(
                                        "Restarting transient child {} (attempt {}), backoff: {:?}",
                                        id, spec.restart_count, delay
                                    );
                                    tokio::time::sleep(delay).await;
                                    true
                                } else {
                                    error!(
                                        "Transient child {} failed after {} retries, giving up",
                                        id, spec.restart_count
                                    );
                                    spec.state = ChildState::Dead;
                                    false
                                }
                            } else {
                                debug!("Transient child {} terminated normally", id);
                                false
                            }
                        }
                        RestartStrategy::Permanent {
                            max_retries,
                            backoff,
                        } => {
                            let should_retry = if let Some(max) = max_retries {
                                spec.restart_count < *max
                            } else {
                                true
                            };

                            if should_retry {
                                spec.restart_count += 1;
                                spec.last_restart = Some(chrono::Utc::now());
                                spec.state = ChildState::Restarting;

                                let delay = backoff.delay_for_attempt(spec.restart_count - 1);
                                info!(
                                    "Restarting permanent child {} (attempt {}), backoff: {:?}",
                                    id, spec.restart_count, delay
                                );
                                tokio::time::sleep(delay).await;
                                true
                            } else {
                                error!(
                                    "Permanent child {} reached max retries ({})",
                                    id, spec.restart_count
                                );
                                spec.state = ChildState::Dead;
                                false
                            }
                        }
                        RestartStrategy::Temporary {
                            max_retries,
                            backoff,
                        } => {
                            if spec.restart_count < *max_retries {
                                spec.restart_count += 1;
                                spec.last_restart = Some(chrono::Utc::now());
                                spec.state = ChildState::Restarting;

                                let delay = backoff.delay_for_attempt(spec.restart_count - 1);
                                info!(
                                    "Restarting temporary child {} (attempt {} of {}), backoff: {:?}",
                                    id, spec.restart_count, max_retries, delay
                                );
                                tokio::time::sleep(delay).await;
                                true
                            } else {
                                error!(
                                    "Temporary child {} reached max attempts ({}), giving up",
                                    id, max_retries
                                );
                                spec.state = ChildState::Dead;
                                false
                            }
                        }
                    }
                } else {
                    false
                }
            };

            if !should_restart {
                break;
            }
        }
    }

    /// Get statistics for a child
    pub async fn get_child_stats(&self, id: &str) -> Result<ChildStats> {
        let children = self.children.read().await;

        children
            .get(id)
            .map(|spec| ChildStats {
                id: id.to_string(),
                restart_count: spec.restart_count,
                last_restart: spec.last_restart,
                state: format!("{:?}", spec.state),
            })
            .ok_or_else(|| OSIRISError::DomainNotFound(format!("Child {} not found", id)))
    }

    /// Get all children statistics
    pub async fn get_all_children_stats(&self) -> Vec<ChildStats> {
        let children = self.children.read().await;

        children
            .iter()
            .map(|(id, spec)| ChildStats {
                id: id.clone(),
                restart_count: spec.restart_count,
                last_restart: spec.last_restart,
                state: format!("{:?}", spec.state),
            })
            .collect()
    }

    /// Check supervisor health
    pub async fn get_health(&self) -> SupervisorHealth {
        let children = self.children.read().await;
        let handles = self.handles.read().await;

        let total_children = children.len();
        let dead_children = children
            .iter()
            .filter(|(_, spec)| spec.state == ChildState::Dead)
            .count();
        let active_handles = handles.iter().filter(|(_, h)| !h.is_finished()).count();

        SupervisorHealth {
            total_children,
            dead_children,
            active_handles,
            is_healthy: dead_children == 0,
        }
    }

    /// Emit supervision signal
    pub fn create_signal(&self, id: &str, level: SignalLevel) -> OSIRISSignal {
        OSIRISSignal::with_routing(
            "supervision_event".to_string(),
            format!("Supervision event for child: {}", id),
            level,
            Some("supervisor".to_string()),
            Some(id.to_string()),
        )
    }
}

/// Child statistics
#[derive(Debug, Clone)]
pub struct ChildStats {
    /// Child ID
    pub id: String,
    /// Number of restarts
    pub restart_count: u32,
    /// Last restart time
    pub last_restart: Option<chrono::DateTime<chrono::Utc>>,
    /// Current state
    pub state: String,
}

/// Supervisor health status
#[derive(Debug, Clone)]
pub struct SupervisorHealth {
    /// Total number of children
    pub total_children: usize,
    /// Number of dead children
    pub dead_children: usize,
    /// Number of active task handles
    pub active_handles: usize,
    /// Is supervisor healthy
    pub is_healthy: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backoff_strategy_none() {
        let strategy = BackoffStrategy::None;
        assert_eq!(strategy.delay_for_attempt(0), Duration::ZERO);
        assert_eq!(strategy.delay_for_attempt(10), Duration::ZERO);
    }

    #[test]
    fn test_backoff_strategy_fixed() {
        let strategy = BackoffStrategy::Fixed { delay_ms: 100 };
        assert_eq!(strategy.delay_for_attempt(0), Duration::from_millis(100));
        assert_eq!(strategy.delay_for_attempt(5), Duration::from_millis(100));
    }

    #[test]
    fn test_backoff_strategy_exponential() {
        let strategy = BackoffStrategy::Exponential {
            initial_delay_ms: 100,
            multiplier: 2.0,
            max_delay_ms: 10000,
        };

        // 100 ms
        assert_eq!(strategy.delay_for_attempt(0), Duration::from_millis(100));
        // 200 ms
        assert_eq!(strategy.delay_for_attempt(1), Duration::from_millis(200));
        // 400 ms
        assert_eq!(strategy.delay_for_attempt(2), Duration::from_millis(400));
        // Should cap at max_delay_ms
        assert!(strategy.delay_for_attempt(10) <= Duration::from_millis(10000));
    }

    #[test]
    fn test_restart_strategy_default() {
        let strategy = RestartStrategy::default();
        matches!(strategy, RestartStrategy::Transient { .. });
    }

    #[tokio::test]
    async fn test_supervisor_creation() {
        let supervisor = SupervisorTree::default_tree();
        let health = supervisor.get_health().await;

        assert_eq!(health.total_children, 0);
        assert_eq!(health.dead_children, 0);
        assert!(health.is_healthy);
    }

    #[tokio::test]
    async fn test_register_child() {
        let supervisor = SupervisorTree::default_tree();
        let result = supervisor
            .register_child(
                "test_child".to_string(),
                RestartStrategy::Transient {
                    max_retries: Some(3),
                    backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                },
            )
            .await;

        assert!(result.is_ok());

        let health = supervisor.get_health().await;
        assert_eq!(health.total_children, 1);
    }

    #[tokio::test]
    async fn test_register_duplicate_child() {
        let supervisor = SupervisorTree::default_tree();

        let _ = supervisor
            .register_child(
                "test_child".to_string(),
                RestartStrategy::Transient {
                    max_retries: Some(3),
                    backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                },
            )
            .await;

        let result = supervisor
            .register_child(
                "test_child".to_string(),
                RestartStrategy::Transient {
                    max_retries: Some(3),
                    backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                },
            )
            .await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_child_stats() {
        let supervisor = SupervisorTree::default_tree();

        supervisor
            .register_child(
                "test_child".to_string(),
                RestartStrategy::Transient {
                    max_retries: Some(3),
                    backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                },
            )
            .await
            .unwrap();

        let stats = supervisor.get_child_stats("test_child").await.unwrap();
        assert_eq!(stats.id, "test_child");
        assert_eq!(stats.restart_count, 0);
    }

    #[tokio::test]
    async fn test_get_all_children_stats() {
        let supervisor = SupervisorTree::default_tree();

        for i in 0..3 {
            supervisor
                .register_child(
                    format!("child_{}", i),
                    RestartStrategy::Transient {
                        max_retries: Some(3),
                        backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                    },
                )
                .await
                .unwrap();
        }

        let all_stats = supervisor.get_all_children_stats().await;
        assert_eq!(all_stats.len(), 3);
    }

    #[tokio::test]
    async fn test_supervisor_signal() {
        let supervisor = SupervisorTree::default_tree();
        let signal = supervisor.create_signal("test_child", SignalLevel::Warning);

        assert_eq!(signal.signal_type, "supervision_event");
        assert_eq!(signal.source, Some("supervisor".to_string()));
        assert_eq!(signal.target, Some("test_child".to_string()));
    }
}
