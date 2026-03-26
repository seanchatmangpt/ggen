//! Chaos injection engine for random failure injection
//!
//! Injects failures into running systems:
//! - Panics (component crashes)
//! - Network delays (latency + jitter)
//! - Clock skew (time manipulation)
//! - Task killing (process termination)

use crate::chaos::event_store::{EventStore, FailureEvent, InMemoryEventStore};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;

/// Configuration for chaos injection
#[derive(Clone, Debug)]
pub struct ChaosConfig {
    /// Enable panic injection
    pub enable_panics: bool,
    /// Enable network chaos
    pub enable_network: bool,
    /// Enable clock skew
    pub enable_clock_skew: bool,
    /// Base latency for network injection (ms)
    pub base_latency_ms: u64,
    /// Max jitter (ms)
    pub max_jitter_ms: u64,
    /// Probability of failure (0.0 - 1.0)
    pub failure_probability: f64,
}

impl Default for ChaosConfig {
    fn default() -> Self {
        ChaosConfig {
            enable_panics: true,
            enable_network: true,
            enable_clock_skew: true,
            base_latency_ms: 0,
            max_jitter_ms: 100,
            failure_probability: 0.1,
        }
    }
}

/// Chaos engine for injecting failures
pub struct ChaosEngine {
    config: ChaosConfig,
    event_store: Arc<tokio::sync::Mutex<Box<dyn EventStore>>>,
    active_failures: Arc<tokio::sync::Mutex<Vec<ActiveFailure>>>,
    is_active: Arc<AtomicBool>,
}

struct ActiveFailure {
    component: String,
    failure_type: String,
    start_time: u64,
    duration_ms: u64,
}

impl ChaosEngine {
    /// Create new chaos engine with config
    pub fn new(config: ChaosConfig) -> Self {
        ChaosEngine {
            config,
            event_store: Arc::new(tokio::sync::Mutex::new(Box::new(InMemoryEventStore::new()))),
            active_failures: Arc::new(tokio::sync::Mutex::new(Vec::new())),
            is_active: Arc::new(AtomicBool::new(true)),
        }
    }

    /// Create with default config
    pub fn default_engine() -> Self {
        Self::new(ChaosConfig::default())
    }

    /// Inject panic into component
    pub async fn inject_panic(&self, component: &str) -> Result<(), String> {
        if !self.config.enable_panics {
            return Ok(());
        }

        let event = FailureEvent::PanicOccurred {
            component: component.to_string(),
            backtrace: Self::capture_backtrace(),
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Simulate network partition
    pub async fn inject_network_partition(
        &self, node: &str, duration: Duration,
    ) -> Result<(), String> {
        if !self.config.enable_network {
            return Ok(());
        }

        let duration_ms = duration.as_millis() as u64;
        let event = FailureEvent::NetworkPartition {
            node: node.to_string(),
            duration_ms,
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        // Simulate the partition delay
        sleep(duration).await;

        Ok(())
    }

    /// Simulate network latency
    pub async fn inject_network_delay(&self) -> Result<(), String> {
        if !self.config.enable_network {
            return Ok(());
        }

        use rand::Rng;
        let mut rng = rand::thread_rng();
        let jitter = rng.gen_range(0..=self.config.max_jitter_ms);
        let total_latency = self.config.base_latency_ms + jitter;

        sleep(Duration::from_millis(total_latency)).await;

        Ok(())
    }

    /// Simulate clock skew (time advancement)
    pub async fn inject_clock_skew(&self, amount: Duration) -> Result<(), String> {
        if !self.config.enable_clock_skew {
            return Ok(());
        }

        let amount_ms = amount.as_millis() as u64;
        let event = FailureEvent::ClockSkew {
            amount_ms,
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Kill a task
    pub async fn kill_task(&self, task_id: &str, component: &str) -> Result<(), String> {
        let event = FailureEvent::TaskKilled {
            task_id: task_id.to_string(),
            component: component.to_string(),
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Simulate lock timeout
    pub async fn inject_lock_timeout(
        &self, component: &str, timeout: Duration,
    ) -> Result<(), String> {
        let duration = timeout.as_millis() as u64;
        let event = FailureEvent::LockTimeout {
            component: component.to_string(),
            duration,
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Simulate resource exhaustion
    pub async fn inject_resource_exhaustion(
        &self, component: &str, resource_type: &str, available: u64, required: u64,
    ) -> Result<(), String> {
        let event = FailureEvent::ResourceExhaustion {
            resource_type: resource_type.to_string(),
            component: component.to_string(),
            available,
            required,
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Start cascading failure
    pub async fn start_cascading_failure(
        &self, root_cause: &str, affected_components: Vec<String>,
    ) -> Result<(), String> {
        let event = FailureEvent::CascadingFailureStart {
            root_cause: root_cause.to_string(),
            affected_components,
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Record recovery action
    pub async fn record_recovery(
        &self, action: &str, component: &str, success: bool,
    ) -> Result<(), String> {
        let event = FailureEvent::RecoveryAction {
            action: action.to_string(),
            component: component.to_string(),
            success,
            timestamp: self.current_timestamp(),
        };

        let mut store = self.event_store.lock().await;
        store.record(event)?;

        Ok(())
    }

    /// Get all recorded events
    pub async fn get_events(&self) -> Vec<FailureEvent> {
        self.event_store.lock().await.get_all()
    }

    /// Get events for component
    pub async fn get_component_events(&self, component: &str) -> Vec<FailureEvent> {
        self.event_store.lock().await.get_for_component(component)
    }

    /// Export event log as JSON
    pub async fn export_events_json(&self) -> Result<String, String> {
        self.event_store.lock().await.export_json()
    }

    /// Disable chaos injection
    pub fn disable(&self) {
        self.is_active.store(false, Ordering::SeqCst);
    }

    /// Enable chaos injection
    pub fn enable(&self) {
        self.is_active.store(true, Ordering::SeqCst);
    }

    /// Check if active
    pub fn is_active(&self) -> bool {
        self.is_active.load(Ordering::SeqCst)
    }

    fn current_timestamp(&self) -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64
    }

    fn capture_backtrace() -> String {
        // In real implementation, would capture actual backtrace
        "backtrace not captured in test".to_string()
    }
}

/// Panic injection builder
pub struct PanicInjection {
    component: String,
    message: String,
}

impl PanicInjection {
    pub fn new(component: String) -> Self {
        PanicInjection {
            component,
            message: "Injected panic".to_string(),
        }
    }

    pub fn with_message(mut self, message: String) -> Self {
        self.message = message;
        self
    }

    pub fn trigger(&self) -> ! {
        panic!("[{}] {}", self.component, self.message);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_chaos_engine_creation() {
        let engine = ChaosEngine::default_engine();
        assert!(engine.is_active());
    }

    #[tokio::test]
    async fn test_inject_panic() {
        let engine = ChaosEngine::default_engine();
        assert!(engine.inject_panic("test-component").await.is_ok());

        let events = engine.get_events().await;
        assert_eq!(events.len(), 1);
    }

    #[tokio::test]
    async fn test_inject_network_partition() {
        let engine = ChaosEngine::default_engine();
        let duration = Duration::from_millis(100);
        assert!(engine
            .inject_network_partition("node-1", duration)
            .await
            .is_ok());

        let events = engine.get_events().await;
        assert_eq!(events.len(), 1);
    }

    #[tokio::test]
    async fn test_kill_task() {
        let engine = ChaosEngine::default_engine();
        assert!(engine.kill_task("task-123", "executor").await.is_ok());

        let events = engine.get_events().await;
        assert_eq!(events.len(), 1);
    }

    #[tokio::test]
    async fn test_enable_disable() {
        let engine = ChaosEngine::default_engine();
        assert!(engine.is_active());
        engine.disable();
        assert!(!engine.is_active());
        engine.enable();
        assert!(engine.is_active());
    }
}
