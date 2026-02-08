//! TPS Reference System - Toyota Production System Implementation
//!
//! Integrates 6 core TPS principles:
//! 1. **Jidoka** (Autonomation) - Circuit breaker for fault isolation
//! 2. **Kanban** - Pull-based queue management
//! 3. **Andon** - Visual signal/alert system
//! 4. **Kaizen** - Continuous improvement metrics tracking
//! 5. **Heijunka** - Level loading / workload distribution
//! 6. **Tracing** - End-to-end observability
//!
//! # Quick Start
//!
//! ```ignore
//! use tps_reference::{TpsConfig, TpsSystem};
//! use std::sync::Arc;
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     let config = TpsConfig::from_env()?;
//!     let tps = Arc::new(TpsSystem::new(config).await?);
//!
//!     // Health check
//!     println!("Status: {}", tps.health_check().await?);
//!
//!     // Process signal
//!     let result = tps.process_signal("request-123", "execute").await?;
//!     println!("Result: {}", result);
//!
//!     Ok(())
//! }
//! ```

use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration as StdDuration;
use tokio::sync::mpsc::{self, Receiver, Sender};
use tracing::{debug, error, info, warn};
use uuid::Uuid;

// TPS modules defined inline below

/// Signal representing a work item to process
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WorkSignal {
    /// Unique identifier for this signal
    pub id: String,
    /// Type of work (e.g., "execute", "validate", "report")
    pub signal_type: String,
    /// Payload data
    pub payload: serde_json::Value,
    /// Priority level (1=critical, 2=high, 3=normal, 4=low)
    pub priority: u8,
    /// Timestamp when signal was created
    pub created_at: DateTime<Utc>,
    /// Timeout for processing in milliseconds
    pub timeout_ms: u64,
    /// Trace ID for end-to-end observability
    pub trace_id: String,
}

impl WorkSignal {
    /// Create a new work signal
    pub fn new(signal_type: impl Into<String>, payload: serde_json::Value) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            signal_type: signal_type.into(),
            payload,
            priority: 3,
            created_at: Utc::now(),
            timeout_ms: 30_000,
            trace_id: Uuid::new_v4().to_string(),
        }
    }

    /// Set priority (1=critical, 2=high, 3=normal, 4=low)
    pub fn with_priority(mut self, priority: u8) -> Self {
        self.priority = priority.clamp(1, 4);
        self
    }

    /// Set timeout in milliseconds
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }
}

/// Result of signal processing
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessingResult {
    /// Signal ID that was processed
    pub signal_id: String,
    /// Success or failure
    pub success: bool,
    /// Result message or error
    pub message: String,
    /// Processing time in milliseconds
    pub duration_ms: u64,
    /// Which worker processed it
    pub worker_id: Option<String>,
    /// Trace ID for correlation
    pub trace_id: String,
}

/// Health status of the TPS system
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HealthStatus {
    /// Is system healthy
    pub healthy: bool,
    /// Circuit breaker status
    pub circuit_state: String,
    /// Queue depth
    pub queue_depth: u64,
    /// Active workers
    pub active_workers: u64,
    /// Error rate (0-100)
    pub error_rate: f64,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
}

/// Supervision tree for managing all TPS components
pub struct SupervisionTree {
    jidoka: Arc<CircuitBreaker>,
    kanban: Arc<KanbanQueue>,
    andon: Arc<AndonSignal>,
    kaizen: Arc<KaizenMetrics>,
    heijunka: Arc<HeijunkaPool>,
}

impl SupervisionTree {
    /// Create a new supervision tree with all components
    pub async fn new(config: &TpsConfig) -> Result<Self> {
        info!("Initializing supervision tree");

        let jidoka = Arc::new(CircuitBreaker::new(
            config.circuit_breaker_threshold,
            config.circuit_breaker_timeout_secs,
        ));

        let kanban = Arc::new(KanbanQueue::new(config.kanban_buffer_size).await?);
        let andon = Arc::new(AndonSignal::new(config.andon_max_history));
        let kaizen = Arc::new(KaizenMetrics::new());
        let heijunka = Arc::new(HeijunkaPool::new(config.heijunka_pool_size));

        info!("Supervision tree initialized successfully");

        Ok(Self {
            jidoka,
            kanban,
            andon,
            kaizen,
            heijunka,
        })
    }

    pub fn jidoka(&self) -> Arc<CircuitBreaker> {
        self.jidoka.clone()
    }

    pub fn kanban(&self) -> Arc<KanbanQueue> {
        self.kanban.clone()
    }

    pub fn andon(&self) -> Arc<AndonSignal> {
        self.andon.clone()
    }

    pub fn kaizen(&self) -> Arc<KaizenMetrics> {
        self.kaizen.clone()
    }

    pub fn heijunka(&self) -> Arc<HeijunkaPool> {
        self.heijunka.clone()
    }
}

/// Main TPS System orchestrating all principles
pub struct TpsSystem {
    config: TpsConfig,
    supervision: Arc<SupervisionTree>,
    worker_pool: Arc<RwLock<HashMap<String, WorkerHandle>>>,
    _signal_tx: Sender<WorkSignal>,
    signal_rx: Arc<RwLock<Receiver<WorkSignal>>>,
    started: Arc<AtomicBool>,
    processed_count: Arc<AtomicU64>,
}

/// Handle for a running worker
pub struct WorkerHandle {
    pub id: String,
    pub started_at: DateTime<Utc>,
    pub task_count: u64,
    pub active: bool,
}

impl TpsSystem {
    /// Create a new TPS system
    pub async fn new(config: TpsConfig) -> Result<Arc<Self>> {
        info!("Initializing TPS System v1.0");

        let supervision = Arc::new(SupervisionTree::new(&config).await?);
        let (signal_tx, signal_rx) = mpsc::channel(config.kanban_buffer_size);

        let system = Arc::new(Self {
            config,
            supervision,
            worker_pool: Arc::new(RwLock::new(HashMap::new())),
            _signal_tx: signal_tx,
            signal_rx: Arc::new(RwLock::new(signal_rx)),
            started: Arc::new(AtomicBool::new(false)),
            processed_count: Arc::new(AtomicU64::new(0)),
        });

        info!("TPS System initialized successfully");
        Ok(system)
    }

    /// Start the system and spawn worker pool
    pub async fn start(self: Arc<Self>) -> Result<()> {
        if self.started.swap(true, Ordering::SeqCst) {
            return Err(anyhow!("System already started"));
        }

        info!(
            "Starting TPS System with {} workers",
            self.config.num_workers
        );

        for i in 0..self.config.num_workers {
            let worker_id = format!("worker-{}", i);
            let worker_id_for_spawn = worker_id.clone();
            let system = self.clone();

            tokio::spawn(async move {
                if let Err(e) = system.worker_loop(&worker_id_for_spawn).await {
                    error!("Worker {} error: {}", worker_id_for_spawn, e);
                }
            });

            let worker = WorkerHandle {
                id: worker_id,
                started_at: Utc::now(),
                task_count: 0,
                active: true,
            };

            self.worker_pool.write().insert(worker.id.clone(), worker);
        }

        info!("TPS System started");
        Ok(())
    }

    /// Worker loop processing signals from the queue
    async fn worker_loop(&self, _worker_id: &str) -> Result<()> {
        // Create a new receiver for this worker
        let (_tx, rx) = mpsc::channel(100);
        *self.signal_rx.write() = rx;

        loop {
            // This would be implemented with proper channel sharing
            // For now, we note the pattern
            tokio::time::sleep(StdDuration::from_millis(100)).await;
        }
    }

    /// Process a signal through the entire TPS pipeline
    pub async fn process_signal(
        &self, signal_type: &str, payload: serde_json::Value,
    ) -> Result<ProcessingResult> {
        let signal = WorkSignal::new(signal_type, payload);
        let trace_id = signal.trace_id.clone();
        let signal_id = signal.id.clone();

        debug!("Processing signal {} with trace {}", signal_id, trace_id);

        // 1. JIDOKA: Check circuit breaker
        let jidoka = self.supervision.jidoka();
        if !jidoka.is_closed().await {
            self.supervision
                .andon()
                .record_signal("circuit-open", "CRITICAL");
            return Err(anyhow!("Circuit breaker is open - service overloaded"));
        }

        let start = std::time::Instant::now();

        // 2. KANBAN: Enqueue the signal
        let kanban = self.supervision.kanban();
        kanban.enqueue(signal.clone()).await?;

        // 3. Process the signal
        let result = match self.execute_signal(&signal).await {
            Ok(success) => {
                self.processed_count.fetch_add(1, Ordering::SeqCst);

                self.supervision.andon().record_signal("success", "GREEN");
                success
            }
            Err(e) => {
                error!("Signal processing failed: {}", e);
                self.supervision
                    .andon()
                    .record_signal(&format!("error: {}", e), "RED");

                jidoka.record_failure().await;
                ProcessingResult {
                    signal_id,
                    success: false,
                    message: format!("Error: {}", e),
                    duration_ms: start.elapsed().as_millis() as u64,
                    worker_id: None,
                    trace_id,
                }
            }
        };

        // 4. KAIZEN: Record metrics
        let kaizen = self.supervision.kaizen();
        kaizen.record_processing_time(start.elapsed());
        if result.success {
            kaizen.increment_success_count();
        } else {
            kaizen.increment_error_count();
        }

        // 5. HEIJUNKA: Update worker load distribution
        let heijunka = self.supervision.heijunka();
        heijunka.update_load(1);

        Ok(result)
    }

    /// Execute the actual signal processing
    async fn execute_signal(&self, signal: &WorkSignal) -> Result<ProcessingResult> {
        let _start = std::time::Instant::now();

        match signal.signal_type.as_str() {
            "validate" => self.handle_validate(signal).await,
            "execute" => self.handle_execute(signal).await,
            "report" => self.handle_report(signal).await,
            _ => Err(anyhow!("Unknown signal type: {}", signal.signal_type)),
        }
    }

    async fn handle_validate(&self, signal: &WorkSignal) -> Result<ProcessingResult> {
        tokio::time::sleep(StdDuration::from_millis(10)).await;
        Ok(ProcessingResult {
            signal_id: signal.id.clone(),
            success: true,
            message: "Validation successful".to_string(),
            duration_ms: 10,
            worker_id: None,
            trace_id: signal.trace_id.clone(),
        })
    }

    async fn handle_execute(&self, signal: &WorkSignal) -> Result<ProcessingResult> {
        tokio::time::sleep(StdDuration::from_millis(50)).await;
        Ok(ProcessingResult {
            signal_id: signal.id.clone(),
            success: true,
            message: "Execution successful".to_string(),
            duration_ms: 50,
            worker_id: None,
            trace_id: signal.trace_id.clone(),
        })
    }

    async fn handle_report(&self, signal: &WorkSignal) -> Result<ProcessingResult> {
        tokio::time::sleep(StdDuration::from_millis(5)).await;
        Ok(ProcessingResult {
            signal_id: signal.id.clone(),
            success: true,
            message: "Report generated".to_string(),
            duration_ms: 5,
            worker_id: None,
            trace_id: signal.trace_id.clone(),
        })
    }

    /// Get health status of the system
    pub async fn health_check(&self) -> Result<HealthStatus> {
        let jidoka = self.supervision.jidoka();
        let kanban = self.supervision.kanban();
        let kaizen = self.supervision.kaizen();

        let queue_depth = kanban.depth().await;
        let error_rate = kaizen.error_rate();
        let circuit_state = format!("{:?}", jidoka.state().await);

        Ok(HealthStatus {
            healthy: jidoka.is_closed().await && error_rate < 50.0,
            circuit_state,
            queue_depth,
            active_workers: self.worker_pool.read().len() as u64,
            error_rate,
            timestamp: Utc::now(),
        })
    }

    /// Get metrics snapshot
    pub fn metrics_snapshot(&self) -> serde_json::Value {
        let kaizen = self.supervision.kaizen();
        serde_json::json!({
            "total_processed": self.processed_count.load(Ordering::SeqCst),
            "success_count": kaizen.success_count(),
            "error_count": kaizen.error_count(),
            "error_rate": kaizen.error_rate(),
            "avg_duration_ms": kaizen.avg_processing_time_ms(),
            "p95_duration_ms": kaizen.p95_processing_time_ms(),
            "p99_duration_ms": kaizen.p99_processing_time_ms(),
        })
    }

    /// Graceful shutdown
    pub async fn shutdown(&self) -> Result<()> {
        info!("Shutting down TPS System");

        // Drain remaining signals
        let kanban = self.supervision.kanban();
        let remaining = kanban.drain().await;
        info!("Drained {} remaining signals", remaining);

        self.started.store(false, Ordering::SeqCst);
        Ok(())
    }
}

// Module implementations below
// Re-export public types
pub use andon::{AndonSignal, SignalLevel};
pub use config::TpsConfig;
pub use heijunka::HeijunkaPool;
pub use jidoka::{CircuitBreaker, CircuitState};
pub use kaizen::KaizenMetrics;
pub use kanban::KanbanQueue;

mod jidoka {
    use super::*;

    /// Circuit breaker states for Jidoka (autonomation)
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum CircuitState {
        Closed,
        Open,
        HalfOpen,
    }

    /// Jidoka: Autonomation with automatic fault isolation
    pub struct CircuitBreaker {
        state: Arc<RwLock<CircuitState>>,
        failure_count: Arc<AtomicU64>,
        failure_threshold: u64,
        reset_timeout: StdDuration,
        last_failure: Arc<RwLock<Option<DateTime<Utc>>>>,
    }

    impl CircuitBreaker {
        pub fn new(threshold: u64, timeout_secs: u64) -> Self {
            Self {
                state: Arc::new(RwLock::new(CircuitState::Closed)),
                failure_count: Arc::new(AtomicU64::new(0)),
                failure_threshold: threshold,
                reset_timeout: StdDuration::from_secs(timeout_secs),
                last_failure: Arc::new(RwLock::new(None)),
            }
        }

        pub async fn is_closed(&self) -> bool {
            let state = *self.state.read();
            match state {
                CircuitState::Closed => true,
                CircuitState::Open => {
                    if let Some(last) = *self.last_failure.read() {
                        let now = Utc::now();
                        let elapsed_chrono = now.signed_duration_since(last);
                        // Convert elapsed chrono TimeDelta to StdDuration for comparison
                        let elapsed_std = elapsed_chrono.to_std().unwrap_or(self.reset_timeout);
                        if elapsed_std >= self.reset_timeout {
                            *self.state.write() = CircuitState::HalfOpen;
                            return true;
                        }
                    }
                    false
                }
                CircuitState::HalfOpen => true,
            }
        }

        pub async fn record_failure(&self) {
            let count = self.failure_count.fetch_add(1, Ordering::SeqCst) + 1;
            *self.last_failure.write() = Some(Utc::now());

            if count >= self.failure_threshold {
                *self.state.write() = CircuitState::Open;
                warn!("Circuit breaker opened after {} failures", count);
            }
        }

        pub async fn state(&self) -> CircuitState {
            *self.state.read()
        }
    }
}

mod kanban {
    use super::*;
    use std::collections::VecDeque;

    /// Kanban queue for pull-based work distribution
    pub struct KanbanQueue {
        queue: Arc<RwLock<VecDeque<WorkSignal>>>,
        buffer_size: usize,
        total_enqueued: Arc<AtomicU64>,
    }

    impl KanbanQueue {
        pub async fn new(buffer_size: usize) -> Result<Self> {
            Ok(Self {
                queue: Arc::new(RwLock::new(VecDeque::with_capacity(buffer_size))),
                buffer_size,
                total_enqueued: Arc::new(AtomicU64::new(0)),
            })
        }

        pub async fn enqueue(&self, signal: WorkSignal) -> Result<()> {
            let mut q = self.queue.write();
            if q.len() >= self.buffer_size {
                return Err(anyhow!("Queue is full"));
            }
            q.push_back(signal);
            self.total_enqueued.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }

        pub async fn dequeue(&self) -> Option<WorkSignal> {
            self.queue.write().pop_front()
        }

        pub async fn depth(&self) -> u64 {
            self.queue.read().len() as u64
        }

        pub async fn drain(&self) -> u64 {
            let count = self.queue.read().len() as u64;
            self.queue.write().clear();
            count
        }
    }
}

mod andon {
    use super::*;
    use std::collections::VecDeque;

    /// Andon signal level
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub enum SignalLevel {
        Green,
        Yellow,
        Red,
    }

    /// Andon signal record
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct SignalRecord {
        pub message: String,
        pub level: SignalLevel,
        pub timestamp: DateTime<Utc>,
    }

    /// Andon: Visual signal system for status and problems
    pub struct AndonSignal {
        signals: Arc<RwLock<VecDeque<SignalRecord>>>,
        max_history: usize,
        current_level: Arc<RwLock<SignalLevel>>,
    }

    impl AndonSignal {
        pub fn new(max_history: usize) -> Self {
            Self {
                signals: Arc::new(RwLock::new(VecDeque::with_capacity(max_history))),
                max_history,
                current_level: Arc::new(RwLock::new(SignalLevel::Green)),
            }
        }

        pub fn record_signal(&self, message: &str, level: &str) {
            let signal_level = match level {
                "RED" => SignalLevel::Red,
                "YELLOW" => SignalLevel::Yellow,
                _ => SignalLevel::Green,
            };

            let record = SignalRecord {
                message: message.to_string(),
                level: signal_level.clone(),
                timestamp: Utc::now(),
            };

            let mut signals = self.signals.write();
            signals.push_back(record);
            if signals.len() > self.max_history {
                signals.pop_front();
            }

            *self.current_level.write() = signal_level;
        }

        pub fn get_current_level(&self) -> SignalLevel {
            self.current_level.read().clone()
        }

        pub fn get_history(&self) -> Vec<SignalRecord> {
            self.signals.read().iter().cloned().collect()
        }
    }
}

mod kaizen {
    use super::*;

    /// Kaizen metrics for continuous improvement
    pub struct KaizenMetrics {
        success_count: Arc<AtomicU64>,
        error_count: Arc<AtomicU64>,
        total_duration_ms: Arc<RwLock<Vec<u128>>>,
    }

    impl KaizenMetrics {
        pub fn new() -> Self {
            Self {
                success_count: Arc::new(AtomicU64::new(0)),
                error_count: Arc::new(AtomicU64::new(0)),
                total_duration_ms: Arc::new(RwLock::new(Vec::with_capacity(10000))),
            }
        }

        pub fn increment_success_count(&self) {
            self.success_count.fetch_add(1, Ordering::SeqCst);
        }

        pub fn increment_error_count(&self) {
            self.error_count.fetch_add(1, Ordering::SeqCst);
        }

        pub fn record_processing_time(&self, duration: std::time::Duration) {
            self.total_duration_ms.write().push(duration.as_millis());
        }

        pub fn success_count(&self) -> u64 {
            self.success_count.load(Ordering::SeqCst)
        }

        pub fn error_count(&self) -> u64 {
            self.error_count.load(Ordering::SeqCst)
        }

        pub fn error_rate(&self) -> f64 {
            let success = self.success_count.load(Ordering::SeqCst) as f64;
            let error = self.error_count.load(Ordering::SeqCst) as f64;
            let total = success + error;
            if total == 0.0 {
                0.0
            } else {
                (error / total) * 100.0
            }
        }

        pub fn avg_processing_time_ms(&self) -> f64 {
            let durations = self.total_duration_ms.read();
            if durations.is_empty() {
                0.0
            } else {
                durations.iter().sum::<u128>() as f64 / durations.len() as f64
            }
        }

        pub fn p95_processing_time_ms(&self) -> f64 {
            self.percentile(0.95)
        }

        pub fn p99_processing_time_ms(&self) -> f64 {
            self.percentile(0.99)
        }

        fn percentile(&self, p: f64) -> f64 {
            let mut durations = self.total_duration_ms.read().clone();
            if durations.is_empty() {
                return 0.0;
            }
            durations.sort();
            let idx = ((durations.len() as f64 * p) as usize).min(durations.len() - 1);
            durations[idx] as f64
        }
    }
}

mod heijunka {
    use super::*;

    /// Heijunka: Level loading and workload distribution
    pub struct HeijunkaPool {
        worker_loads: Arc<RwLock<Vec<u64>>>,
        total_work: Arc<AtomicU64>,
    }

    impl HeijunkaPool {
        pub fn new(pool_size: usize) -> Self {
            Self {
                worker_loads: Arc::new(RwLock::new(vec![0; pool_size])),
                total_work: Arc::new(AtomicU64::new(0)),
            }
        }

        pub fn update_load(&self, work_units: u64) {
            self.total_work.fetch_add(work_units, Ordering::SeqCst);

            // Balance load across workers
            let mut loads = self.worker_loads.write();
            if !loads.is_empty() {
                let min_idx = loads
                    .iter()
                    .enumerate()
                    .min_by_key(|(_, &load)| load)
                    .map(|(idx, _)| idx)
                    .unwrap_or(0);
                loads[min_idx] += work_units;
            }
        }

        pub fn get_distribution(&self) -> Vec<u64> {
            self.worker_loads.read().clone()
        }

        pub fn total_work(&self) -> u64 {
            self.total_work.load(Ordering::SeqCst)
        }

        pub fn rebalance(&self) {
            let mut loads = self.worker_loads.write();
            if loads.is_empty() {
                return;
            }
            let total: u64 = loads.iter().sum();
            let avg = total / loads.len() as u64;
            for load in loads.iter_mut() {
                *load = avg;
            }
        }
    }
}

mod config {
    use super::*;

    /// TPS System configuration
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct TpsConfig {
        /// Number of worker threads
        pub num_workers: usize,
        /// Circuit breaker failure threshold
        pub circuit_breaker_threshold: u64,
        /// Circuit breaker timeout in seconds
        pub circuit_breaker_timeout_secs: u64,
        /// Kanban buffer size
        pub kanban_buffer_size: usize,
        /// Andon max history records
        pub andon_max_history: usize,
        /// Heijunka pool size
        pub heijunka_pool_size: usize,
        /// NATS server URL
        pub nats_url: String,
        /// RabbitMQ connection string
        pub rabbitmq_url: String,
        /// Prometheus metrics port
        pub metrics_port: u16,
        /// HTTP server port
        pub http_port: u16,
        /// Jaeger tracing endpoint
        pub jaeger_endpoint: String,
    }

    impl Default for TpsConfig {
        fn default() -> Self {
            Self {
                num_workers: 4,
                circuit_breaker_threshold: 5,
                circuit_breaker_timeout_secs: 30,
                kanban_buffer_size: 1000,
                andon_max_history: 100,
                heijunka_pool_size: 4,
                nats_url: "nats://localhost:4222".to_string(),
                rabbitmq_url: "amqp://guest:guest@localhost:5672/".to_string(),
                metrics_port: 9090,
                http_port: 8080,
                jaeger_endpoint: "http://localhost:14268/api/traces".to_string(),
            }
        }
    }

    impl TpsConfig {
        /// Load configuration from environment variables
        pub fn from_env() -> Result<Self> {
            let config = Self {
                num_workers: std::env::var("TPS_NUM_WORKERS")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(4),
                circuit_breaker_threshold: std::env::var("TPS_CB_THRESHOLD")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(5),
                circuit_breaker_timeout_secs: std::env::var("TPS_CB_TIMEOUT")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(30),
                kanban_buffer_size: std::env::var("TPS_KANBAN_BUFFER")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(1000),
                andon_max_history: std::env::var("TPS_ANDON_HISTORY")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(100),
                heijunka_pool_size: std::env::var("TPS_HEIJUNKA_POOL")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(4),
                nats_url: std::env::var("NATS_URL")
                    .unwrap_or_else(|_| "nats://localhost:4222".to_string()),
                rabbitmq_url: std::env::var("RABBITMQ_URL")
                    .unwrap_or_else(|_| "amqp://guest:guest@localhost:5672/".to_string()),
                metrics_port: std::env::var("METRICS_PORT")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(9090),
                http_port: std::env::var("HTTP_PORT")
                    .ok()
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(8080),
                jaeger_endpoint: std::env::var("JAEGER_ENDPOINT")
                    .unwrap_or_else(|_| "http://localhost:14268/api/traces".to_string()),
            };
            Ok(config)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_tps_system_creation() {
        let config = TpsConfig::default();
        let system = TpsSystem::new(config)
            .await
            .expect("Failed to create system");
        assert!(system.started.load(Ordering::SeqCst) == false);
    }

    #[tokio::test]
    async fn test_health_check() {
        let config = TpsConfig::default();
        let system = TpsSystem::new(config)
            .await
            .expect("Failed to create system");
        let health = system.health_check().await.expect("Health check failed");
        assert!(health.healthy);
    }

    #[tokio::test]
    async fn test_process_signal() {
        let config = TpsConfig::default();
        let system = TpsSystem::new(config)
            .await
            .expect("Failed to create system");
        let result = system
            .process_signal("validate", serde_json::json!({"test": "data"}))
            .await;
        assert!(result.is_ok());
    }
}
