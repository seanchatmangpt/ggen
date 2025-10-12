//! Observability layer using tracing ecosystem
//!
//! This module provides a new observability layer using the `tracing` crate
//! for ecosystem compatibility with tokio-console, flamegraphs, and distributed tracing.
//!
//! # Example
//!
//! ```rust
//! use cleanroom::observability::{ObservabilityLayer, Metrics};
//!
//! let layer = ObservabilityLayer::new()
//!     .with_tokio_console()
//!     .with_metrics_exporter();
//! environment.attach_observability(layer)?;
//! ```

use crate::error::Result;
use crate::cleanroom::CleanroomEnvironment;
use std::sync::Arc;
use std::time::Instant;
use std::collections::HashMap;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};

/// Observability layer for cleanroom environment
pub struct ObservabilityLayer {
    /// Metrics collection configuration
    metrics_config: MetricsConfig,
    /// Tracing configuration
    tracing_config: TracingConfig,
    /// Exporters for data
    exporters: Vec<Box<dyn MetricsExporter + Send + Sync>>,
    /// Span collectors
    span_collectors: Vec<Box<dyn SpanCollector + Send + Sync>>,
}

/// Metrics collection configuration
#[derive(Debug, Clone)]
pub struct MetricsConfig {
    /// Enable metrics collection
    pub enabled: bool,
    /// Collection interval
    pub collection_interval: std::time::Duration,
    /// Retention period
    pub retention_period: std::time::Duration,
    /// Maximum number of metrics to keep
    pub max_metrics: usize,
}

/// Tracing configuration
#[derive(Debug, Clone)]
pub struct TracingConfig {
    /// Enable tracing
    pub enabled: bool,
    /// Trace level
    pub level: TracingLevel,
    /// Enable tokio-console integration
    pub tokio_console: bool,
    /// Enable distributed tracing
    pub distributed_tracing: bool,
    /// Trace sampling rate (0.0 to 1.0)
    pub sampling_rate: f64,
}

/// Tracing levels
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TracingLevel {
    /// Error level
    Error,
    /// Warning level
    Warn,
    /// Info level
    Info,
    /// Debug level
    Debug,
    /// Trace level
    Trace,
}

/// Metrics data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metrics {
    /// Timestamp
    pub timestamp: Instant,
    /// Session ID
    pub session_id: uuid::Uuid,
    /// Resource usage metrics
    pub resource_usage: ResourceUsageMetrics,
    /// Performance metrics
    pub performance: PerformanceMetrics,
    /// Container metrics
    pub containers: ContainerMetrics,
    /// Test metrics
    pub tests: TestMetrics,
}

/// Resource usage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsageMetrics {
    /// CPU usage percentage
    pub cpu_usage_percent: f64,
    /// Memory usage in bytes
    pub memory_usage_bytes: u64,
    /// Disk usage in bytes
    pub disk_usage_bytes: u64,
    /// Network bytes sent
    pub network_bytes_sent: u64,
    /// Network bytes received
    pub network_bytes_received: u64,
}

/// Performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Average test execution time
    pub avg_test_execution_time: std::time::Duration,
    /// Container startup time
    pub container_startup_time: std::time::Duration,
    /// Total execution time
    pub total_execution_time: std::time::Duration,
    /// Throughput (tests per second)
    pub throughput: f64,
}

/// Container metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerMetrics {
    /// Total containers created
    pub total_created: u32,
    /// Total containers destroyed
    pub total_destroyed: u32,
    /// Currently running containers
    pub currently_running: u32,
    /// Average container lifetime
    pub avg_lifetime: std::time::Duration,
}

/// Test metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestMetrics {
    /// Total tests executed
    pub total_executed: u32,
    /// Tests passed
    pub passed: u32,
    /// Tests failed
    pub failed: u32,
    /// Success rate
    pub success_rate: f64,
    /// Average test duration
    pub avg_duration: std::time::Duration,
}

/// Span data for tracing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Span {
    /// Span ID
    pub id: String,
    /// Parent span ID
    pub parent_id: Option<String>,
    /// Span name
    pub name: String,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Duration
    pub duration: Option<std::time::Duration>,
    /// Span status
    pub status: SpanStatus,
    /// Attributes
    pub attributes: HashMap<String, String>,
    /// Events
    pub events: Vec<SpanEvent>,
}

/// Span status
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SpanStatus {
    /// Span is active
    Active,
    /// Span completed successfully
    Completed,
    /// Span failed
    Failed,
    /// Span was cancelled
    Cancelled,
}

/// Span event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanEvent {
    /// Event name
    pub name: String,
    /// Event timestamp
    pub timestamp: Instant,
    /// Event attributes
    pub attributes: HashMap<String, String>,
}

/// Metrics exporter trait
pub trait MetricsExporter: std::fmt::Debug {
    /// Export metrics
    fn export(&self, metrics: &Metrics) -> Result<()>;
    /// Get exporter name
    fn name(&self) -> &str;
}

/// Span collector trait
pub trait SpanCollector: std::fmt::Debug {
    /// Collect a span
    fn collect(&self, span: &Span) -> Result<()>;
    /// Get collector name
    fn name(&self) -> &str;
}

impl ObservabilityLayer {
    /// Create a new observability layer
    pub fn new() -> Self {
        Self {
            metrics_config: MetricsConfig {
                enabled: true,
                collection_interval: std::time::Duration::from_secs(5),
                retention_period: std::time::Duration::from_secs(3600),
                max_metrics: 1000,
            },
            tracing_config: TracingConfig {
                enabled: true,
                level: TracingLevel::Info,
                tokio_console: false,
                distributed_tracing: false,
                sampling_rate: 1.0,
            },
            exporters: Vec::new(),
            span_collectors: Vec::new(),
        }
    }

    /// Enable tokio-console integration
    pub fn with_tokio_console(mut self) -> Self {
        self.tracing_config.tokio_console = true;
        self
    }

    /// Enable distributed tracing
    pub fn with_distributed_tracing(mut self) -> Self {
        self.tracing_config.distributed_tracing = true;
        self
    }

    /// Set tracing level
    pub fn with_tracing_level(mut self, level: TracingLevel) -> Self {
        self.tracing_config.level = level;
        self
    }

    /// Set sampling rate
    pub fn with_sampling_rate(mut self, rate: f64) -> Self {
        self.tracing_config.sampling_rate = rate.clamp(0.0, 1.0);
        self
    }

    /// Add a metrics exporter
    pub fn with_exporter(mut self, exporter: Box<dyn MetricsExporter + Send + Sync>) -> Self {
        self.exporters.push(exporter);
        self
    }

    /// Add a span collector
    pub fn with_span_collector(mut self, collector: Box<dyn SpanCollector + Send + Sync>) -> Self {
        self.span_collectors.push(collector);
        self
    }

    /// Attach to a cleanroom environment
    pub fn attach(self, environment: &CleanroomEnvironment) -> Result<ObservabilityManager> {
        ObservabilityManager::new(environment, self)
    }

    /// Get metrics configuration
    pub fn metrics_config(&self) -> &MetricsConfig {
        &self.metrics_config
    }

    /// Get tracing configuration
    pub fn tracing_config(&self) -> &TracingConfig {
        &self.tracing_config
    }
}

impl Default for ObservabilityLayer {
    fn default() -> Self {
        Self::new()
    }
}

/// Observability manager for a cleanroom environment
pub struct ObservabilityManager {
    environment: Arc<CleanroomEnvironment>,
    metrics_config: MetricsConfig,
    tracing_config: TracingConfig,
    exporters: Vec<Box<dyn MetricsExporter + Send + Sync>>,
    span_collectors: Vec<Box<dyn SpanCollector + Send + Sync>>,
    metrics_history: Arc<RwLock<Vec<Metrics>>>,
    active_spans: Arc<RwLock<HashMap<String, Span>>>,
    metrics_task: Option<tokio::task::JoinHandle<()>>,
}

impl ObservabilityManager {
    /// Create a new observability manager
    fn new(environment: &CleanroomEnvironment, layer: ObservabilityLayer) -> Result<Self> {
        let manager = Self {
            environment: Arc::new(environment.clone()),
            metrics_config: layer.metrics_config,
            tracing_config: layer.tracing_config,
            exporters: layer.exporters,
            span_collectors: layer.span_collectors,
            metrics_history: Arc::new(RwLock::new(Vec::new())),
            active_spans: Arc::new(RwLock::new(HashMap::new())),
            metrics_task: None,
        };

        Ok(manager)
    }

    /// Start metrics collection
    pub async fn start_metrics_collection(&mut self) -> Result<()> {
        if !self.metrics_config.enabled {
            return Ok(());
        }

        let environment = self.environment.clone();
        let metrics_config = self.metrics_config.clone();
        let exporters = self.exporters.clone();
        let metrics_history = self.metrics_history.clone();

        let task = tokio::spawn(async move {
            let mut interval = tokio::time::interval(metrics_config.collection_interval);
            
            loop {
                interval.tick().await;
                
                // Collect metrics
                let metrics = Self::collect_metrics(&environment).await;
                
                // Export metrics
                for exporter in &exporters {
                    if let Err(e) = exporter.export(&metrics) {
                        tracing::error!("Failed to export metrics: {}", e);
                    }
                }
                
                // Store in history
                {
                    let mut history = metrics_history.write().await;
                    history.push(metrics);
                    
                    // Trim history if needed
                    if history.len() > metrics_config.max_metrics {
                        history.drain(0..history.len() - metrics_config.max_metrics);
                    }
                }
            }
        });

        self.metrics_task = Some(task);
        Ok(())
    }

    /// Stop metrics collection
    pub async fn stop_metrics_collection(&mut self) {
        if let Some(task) = self.metrics_task.take() {
            task.abort();
        }
    }

    /// Collect current metrics
    async fn collect_metrics(environment: &CleanroomEnvironment) -> Metrics {
        let env_metrics = environment.get_metrics().await;
        
        Metrics {
            timestamp: Instant::now(),
            session_id: environment.session_id(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: env_metrics.peak_cpu_usage_percent,
                memory_usage_bytes: env_metrics.peak_memory_usage_bytes,
                disk_usage_bytes: 0, // Would need to implement disk monitoring
                network_bytes_sent: 0, // Would need to implement network monitoring
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: env_metrics.average_execution_time,
                container_startup_time: std::time::Duration::from_secs(0), // Would need to implement
                total_execution_time: std::time::Duration::from_millis(env_metrics.total_duration_ms),
                throughput: env_metrics.tests_executed as f64 / (env_metrics.total_duration_ms as f64 / 1000.0),
            },
            containers: ContainerMetrics {
                total_created: env_metrics.containers_created,
                total_destroyed: env_metrics.containers_destroyed,
                currently_running: env_metrics.containers_created - env_metrics.containers_destroyed,
                avg_lifetime: std::time::Duration::from_secs(0), // Would need to implement
            },
            tests: TestMetrics {
                total_executed: env_metrics.tests_executed,
                passed: env_metrics.tests_passed,
                failed: env_metrics.tests_failed,
                success_rate: if env_metrics.tests_executed > 0 {
                    env_metrics.tests_passed as f64 / env_metrics.tests_executed as f64
                } else {
                    0.0
                },
                avg_duration: env_metrics.average_execution_time,
            },
        }
    }

    /// Start a new span
    pub async fn start_span(&self, name: String, parent_id: Option<String>) -> Result<String> {
        let span_id = uuid::Uuid::new_v4().to_string();
        let span = Span {
            id: span_id.clone(),
            parent_id,
            name,
            start_time: Instant::now(),
            end_time: None,
            duration: None,
            status: SpanStatus::Active,
            attributes: HashMap::new(),
            events: Vec::new(),
        };

        // Store active span
        {
            let mut active_spans = self.active_spans.write().await;
            active_spans.insert(span_id.clone(), span);
        }

        // Trace with tracing crate
        tracing::info_span!("cleanroom_span", span_id = %span_id).entered();

        Ok(span_id)
    }

    /// End a span
    pub async fn end_span(&self, span_id: &str, status: SpanStatus) -> Result<()> {
        let mut span = {
            let mut active_spans = self.active_spans.write().await;
            active_spans.remove(span_id).ok_or_else(|| {
                crate::error::CleanroomError::internal_error("Span not found")
            })?
        };

        span.end_time = Some(Instant::now());
        span.duration = Some(span.end_time.unwrap().duration_since(span.start_time));
        span.status = status;

        // Collect span
        for collector in &self.span_collectors {
            if let Err(e) = collector.collect(&span) {
                tracing::error!("Failed to collect span: {}", e);
            }
        }

        Ok(())
    }

    /// Add span event
    pub async fn add_span_event(
        &self,
        span_id: &str,
        name: String,
        attributes: HashMap<String, String>,
    ) -> Result<()> {
        let event = SpanEvent {
            name,
            timestamp: Instant::now(),
            attributes,
        };

        {
            let mut active_spans = self.active_spans.write().await;
            if let Some(span) = active_spans.get_mut(span_id) {
                span.events.push(event);
            }
        }

        Ok(())
    }

    /// Add span attribute
    pub async fn add_span_attribute(
        &self,
        span_id: &str,
        key: String,
        value: String,
    ) -> Result<()> {
        {
            let mut active_spans = self.active_spans.write().await;
            if let Some(span) = active_spans.get_mut(span_id) {
                span.attributes.insert(key, value);
            }
        }

        Ok(())
    }

    /// Get metrics history
    pub async fn get_metrics_history(&self) -> Vec<Metrics> {
        let history = self.metrics_history.read().await;
        history.clone()
    }

    /// Get active spans
    pub async fn get_active_spans(&self) -> Vec<Span> {
        let spans = self.active_spans.read().await;
        spans.values().cloned().collect()
    }

    /// Get observability statistics
    pub async fn get_statistics(&self) -> ObservabilityStatistics {
        let metrics_history = self.metrics_history.read().await;
        let active_spans = self.active_spans.read().await;

        ObservabilityStatistics {
            total_metrics_collected: metrics_history.len(),
            active_spans_count: active_spans.len(),
            exporters_count: self.exporters.len(),
            span_collectors_count: self.span_collectors.len(),
            metrics_collection_enabled: self.metrics_config.enabled,
            tracing_enabled: self.tracing_config.enabled,
        }
    }
}

/// Observability statistics
#[derive(Debug, Clone)]
pub struct ObservabilityStatistics {
    /// Total metrics collected
    pub total_metrics_collected: usize,
    /// Number of active spans
    pub active_spans_count: usize,
    /// Number of exporters
    pub exporters_count: usize,
    /// Number of span collectors
    pub span_collectors_count: usize,
    /// Whether metrics collection is enabled
    pub metrics_collection_enabled: bool,
    /// Whether tracing is enabled
    pub tracing_enabled: bool,
}

/// Console metrics exporter
#[derive(Debug)]
pub struct ConsoleMetricsExporter;

impl ConsoleMetricsExporter {
    /// Create a new console metrics exporter
    pub fn new() -> Self {
        Self
    }
}

impl MetricsExporter for ConsoleMetricsExporter {
    fn export(&self, metrics: &Metrics) -> Result<()> {
        println!("=== Cleanroom Metrics ===");
        println!("Session ID: {}", metrics.session_id);
        println!("Timestamp: {:?}", metrics.timestamp);
        println!("Resource Usage:");
        println!("  CPU: {:.1}%", metrics.resource_usage.cpu_usage_percent);
        println!("  Memory: {} bytes", metrics.resource_usage.memory_usage_bytes);
        println!("Performance:");
        println!("  Throughput: {:.2} tests/sec", metrics.performance.throughput);
        println!("  Avg Test Duration: {:?}", metrics.performance.avg_test_execution_time);
        println!("Tests:");
        println!("  Total: {}", metrics.tests.total_executed);
        println!("  Passed: {}", metrics.tests.passed);
        println!("  Failed: {}", metrics.tests.failed);
        println!("  Success Rate: {:.1}%", metrics.tests.success_rate * 100.0);
        println!("========================");
        Ok(())
    }

    fn name(&self) -> &str {
        "console"
    }
}

/// Console span collector
#[derive(Debug)]
pub struct ConsoleSpanCollector;

impl ConsoleSpanCollector {
    /// Create a new console span collector
    pub fn new() -> Self {
        Self
    }
}

impl SpanCollector for ConsoleSpanCollector {
    fn collect(&self, span: &Span) -> Result<()> {
        println!("=== Cleanroom Span ===");
        println!("ID: {}", span.id);
        println!("Name: {}", span.name);
        println!("Status: {:?}", span.status);
        println!("Duration: {:?}", span.duration);
        println!("Events: {}", span.events.len());
        println!("Attributes: {}", span.attributes.len());
        println!("=====================");
        Ok(())
    }

    fn name(&self) -> &str {
        "console"
    }
}

/// Convenience function to create an observability layer
pub fn observability_layer() -> ObservabilityLayer {
    ObservabilityLayer::new()
}

/// Convenience function to create a console metrics exporter
pub fn console_metrics_exporter() -> Box<dyn MetricsExporter + Send + Sync> {
    Box::new(ConsoleMetricsExporter::new())
}

/// Convenience function to create a console span collector
pub fn console_span_collector() -> Box<dyn SpanCollector + Send + Sync> {
    Box::new(ConsoleSpanCollector::new())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::CleanroomConfig;

    #[tokio::test]
    async fn test_observability_layer() {
        let layer = ObservabilityLayer::new()
            .with_tracing_level(TracingLevel::Debug)
            .with_sampling_rate(0.5);
        
        assert!(layer.metrics_config().enabled);
        assert!(layer.tracing_config().enabled);
        assert_eq!(layer.tracing_config().level, TracingLevel::Debug);
        assert_eq!(layer.tracing_config().sampling_rate, 0.5);
    }

    #[tokio::test]
    async fn test_observability_manager() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        
        let layer = ObservabilityLayer::new();
        let mut manager = layer.attach(&environment).unwrap();
        
        assert!(manager.metrics_config.enabled);
        assert!(manager.tracing_config.enabled);
    }

    #[tokio::test]
    async fn test_span_management() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        
        let layer = ObservabilityLayer::new();
        let manager = layer.attach(&environment).unwrap();
        
        let span_id = manager.start_span("test_span".to_string(), None).await.unwrap();
        assert!(!span_id.is_empty());
        
        manager.add_span_attribute(&span_id, "key".to_string(), "value".to_string()).await.unwrap();
        manager.add_span_event(&span_id, "test_event".to_string(), HashMap::new()).await.unwrap();
        
        manager.end_span(&span_id, SpanStatus::Completed).await.unwrap();
        
        let active_spans = manager.get_active_spans().await;
        assert_eq!(active_spans.len(), 0);
    }

    #[tokio::test]
    async fn test_metrics_collection() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        
        let layer = ObservabilityLayer::new()
            .with_exporter(console_metrics_exporter());
        let mut manager = layer.attach(&environment).unwrap();
        
        manager.start_metrics_collection().await.unwrap();
        
        // Let it collect some metrics
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        
        let history = manager.get_metrics_history().await;
        assert!(!history.is_empty());
        
        manager.stop_metrics_collection().await;
    }

    #[tokio::test]
    async fn test_console_exporter() {
        let exporter = ConsoleMetricsExporter::new();
        assert_eq!(exporter.name(), "console");
        
        let metrics = Metrics {
            timestamp: Instant::now(),
            session_id: uuid::Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 25.0,
                memory_usage_bytes: 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: std::time::Duration::from_millis(100),
                container_startup_time: std::time::Duration::from_millis(50),
                total_execution_time: std::time::Duration::from_secs(10),
                throughput: 10.0,
            },
            containers: ContainerMetrics {
                total_created: 5,
                total_destroyed: 3,
                currently_running: 2,
                avg_lifetime: std::time::Duration::from_secs(60),
            },
            tests: TestMetrics {
                total_executed: 100,
                passed: 95,
                failed: 5,
                success_rate: 0.95,
                avg_duration: std::time::Duration::from_millis(100),
            },
        };
        
        exporter.export(&metrics).unwrap();
    }

    #[tokio::test]
    async fn test_console_span_collector() {
        let collector = ConsoleSpanCollector::new();
        assert_eq!(collector.name(), "console");
        
        let span = Span {
            id: "test_span".to_string(),
            parent_id: None,
            name: "test".to_string(),
            start_time: Instant::now(),
            end_time: Some(Instant::now()),
            duration: Some(std::time::Duration::from_millis(100)),
            status: SpanStatus::Completed,
            attributes: HashMap::new(),
            events: Vec::new(),
        };
        
        collector.collect(&span).unwrap();
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let _layer = observability_layer();
        let _exporter = console_metrics_exporter();
        let _collector = console_span_collector();
        
        // Just verify they compile and create valid instances
        assert!( _layer.metrics_config().enabled);
        assert_eq!(_exporter.name(), "console");
        assert_eq!(_collector.name(), "console");
    }
}
