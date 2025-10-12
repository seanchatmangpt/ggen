//! Tracing and observability for cleanroom testing
//!
//! This module provides tracing and observability following core team best practices:
//! - Distributed tracing
//! - Span management
//! - Metrics collection
//! - Log aggregation

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use serde::{Deserializer, Serializer};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use crate::serializable_instant::SerializableInstant;
use uuid::Uuid;
use tokio::sync::RwLock;

/// Serializable wrapper for SerializableInstant
#[derive(Debug, Clone)]
pub struct SerializableSerializableInstant(pub SerializableInstant);

impl serde::Serialize for SerializableSerializableInstant {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let elapsed = self.0.elapsed();
        serializer.serialize_u64(elapsed.as_secs() * 1_000_000_000 + elapsed.subsec_nanos() as u64)
    }
}

impl<'de> serde::Deserialize<'de> for SerializableSerializableInstant {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let nanos = u64::deserialize(deserializer)?;
        let secs = nanos / 1_000_000_000;
        let subsec_nanos = (nanos % 1_000_000_000) as u32;
        let duration = Duration::new(secs, subsec_nanos);
        Ok(SerializableSerializableInstant(SerializableInstant::now() - duration))
    }
}

impl SerializableSerializableInstant {
    /// Create a new SerializableSerializableInstant with current time
    pub fn now() -> Self {
        Self(SerializableInstant::now())
    }
}

/// Tracing manager for cleanroom testing
#[derive(Debug)]
pub struct TracingManager {
    /// Session ID
    session_id: Uuid,
    /// Tracing data
    tracing_data: Arc<RwLock<TracingData>>,
    /// Enabled flag
    enabled: bool,
}

/// Tracing data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracingData {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: Option<SerializableInstant>,
    /// Spans
    pub spans: HashMap<String, Span>,
    /// Metrics
    pub metrics: HashMap<String, Metric>,
    /// Logs
    pub logs: Vec<LogEntry>,
    /// Tracing statistics
    pub statistics: TracingStatistics,
}

impl TracingData {
    /// Add a span to the tracing data
    pub fn add_span(&mut self, span: Span) {
        self.spans.insert(span.name.clone(), span);
    }

    /// Get a span by name
    pub fn get_span(&self, name: &str) -> Option<&Span> {
        self.spans.get(name)
    }

    /// Add a log entry
    pub fn add_log(&mut self, log: LogEntry) {
        self.logs.push(log);
    }

    /// Add a metric
    pub fn add_metric(&mut self, metric: Metric) {
        self.metrics.insert(metric.name.clone(), metric);
    }
}

/// Span structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Span {
    /// Span name
    pub name: String,
    /// Span ID
    pub span_id: Uuid,
    /// Parent span ID
    pub parent_span_id: Option<Uuid>,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: Option<SerializableInstant>,
    /// Duration
    pub duration: Option<Duration>,
    /// Span status
    pub status: SpanStatus,
    /// Tags
    pub tags: HashMap<String, String>,
    /// Events
    pub events: Vec<SpanEvent>,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

impl Span {
    /// Create a new span
    pub fn new(
        name: String,
        span_id: Uuid,
        parent_span_id: Option<Uuid>,
        start_time: SerializableInstant,
    ) -> Self {
        Self {
            name,
            span_id,
            parent_span_id,
            start_time,
            end_time: None,
            duration: None,
            status: SpanStatus::Running,
            tags: HashMap::new(),
            events: Vec::new(),
            metadata: HashMap::new(),
        }
    }
    
    /// End the span
    pub fn end(&mut self, end_time: SerializableInstant, status: SpanStatus) {
        self.end_time = Some(end_time);
        self.status = status;
        self.duration = Some(end_time.duration_since(self.start_time));
    }
    
    /// Add a tag to the span
    pub fn add_tag(&mut self, key: String, value: String) {
        self.tags.insert(key, value);
    }
    
    /// Get the duration of the span
    pub fn get_duration(&self) -> Option<Duration> {
        self.duration
    }
}

/// Span status enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SpanStatus {
    /// Span is running
    Running,
    /// Span completed successfully
    Completed,
    /// Span failed
    Failed(String),
    /// Span was cancelled
    Cancelled,
}

/// Span event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanEvent {
    /// Event name
    pub name: String,
    /// Event timestamp
    pub timestamp: SerializableSerializableInstant,
    /// Event data
    pub data: HashMap<String, String>,
}

/// Metric structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metric {
    /// Metric name
    pub name: String,
    /// Metric value
    pub value: f64,
    /// Metric type
    pub metric_type: MetricType,
    /// Timestamp
    pub timestamp: SerializableSerializableInstant,
    /// Tags
    pub tags: HashMap<String, String>,
    /// Unit
    pub unit: Option<String>,
}

impl Metric {
    /// Create a new metric
    pub fn new(
        name: String,
        value: f64,
        metric_type: MetricType,
        timestamp: SerializableSerializableInstant,
    ) -> Self {
        Self {
            name,
            value,
            metric_type,
            timestamp,
            tags: HashMap::new(),
            unit: None,
        }
    }
}

/// Metric type enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum MetricType {
    /// Counter metric
    Counter,
    /// Gauge metric
    Gauge,
    /// Histogram metric
    Histogram,
    /// Summary metric
    Summary,
}

/// Log entry structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    /// Log level
    pub level: LogLevel,
    /// Log message
    pub message: String,
    /// Timestamp
    pub timestamp: SerializableSerializableInstant,
    /// Source
    pub source: Option<String>,
    /// Tags
    pub tags: HashMap<String, String>,
    /// Metadata
    pub metadata: HashMap<String, String>,
    /// Span name (for compatibility with tests)
    pub span_name: Option<String>,
    /// Fields (for compatibility with tests)
    pub fields: HashMap<String, String>,
}

/// Type alias for Log
pub type Log = LogEntry;

impl LogEntry {
    /// Create a new log entry
    pub fn new(
        level: LogLevel,
        message: String,
        timestamp: SerializableSerializableInstant,
    ) -> Self {
        Self {
            level,
            message,
            timestamp,
            source: None,
            tags: HashMap::new(),
            metadata: HashMap::new(),
            span_name: None,
            fields: HashMap::new(),
        }
    }
}

/// Log level enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum LogLevel {
    /// Trace level
    Trace,
    /// Debug level
    Debug,
    /// Info level
    Info,
    /// Warn level
    Warn,
    /// Error level
    Error,
}

/// Tracing statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracingStatistics {
    /// Total spans
    pub total_spans: u32,
    /// Completed spans
    pub completed_spans: u32,
    /// Failed spans
    pub failed_spans: u32,
    /// Running spans
    pub running_spans: u32,
    /// Total metrics
    pub total_metrics: u32,
    /// Total logs
    pub total_logs: u32,
    /// Average span duration
    pub average_span_duration_ms: f64,
    /// Total trace duration
    pub total_trace_duration_ms: f64,
}

impl TracingManager {
    /// Create a new tracing manager
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            tracing_data: Arc::new(RwLock::new(TracingData::new(session_id))),
            enabled: true,
        }
    }
    
    /// Create a disabled tracing manager
    pub fn disabled() -> Self {
        Self {
            session_id: Uuid::new_v4(),
            tracing_data: Arc::new(RwLock::new(TracingData::new(Uuid::new_v4()))),
            enabled: false,
        }
    }
    
    /// Check if tracing is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Start a new span
    pub async fn start_span(&self, name: String, parent_span_id: Option<Uuid>) -> Result<Uuid> {
        if !self.enabled {
            return Ok(Uuid::new_v4());
        }
        
        let span_id = Uuid::new_v4();
        let span = Span {
            name: name.clone(),
            span_id,
            parent_span_id,
            start_time: SerializableInstant::now(),
            end_time: None,
            duration: None,
            status: SpanStatus::Running,
            tags: HashMap::new(),
            events: Vec::new(),
            metadata: HashMap::new(),
        };
        
        let mut data = self.tracing_data.write().await;
        data.spans.insert(name, span);
        self.update_statistics(&mut data).await;
        
        Ok(span_id)
    }
    
    /// End a span
    pub async fn end_span(&self, name: &str, status: SpanStatus) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.tracing_data.write().await;
        if let Some(span) = data.spans.get_mut(name) {
            span.end_time = Some(SerializableInstant::now());
            span.duration = Some(span.end_time.unwrap().duration_since(span.start_time));
            span.status = status;
        }
        
        self.update_statistics(&mut data).await;
        Ok(())
    }

    /// Add event to span
    pub async fn add_span_event(&self, span_name: &str, event_name: String, data: HashMap<String, String>) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }

        let mut tracing_data = self.tracing_data.write().await;
        if let Some(span) = tracing_data.spans.get_mut(span_name) {
            let event = SpanEvent {
                name: event_name,
                timestamp: SerializableSerializableInstant(SerializableInstant::now()),
                data,
            };
            span.events.push(event);
        }

        Ok(())
    }

    /// Add tag to span
    pub async fn add_span_tag(&self, span_name: &str, key: String, value: String) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }

        let mut data = self.tracing_data.write().await;
        if let Some(span) = data.spans.get_mut(span_name) {
            span.tags.insert(key, value);
        }

        Ok(())
    }

    /// Record a metric
    pub async fn record_metric(
        &self,
        name: String,
        value: f64,
        metric_type: MetricType,
        tags: HashMap<String, String>,
        unit: Option<String>,
    ) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let metric = Metric {
            name: name.clone(),
            value,
            metric_type,
            timestamp: SerializableSerializableInstant(SerializableInstant::now()),
            tags,
            unit,
        };
        
        let mut data = self.tracing_data.write().await;
        data.metrics.insert(name, metric);
        self.update_statistics(&mut data).await;
        
        Ok(())
    }
    
    /// Log an entry
    pub async fn log(
        &self,
        level: LogLevel,
        message: String,
        source: Option<String>,
        tags: HashMap<String, String>,
        metadata: HashMap<String, String>,
    ) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }

        let log_entry = LogEntry {
            level,
            message,
            timestamp: SerializableSerializableInstant(SerializableInstant::now()),
            source,
            tags,
            metadata,
            span_name: None,
            fields: HashMap::new(),
        };
        
        let mut data = self.tracing_data.write().await;
        data.logs.push(log_entry);
        self.update_statistics(&mut data).await;

        Ok(())
    }

    /// Get tracing data
    pub async fn get_tracing_data(&self) -> TracingData {
        let data = self.tracing_data.read().await;
        data.clone()
    }
    
    /// Get span by name
    pub async fn get_span(&self, name: &str) -> Result<Option<Span>> {
        if !self.enabled {
            return Ok(None);
        }
        
        let data = self.tracing_data.read().await;
        Ok(data.spans.get(name).cloned())
    }
    
    /// Get all spans
    pub async fn get_all_spans(&self) -> Result<HashMap<String, Span>> {
        if !self.enabled {
            return Ok(HashMap::new());
        }
        
        let data = self.tracing_data.read().await;
        Ok(data.spans.clone())
    }
    
    /// Get metrics by name
    pub async fn get_metric(&self, name: &str) -> Result<Option<Metric>> {
        if !self.enabled {
            return Ok(None);
        }
        
        let data = self.tracing_data.read().await;
        Ok(data.metrics.get(name).cloned())
    }
    
    /// Get all metrics
    pub async fn get_all_metrics(&self) -> Result<HashMap<String, Metric>> {
        if !self.enabled {
            return Ok(HashMap::new());
        }
        
        let data = self.tracing_data.read().await;
        Ok(data.metrics.clone())
    }
    
    /// Get all logs
    pub async fn get_all_logs(&self) -> Result<Vec<LogEntry>> {
        if !self.enabled {
            return Ok(Vec::new());
        }
        
        let data = self.tracing_data.read().await;
        Ok(data.logs.clone())
    }
    
    /// Generate tracing report
    pub async fn generate_tracing_report(&self) -> Result<TracingReport> {
        if !self.enabled {
            return Err(CleanroomError::tracing_error("Tracing is disabled"));
        }
        
        let data = self.tracing_data.read().await;
        
        let mut report = TracingReport {
            session_id: data.session_id,
            start_time: data.start_time,
            end_time: data.end_time,
            statistics: data.statistics.clone(),
            spans: Vec::new(),
            metrics: Vec::new(),
            logs: Vec::new(),
            recommendations: Vec::new(),
        };
        
        // Generate span summaries
        for (name, span) in &data.spans {
            report.spans.push(SpanSummary {
                name: name.clone(),
                span_id: span.span_id,
                parent_span_id: span.parent_span_id,
                start_time: span.start_time,
                end_time: span.end_time,
                duration: span.duration,
                status: span.status.clone(),
                events_count: span.events.len(),
                tags_count: span.tags.len(),
            });
        }
        
        // Generate metric summaries
        for (name, metric) in &data.metrics {
            report.metrics.push(MetricSummary {
                name: name.clone(),
                value: metric.value,
                metric_type: metric.metric_type.clone(),
                timestamp: metric.timestamp.clone(),
                unit: metric.unit.clone(),
                tags_count: metric.tags.len(),
            });
        }
        
        // Copy logs
        report.logs = data.logs.clone();
        
        // Generate recommendations
        self.generate_recommendations(&data, &mut report).await;
        
        Ok(report)
    }
    
    /// Update statistics
    async fn update_statistics(&self, data: &mut TracingData) {
        let mut stats = TracingStatistics {
            total_spans: 0,
            completed_spans: 0,
            failed_spans: 0,
            running_spans: 0,
            total_metrics: 0,
            total_logs: 0,
            average_span_duration_ms: 0.0,
            total_trace_duration_ms: 0.0,
        };
        
        // Count span statistics
        let mut total_duration_ms = 0.0;
        let mut completed_count = 0;
        
        for span in data.spans.values() {
            stats.total_spans += 1;
            
            match span.status {
                SpanStatus::Completed => {
                    stats.completed_spans += 1;
                    if let Some(duration) = span.duration {
                        total_duration_ms += duration.as_millis() as f64;
                        completed_count += 1;
                    }
                }
                SpanStatus::Failed(_) => stats.failed_spans += 1,
                SpanStatus::Running => stats.running_spans += 1,
                SpanStatus::Cancelled => {}
            }
        }
        
        if completed_count > 0 {
            stats.average_span_duration_ms = total_duration_ms / completed_count as f64;
        }
        
        // Count metrics and logs
        stats.total_metrics = data.metrics.len() as u32;
        stats.total_logs = data.logs.len() as u32;
        
        // Calculate total trace duration
        if let Some(end_time) = data.end_time {
            stats.total_trace_duration_ms = end_time.duration_since(data.start_time).as_millis() as f64;
        } else {
            stats.total_trace_duration_ms = SerializableInstant::now().duration_since(data.start_time).as_millis() as f64;
        }
        
        data.statistics = stats;
    }
    
    /// Generate recommendations
    async fn generate_recommendations(&self, data: &TracingData, report: &mut TracingReport) {
        // Failed spans recommendation
        if data.statistics.failed_spans > 0 {
            report.recommendations.push(format!(
                "{} spans failed, review and fix failed operations",
                data.statistics.failed_spans
            ));
        }
        
        // Running spans recommendation
        if data.statistics.running_spans > 0 {
            report.recommendations.push(format!(
                "{} spans are still running, consider adding timeouts",
                data.statistics.running_spans
            ));
        }
        
        // Long duration recommendation
        if data.statistics.average_span_duration_ms > 1000.0 { // 1 second
            report.recommendations.push(format!(
                "Average span duration is {:.1}ms, consider optimizing slow operations",
                data.statistics.average_span_duration_ms
            ));
        }
        
        // High error log count recommendation
        let error_logs = data.logs.iter().filter(|log| matches!(log.level, LogLevel::Error)).count();
        if error_logs > 0 {
            report.recommendations.push(format!(
                "{} error logs detected, review and fix error conditions",
                error_logs
            ));
        }
    }
}

impl TracingData {
    /// Create new tracing data
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            start_time: SerializableInstant::now(),
            end_time: None,
            spans: HashMap::new(),
            metrics: HashMap::new(),
            logs: Vec::new(),
            statistics: TracingStatistics {
                total_spans: 0,
                completed_spans: 0,
                failed_spans: 0,
                running_spans: 0,
                total_metrics: 0,
                total_logs: 0,
                average_span_duration_ms: 0.0,
                total_trace_duration_ms: 0.0,
            },
        }
    }
}

/// Tracing report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracingReport {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: Option<SerializableInstant>,
    /// Statistics
    pub statistics: TracingStatistics,
    /// Spans
    pub spans: Vec<SpanSummary>,
    /// Metrics
    pub metrics: Vec<MetricSummary>,
    /// Logs
    pub logs: Vec<LogEntry>,
    /// Recommendations
    pub recommendations: Vec<String>,
}

impl TracingReport {
    /// Create a new tracing report
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            start_time: SerializableInstant::now(),
            end_time: None,
            statistics: TracingStatistics {
                total_spans: 0,
                completed_spans: 0,
                failed_spans: 0,
                running_spans: 0,
                total_metrics: 0,
                total_logs: 0,
                average_span_duration_ms: 0.0,
                total_trace_duration_ms: 0.0,
            },
            spans: Vec::new(),
            metrics: Vec::new(),
            logs: Vec::new(),
            recommendations: Vec::new(),
        }
    }
}

/// Span summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanSummary {
    /// Span name
    pub name: String,
    /// Span ID
    pub span_id: Uuid,
    /// Parent span ID
    pub parent_span_id: Option<Uuid>,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: Option<SerializableInstant>,
    /// Duration
    pub duration: Option<Duration>,
    /// Status
    pub status: SpanStatus,
    /// Events count
    pub events_count: usize,
    /// Tags count
    pub tags_count: usize,
}

/// Metric summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricSummary {
    /// Metric name
    pub name: String,
    /// Metric value
    pub value: f64,
    /// Metric type
    pub metric_type: MetricType,
    /// Timestamp
    pub timestamp: SerializableSerializableInstant,
    /// Unit
    pub unit: Option<String>,
    /// Tags count
    pub tags_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_tracing_manager_creation() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        assert!(manager.is_enabled());
        assert_eq!(manager.session_id, session_id);
    }
    
    #[tokio::test]
    async fn test_tracing_manager_disabled() {
        let manager = TracingManager::disabled();
        assert!(!manager.is_enabled());
    }
    
    #[tokio::test]
    async fn test_start_span() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        let span_id = manager.start_span("test_span".to_string(), None).await.unwrap();
        assert!(span_id != Uuid::nil());
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.spans.len(), 1);
        assert!(data.spans.contains_key("test_span"));
        
        let span = &data.spans["test_span"];
        assert_eq!(span.name, "test_span");
        assert!(matches!(span.status, SpanStatus::Running));
    }
    
    #[tokio::test]
    async fn test_end_span() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Start span
        let _span_id = manager.start_span("test_span".to_string(), None).await.unwrap();
        
        // End span
        manager.end_span("test_span", SpanStatus::Completed).await.unwrap();
        
        let span = manager.get_span("test_span").await.unwrap().unwrap();
        assert!(matches!(span.status, SpanStatus::Completed));
        assert!(span.end_time.is_some());
        assert!(span.duration.is_some());
    }
    
    #[tokio::test]
    async fn test_add_span_event() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Start span
        let _span_id = manager.start_span("test_span".to_string(), None).await.unwrap();
        
        // Add event
        let mut event_data = HashMap::new();
        event_data.insert("key".to_string(), "value".to_string());
        manager.add_span_event("test_span", "test_event".to_string(), event_data).await.unwrap();
        
        let span = manager.get_span("test_span").await.unwrap().unwrap();
        assert_eq!(span.events.len(), 1);
        assert_eq!(span.events[0].name, "test_event");
    }
    
    #[tokio::test]
    async fn test_record_metric() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Record metric
        let mut tags = HashMap::new();
        tags.insert("env".to_string(), "test".to_string());
        manager.record_metric(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            tags,
            Some("count".to_string()),
        ).await.unwrap();
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.metrics.len(), 1);
        assert!(data.metrics.contains_key("test_metric"));
        
        let metric = &data.metrics["test_metric"];
        assert_eq!(metric.name, "test_metric");
        assert_eq!(metric.value, 42.0);
        assert!(matches!(metric.metric_type, MetricType::Counter));
    }
    
    #[tokio::test]
    async fn test_log() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Log entry
        let mut tags = HashMap::new();
        tags.insert("component".to_string(), "test".to_string());
        let mut metadata = HashMap::new();
        metadata.insert("version".to_string(), "1.0.0".to_string());
        manager.log(
            LogLevel::Info,
            "Test message".to_string(),
            Some("test.rs".to_string()),
            tags,
            metadata,
        ).await.unwrap();
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.logs.len(), 1);
        
        let log = &data.logs[0];
        assert!(matches!(log.level, LogLevel::Info));
        assert_eq!(log.message, "Test message");
        assert_eq!(log.source, Some("test.rs".to_string()));
    }
    
    #[tokio::test]
    async fn test_tracing_report() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Start and end span
        let _span_id = manager.start_span("test_span".to_string(), None).await.unwrap();
        manager.end_span("test_span", SpanStatus::Completed).await.unwrap();
        
        // Record metric
        manager.record_metric(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            HashMap::new(),
            None,
        ).await.unwrap();
        
        // Generate report
        let report = manager.generate_tracing_report().await.unwrap();
        assert_eq!(report.session_id, session_id);
        assert_eq!(report.spans.len(), 1);
        assert_eq!(report.metrics.len(), 1);
        assert_eq!(report.spans[0].name, "test_span");
        assert_eq!(report.metrics[0].name, "test_metric");
    }
    
    #[tokio::test]
    async fn test_tracing_manager_error_handling() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Test invalid span operations
        let result = manager.end_span("nonexistent_span", SpanStatus::Completed).await;
        assert!(result.is_err());
        
        // Test metric recording with invalid values
        let result = manager.record_metric(
            "invalid_metric".to_string(),
            f64::NAN,
            MetricType::Counter,
            HashMap::new(),
            None,
        ).await;
        assert!(result.is_err());
    }
    
    #[tokio::test]
    async fn test_span_hierarchy() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Create parent span
        let parent_id = manager.start_span("parent".to_string(), None).await.unwrap();
        
        // Create child span
        let child_id = manager.start_span("child".to_string(), Some(parent_id)).await.unwrap();
        
        // End child span
        manager.end_span("child", SpanStatus::Completed).await.unwrap();
        
        // End parent span
        manager.end_span("parent", SpanStatus::Completed).await.unwrap();
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.spans.len(), 2);
        
        // Verify hierarchy
        let parent_span = data.spans.iter().find(|(_, s)| s.name == "parent").unwrap();
        let child_span = data.spans.iter().find(|(_, s)| s.name == "child").unwrap();
        
        assert_eq!(child_span.1.parent_span_id, Some(parent_id));
        assert_eq!(parent_span.1.parent_span_id, None);
    }
    
    #[tokio::test]
    async fn test_metric_aggregation() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Record multiple metrics with same name
        manager.record_metric(
            "counter".to_string(),
            1.0,
            MetricType::Counter,
            HashMap::new(),
            None,
        ).await.unwrap();
        
        manager.record_metric(
            "counter".to_string(),
            2.0,
            MetricType::Counter,
            HashMap::new(),
            None,
        ).await.unwrap();
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.metrics.len(), 1);
        assert_eq!(data.metrics["counter"].value, 3.0); // Should be aggregated
    }
    
    #[tokio::test]
    async fn test_log_level_filtering() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Log messages at different levels
        manager.log(
            LogLevel::Debug,
            "Debug message".to_string(),
            None,
            HashMap::new(),
            HashMap::new(),
        ).await.unwrap();
        
        manager.log(
            LogLevel::Info,
            "Info message".to_string(),
            None,
            HashMap::new(),
            HashMap::new(),
        ).await.unwrap();
        
        manager.log(
            LogLevel::Warn,
            "Warning message".to_string(),
            None,
            HashMap::new(),
            HashMap::new(),
        ).await.unwrap();
        
        manager.log(
            LogLevel::Error,
            "Error message".to_string(),
            None,
            HashMap::new(),
            HashMap::new(),
        ).await.unwrap();
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.logs.len(), 4);
        
        // Verify all levels are present
        let levels: std::collections::HashSet<_> = data.logs.iter().map(|l| l.level.clone()).collect();
        assert!(levels.contains(&LogLevel::Debug));
        assert!(levels.contains(&LogLevel::Info));
        assert!(levels.contains(&LogLevel::Warn));
        assert!(levels.contains(&LogLevel::Error));
    }
    
    #[tokio::test]
    async fn test_concurrent_tracing() {
        let session_id = Uuid::new_v4();
        let manager = Arc::new(TracingManager::new(session_id));
        
        // Spawn multiple tasks that trace concurrently
        let mut handles = Vec::new();
        
        for i in 0..10 {
            let manager_clone = manager.clone();
            let handle = tokio::spawn(async move {
                let span_id = manager_clone.start_span(format!("span_{}", i), None).await.unwrap();
                
                manager_clone.log(
                    LogLevel::Info,
                    format!("Message from task {}", i),
                    None,
                    HashMap::new(),
                    HashMap::new(),
                ).await.unwrap();
                
                manager_clone.record_metric(
                    format!("metric_{}", i),
                    i as f64,
                    MetricType::Counter,
                    HashMap::new(),
                    None,
                ).await.unwrap();
                
                manager_clone.end_span(&format!("span_{}", i), SpanStatus::Completed).await.unwrap();
            });
            handles.push(handle);
        }
        
        // Wait for all tasks to complete
        for handle in handles {
            handle.await.unwrap();
        }
        
        let data = manager.get_tracing_data().await;
        assert_eq!(data.spans.len(), 10);
        assert_eq!(data.logs.len(), 10);
        assert_eq!(data.metrics.len(), 10);
    }
    
    #[tokio::test]
    async fn test_tracing_data_serialization() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Create some tracing data
        manager.start_span("test_span".to_string(), None).await.unwrap();
        manager.log(
            LogLevel::Info,
            "Test message".to_string(),
            None,
            HashMap::new(),
            HashMap::new(),
        ).await.unwrap();
        manager.record_metric(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            HashMap::new(),
            None,
        ).await.unwrap();
        
        let data = manager.get_tracing_data().await;
        
        // Test JSON serialization
        let json = serde_json::to_string(&data).unwrap();
        let deserialized: TracingData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(deserialized.session_id, data.session_id);
        assert_eq!(deserialized.spans.len(), data.spans.len());
        assert_eq!(deserialized.logs.len(), data.logs.len());
        assert_eq!(deserialized.metrics.len(), data.metrics.len());
    }
    
    #[tokio::test]
    async fn test_tracing_report_serialization() {
        let session_id = Uuid::new_v4();
        let manager = TracingManager::new(session_id);
        
        // Create tracing data
        manager.start_span("test_span".to_string(), None).await.unwrap();
        manager.end_span("test_span", SpanStatus::Completed).await.unwrap();
        
        let report = manager.generate_tracing_report().await.unwrap();
        
        // Test JSON serialization
        let json = serde_json::to_string(&report).unwrap();
        let deserialized: TracingReport = serde_json::from_str(&json).unwrap();
        
        assert_eq!(deserialized.session_id, report.session_id);
        assert_eq!(deserialized.spans.len(), report.spans.len());
        assert_eq!(deserialized.metrics.len(), report.metrics.len());
        assert_eq!(deserialized.logs.len(), report.logs.len());
    }

    #[test]
    fn test_serializable_instant_new() {
        let instant = SerializableInstant::now();
        assert!(instant.elapsed().as_nanos() >= 0);
    }

    #[test]
    fn test_serializable_instant_serialization() {
        let instant = SerializableInstant::now();
        
        let json = serde_json::to_string(&instant).unwrap();
        let deserialized: SerializableInstant = serde_json::from_str(&json).unwrap();
        
        // Due to approximation in serialization, we can't test exact equality
        // But the deserialized value should be close to the original
        let diff = instant.duration_since(deserialized);
        assert!(diff.as_millis() < 1000); // Should be within 1 second
    }

    #[test]
    fn test_serializable_instant_duration_calculations() {
        let start = SerializableInstant::now();
        std::thread::sleep(Duration::from_millis(10));
        let end = SerializableInstant::now();
        
        let duration = end.duration_since(start);
        assert!(duration.as_millis() >= 10);
    }

    #[test]
    fn test_serializable_instant_elapsed() {
        let instant = SerializableInstant::now();
        std::thread::sleep(Duration::from_millis(5));
        let elapsed = instant.elapsed();
        assert!(elapsed.as_millis() >= 5);
    }

    #[test]
    fn test_serializable_instant_from_instant() {
        let std_instant = Instant::now();
        let serializable = SerializableInstant::from(std_instant);
        let back_to_instant: Instant = serializable.into();
        
        // Should be very close
        let diff = std_instant.duration_since(back_to_instant);
        assert!(diff.as_millis() < 1);
    }

    #[test]
    fn test_serializable_instant_ordering() {
        let instant1 = SerializableInstant::now();
        std::thread::sleep(Duration::from_millis(1));
        let instant2 = SerializableInstant::now();
        
        assert!(instant1 < instant2);
        assert!(instant2 > instant1);
        assert_eq!(instant1, instant1);
    }

    #[test]
    fn test_serializable_instant_add_sub() {
        let instant = SerializableInstant::now();
        let duration = Duration::from_secs(1);
        
        let added = instant + duration;
        let subtracted = instant - duration;
        
        assert!(added > instant);
        assert!(subtracted < instant);
        
        let diff = added.duration_since(subtracted);
        assert_eq!(diff, Duration::from_secs(2));
    }

    #[test]
    fn test_serializable_instant_display() {
        let instant = SerializableInstant::now();
        let display_str = format!("{:?}", instant);
        
        assert!(display_str.contains("SerializableInstant"));
    }

    #[test]
    fn test_serializable_instant_debug() {
        let instant = SerializableInstant::now();
        let debug_str = format!("{:?}", instant);
        
        assert!(debug_str.contains("SerializableInstant"));
    }

    #[test]
    fn test_span_new() {
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            Some(Uuid::new_v4()),
            SerializableInstant::now(),
        );
        
        assert_eq!(span.name, "test_span");
        assert_eq!(span.parent_span_id, Some(Uuid::new_v4()));
        assert_eq!(span.status, SpanStatus::Running);
        assert!(span.tags.is_empty());
        assert!(span.end_time.is_none());
        assert!(span.duration.is_none());
    }

    #[test]
    fn test_span_serialization() {
        let mut span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            Some(Uuid::new_v4()),
            SerializableInstant::now(),
        );
        
        span.tags.insert("key".to_string(), "value".to_string());
        span.status = SpanStatus::Completed;
        
        let json = serde_json::to_string(&span).unwrap();
        let deserialized: Span = serde_json::from_str(&json).unwrap();
        
        assert_eq!(span.name, deserialized.name);
        assert_eq!(span.parent_span_id, deserialized.parent_span_id);
        assert_eq!(span.status, deserialized.status);
        assert_eq!(span.tags, deserialized.tags);
    }

    #[test]
    fn test_span_status_serialization() {
        let statuses = vec![
            SpanStatus::Running,
            SpanStatus::Completed,
            SpanStatus::Failed("test error".to_string()),
            SpanStatus::Cancelled,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: SpanStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_span_end() {
        let mut span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        let end_time = SerializableInstant::now();
        span.end(end_time, SpanStatus::Completed);
        
        assert_eq!(span.status, SpanStatus::Completed);
        assert_eq!(span.end_time, Some(end_time));
        assert!(span.duration.is_some());
    }

    #[test]
    fn test_span_add_tag() {
        let mut span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        span.add_tag("key".to_string(), "value".to_string());
        
        assert_eq!(span.tags.len(), 1);
        assert_eq!(span.tags.get("key"), Some(&"value".to_string()));
    }

    #[test]
    fn test_span_get_duration() {
        let mut span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        // Initially no duration
        assert!(span.get_duration().is_none());
        
        // End the span
        let end_time = SerializableInstant::now();
        span.end(end_time, SpanStatus::Completed);
        
        // Now should have duration
        assert!(span.get_duration().is_some());
    }

    #[test]
    fn test_span_debug() {
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        let debug_str = format!("{:?}", span);
        
        assert!(debug_str.contains("Span"));
        assert!(debug_str.contains("test_span"));
    }

    #[test]
    fn test_span_clone() {
        let span1 = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            Some(Uuid::new_v4()),
            SerializableInstant::now(),
        );
        
        let span2 = span1.clone();
        
        assert_eq!(span1.name, span2.name);
        assert_eq!(span1.parent_span_id, span2.parent_span_id);
        assert_eq!(span1.status, span2.status);
        assert_eq!(span1.tags, span2.tags);
    }

    #[test]
    fn test_log_new() {
        let log = Log::new(
            LogLevel::Info,
            "test message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        assert_eq!(log.level, LogLevel::Info);
        assert_eq!(log.message, "test message");
        assert_eq!(log.span_name, None);
        assert!(log.tags.is_empty());
        assert!(log.fields.is_empty());
    }

    #[test]
    fn test_log_serialization() {
        let mut tags = HashMap::new();
        tags.insert("key".to_string(), "value".to_string());
        
        let mut fields = HashMap::new();
        fields.insert("field".to_string(), "data".to_string());
        
        let log = Log::new(
            LogLevel::Warn,
            "test message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        let json = serde_json::to_string(&log).unwrap();
        let deserialized: Log = serde_json::from_str(&json).unwrap();
        
        assert_eq!(log.level, deserialized.level);
        assert_eq!(log.message, deserialized.message);
        assert_eq!(log.span_name, deserialized.span_name);
        assert_eq!(log.tags, deserialized.tags);
        assert_eq!(log.fields, deserialized.fields);
    }

    #[test]
    fn test_log_level_serialization() {
        let levels = vec![
            LogLevel::Debug,
            LogLevel::Info,
            LogLevel::Warn,
            LogLevel::Error,
        ];

        for level in levels {
            let json = serde_json::to_string(&level).unwrap();
            let deserialized: LogLevel = serde_json::from_str(&json).unwrap();
            assert_eq!(level, deserialized);
        }
    }

    #[test]
    fn test_log_debug() {
        let log = Log::new(
            LogLevel::Error,
            "error message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        let debug_str = format!("{:?}", log);
        
        assert!(debug_str.contains("Log"));
        assert!(debug_str.contains("error message"));
    }

    #[test]
    fn test_log_clone() {
        let mut tags = HashMap::new();
        tags.insert("key".to_string(), "value".to_string());
        
        let log1 = Log::new(
            LogLevel::Info,
            "test message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        let log2 = log1.clone();
        
        assert_eq!(log1.level, log2.level);
        assert_eq!(log1.message, log2.message);
        assert_eq!(log1.span_name, log2.span_name);
        assert_eq!(log1.tags, log2.tags);
        assert_eq!(log1.fields, log2.fields);
    }

    #[test]
    fn test_metric_new() {
        let metric = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            SerializableSerializableInstant::now(),
        );
        
        assert_eq!(metric.name, "test_metric");
        assert_eq!(metric.value, 42.0);
        assert_eq!(metric.metric_type, MetricType::Counter);
        assert!(metric.tags.is_empty());
        assert_eq!(metric.unit, Some("bytes".to_string()));
    }

    #[test]
    fn test_metric_serialization() {
        let mut tags = HashMap::new();
        tags.insert("key".to_string(), "value".to_string());
        
        let metric = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Gauge,
            SerializableSerializableInstant::now(),
        );
        
        let json = serde_json::to_string(&metric).unwrap();
        let deserialized: Metric = serde_json::from_str(&json).unwrap();
        
        assert_eq!(metric.name, deserialized.name);
        assert_eq!(metric.value, deserialized.value);
        assert_eq!(metric.metric_type, deserialized.metric_type);
        assert_eq!(metric.tags, deserialized.tags);
        assert_eq!(metric.unit, deserialized.unit);
    }

    #[test]
    fn test_metric_type_serialization() {
        let types = vec![
            MetricType::Counter,
            MetricType::Gauge,
            MetricType::Histogram,
            MetricType::Summary,
        ];

        for metric_type in types {
            let json = serde_json::to_string(&metric_type).unwrap();
            let deserialized: MetricType = serde_json::from_str(&json).unwrap();
            assert_eq!(metric_type, deserialized);
        }
    }

    #[test]
    fn test_metric_debug() {
        let metric = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            SerializableSerializableInstant::now(),
        );
        
        let debug_str = format!("{:?}", metric);
        
        assert!(debug_str.contains("Metric"));
        assert!(debug_str.contains("test_metric"));
    }

    #[test]
    fn test_metric_clone() {
        let mut tags = HashMap::new();
        tags.insert("key".to_string(), "value".to_string());
        
        let metric1 = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            SerializableSerializableInstant::now(),
        );
        
        let metric2 = metric1.clone();
        
        assert_eq!(metric1.name, metric2.name);
        assert_eq!(metric1.value, metric2.value);
        assert_eq!(metric1.metric_type, metric2.metric_type);
        assert_eq!(metric1.tags, metric2.tags);
        assert_eq!(metric1.unit, metric2.unit);
    }

    #[test]
    fn test_tracing_data_new() {
        let session_id = Uuid::new_v4();
        let data = TracingData::new(session_id);
        
        assert_eq!(data.session_id, session_id);
        assert!(data.spans.is_empty());
        assert!(data.logs.is_empty());
        assert!(data.metrics.is_empty());
    }

    #[test]
    fn test_tracing_data_add_span() {
        let session_id = Uuid::new_v4();
        let mut data = TracingData::new(session_id);
        
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        data.add_span(span);
        
        assert_eq!(data.spans.len(), 1);
        assert!(data.spans.contains_key("test_span"));
    }

    #[test]
    fn test_tracing_data_add_log() {
        let session_id = Uuid::new_v4();
        let mut data = TracingData::new(session_id);
        
        let log = Log::new(
            LogLevel::Info,
            "test message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        data.add_log(log);
        
        assert_eq!(data.logs.len(), 1);
    }

    #[test]
    fn test_tracing_data_add_metric() {
        let session_id = Uuid::new_v4();
        let mut data = TracingData::new(session_id);
        
        let metric = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            SerializableSerializableInstant::now(),
        );
        
        data.add_metric(metric);
        
        assert_eq!(data.metrics.len(), 1);
    }

    #[test]
    fn test_tracing_data_get_span() {
        let session_id = Uuid::new_v4();
        let mut data = TracingData::new(session_id);
        
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        data.add_span(span);
        
        let retrieved = data.get_span("test_span");
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().name, "test_span");
        
        let not_found = data.get_span("nonexistent");
        assert!(not_found.is_none());
    }

    #[test]
    fn test_tracing_data_serialization_sync() {
        let session_id = Uuid::new_v4();
        let mut data = TracingData::new(session_id);
        
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        let log = Log::new(
            LogLevel::Info,
            "test message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        let metric = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            SerializableSerializableInstant::now(),
        );
        
        data.add_span(span);
        data.add_log(log);
        data.add_metric(metric);
        
        let json = serde_json::to_string(&data).unwrap();
        let deserialized: TracingData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(data.session_id, deserialized.session_id);
        assert_eq!(data.spans.len(), deserialized.spans.len());
        assert_eq!(data.logs.len(), deserialized.logs.len());
        assert_eq!(data.metrics.len(), deserialized.metrics.len());
    }

    #[test]
    fn test_tracing_data_debug() {
        let session_id = Uuid::new_v4();
        let data = TracingData::new(session_id);
        
        let debug_str = format!("{:?}", data);
        
        assert!(debug_str.contains("TracingData"));
        assert!(debug_str.contains("session_id"));
    }

    #[test]
    fn test_tracing_data_clone() {
        let session_id = Uuid::new_v4();
        let mut data1 = TracingData::new(session_id);
        
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        data1.add_span(span);
        
        let data2 = data1.clone();
        
        assert_eq!(data1.session_id, data2.session_id);
        assert_eq!(data1.spans.len(), data2.spans.len());
        assert_eq!(data1.logs.len(), data2.logs.len());
        assert_eq!(data1.metrics.len(), data2.metrics.len());
    }

    #[test]
    fn test_tracing_report_new() {
        let session_id = Uuid::new_v4();
        let report = TracingReport::new(session_id);
        
        assert_eq!(report.session_id, session_id);
        assert!(report.spans.is_empty());
        assert!(report.logs.is_empty());
        assert!(report.metrics.is_empty());
    }

    #[test]
    fn test_tracing_report_serialization_sync() {
        let session_id = Uuid::new_v4();
        let mut report = TracingReport::new(session_id);
        
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        let log = Log::new(
            LogLevel::Info,
            "test message".to_string(),
            SerializableSerializableInstant::now(),
        );
        
        let metric = Metric::new(
            "test_metric".to_string(),
            42.0,
            MetricType::Counter,
            SerializableSerializableInstant::now(),
        );
        
        report.spans.push(SpanSummary {
            name: span.name,
            span_id: span.span_id,
            parent_span_id: span.parent_span_id,
            start_time: span.start_time,
            end_time: span.end_time,
            duration: span.duration,
            status: span.status,
            events_count: 0,
            tags_count: span.tags.len(),
        });
        report.logs.push(log);
        report.metrics.push(MetricSummary {
            name: metric.name,
            value: metric.value,
            metric_type: metric.metric_type,
            timestamp: metric.timestamp,
            unit: None,
            tags_count: metric.tags.len(),
        });
        
        let json = serde_json::to_string(&report).unwrap();
        let deserialized: TracingReport = serde_json::from_str(&json).unwrap();
        
        assert_eq!(report.session_id, deserialized.session_id);
        assert_eq!(report.spans.len(), deserialized.spans.len());
        assert_eq!(report.logs.len(), deserialized.logs.len());
        assert_eq!(report.metrics.len(), deserialized.metrics.len());
    }

    #[test]
    fn test_tracing_report_debug() {
        let session_id = Uuid::new_v4();
        let report = TracingReport::new(session_id);
        
        let debug_str = format!("{:?}", report);
        
        assert!(debug_str.contains("TracingReport"));
        assert!(debug_str.contains("session_id"));
    }

    #[test]
    fn test_tracing_report_clone() {
        let session_id = Uuid::new_v4();
        let mut report1 = TracingReport::new(session_id);
        
        let span = Span::new(
            "test_span".to_string(),
            Uuid::new_v4(),
            None,
            SerializableInstant::now(),
        );
        
        report1.spans.push(SpanSummary {
            name: span.name,
            span_id: span.span_id,
            parent_span_id: span.parent_span_id,
            start_time: span.start_time,
            end_time: span.end_time,
            duration: span.duration,
            status: span.status,
            events_count: 0,
            tags_count: span.tags.len(),
        });
        
        let report2 = report1.clone();
        
        assert_eq!(report1.session_id, report2.session_id);
        assert_eq!(report1.spans.len(), report2.spans.len());
        assert_eq!(report1.logs.len(), report2.logs.len());
        assert_eq!(report1.metrics.len(), report2.metrics.len());
    }
}