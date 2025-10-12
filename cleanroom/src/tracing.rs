//! Tracing and observability for cleanroom testing
//!
//! This module provides tracing and observability following core team best practices:
//! - Distributed tracing
//! - Span management
//! - Metrics collection
//! - Log aggregation

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use uuid::Uuid;

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
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Spans
    pub spans: HashMap<String, Span>,
    /// Metrics
    pub metrics: HashMap<String, Metric>,
    /// Logs
    pub logs: Vec<LogEntry>,
    /// Tracing statistics
    pub statistics: TracingStatistics,
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
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
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

/// Span status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    pub timestamp: Instant,
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
    pub timestamp: Instant,
    /// Tags
    pub tags: HashMap<String, String>,
    /// Unit
    pub unit: Option<String>,
}

/// Metric type enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    pub timestamp: Instant,
    /// Source
    pub source: Option<String>,
    /// Tags
    pub tags: HashMap<String, String>,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

/// Log level enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
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
            start_time: Instant::now(),
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
            span.end_time = Some(Instant::now());
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
                timestamp: Instant::now(),
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
            timestamp: Instant::now(),
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
            timestamp: Instant::now(),
            source,
            tags,
            metadata,
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
                timestamp: metric.timestamp,
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
            stats.total_trace_duration_ms = Instant::now().duration_since(data.start_time).as_millis() as f64;
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
            start_time: Instant::now(),
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
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
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
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
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
    pub timestamp: Instant,
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
}