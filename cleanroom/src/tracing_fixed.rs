//! Fixed tracing and observability for cleanroom testing framework
//!
//! This module provides tracing and observability following core team best practices:
//! - Distributed tracing
//! - Structured logging
//! - Performance monitoring
//! - Error tracking
//! - Metrics collection

use crate::error::{Result, CleanroomError};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Tracing manager for handling distributed tracing
#[derive(Debug)]
pub struct TracingManager {
    /// Session ID
    session_id: Uuid,
    /// Active traces
    active_traces: Arc<RwLock<HashMap<String, Trace>>>,
    /// Completed traces
    completed_traces: Arc<RwLock<Vec<Trace>>>,
    /// Trace events
    trace_events: Arc<RwLock<Vec<TraceEvent>>>,
    /// Manager enabled status
    enabled: bool,
}

/// Trace data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trace {
    /// Trace ID
    pub trace_id: Uuid,
    /// Trace name
    pub name: String,
    /// Trace status
    pub status: TraceStatus,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: Option<SerializableInstant>,
    /// Duration
    pub duration: Option<std::time::Duration>,
    /// Trace events
    pub events: Vec<TraceEvent>,
    /// Metadata
    pub metadata: HashMap<String, String>,
    /// Parent trace ID (for nested traces)
    pub parent_trace_id: Option<Uuid>,
    /// Child trace IDs
    pub child_trace_ids: Vec<Uuid>,
}

/// Trace event data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceEvent {
    /// Event ID
    pub event_id: Uuid,
    /// Trace ID
    pub trace_id: Uuid,
    /// Event name
    pub name: String,
    /// Event type
    pub event_type: TraceEventType,
    /// Timestamp
    pub timestamp: SerializableInstant,
    /// Duration
    pub duration: Option<std::time::Duration>,
    /// Event data
    pub data: HashMap<String, String>,
    /// Event level
    pub level: TraceLevel,
    /// Event message
    pub message: Option<String>,
}

/// Trace status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TraceStatus {
    /// Trace is active
    Active,
    /// Trace completed successfully
    Completed,
    /// Trace failed
    Failed,
    /// Trace was cancelled
    Cancelled,
}

/// Trace event type enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TraceEventType {
    /// Function call
    FunctionCall,
    /// Function return
    FunctionReturn,
    /// Database query
    DatabaseQuery,
    /// HTTP request
    HttpRequest,
    /// HTTP response
    HttpResponse,
    /// Container operation
    ContainerOperation,
    /// Test execution
    TestExecution,
    /// Error occurrence
    Error,
    /// Performance measurement
    Performance,
    /// Custom event type
    Custom(String),
}

/// Trace level enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TraceLevel {
    /// Debug level
    Debug,
    /// Info level
    Info,
    /// Warning level
    Warning,
    /// Error level
    Error,
    /// Critical level
    Critical,
}

/// Trace summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceSummary {
    /// Total traces
    pub total_traces: usize,
    /// Active traces
    pub active_traces: usize,
    /// Completed traces
    pub completed_traces: usize,
    /// Failed traces
    pub failed_traces: usize,
    /// Total events
    pub total_events: usize,
    /// Average trace duration
    pub average_trace_duration: f64,
    /// Most common event type
    pub most_common_event_type: Option<String>,
}

/// Trace report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceReport {
    /// Session ID
    pub session_id: Uuid,
    /// Trace summary
    pub summary: TraceSummary,
    /// Active traces
    pub active_traces: Vec<Trace>,
    /// Completed traces
    pub completed_traces: Vec<Trace>,
    /// Generated timestamp
    pub generated_at: SerializableInstant,
}

impl TracingManager {
    /// Create a new tracing manager
    pub fn new() -> Self {
        Self::new_with_enabled(true)
    }

    /// Create a new tracing manager with enabled status
    pub fn new_with_enabled(enabled: bool) -> Self {
        let session_id = Uuid::new_v4();
        let active_traces = Arc::new(RwLock::new(HashMap::new()));
        let completed_traces = Arc::new(RwLock::new(Vec::new()));
        let trace_events = Arc::new(RwLock::new(Vec::new()));

        Self {
            session_id,
            active_traces,
            completed_traces,
            trace_events,
            enabled,
        }
    }

    /// Create a disabled tracing manager
    pub fn disabled() -> Self {
        Self::new_with_enabled(false)
    }

    /// Check if tracing manager is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Start a new trace
    pub async fn start_trace(&self, name: String, parent_trace_id: Option<Uuid>) -> Result<Uuid> {
        if !self.enabled {
            return Err(CleanroomError::tracing_error("Tracing manager is disabled"));
        }

        let trace_id = Uuid::new_v4();
        let trace = Trace {
            trace_id,
            name: name.clone(),
            status: TraceStatus::Active,
            start_time: SerializableInstant::from(Instant::now()),
            end_time: None,
            duration: None,
            events: Vec::new(),
            metadata: HashMap::new(),
            parent_trace_id,
            child_trace_ids: Vec::new(),
        };

        // Store active trace
        {
            let mut active_traces = self.active_traces.write().await;
            active_traces.insert(name, trace);
        }

        // Add to parent's child traces if applicable
        if let Some(parent_id) = parent_trace_id {
            let mut active_traces = self.active_traces.write().await;
            for trace in active_traces.values_mut() {
                if trace.trace_id == parent_id {
                    trace.child_trace_ids.push(trace_id);
                    break;
                }
            }
        }

        Ok(trace_id)
    }

    /// Log a trace event
    pub async fn log_trace_event(
        &self,
        trace_id: Uuid,
        name: String,
        event_type: TraceEventType,
        level: TraceLevel,
        message: Option<String>,
        data: HashMap<String, String>,
    ) -> Result<()> {
        if !self.enabled {
            return Err(CleanroomError::tracing_error("Tracing manager is disabled"));
        }

        let event = TraceEvent {
            event_id: Uuid::new_v4(),
            trace_id,
            name,
            event_type,
            timestamp: SerializableInstant::from(Instant::now()),
            duration: None,
            data,
            level,
            message,
        };

        // Add event to trace
        {
            let mut active_traces = self.active_traces.write().await;
            for trace in active_traces.values_mut() {
                if trace.trace_id == trace_id {
                    trace.events.push(event.clone());
                    break;
                }
            }
        }

        // Store event globally
        {
            let mut trace_events = self.trace_events.write().await;
            trace_events.push(event);
        }

        Ok(())
    }

    /// End a trace
    pub async fn end_trace(&self, trace_id: Uuid, status: TraceStatus) -> Result<()> {
        if !self.enabled {
            return Err(CleanroomError::tracing_error("Tracing manager is disabled"));
        }

        let mut active_traces = self.active_traces.write().await;
        let mut completed_traces = self.completed_traces.write().await;

        // Find and remove trace from active traces
        let mut trace_to_complete = None;
        for (name, trace) in active_traces.iter() {
            if trace.trace_id == trace_id {
                trace_to_complete = Some((name.clone(), trace.clone()));
                break;
            }
        }

        if let Some((name, mut trace)) = trace_to_complete {
            trace.status = status;
            trace.end_time = Some(SerializableInstant::from(Instant::now()));
            trace.duration = Some(trace.end_time.unwrap().into() - trace.start_time.into());

            // Move to completed traces
            completed_traces.push(trace);
            active_traces.remove(&name);
        } else {
            return Err(CleanroomError::tracing_error("Trace not found"));
        }

        Ok(())
    }

    /// Get all traces
    pub async fn get_traces(&self) -> Vec<Trace> {
        let mut all_traces = Vec::new();

        // Add active traces
        {
            let active_traces = self.active_traces.read().await;
            all_traces.extend(active_traces.values().cloned());
        }

        // Add completed traces
        {
            let completed_traces = self.completed_traces.read().await;
            all_traces.extend(completed_traces.clone());
        }

        all_traces
    }

    /// Get active traces
    pub async fn get_active_traces(&self) -> Vec<Trace> {
        let active_traces = self.active_traces.read().await;
        active_traces.values().cloned().collect()
    }

    /// Get completed traces
    pub async fn get_completed_traces(&self) -> Vec<Trace> {
        let completed_traces = self.completed_traces.read().await;
        completed_traces.clone()
    }

    /// Get trace by ID
    pub async fn get_trace(&self, trace_id: Uuid) -> Option<Trace> {
        // Check active traces
        {
            let active_traces = self.active_traces.read().await;
            for trace in active_traces.values() {
                if trace.trace_id == trace_id {
                    return Some(trace.clone());
                }
            }
        }

        // Check completed traces
        {
            let completed_traces = self.completed_traces.read().await;
            for trace in completed_traces.iter() {
                if trace.trace_id == trace_id {
                    return Some(trace.clone());
                }
            }
        }

        None
    }

    /// Get trace events
    pub async fn get_trace_events(&self) -> Vec<TraceEvent> {
        let trace_events = self.trace_events.read().await;
        trace_events.clone()
    }

    /// Get trace events for a specific trace
    pub async fn get_trace_events_for_trace(&self, trace_id: Uuid) -> Vec<TraceEvent> {
        let trace_events = self.trace_events.read().await;
        trace_events
            .iter()
            .filter(|event| event.trace_id == trace_id)
            .cloned()
            .collect()
    }

    /// Clear all traces
    pub async fn clear_traces(&self) -> Result<()> {
        if !self.enabled {
            return Err(CleanroomError::tracing_error("Tracing manager is disabled"));
        }

        {
            let mut active_traces = self.active_traces.write().await;
            active_traces.clear();
        }

        {
            let mut completed_traces = self.completed_traces.write().await;
            completed_traces.clear();
        }

        {
            let mut trace_events = self.trace_events.write().await;
            trace_events.clear();
        }

        Ok(())
    }

    /// Generate trace summary
    pub async fn get_trace_summary(&self) -> TraceSummary {
        let active_traces = self.get_active_traces().await;
        let completed_traces = self.get_completed_traces().await;
        let trace_events = self.get_trace_events().await;

        let total_traces = active_traces.len() + completed_traces.len();
        let failed_traces = completed_traces
            .iter()
            .filter(|trace| matches!(trace.status, TraceStatus::Failed))
            .count();

        let total_duration: f64 = completed_traces
            .iter()
            .filter_map(|trace| trace.duration)
            .map(|duration| duration.as_millis() as f64)
            .sum();

        let average_trace_duration = if !completed_traces.is_empty() {
            total_duration / completed_traces.len() as f64
        } else {
            0.0
        };

        // Find most common event type
        let mut event_type_counts: HashMap<String, usize> = HashMap::new();
        for event in &trace_events {
            let type_name = match &event.event_type {
                TraceEventType::FunctionCall => "function_call".to_string(),
                TraceEventType::FunctionReturn => "function_return".to_string(),
                TraceEventType::DatabaseQuery => "database_query".to_string(),
                TraceEventType::HttpRequest => "http_request".to_string(),
                TraceEventType::HttpResponse => "http_response".to_string(),
                TraceEventType::ContainerOperation => "container_operation".to_string(),
                TraceEventType::TestExecution => "test_execution".to_string(),
                TraceEventType::Error => "error".to_string(),
                TraceEventType::Performance => "performance".to_string(),
                TraceEventType::Custom(name) => name.clone(),
            };
            *event_type_counts.entry(type_name).or_insert(0) += 1;
        }

        let most_common_event_type = event_type_counts
            .iter()
            .max_by_key(|(_, count)| *count)
            .map(|(name, _)| name.clone());

        TraceSummary {
            total_traces,
            active_traces: active_traces.len(),
            completed_traces: completed_traces.len(),
            failed_traces,
            total_events: trace_events.len(),
            average_trace_duration,
            most_common_event_type,
        }
    }

    /// Generate trace report
    pub async fn generate_trace_report(&self) -> Result<TraceReport> {
        let summary = self.get_trace_summary().await;
        let active_traces = self.get_active_traces().await;
        let completed_traces = self.get_completed_traces().await;

        Ok(TraceReport {
            session_id: self.session_id,
            summary,
            active_traces,
            completed_traces,
            generated_at: SerializableInstant::from(Instant::now()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracing_manager_creation() {
        let manager = TracingManager::new();
        assert!(manager.is_enabled());
        assert!(!manager.session_id.is_nil());
    }

    #[test]
    fn test_tracing_manager_disabled() {
        let manager = TracingManager::disabled();
        assert!(!manager.is_enabled());
    }

    #[tokio::test]
    async fn test_start_trace() {
        let manager = TracingManager::new();
        
        let trace_id = manager.start_trace("test_trace".to_string(), None).await.unwrap();
        assert!(!trace_id.is_nil());

        let active_traces = manager.get_active_traces().await;
        assert_eq!(active_traces.len(), 1);
        assert_eq!(active_traces[0].trace_id, trace_id);
        assert_eq!(active_traces[0].name, "test_trace");
        assert!(matches!(active_traces[0].status, TraceStatus::Active));
    }

    #[tokio::test]
    async fn test_start_trace_disabled() {
        let manager = TracingManager::disabled();
        
        let result = manager.start_trace("test_trace".to_string(), None).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("disabled"));
    }

    #[tokio::test]
    async fn test_log_trace_event() {
        let manager = TracingManager::new();
        
        let trace_id = manager.start_trace("test_trace".to_string(), None).await.unwrap();
        
        let mut data = HashMap::new();
        data.insert("key".to_string(), "value".to_string());
        
        let result = manager.log_trace_event(
            trace_id,
            "test_event".to_string(),
            TraceEventType::FunctionCall,
            TraceLevel::Info,
            Some("Test message".to_string()),
            data,
        ).await;
        
        assert!(result.is_ok());

        let events = manager.get_trace_events_for_trace(trace_id).await;
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].name, "test_event");
        assert!(matches!(events[0].event_type, TraceEventType::FunctionCall));
        assert!(matches!(events[0].level, TraceLevel::Info));
    }

    #[tokio::test]
    async fn test_log_trace_event_disabled() {
        let manager = TracingManager::disabled();
        
        let result = manager.log_trace_event(
            Uuid::new_v4(),
            "test_event".to_string(),
            TraceEventType::FunctionCall,
            TraceLevel::Info,
            None,
            HashMap::new(),
        ).await;
        
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("disabled"));
    }

    #[tokio::test]
    async fn test_end_trace() {
        let manager = TracingManager::new();
        
        let trace_id = manager.start_trace("test_trace".to_string(), None).await.unwrap();
        
        // End trace
        let result = manager.end_trace(trace_id, TraceStatus::Completed).await;
        assert!(result.is_ok());

        // Check trace is moved to completed
        let active_traces = manager.get_active_traces().await;
        assert_eq!(active_traces.len(), 0);

        let completed_traces = manager.get_completed_traces().await;
        assert_eq!(completed_traces.len(), 1);
        assert_eq!(completed_traces[0].trace_id, trace_id);
        assert!(matches!(completed_traces[0].status, TraceStatus::Completed));
        assert!(completed_traces[0].end_time.is_some());
        assert!(completed_traces[0].duration.is_some());
    }

    #[tokio::test]
    async fn test_end_trace_not_found() {
        let manager = TracingManager::new();
        
        let result = manager.end_trace(Uuid::new_v4(), TraceStatus::Completed).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("not found"));
    }

    #[tokio::test]
    async fn test_get_traces() {
        let manager = TracingManager::new();
        
        // Start multiple traces
        let trace1 = manager.start_trace("trace1".to_string(), None).await.unwrap();
        let trace2 = manager.start_trace("trace2".to_string(), None).await.unwrap();
        
        // End one trace
        manager.end_trace(trace1, TraceStatus::Completed).await.unwrap();
        
        let all_traces = manager.get_traces().await;
        assert_eq!(all_traces.len(), 2);
        
        let active_traces = manager.get_active_traces().await;
        assert_eq!(active_traces.len(), 1);
        
        let completed_traces = manager.get_completed_traces().await;
        assert_eq!(completed_traces.len(), 1);
    }

    #[tokio::test]
    async fn test_get_trace_by_id() {
        let manager = TracingManager::new();
        
        let trace_id = manager.start_trace("test_trace".to_string(), None).await.unwrap();
        
        let trace = manager.get_trace(trace_id).await;
        assert!(trace.is_some());
        assert_eq!(trace.unwrap().name, "test_trace");
        
        // Test non-existent trace
        let non_existent = manager.get_trace(Uuid::new_v4()).await;
        assert!(non_existent.is_none());
    }

    #[tokio::test]
    async fn test_clear_traces() {
        let manager = TracingManager::new();
        
        // Start and end some traces
        let trace_id = manager.start_trace("test_trace".to_string(), None).await.unwrap();
        manager.end_trace(trace_id, TraceStatus::Completed).await.unwrap();
        
        // Clear all traces
        assert!(manager.clear_traces().await.is_ok());
        
        // Verify traces are cleared
        let all_traces = manager.get_traces().await;
        assert_eq!(all_traces.len(), 0);
        
        let events = manager.get_trace_events().await;
        assert_eq!(events.len(), 0);
    }

    #[tokio::test]
    async fn test_get_trace_summary() {
        let manager = TracingManager::new();
        
        // Start and end some traces
        let trace1 = manager.start_trace("trace1".to_string(), None).await.unwrap();
        let trace2 = manager.start_trace("trace2".to_string(), None).await.unwrap();
        
        manager.end_trace(trace1, TraceStatus::Completed).await.unwrap();
        manager.end_trace(trace2, TraceStatus::Failed).await.unwrap();
        
        let summary = manager.get_trace_summary().await;
        assert_eq!(summary.total_traces, 2);
        assert_eq!(summary.active_traces, 0);
        assert_eq!(summary.completed_traces, 2);
        assert_eq!(summary.failed_traces, 1);
    }

    #[tokio::test]
    async fn test_generate_trace_report() {
        let manager = TracingManager::new();
        
        let trace_id = manager.start_trace("test_trace".to_string(), None).await.unwrap();
        manager.end_trace(trace_id, TraceStatus::Completed).await.unwrap();
        
        let report = manager.generate_trace_report().await.unwrap();
        assert_eq!(report.session_id, manager.session_id);
        assert_eq!(report.summary.total_traces, 1);
        assert_eq!(report.completed_traces.len(), 1);
    }

    #[test]
    fn test_trace_status_serialization() {
        let statuses = vec![
            TraceStatus::Active,
            TraceStatus::Completed,
            TraceStatus::Failed,
            TraceStatus::Cancelled,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: TraceStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_trace_event_type_serialization() {
        let types = vec![
            TraceEventType::FunctionCall,
            TraceEventType::FunctionReturn,
            TraceEventType::DatabaseQuery,
            TraceEventType::HttpRequest,
            TraceEventType::HttpResponse,
            TraceEventType::ContainerOperation,
            TraceEventType::TestExecution,
            TraceEventType::Error,
            TraceEventType::Performance,
            TraceEventType::Custom("custom".to_string()),
        ];

        for event_type in types {
            let json = serde_json::to_string(&event_type).unwrap();
            let deserialized: TraceEventType = serde_json::from_str(&json).unwrap();
            assert_eq!(event_type, deserialized);
        }
    }

    #[test]
    fn test_trace_level_serialization() {
        let levels = vec![
            TraceLevel::Debug,
            TraceLevel::Info,
            TraceLevel::Warning,
            TraceLevel::Error,
            TraceLevel::Critical,
        ];

        for level in levels {
            let json = serde_json::to_string(&level).unwrap();
            let deserialized: TraceLevel = serde_json::from_str(&json).unwrap();
            assert_eq!(level, deserialized);
        }
    }

    #[test]
    fn test_trace_serialization() {
        let trace = Trace {
            trace_id: Uuid::new_v4(),
            name: "test_trace".to_string(),
            status: TraceStatus::Active,
            start_time: SerializableInstant::from(Instant::now()),
            end_time: None,
            duration: None,
            events: vec![],
            metadata: {
                let mut map = HashMap::new();
                map.insert("key1".to_string(), "value1".to_string());
                map
            },
            parent_trace_id: None,
            child_trace_ids: vec![],
        };

        let json = serde_json::to_string(&trace).unwrap();
        let deserialized: Trace = serde_json::from_str(&json).unwrap();
        
        assert_eq!(trace.trace_id, deserialized.trace_id);
        assert_eq!(trace.name, deserialized.name);
        assert_eq!(trace.status, deserialized.status);
        assert_eq!(trace.metadata, deserialized.metadata);
    }

    #[test]
    fn test_trace_event_serialization() {
        let event = TraceEvent {
            event_id: Uuid::new_v4(),
            trace_id: Uuid::new_v4(),
            name: "test_event".to_string(),
            event_type: TraceEventType::FunctionCall,
            timestamp: SerializableInstant::from(Instant::now()),
            duration: None,
            data: {
                let mut map = HashMap::new();
                map.insert("key1".to_string(), "value1".to_string());
                map
            },
            level: TraceLevel::Info,
            message: Some("Test message".to_string()),
        };

        let json = serde_json::to_string(&event).unwrap();
        let deserialized: TraceEvent = serde_json::from_str(&json).unwrap();
        
        assert_eq!(event.event_id, deserialized.event_id);
        assert_eq!(event.trace_id, deserialized.trace_id);
        assert_eq!(event.name, deserialized.name);
        assert_eq!(event.event_type, deserialized.event_type);
        assert_eq!(event.level, deserialized.level);
        assert_eq!(event.message, deserialized.message);
        assert_eq!(event.data, deserialized.data);
    }

    #[test]
    fn test_trace_summary_serialization() {
        let summary = TraceSummary {
            total_traces: 10,
            active_traces: 2,
            completed_traces: 8,
            failed_traces: 1,
            total_events: 50,
            average_trace_duration: 150.5,
            most_common_event_type: Some("function_call".to_string()),
        };

        let json = serde_json::to_string(&summary).unwrap();
        let deserialized: TraceSummary = serde_json::from_str(&json).unwrap();
        
        assert_eq!(summary.total_traces, deserialized.total_traces);
        assert_eq!(summary.active_traces, deserialized.active_traces);
        assert_eq!(summary.completed_traces, deserialized.completed_traces);
        assert_eq!(summary.failed_traces, deserialized.failed_traces);
        assert_eq!(summary.total_events, deserialized.total_events);
        assert_eq!(summary.average_trace_duration, deserialized.average_trace_duration);
        assert_eq!(summary.most_common_event_type, deserialized.most_common_event_type);
    }

    #[test]
    fn test_trace_report_serialization() {
        let report = TraceReport {
            session_id: Uuid::new_v4(),
            summary: TraceSummary {
                total_traces: 5,
                active_traces: 1,
                completed_traces: 4,
                failed_traces: 0,
                total_events: 20,
                average_trace_duration: 100.0,
                most_common_event_type: None,
            },
            active_traces: vec![],
            completed_traces: vec![],
            generated_at: SerializableInstant::from(Instant::now()),
        };

        let json = serde_json::to_string(&report).unwrap();
        let deserialized: TraceReport = serde_json::from_str(&json).unwrap();
        
        assert_eq!(report.session_id, deserialized.session_id);
        assert_eq!(report.summary.total_traces, deserialized.summary.total_traces);
        assert_eq!(report.active_traces.len(), deserialized.active_traces.len());
        assert_eq!(report.completed_traces.len(), deserialized.completed_traces.len());
    }
}
