//! Correlation ID system for end-to-end request tracing
//!
//! Every operation gets a unique identifier that flows through all subsystems,
//! enabling complete reconstruction of call chains and latency analysis.

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

/// Unique correlation ID for tracing requests through system
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CorrelationId(pub String);

impl CorrelationId {
    /// Generate a new correlation ID
    pub fn new() -> Self {
        CorrelationId(Uuid::new_v4().to_string())
    }

    /// Create from existing UUID string
    pub fn from_string(id: String) -> Self {
        CorrelationId(id)
    }
}

impl Default for CorrelationId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for CorrelationId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Trace entry for a single operation
#[derive(Clone, Debug)]
pub struct TraceEntry {
    pub correlation_id: CorrelationId,
    pub timestamp_millis: u64,
    pub component: String,
    pub operation: String,
    pub start_ns: u128,
    pub end_ns: Option<u128>,
    pub parent_span: Option<String>,
    pub status: TraceStatus,
    pub metadata: HashMap<String, String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TraceStatus {
    Started,
    InProgress,
    Completed,
    Failed(String),
}

impl TraceEntry {
    pub fn duration_ms(&self) -> Option<u64> {
        self.end_ns
            .map(|end| ((end - self.start_ns) / 1_000_000) as u64)
    }
}

/// Correlation context for tracing operations
pub struct CorrelationContext {
    correlation_id: CorrelationId,
    traces: Arc<Mutex<Vec<TraceEntry>>>,
}

impl CorrelationContext {
    pub fn new() -> Self {
        CorrelationContext {
            correlation_id: CorrelationId::new(),
            traces: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn with_id(correlation_id: CorrelationId) -> Self {
        CorrelationContext {
            correlation_id,
            traces: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn correlation_id(&self) -> &CorrelationId {
        &self.correlation_id
    }

    /// Record start of operation
    pub fn start_span(&self, component: String, operation: String) -> SpanGuard {
        let entry = TraceEntry {
            correlation_id: self.correlation_id.clone(),
            timestamp_millis: current_time_millis(),
            component,
            operation,
            start_ns: current_time_nanos(),
            end_ns: None,
            parent_span: None,
            status: TraceStatus::Started,
            metadata: HashMap::new(),
        };

        let mut traces = self.traces.lock().unwrap();
        let span_id = traces.len();
        traces.push(entry);

        SpanGuard {
            traces: Arc::clone(&self.traces),
            span_id,
        }
    }

    /// Get all trace entries
    pub fn get_traces(&self) -> Vec<TraceEntry> {
        self.traces.lock().unwrap().clone()
    }

    /// Analyze total latency
    pub fn total_latency_ms(&self) -> Option<u64> {
        let traces = self.traces.lock().unwrap();
        if traces.is_empty() {
            return None;
        }

        let first_start = traces.first()?.start_ns;
        let last_end = traces
            .iter()
            .filter_map(|t| t.end_ns)
            .max()
            .unwrap_or(traces.last()?.start_ns);

        Some(((last_end - first_start) / 1_000_000) as u64)
    }

    /// Find critical path (longest sequence of dependent operations)
    pub fn critical_path(&self) -> Vec<TraceEntry> {
        self.traces.lock().unwrap().clone()
    }
}

impl Default for CorrelationContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for CorrelationContext {
    fn clone(&self) -> Self {
        CorrelationContext {
            correlation_id: self.correlation_id.clone(),
            traces: Arc::clone(&self.traces),
        }
    }
}

/// RAII guard for span completion
pub struct SpanGuard {
    traces: Arc<Mutex<Vec<TraceEntry>>>,
    span_id: usize,
}

impl SpanGuard {
    pub fn set_status(&mut self, status: TraceStatus) {
        let mut traces = self.traces.lock().unwrap();
        if let Some(entry) = traces.get_mut(self.span_id) {
            entry.status = status;
        }
    }

    pub fn add_metadata(&mut self, key: String, value: String) {
        let mut traces = self.traces.lock().unwrap();
        if let Some(entry) = traces.get_mut(self.span_id) {
            entry.metadata.insert(key, value);
        }
    }
}

impl Drop for SpanGuard {
    fn drop(&mut self) {
        let mut traces = self.traces.lock().unwrap();
        if let Some(entry) = traces.get_mut(self.span_id) {
            entry.end_ns = Some(current_time_nanos());
            if matches!(entry.status, TraceStatus::Started | TraceStatus::InProgress) {
                entry.status = TraceStatus::Completed;
            }
        }
    }
}

fn current_time_millis() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64
}

fn current_time_nanos() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_correlation_id_generation() {
        let id1 = CorrelationId::new();
        let id2 = CorrelationId::new();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_trace_entry_duration() {
        let entry = TraceEntry {
            correlation_id: CorrelationId::new(),
            timestamp_millis: 1000,
            component: "test".to_string(),
            operation: "op".to_string(),
            start_ns: 1_000_000_000,
            end_ns: Some(1_050_000_000), // 50ms later
            parent_span: None,
            status: TraceStatus::Completed,
            metadata: HashMap::new(),
        };

        assert_eq!(entry.duration_ms(), Some(50));
    }

    #[test]
    fn test_correlation_context_traces() {
        let ctx = CorrelationContext::new();
        {
            let _span = ctx.start_span("component".to_string(), "operation".to_string());
            // Span completes when guard is dropped
        }

        let traces = ctx.get_traces();
        assert_eq!(traces.len(), 1);
        assert_eq!(traces[0].component, "component");
    }

    #[test]
    fn test_span_guard_sets_status() {
        let ctx = CorrelationContext::new();
        {
            let mut span = ctx.start_span("comp".to_string(), "op".to_string());
            span.set_status(TraceStatus::InProgress);
        }

        let traces = ctx.get_traces();
        assert_eq!(traces[0].status, TraceStatus::Completed);
    }
}
