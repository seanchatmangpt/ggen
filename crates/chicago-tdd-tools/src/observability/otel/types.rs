//! OpenTelemetry Type Definitions
//!
//! Minimal OTEL type definitions ported from knhk-otel for standalone use.
//! These types are used by the OTEL validation features.
//!
//! # Poka-Yoke: Type-Level Validation
//!
//! This module uses enums instead of `Option<T>` to prevent invalid states at compile time.
//! Use `SpanState` for active vs completed spans, and `SpanRelationship` for root vs child spans.

use std::collections::BTreeMap;

/// Trace ID (128-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraceId(pub u128);

/// Span ID (64-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpanId(pub u64);

/// Span relationship type
///
/// **Poka-Yoke**: Use this enum instead of `Option<SpanId>` to prevent invalid states.
/// A span is either a root span (no parent) or a child span (has a parent).
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::otel::types::{SpanRelationship, SpanId};
///
/// // Root span - no parent
/// let root = SpanRelationship::Root;
/// assert!(matches!(root, SpanRelationship::Root));
///
/// // Child span - has parent
/// let child = SpanRelationship::Child { parent_span_id: SpanId(12345) };
/// assert!(matches!(child, SpanRelationship::Child { parent_span_id: SpanId(12345) }));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanRelationship {
    /// Root span (no parent)
    Root,
    /// Child span (has a parent)
    Child {
        /// Parent span ID
        parent_span_id: SpanId,
    },
}

impl SpanRelationship {
    /// Get the parent span ID if this is a child span
    #[must_use]
    pub const fn parent_span_id(&self) -> Option<SpanId> {
        match self {
            Self::Root => None,
            Self::Child { parent_span_id } => Some(*parent_span_id),
        }
    }

    /// Check if this is a root span
    #[must_use]
    pub const fn is_root(&self) -> bool {
        matches!(self, Self::Root)
    }

    /// Check if this is a child span
    #[must_use]
    pub const fn is_child(&self) -> bool {
        matches!(self, Self::Child { .. })
    }
}

/// Span context
#[derive(Debug, Clone)]
pub struct SpanContext {
    /// Trace ID
    pub trace_id: TraceId,
    /// Span ID
    pub span_id: SpanId,
    /// Span relationship (root or child)
    /// **Poka-Yoke**: Use enum instead of `Option<SpanId>` to prevent invalid states
    pub relationship: SpanRelationship,
    /// Context flags
    pub flags: u8,
}

impl SpanContext {
    /// Create a new root span context
    #[must_use]
    pub const fn root(trace_id: TraceId, span_id: SpanId, flags: u8) -> Self {
        Self { trace_id, span_id, relationship: SpanRelationship::Root, flags }
    }

    /// Create a new child span context
    #[must_use]
    pub const fn child(
        trace_id: TraceId,
        span_id: SpanId,
        parent_span_id: SpanId,
        flags: u8,
    ) -> Self {
        Self { trace_id, span_id, relationship: SpanRelationship::Child { parent_span_id }, flags }
    }

    /// Get the parent span ID (if this is a child span)
    #[must_use]
    pub const fn parent_span_id(&self) -> Option<SpanId> {
        self.relationship.parent_span_id()
    }

    /// Check if this is a root span
    #[must_use]
    pub const fn is_root(&self) -> bool {
        self.relationship.is_root()
    }

    /// Check if this is a child span
    #[must_use]
    pub const fn is_child(&self) -> bool {
        self.relationship.is_child()
    }
}

/// Span attributes
pub type Attributes = BTreeMap<String, String>;

/// Span event
#[derive(Debug, Clone)]
pub struct SpanEvent {
    /// Event name
    pub name: String,
    /// Timestamp in milliseconds
    pub timestamp_ms: u64,
    /// Event attributes
    pub attributes: Attributes,
}

/// Span status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanStatus {
    /// Span completed successfully
    Ok,
    /// Span ended with an error
    Error,
    /// Span status is unset
    Unset,
}

/// Span state type
///
/// **Poka-Yoke**: Use this enum instead of `Option<u64>` to prevent invalid states.
/// A span is either active (not yet ended) or completed (has an end time).
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::otel::types::SpanState;
///
/// // Active span - not yet ended
/// let active = SpanState::Active { start_time_ms: 1000 };
/// assert!(matches!(active, SpanState::Active { start_time_ms: 1000 }));
///
/// // Completed span - has end time
/// let completed = SpanState::Completed { start_time_ms: 1000, end_time_ms: 2000 };
/// assert!(matches!(completed, SpanState::Completed { start_time_ms: 1000, end_time_ms: 2000 }));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanState {
    /// Active span (not yet ended)
    Active {
        /// Start time in milliseconds since epoch
        start_time_ms: u64,
    },
    /// Completed span (has end time)
    Completed {
        /// Start time in milliseconds since epoch
        start_time_ms: u64,
        /// End time in milliseconds since epoch
        end_time_ms: u64,
    },
}

impl SpanState {
    /// Get the start time
    #[must_use]
    pub const fn start_time_ms(&self) -> u64 {
        match self {
            Self::Active { start_time_ms } | Self::Completed { start_time_ms, .. } => {
                *start_time_ms
            }
        }
    }

    /// Get the end time (if completed)
    #[must_use]
    pub const fn end_time_ms(&self) -> Option<u64> {
        match self {
            Self::Active { .. } => None,
            Self::Completed { end_time_ms, .. } => Some(*end_time_ms),
        }
    }

    /// Check if the span is active
    #[must_use]
    pub const fn is_active(&self) -> bool {
        matches!(self, Self::Active { .. })
    }

    /// Check if the span is completed
    #[must_use]
    pub const fn is_completed(&self) -> bool {
        matches!(self, Self::Completed { .. })
    }

    /// Complete the span (transition from active to completed)
    ///
    /// # Errors
    ///
    /// Returns an error if the span is already completed or if `end_time` < `start_time`.
    pub fn complete(self, end_time_ms: u64) -> Result<Self, String> {
        match self {
            Self::Active { start_time_ms } => {
                if end_time_ms < start_time_ms {
                    return Err(format!(
                        "End time {end_time_ms} must be >= start time {start_time_ms}"
                    ));
                }
                Ok(Self::Completed { start_time_ms, end_time_ms })
            }
            Self::Completed { .. } => Err("Span is already completed".to_string()),
        }
    }
}

/// Span
#[derive(Debug, Clone)]
pub struct Span {
    /// Span context (trace ID, span ID, etc.)
    pub context: SpanContext,
    /// Span name
    pub name: String,
    /// Span state (active or completed)
    /// **Poka-Yoke**: Use enum instead of `Option<u64>` to prevent invalid states
    pub state: SpanState,
    /// Span attributes (key-value pairs)
    pub attributes: Attributes,
    /// Span events (annotations)
    pub events: Vec<SpanEvent>,
    /// Span status (Ok, Error, or Unset)
    pub status: SpanStatus,
}

impl Span {
    /// Create a new active span
    #[must_use]
    pub const fn new_active(
        context: SpanContext,
        name: String,
        start_time_ms: u64,
        attributes: Attributes,
        events: Vec<SpanEvent>,
        status: SpanStatus,
    ) -> Self {
        Self {
            context,
            name,
            state: SpanState::Active { start_time_ms },
            attributes,
            events,
            status,
        }
    }

    /// Create a new completed span
    ///
    /// # Errors
    ///
    /// Returns an error if span creation fails.
    pub fn new_completed(
        context: SpanContext,
        name: String,
        start_time_ms: u64,
        end_time_ms: u64,
        attributes: Attributes,
        events: Vec<SpanEvent>,
        status: SpanStatus,
    ) -> Result<Self, String> {
        if end_time_ms < start_time_ms {
            return Err(format!("End time {end_time_ms} must be >= start time {start_time_ms}"));
        }
        Ok(Self {
            context,
            name,
            state: SpanState::Completed { start_time_ms, end_time_ms },
            attributes,
            events,
            status,
        })
    }

    /// Get the start time
    #[must_use]
    pub const fn start_time_ms(&self) -> u64 {
        self.state.start_time_ms()
    }

    /// Get the end time (if completed)
    #[must_use]
    pub const fn end_time_ms(&self) -> Option<u64> {
        self.state.end_time_ms()
    }

    /// Check if the span is active
    #[must_use]
    pub const fn is_active(&self) -> bool {
        self.state.is_active()
    }

    /// Check if the span is completed
    #[must_use]
    pub const fn is_completed(&self) -> bool {
        self.state.is_completed()
    }

    /// Complete the span (transition from active to completed)
    ///
    /// # Errors
    ///
    /// Returns an error if the span is already completed or if `end_time` < `start_time`.
    pub fn complete(&mut self, end_time_ms: u64) -> Result<(), String> {
        self.state = self.state.complete(end_time_ms)?;
        Ok(())
    }
}

/// Metric value
#[derive(Debug, Clone)]
pub enum MetricValue {
    /// Counter metric (monotonically increasing)
    Counter(u64),
    /// Gauge metric (can increase or decrease)
    Gauge(f64),
    /// Histogram metric (distribution of values)
    Histogram(Vec<u64>),
}

/// Metric
#[derive(Debug, Clone)]
pub struct Metric {
    /// Metric name
    pub name: String,
    /// Metric value (Counter, Gauge, or Histogram)
    pub value: MetricValue,
    /// Timestamp in milliseconds since epoch
    pub timestamp_ms: u64,
    /// Metric attributes (key-value pairs)
    pub attributes: Attributes,
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    // ========================================================================
    // TraceId and SpanId Tests
    // ========================================================================

    #[test]
    fn test_trace_id() {
        let trace_id = TraceId(12345);
        assert_eq!(trace_id.0, 12345, "TraceId should store value");
    }

    #[test]
    fn test_span_id() {
        let span_id = SpanId(67890);
        assert_eq!(span_id.0, 67890, "SpanId should store value");
    }

    #[test]
    fn test_trace_id_clone_copy() {
        let trace_id1 = TraceId(12345);
        let trace_id2 = trace_id1;
        assert_eq!(trace_id1.0, trace_id2.0, "TraceId should be Copy");
    }

    #[test]
    fn test_span_id_clone_copy() {
        let span_id1 = SpanId(67890);
        let span_id2 = span_id1;
        assert_eq!(span_id1.0, span_id2.0, "SpanId should be Copy");
    }

    // ========================================================================
    // SpanRelationship Tests
    // ========================================================================

    #[test]
    fn test_span_relationship_root() {
        let relationship = SpanRelationship::Root;
        assert!(relationship.is_root(), "Root should be root");
        assert!(!relationship.is_child(), "Root should not be child");
        assert_eq!(relationship.parent_span_id(), None, "Root should have no parent");
    }

    #[test]
    fn test_span_relationship_child() {
        let parent_id = SpanId(12345);
        let relationship = SpanRelationship::Child { parent_span_id: parent_id };
        assert!(!relationship.is_root(), "Child should not be root");
        assert!(relationship.is_child(), "Child should be child");
        assert_eq!(relationship.parent_span_id(), Some(parent_id), "Child should have parent");
    }

    #[test]
    fn test_span_relationship_parent_span_id() {
        let parent_id = SpanId(12345);
        let root = SpanRelationship::Root;
        let child = SpanRelationship::Child { parent_span_id: parent_id };

        assert_eq!(root.parent_span_id(), None, "Root should return None");
        assert_eq!(child.parent_span_id(), Some(parent_id), "Child should return parent ID");
    }

    // ========================================================================
    // SpanContext Tests
    // ========================================================================

    #[test]
    fn test_span_context_root() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 0);

        assert_eq!(context.trace_id.0, 12345, "TraceId should match");
        assert_eq!(context.span_id.0, 67890, "SpanId should match");
        assert!(context.is_root(), "Context should be root");
        assert!(!context.is_child(), "Context should not be child");
        assert_eq!(context.parent_span_id(), None, "Root context should have no parent");
    }

    #[test]
    fn test_span_context_child() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let parent_id = SpanId(11111);
        let context = SpanContext::child(trace_id, span_id, parent_id, 0);

        assert_eq!(context.trace_id.0, 12345, "TraceId should match");
        assert_eq!(context.span_id.0, 67890, "SpanId should match");
        assert!(!context.is_root(), "Context should not be root");
        assert!(context.is_child(), "Context should be child");
        assert_eq!(context.parent_span_id(), Some(parent_id), "Child context should have parent");
    }

    // ========================================================================
    // SpanState Tests
    // ========================================================================

    #[test]
    fn test_span_state_active() {
        let state = SpanState::Active { start_time_ms: 1000 };
        assert!(state.is_active(), "State should be active");
        assert!(!state.is_completed(), "State should not be completed");
        assert_eq!(state.start_time_ms(), 1000, "Start time should match");
        assert_eq!(state.end_time_ms(), None, "Active span should have no end time");
    }

    #[test]
    fn test_span_state_completed() {
        let state = SpanState::Completed { start_time_ms: 1000, end_time_ms: 2000 };
        assert!(!state.is_active(), "State should not be active");
        assert!(state.is_completed(), "State should be completed");
        assert_eq!(state.start_time_ms(), 1000, "Start time should match");
        assert_eq!(state.end_time_ms(), Some(2000), "End time should match");
    }

    #[test]
    fn test_span_state_complete_success() {
        let active = SpanState::Active { start_time_ms: 1000 };
        let completed = active.complete(2000).expect("Should complete successfully");

        assert!(completed.is_completed(), "Should be completed");
        assert_eq!(completed.start_time_ms(), 1000, "Start time should be preserved");
        assert_eq!(completed.end_time_ms(), Some(2000), "End time should be set");
    }

    #[test]
    fn test_span_state_complete_already_completed() {
        let completed = SpanState::Completed { start_time_ms: 1000, end_time_ms: 2000 };
        let result = completed.complete(3000);

        assert!(result.is_err(), "Should fail if already completed");
        assert!(
            result.unwrap_err().contains("already completed"),
            "Error should mention already completed"
        );
    }

    #[test]
    fn test_span_state_complete_invalid_time() {
        let active = SpanState::Active { start_time_ms: 2000 };
        let result = active.complete(1000);

        assert!(result.is_err(), "Should fail if end_time < start_time");
        assert!(result.unwrap_err().contains("must be >="), "Error should mention time constraint");
    }

    // ========================================================================
    // SpanStatus Tests
    // ========================================================================

    #[test]
    fn test_span_status_variants() {
        assert_eq!(SpanStatus::Ok, SpanStatus::Ok, "Ok should equal Ok");
        assert_eq!(SpanStatus::Error, SpanStatus::Error, "Error should equal Error");
        assert_eq!(SpanStatus::Unset, SpanStatus::Unset, "Unset should equal Unset");
        assert_ne!(SpanStatus::Ok, SpanStatus::Error, "Ok should not equal Error");
    }

    // ========================================================================
    // Span Tests
    // ========================================================================

    #[test]
    fn test_span_new_active() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 0);
        let attributes = Attributes::new();
        let events = Vec::new();

        let span = Span::new_active(
            context.clone(),
            "test_span".to_string(),
            1000,
            attributes.clone(),
            events.clone(),
            SpanStatus::Ok,
        );

        assert_eq!(span.name, "test_span", "Name should match");
        assert!(span.is_active(), "Span should be active");
        assert!(!span.is_completed(), "Span should not be completed");
        assert_eq!(span.start_time_ms(), 1000, "Start time should match");
        assert_eq!(span.end_time_ms(), None, "Active span should have no end time");
        assert_eq!(span.status, SpanStatus::Ok, "Status should match");
    }

    #[test]
    fn test_span_new_completed() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 0);
        let attributes = Attributes::new();
        let events = Vec::new();

        let span = Span::new_completed(
            context.clone(),
            "test_span".to_string(),
            1000,
            2000,
            attributes.clone(),
            events.clone(),
            SpanStatus::Ok,
        )
        .expect("Should create completed span");

        assert_eq!(span.name, "test_span", "Name should match");
        assert!(!span.is_active(), "Span should not be active");
        assert!(span.is_completed(), "Span should be completed");
        assert_eq!(span.start_time_ms(), 1000, "Start time should match");
        assert_eq!(span.end_time_ms(), Some(2000), "End time should match");
    }

    #[test]
    fn test_span_new_completed_invalid_time() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 0);
        let attributes = Attributes::new();
        let events = Vec::new();

        let result = Span::new_completed(
            context,
            "test_span".to_string(),
            2000,
            1000,
            attributes,
            events,
            SpanStatus::Ok,
        );

        assert!(result.is_err(), "Should fail if end_time < start_time");
        assert!(result.unwrap_err().contains("must be >="), "Error should mention time constraint");
    }

    #[test]
    fn test_span_complete() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 0);
        let attributes = Attributes::new();
        let events = Vec::new();

        let mut span = Span::new_active(
            context,
            "test_span".to_string(),
            1000,
            attributes,
            events,
            SpanStatus::Ok,
        );
        assert!(span.is_active(), "Span should start active");

        span.complete(2000).expect("Should complete successfully");
        assert!(span.is_completed(), "Span should be completed");
        assert_eq!(span.end_time_ms(), Some(2000), "End time should be set");
    }

    #[test]
    fn test_span_complete_already_completed() {
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 0);
        let attributes = Attributes::new();
        let events = Vec::new();

        let mut span = Span::new_completed(
            context,
            "test_span".to_string(),
            1000,
            2000,
            attributes,
            events,
            SpanStatus::Ok,
        )
        .expect("Should create completed span");
        let result = span.complete(3000);

        assert!(result.is_err(), "Should fail if already completed");
    }

    // ========================================================================
    // SpanEvent Tests
    // ========================================================================

    #[test]
    fn test_span_event() {
        let mut attributes = Attributes::new();
        attributes.insert("key1".to_string(), "value1".to_string());

        let event = SpanEvent { name: "test_event".to_string(), timestamp_ms: 1000, attributes };

        assert_eq!(event.name, "test_event", "Event name should match");
        assert_eq!(event.timestamp_ms, 1000, "Timestamp should match");
        assert_eq!(event.attributes.len(), 1, "Attributes should have 1 entry");
    }

    // ========================================================================
    // MetricValue Tests
    // ========================================================================

    #[test]
    fn test_metric_value_counter() {
        let value = MetricValue::Counter(42);
        match value {
            MetricValue::Counter(v) => assert_eq!(v, 42, "Counter value should match"),
            _ => panic!("Expected Counter variant"),
        }
    }

    #[test]
    fn test_metric_value_gauge() {
        let value = MetricValue::Gauge(3.14);
        match value {
            MetricValue::Gauge(v) => assert_eq!(v, 3.14, "Gauge value should match"),
            _ => panic!("Expected Gauge variant"),
        }
    }

    #[test]
    fn test_metric_value_histogram() {
        let buckets = vec![1, 2, 3, 4, 5];
        let value = MetricValue::Histogram(buckets.clone());
        match value {
            MetricValue::Histogram(v) => assert_eq!(v, buckets, "Histogram buckets should match"),
            _ => panic!("Expected Histogram variant"),
        }
    }

    // ========================================================================
    // Metric Tests
    // ========================================================================

    #[test]
    fn test_metric() {
        let mut attributes = Attributes::new();
        attributes.insert("key1".to_string(), "value1".to_string());

        let metric = Metric {
            name: "test_metric".to_string(),
            value: MetricValue::Counter(42),
            timestamp_ms: 1000,
            attributes,
        };

        assert_eq!(metric.name, "test_metric", "Metric name should match");
        assert_eq!(metric.timestamp_ms, 1000, "Timestamp should match");
        match metric.value {
            MetricValue::Counter(v) => assert_eq!(v, 42, "Counter value should match"),
            _ => panic!("Expected Counter variant"),
        }
    }
}
