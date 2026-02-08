//! Event log data structures for process mining.
//!
//! This module provides core types for representing event logs, which are the
//! primary input for process discovery and conformance checking algorithms.

use crate::error::{Error, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single event in an event log.
///
/// Events represent discrete occurrences during process execution, such as
/// task completion, state changes, or resource assignments.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Event {
    /// Unique identifier for this event.
    pub id: String,

    /// Activity/task name performed in this event.
    pub activity: String,

    /// Timestamp when the event occurred.
    pub timestamp: DateTime<Utc>,

    /// Optional resource/person who performed the activity.
    pub resource: Option<String>,

    /// Additional event attributes (lifecycle:transition, cost, etc.).
    pub attributes: HashMap<String, AttributeValue>,
}

/// Value types for event attributes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AttributeValue {
    /// String value.
    String(String),
    /// Integer value.
    Integer(i64),
    /// Float value.
    Float(f64),
    /// Boolean value.
    Boolean(bool),
    /// Timestamp value.
    Timestamp(DateTime<Utc>),
    /// List of values.
    List(Vec<AttributeValue>),
}

impl Event {
    /// Create a new event with minimal required fields.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::Event;
    ///
    /// let event = Event::new(
    ///     "evt-1",
    ///     "Approve Invoice",
    ///     "2024-01-15T10:30:00Z"
    /// ).unwrap();
    /// ```
    pub fn new(id: impl Into<String>, activity: impl Into<String>, timestamp: &str) -> Result<Self> {
        let ts = timestamp.parse::<DateTime<Utc>>()
            .map_err(|_| Error::invalid_timestamp(timestamp.to_string()))?;

        Ok(Self {
            id: id.into(),
            activity: activity.into(),
            timestamp: ts,
            resource: None,
            attributes: HashMap::new(),
        })
    }

    /// Set the resource for this event.
    #[must_use]
    pub fn with_resource(mut self, resource: impl Into<String>) -> Self {
        self.resource = Some(resource.into());
        self
    }

    /// Add an attribute to this event.
    #[must_use]
    pub fn with_attribute(mut self, key: impl Into<String>, value: AttributeValue) -> Self {
        self.attributes.insert(key.into(), value);
        self
    }
}

/// A trace (sequence of events) representing a single process instance.
///
/// Traces are the primary unit of analysis in process mining, representing
/// one complete execution of a process from start to end.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Trace {
    /// Unique identifier for this trace/case.
    pub case_id: String,

    /// Ordered sequence of events in this trace.
    pub events: Vec<Event>,
}

impl Trace {
    /// Create a new empty trace.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::Trace;
    ///
    /// let trace = Trace::new("case-001");
    /// ```
    #[must_use]
    pub fn new(case_id: impl Into<String>) -> Self {
        Self {
            case_id: case_id.into(),
            events: Vec::new(),
        }
    }

    /// Add an event to this trace.
    #[must_use]
    pub fn with_event(mut self, event: Event) -> Self {
        self.events.push(event);
        self
    }

    /// Get the activity sequence (names only) for this trace.
    ///
    /// This is useful for process discovery algorithms that work with
    /// activity sequences rather than full event objects.
    #[must_use]
    pub fn activity_sequence(&self) -> Vec<&str> {
        self.events.iter().map(|e| e.activity.as_str()).collect()
    }

    /// Validate this trace has events in chronological order.
    ///
    /// Returns an error if timestamps are not monotonically increasing.
    pub fn validate_chronological(&self) -> Result<()> {
        let mut prev_ts = None;

        for event in &self.events {
            if let Some(prev) = prev_ts {
                if event.timestamp < prev {
                    return Err(Error::invalid_trace(
                        self.case_id.clone(),
                        format!("event {} is out of order", event.id),
                    ));
                }
            }
            prev_ts = Some(event.timestamp);
        }

        Ok(())
    }
}

/// A complete event log containing multiple traces.
///
/// Event logs are the primary input for process mining operations, containing
/// all observed process instances.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EventLog {
    /// Name/title of this event log.
    pub name: String,

    /// All traces in this log.
    pub traces: Vec<Trace>,

    /// Global log attributes (source, version, etc.).
    pub extensions: HashMap<String, String>,
}

impl EventLog {
    /// Create a new empty event log.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process mining::EventLog;
    ///
    /// let log = EventLog::new("Purchase Process Log");
    /// ```
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            traces: Vec::new(),
            extensions: HashMap::new(),
        }
    }

    /// Add a trace to this log.
    #[must_use]
    pub fn with_trace(mut self, trace: Trace) -> Self {
        self.traces.push(trace);
        self
    }

    /// Add a global extension/attribute.
    #[must_use]
    pub fn with_extension(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.extensions.insert(key.into(), value.into());
        self
    }

    /// Get all unique activities across all traces.
    ///
    /// This is useful for process discovery to determine the activity set.
    #[must_use]
    pub fn unique_activities(&self) -> Vec<String> {
        let mut activities = Vec::new();

        for trace in &self.traces {
            for event in &trace.events {
                if !activities.contains(&event.activity) {
                    activities.push(event.activity.clone());
                }
            }
        }

        activities
    }

    /// Get the total number of events across all traces.
    #[must_use]
    pub fn total_events(&self) -> usize {
        self.traces.iter().map(|t| t.events.len()).sum()
    }

    /// Validate all traces in this log.
    ///
    /// Returns an error if any trace is invalid.
    pub fn validate(&self) -> Result<()> {
        for trace in &self.traces {
            trace.validate_chronological()?;
        }
        Ok(())
    }

    /// Get traces filtered by a predicate.
    #[must_use]
    pub fn filter_traces<F>(&self, predicate: F) -> Self
    where
        F: Fn(&Trace) -> bool,
    {
        Self {
            name: self.name.clone(),
            traces: self.traces.iter().filter(|t| predicate(t)).cloned().collect(),
            extensions: self.extensions.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_event(id: &str, activity: &str, timestamp: &str) -> Event {
        Event::new(id, activity, timestamp).unwrap()
    }

    #[test]
    fn test_event_creation() {
        let event = Event::new("evt-1", "Test Activity", "2024-01-15T10:00:00Z")
            .unwrap()
            .with_resource("user-1")
            .with_attribute("cost", AttributeValue::Integer(100));

        assert_eq!(event.id, "evt-1");
        assert_eq!(event.activity, "Test Activity");
        assert_eq!(event.resource, Some("user-1".to_string()));
    }

    #[test]
    fn test_event_invalid_timestamp() {
        let result = Event::new("evt-1", "Test Activity", "invalid-timestamp");
        assert!(matches!(result, Err(Error::InvalidTimestamp { .. })));
    }

    #[test]
    fn test_trace_activity_sequence() {
        let trace = Trace::new("case-1")
            .with_event(make_test_event("e1", "A", "2024-01-15T10:00:00Z"))
            .with_event(make_test_event("e2", "B", "2024-01-15T11:00:00Z"))
            .with_event(make_test_event("e3", "C", "2024-01-15T12:00:00Z"));

        let sequence = trace.activity_sequence();
        assert_eq!(sequence, vec!["A", "B", "C"]);
    }

    #[test]
    fn test_trace_chronological_validation() {
        let valid_trace = Trace::new("case-1")
            .with_event(make_test_event("e1", "A", "2024-01-15T10:00:00Z"))
            .with_event(make_test_event("e2", "B", "2024-01-15T11:00:00Z"));

        assert!(valid_trace.validate_chronological().is_ok());

        let invalid_trace = Trace::new("case-2")
            .with_event(make_test_event("e1", "A", "2024-01-15T11:00:00Z"))
            .with_event(make_test_event("e2", "B", "2024-01-15T10:00:00Z"));

        assert!(invalid_trace.validate_chronological().is_err());
    }

    #[test]
    fn test_event_log_unique_activities() {
        let trace1 = Trace::new("case-1")
            .with_event(make_test_event("e1", "A", "2024-01-15T10:00:00Z"))
            .with_event(make_test_event("e2", "B", "2024-01-15T11:00:00Z"));

        let trace2 = Trace::new("case-2")
            .with_event(make_test_event("e3", "A", "2024-01-15T10:00:00Z"))
            .with_event(make_test_event("e4", "C", "2024-01-15T11:00:00Z"));

        let log = EventLog::new("Test Log")
            .with_trace(trace1)
            .with_trace(trace2);

        let activities = log.unique_activities();
        assert_eq!(activities.len(), 3);
        assert!(activities.contains(&"A".to_string()));
        assert!(activities.contains(&"B".to_string()));
        assert!(activities.contains(&"C".to_string()));
    }

    #[test]
    fn test_event_log_total_events() {
        let trace1 = Trace::new("case-1")
            .with_event(make_test_event("e1", "A", "2024-01-15T10:00:00Z"))
            .with_event(make_test_event("e2", "B", "2024-01-15T11:00:00Z"));

        let trace2 = Trace::new("case-2")
            .with_event(make_test_event("e3", "C", "2024-01-15T10:00:00Z"));

        let log = EventLog::new("Test Log")
            .with_trace(trace1)
            .with_trace(trace2);

        assert_eq!(log.total_events(), 3);
    }

    #[test]
    fn test_event_log_filter() {
        let trace1 = Trace::new("case-1")
            .with_event(make_test_event("e1", "A", "2024-01-15T10:00:00Z"));

        let trace2 = Trace::new("case-2")
            .with_event(make_test_event("e2", "B", "2024-01-15T11:00:00Z"));

        let log = EventLog::new("Test Log")
            .with_trace(trace1)
            .with_trace(trace2);

        let filtered = log.filter_traces(|t| t.case_id == "case-1");
        assert_eq!(filtered.traces.len(), 1);
        assert_eq!(filtered.traces[0].case_id, "case-1");
    }
}
