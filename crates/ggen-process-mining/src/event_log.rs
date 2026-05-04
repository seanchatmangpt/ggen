//! Event log data structures for process mining.
//!
//! This module provides core types for representing event logs, utilizing
//! the high-performance types from the pictl-types crate.

pub use pictl_types::event_log::{
    AttributeValue, Attributes, Event, EventLog, Trace,
};

/// Extension trait for EventLog to maintain compatibility with ggen API.
pub trait EventLogExt {
    /// Create a new empty event log.
    fn new_empty(name: &str) -> Self;
    /// Get unique activities.
    fn unique_activities(&self, activity_key: &str) -> Vec<String>;
    /// Get the total number of events across all traces.
    fn total_events(&self) -> usize;
}

impl EventLogExt for EventLog {
    fn new_empty(name: &str) -> Self {
        let mut attributes = std::collections::HashMap::new();
        attributes.insert(
            "concept:name".to_string(),
            AttributeValue::String(name.to_string()),
        );
        Self::new(Vec::new(), attributes)
    }

    fn unique_activities(&self, activity_key: &str) -> Vec<String> {
        self.get_activities(activity_key)
    }

    fn total_events(&self) -> usize {
        self.event_count()
    }
}
