//! Durable event store for failure event sourcing
//!
//! Records all failure events enabling deterministic replay and analysis
//! of system behavior under failure conditions.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// All possible failure events in the system
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FailureEvent {
    /// Component panicked during operation
    PanicOccurred {
        component: String,
        backtrace: String,
        timestamp: u64,
    },
    /// Lock acquisition timed out
    LockTimeout {
        component: String,
        duration: u64,
        timestamp: u64,
    },
    /// Network partition simulated
    NetworkPartition {
        node: String,
        duration_ms: u64,
        timestamp: u64,
    },
    /// Task was forcibly killed
    TaskKilled {
        task_id: String,
        component: String,
        timestamp: u64,
    },
    /// Clock was advanced (time skew)
    ClockSkew { amount_ms: u64, timestamp: u64 },
    /// Resource exhaustion (memory, connections, etc.)
    ResourceExhaustion {
        resource_type: String,
        component: String,
        available: u64,
        required: u64,
        timestamp: u64,
    },
    /// Cascading failure initiated
    CascadingFailureStart {
        root_cause: String,
        affected_components: Vec<String>,
        timestamp: u64,
    },
    /// Recovery action taken
    RecoveryAction {
        action: String,
        component: String,
        success: bool,
        timestamp: u64,
    },
}

impl FailureEvent {
    pub fn timestamp(&self) -> u64 {
        match self {
            FailureEvent::PanicOccurred { timestamp, .. } => *timestamp,
            FailureEvent::LockTimeout { timestamp, .. } => *timestamp,
            FailureEvent::NetworkPartition { timestamp, .. } => *timestamp,
            FailureEvent::TaskKilled { timestamp, .. } => *timestamp,
            FailureEvent::ClockSkew { timestamp, .. } => *timestamp,
            FailureEvent::ResourceExhaustion { timestamp, .. } => *timestamp,
            FailureEvent::CascadingFailureStart { timestamp, .. } => *timestamp,
            FailureEvent::RecoveryAction { timestamp, .. } => *timestamp,
        }
    }

    pub fn component(&self) -> Option<&str> {
        match self {
            FailureEvent::PanicOccurred { component, .. } => Some(component),
            FailureEvent::LockTimeout { component, .. } => Some(component),
            FailureEvent::NetworkPartition { node, .. } => Some(node),
            FailureEvent::TaskKilled { component, .. } => Some(component),
            FailureEvent::ClockSkew { .. } => None,
            FailureEvent::ResourceExhaustion { component, .. } => Some(component),
            FailureEvent::CascadingFailureStart { .. } => None,
            FailureEvent::RecoveryAction { component, .. } => Some(component),
        }
    }
}

/// Event store trait for persistent failure recording
pub trait EventStore: Send + Sync {
    /// Record a failure event
    fn record(&mut self, event: FailureEvent) -> Result<(), String>;

    /// Get all recorded events
    fn get_all(&self) -> Vec<FailureEvent>;

    /// Get events for a specific component
    fn get_for_component(&self, component: &str) -> Vec<FailureEvent>;

    /// Get events within a time range
    fn get_between(&self, start: u64, end: u64) -> Vec<FailureEvent>;

    /// Clear all events
    fn clear(&mut self);

    /// Get event count
    fn len(&self) -> usize;

    /// Check if empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Export events to JSON for analysis
    fn export_json(&self) -> Result<String, String>;
}

/// In-memory event store for testing
pub struct InMemoryEventStore {
    events: Arc<Mutex<Vec<FailureEvent>>>,
}

impl InMemoryEventStore {
    pub fn new() -> Self {
        InMemoryEventStore {
            events: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl Default for InMemoryEventStore {
    fn default() -> Self {
        Self::new()
    }
}

impl EventStore for InMemoryEventStore {
    fn record(&mut self, event: FailureEvent) -> Result<(), String> {
        self.events.lock().map_err(|e| e.to_string())?.push(event);
        Ok(())
    }

    fn get_all(&self) -> Vec<FailureEvent> {
        self.events.lock().unwrap().clone()
    }

    fn get_for_component(&self, component: &str) -> Vec<FailureEvent> {
        self.events
            .lock()
            .unwrap()
            .iter()
            .filter(|e| e.component().map_or(false, |c| c == component))
            .cloned()
            .collect()
    }

    fn get_between(&self, start: u64, end: u64) -> Vec<FailureEvent> {
        self.events
            .lock()
            .unwrap()
            .iter()
            .filter(|e| {
                let ts = e.timestamp();
                ts >= start && ts <= end
            })
            .cloned()
            .collect()
    }

    fn clear(&mut self) {
        self.events.lock().unwrap().clear();
    }

    fn len(&self) -> usize {
        self.events.lock().unwrap().len()
    }

    fn export_json(&self) -> Result<String, String> {
        let events = self.get_all();
        serde_json::to_string_pretty(&events).map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_event() {
        let mut store = InMemoryEventStore::new();
        let event = FailureEvent::PanicOccurred {
            component: "test".to_string(),
            backtrace: "frame 1\nframe 2".to_string(),
            timestamp: 1000,
        };

        assert!(store.record(event).is_ok());
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_get_for_component() {
        let mut store = InMemoryEventStore::new();
        let _: Result<(), _> = (|| {
            store.record(FailureEvent::PanicOccurred {
                component: "comp-a".to_string(),
                backtrace: "".to_string(),
                timestamp: 1000,
            })?;
            store.record(FailureEvent::PanicOccurred {
                component: "comp-b".to_string(),
                backtrace: "".to_string(),
                timestamp: 1001,
            })?;
            Ok(())
        })();

        let comp_a_events = store.get_for_component("comp-a");
        assert_eq!(comp_a_events.len(), 1);
    }

    #[test]
    fn test_get_between_timestamps() {
        let mut store = InMemoryEventStore::new();
        let _: Result<(), _> = (|| {
            store.record(FailureEvent::ClockSkew {
                amount_ms: 100,
                timestamp: 1000,
            })?;
            store.record(FailureEvent::ClockSkew {
                amount_ms: 200,
                timestamp: 2000,
            })?;
            store.record(FailureEvent::ClockSkew {
                amount_ms: 300,
                timestamp: 3000,
            })?;
            Ok(())
        })();

        let events = store.get_between(1500, 2500);
        assert_eq!(events.len(), 1);
    }

    #[test]
    fn test_export_json() {
        let mut store = InMemoryEventStore::new();
        let _: Result<(), _> = (|| {
            store.record(FailureEvent::TaskKilled {
                task_id: "task-1".to_string(),
                component: "executor".to_string(),
                timestamp: 1000,
            })?;
            Ok(())
        })();

        let json = store.export_json().expect("JSON export failed");
        assert!(json.contains("task-1"));
    }
}
