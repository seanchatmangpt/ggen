//! Event sourcing system with chrono-semantic versioning
//!
//! This module provides an immutable event log that captures all changes
//! to the system with both temporal and semantic versioning.
//!
//! ## Key Concepts
//!
//! - **Event**: An immutable record of a change
//! - **Event Store**: Append-only log of all events
//! - **Event Stream**: Ordered sequence of events
//! - **Chrono-Semantic Version**: Combined temporal + semantic versioning
//!
//! ## Example
//!
//! ```rust
//! use ggen_temporal::event_sourcing::*;
//! use ggen_temporal::vector_clock::VectorClock;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut store = EventStore::new();
//! let mut clock = VectorClock::new("node-1".to_string());
//!
//! // Create an event
//! clock.tick();
//! let event = Event::new(
//!     "entity-123".to_string(),
//!     EventType::Created,
//!     EventData::GraphDelta { added: vec![], removed: vec![] },
//!     clock.timestamp(),
//! ).with_version(ChronoSemanticVersion::new(1, 0, 0));
//!
//! // Append to store
//! store.append(event)?;
//!
//! // Query events
//! let events = store.get_events_for_entity("entity-123");
//! assert_eq!(events.len(), 1);
//! # Ok(())
//! # }
//! ```

use crate::vector_clock::{NodeId, VectorTime};
use crate::{Result, TemporalError};
use chrono::{DateTime, Utc};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

/// Unique identifier for an event
pub type EventId = String;

/// Unique identifier for an entity
pub type EntityId = String;

/// Semantic version with major, minor, and patch components
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SemanticVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl SemanticVersion {
    #[must_use]
    pub const fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    /// Increment major version (breaking changes)
    pub fn bump_major(&mut self) {
        self.major += 1;
        self.minor = 0;
        self.patch = 0;
    }

    /// Increment minor version (new features)
    pub fn bump_minor(&mut self) {
        self.minor += 1;
        self.patch = 0;
    }

    /// Increment patch version (bug fixes)
    pub fn bump_patch(&mut self) {
        self.patch += 1;
    }
}

impl std::fmt::Display for SemanticVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// Semantic delta describing the nature of a change
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SemanticDelta {
    /// Breaking change (requires major version bump)
    Breaking { description: String },
    /// New feature (requires minor version bump)
    Feature { description: String },
    /// Bug fix (requires patch version bump)
    Fix { description: String },
    /// Refactoring (no version bump)
    Refactor { description: String },
    /// Documentation change (no version bump)
    Documentation { description: String },
}

impl SemanticDelta {
    /// Determine version bump from semantic delta
    #[must_use]
    pub const fn version_bump(&self) -> VersionBump {
        match self {
            Self::Breaking { .. } => VersionBump::Major,
            Self::Feature { .. } => VersionBump::Minor,
            Self::Fix { .. } => VersionBump::Patch,
            Self::Refactor { .. } | Self::Documentation { .. } => VersionBump::None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VersionBump {
    Major,
    Minor,
    Patch,
    None,
}

/// Combined chronological and semantic versioning
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ChronoSemanticVersion {
    /// Semantic version
    pub semantic: SemanticVersion,
    /// Chronological timestamp
    pub timestamp: DateTime<Utc>,
    /// Vector time for causal consistency
    pub vector_time: VectorTime,
}

impl ChronoSemanticVersion {
    #[must_use]
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            semantic: SemanticVersion::new(major, minor, patch),
            timestamp: Utc::now(),
            vector_time: VectorTime::new(),
        }
    }

    #[must_use]
    pub fn with_vector_time(mut self, vector_time: VectorTime) -> Self {
        self.vector_time = vector_time;
        self
    }

    #[must_use]
    pub fn with_timestamp(mut self, timestamp: DateTime<Utc>) -> Self {
        self.timestamp = timestamp;
        self
    }

    /// Apply a semantic delta to create a new version
    #[must_use]
    pub fn apply_delta(&self, delta: &SemanticDelta) -> Self {
        let mut semantic = self.semantic.clone();
        match delta.version_bump() {
            VersionBump::Major => semantic.bump_major(),
            VersionBump::Minor => semantic.bump_minor(),
            VersionBump::Patch => semantic.bump_patch(),
            VersionBump::None => {}
        }

        Self {
            semantic,
            timestamp: Utc::now(),
            vector_time: self.vector_time.clone(),
        }
    }
}

impl std::fmt::Display for ChronoSemanticVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} @ {} [{}]",
            self.semantic,
            self.timestamp.format("%Y-%m-%d %H:%M:%S UTC"),
            self.vector_time
        )
    }
}

/// Type of event
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum EventType {
    Created,
    Updated,
    Deleted,
    Snapshot,
    Projection,
}

/// Event data payload
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EventData {
    /// RDF graph delta (triples added/removed)
    GraphDelta {
        added: Vec<String>,
        removed: Vec<String>,
    },
    /// Code generation delta
    CodeDelta {
        file_path: String,
        diff: String,
    },
    /// Snapshot of complete state
    Snapshot { data: serde_json::Value },
    /// Custom event data
    Custom { data: serde_json::Value },
}

/// An immutable event in the event stream
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Event {
    /// Unique event ID
    pub id: EventId,
    /// Entity this event applies to
    pub entity_id: EntityId,
    /// Type of event
    pub event_type: EventType,
    /// Event payload
    pub data: EventData,
    /// Chrono-semantic version
    pub version: Option<ChronoSemanticVersion>,
    /// Vector time for causality
    pub vector_time: VectorTime,
    /// Wall-clock timestamp
    pub timestamp: DateTime<Utc>,
    /// Semantic delta description
    pub semantic_delta: Option<SemanticDelta>,
    /// Causal dependencies (IDs of events that must happen before this)
    pub causal_dependencies: Vec<EventId>,
    /// Node that generated this event
    pub node_id: Option<NodeId>,
}

impl Event {
    /// Create a new event
    #[must_use]
    pub fn new(
        entity_id: EntityId,
        event_type: EventType,
        data: EventData,
        vector_time: VectorTime,
    ) -> Self {
        Self {
            id: format!("evt_{}", Uuid::new_v4()),
            entity_id,
            event_type,
            data,
            version: None,
            vector_time,
            timestamp: Utc::now(),
            semantic_delta: None,
            causal_dependencies: Vec::new(),
            node_id: None,
        }
    }

    /// Set the chrono-semantic version
    #[must_use]
    pub fn with_version(mut self, version: ChronoSemanticVersion) -> Self {
        self.version = Some(version);
        self
    }

    /// Set the semantic delta
    #[must_use]
    pub fn with_semantic_delta(mut self, delta: SemanticDelta) -> Self {
        self.semantic_delta = Some(delta);
        self
    }

    /// Add causal dependencies
    #[must_use]
    pub fn with_dependencies(mut self, deps: Vec<EventId>) -> Self {
        self.causal_dependencies = deps;
        self
    }

    /// Set the node ID
    #[must_use]
    pub fn with_node_id(mut self, node_id: NodeId) -> Self {
        self.node_id = Some(node_id);
        self
    }
}

/// Ordered stream of events
#[derive(Debug, Clone)]
pub struct EventStream {
    events: Vec<Event>,
}

impl EventStream {
    #[must_use]
    pub const fn new(events: Vec<Event>) -> Self {
        Self { events }
    }

    /// Get all events
    #[must_use]
    pub fn events(&self) -> &[Event] {
        &self.events
    }

    /// Filter events by entity
    #[must_use]
    pub fn filter_by_entity(&self, entity_id: &str) -> Self {
        let events = self
            .events
            .iter()
            .filter(|e| e.entity_id == entity_id)
            .cloned()
            .collect();
        Self::new(events)
    }

    /// Filter events by time range
    #[must_use]
    pub fn filter_by_time_range(&self, start: DateTime<Utc>, end: DateTime<Utc>) -> Self {
        let events = self
            .events
            .iter()
            .filter(|e| e.timestamp >= start && e.timestamp <= end)
            .cloned()
            .collect();
        Self::new(events)
    }

    /// Get events that happened before a given vector time
    #[must_use]
    pub fn happened_before(&self, vector_time: &VectorTime) -> Self {
        let events = self
            .events
            .iter()
            .filter(|e| e.vector_time.happened_before(vector_time))
            .cloned()
            .collect();
        Self::new(events)
    }

    /// Sort events by causal order
    #[must_use]
    pub fn causal_order(&self) -> Self {
        let mut events = self.events.clone();
        events.sort_by(|a, b| {
            if a.vector_time.happened_before(&b.vector_time) {
                std::cmp::Ordering::Less
            } else if b.vector_time.happened_before(&a.vector_time) {
                std::cmp::Ordering::Greater
            } else {
                // Concurrent events - use timestamp as tiebreaker
                a.timestamp.cmp(&b.timestamp)
            }
        });
        Self::new(events)
    }
}

/// Append-only event store
#[derive(Clone)]
pub struct EventStore {
    /// All events indexed by ID
    events: Arc<DashMap<EventId, Event>>,
    /// Events indexed by entity ID
    entity_index: Arc<DashMap<EntityId, Vec<EventId>>>,
    /// Events in chronological order
    chronological_log: Arc<parking_lot::RwLock<Vec<EventId>>>,
}

impl EventStore {
    /// Create a new event store
    #[must_use]
    pub fn new() -> Self {
        Self {
            events: Arc::new(DashMap::new()),
            entity_index: Arc::new(DashMap::new()),
            chronological_log: Arc::new(parking_lot::RwLock::new(Vec::new())),
        }
    }

    /// Append an event to the store
    pub fn append(&self, event: Event) -> Result<EventId> {
        let event_id = event.id.clone();

        // Verify causal dependencies exist
        for dep_id in &event.causal_dependencies {
            if !self.events.contains_key(dep_id) {
                return Err(TemporalError::CausalityViolation(format!(
                    "Dependency event {dep_id} not found"
                )));
            }
        }

        // Add to entity index
        self.entity_index
            .entry(event.entity_id.clone())
            .or_insert_with(Vec::new)
            .push(event_id.clone());

        // Add to chronological log
        self.chronological_log.write().push(event_id.clone());

        // Store event
        self.events.insert(event_id.clone(), event);

        Ok(event_id)
    }

    /// Get an event by ID
    #[must_use]
    pub fn get(&self, event_id: &str) -> Option<Event> {
        self.events.get(event_id).map(|e| e.clone())
    }

    /// Get all events for an entity
    #[must_use]
    pub fn get_events_for_entity(&self, entity_id: &str) -> Vec<Event> {
        self.entity_index
            .get(entity_id)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| self.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all events in chronological order
    #[must_use]
    pub fn get_all_events(&self) -> EventStream {
        let log = self.chronological_log.read();
        let events: Vec<Event> = log.iter().filter_map(|id| self.get(id)).collect();
        EventStream::new(events)
    }

    /// Get event stream starting from a specific event
    #[must_use]
    pub fn get_stream_from(&self, start_event_id: &str) -> EventStream {
        let log = self.chronological_log.read();
        let start_idx = log
            .iter()
            .position(|id| id == start_event_id)
            .unwrap_or(0);

        let events: Vec<Event> = log
            .iter()
            .skip(start_idx)
            .filter_map(|id| self.get(id))
            .collect();

        EventStream::new(events)
    }

    /// Get the number of events in the store
    #[must_use]
    pub fn len(&self) -> usize {
        self.events.len()
    }

    /// Check if the store is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }
}

impl Default for EventStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_version() {
        let mut version = SemanticVersion::new(1, 2, 3);
        assert_eq!(version.to_string(), "1.2.3");

        version.bump_patch();
        assert_eq!(version, SemanticVersion::new(1, 2, 4));

        version.bump_minor();
        assert_eq!(version, SemanticVersion::new(1, 3, 0));

        version.bump_major();
        assert_eq!(version, SemanticVersion::new(2, 0, 0));
    }

    #[test]
    fn test_semantic_delta() {
        let delta = SemanticDelta::Breaking {
            description: "Changed API".to_string(),
        };
        assert_eq!(delta.version_bump(), VersionBump::Major);

        let delta = SemanticDelta::Feature {
            description: "New feature".to_string(),
        };
        assert_eq!(delta.version_bump(), VersionBump::Minor);
    }

    #[test]
    fn test_event_creation() {
        let event = Event::new(
            "entity-1".to_string(),
            EventType::Created,
            EventData::Custom {
                data: serde_json::json!({"key": "value"}),
            },
            VectorTime::new(),
        );

        assert!(event.id.starts_with("evt_"));
        assert_eq!(event.entity_id, "entity-1");
    }

    #[test]
    fn test_event_store_append() {
        let store = EventStore::new();
        let event = Event::new(
            "entity-1".to_string(),
            EventType::Created,
            EventData::Custom {
                data: serde_json::json!({}),
            },
            VectorTime::new(),
        );

        let event_id = store.append(event).unwrap();
        assert!(store.get(&event_id).is_some());
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_event_store_entity_query() {
        let store = EventStore::new();

        for i in 0..3 {
            let event = Event::new(
                "entity-1".to_string(),
                EventType::Updated,
                EventData::Custom {
                    data: serde_json::json!({"index": i}),
                },
                VectorTime::new(),
            );
            store.append(event).unwrap();
        }

        let events = store.get_events_for_entity("entity-1");
        assert_eq!(events.len(), 3);
    }

    #[test]
    fn test_chrono_semantic_version() {
        let version = ChronoSemanticVersion::new(1, 0, 0);
        let delta = SemanticDelta::Feature {
            description: "New feature".to_string(),
        };

        let new_version = version.apply_delta(&delta);
        assert_eq!(new_version.semantic, SemanticVersion::new(1, 1, 0));
    }

    #[test]
    fn test_event_stream_filtering() {
        let mut events = vec![];
        for i in 0..5 {
            events.push(Event::new(
                format!("entity-{i}"),
                EventType::Created,
                EventData::Custom {
                    data: serde_json::json!({}),
                },
                VectorTime::new(),
            ));
        }

        let stream = EventStream::new(events);
        let filtered = stream.filter_by_entity("entity-2");
        assert_eq!(filtered.events().len(), 1);
    }
}
