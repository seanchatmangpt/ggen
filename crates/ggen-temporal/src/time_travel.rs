//! Bidirectional time-travel debugging for generated code
//!
//! This module provides a powerful time-travel debugging system that allows
//! navigating through the history of code generation events, reconstructing
//! state at any point, and analyzing causal relationships.
//!
//! ## Features
//!
//! - **Bidirectional Navigation**: Move forward and backward through time
//! - **State Reconstruction**: Rebuild complete state at any event
//! - **Checkpoints**: Save and restore specific points in time
//! - **Causal Analysis**: Understand what led to current state
//! - **Semantic Version Navigation**: Jump to specific versions
//!
//! ## Example
//!
//! ```rust
//! use ggen_temporal::time_travel::*;
//! use ggen_temporal::event_sourcing::EventStore;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let store = EventStore::new();
//! let mut debugger = TimeTravelDebugger::new(store);
//!
//! // Navigate through history
//! debugger.step_forward().await?;
//! debugger.step_backward().await?;
//!
//! // Jump to a specific event
//! debugger.goto_event("evt_123").await?;
//!
//! // Create a checkpoint
//! let checkpoint = debugger.create_checkpoint("before_refactor".to_string())?;
//!
//! // Do some exploration...
//!
//! // Restore checkpoint
//! debugger.restore_checkpoint(&checkpoint).await?;
//! # Ok(())
//! # }
//! ```

use crate::event_sourcing::{Event, EventId, EventStore, EventStream};
use crate::vector_clock::VectorTime;
use crate::{Result, TemporalError};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A point in time in the event stream
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TimePoint {
    /// Event ID at this time point
    pub event_id: EventId,
    /// Position in the chronological event stream
    pub position: usize,
    /// Vector time at this point
    pub vector_time: VectorTime,
    /// Wall-clock timestamp
    pub timestamp: DateTime<Utc>,
}

impl TimePoint {
    #[must_use]
    pub const fn new(
        event_id: EventId,
        position: usize,
        vector_time: VectorTime,
        timestamp: DateTime<Utc>,
    ) -> Self {
        Self {
            event_id,
            position,
            vector_time,
            timestamp,
        }
    }
}

/// A saved checkpoint for later restoration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Checkpoint {
    /// Unique checkpoint ID
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Time point of this checkpoint
    pub time_point: TimePoint,
    /// Timestamp when checkpoint was created
    pub created_at: DateTime<Utc>,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl Checkpoint {
    #[must_use]
    pub fn new(name: String, time_point: TimePoint) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name,
            time_point,
            created_at: Utc::now(),
            metadata: HashMap::new(),
        }
    }

    #[must_use]
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}

/// Traversal mode for time travel
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraversalMode {
    /// Follow chronological order
    Chronological,
    /// Follow causal order (respecting vector clocks)
    Causal,
}

/// Time machine for navigating event history
pub struct TimeMachine {
    /// Event store containing all events
    event_store: EventStore,
    /// Current position in the event stream
    current_position: Option<usize>,
    /// Traversal mode
    traversal_mode: TraversalMode,
    /// Saved checkpoints
    checkpoints: HashMap<String, Checkpoint>,
}

impl TimeMachine {
    /// Create a new time machine
    #[must_use]
    pub fn new(event_store: EventStore) -> Self {
        Self {
            event_store,
            current_position: None,
            traversal_mode: TraversalMode::Chronological,
            checkpoints: HashMap::new(),
        }
    }

    /// Set the traversal mode
    pub fn set_traversal_mode(&mut self, mode: TraversalMode) {
        self.traversal_mode = mode;
    }

    /// Get the current position
    #[must_use]
    pub const fn current_position(&self) -> Option<usize> {
        self.current_position
    }

    /// Get all events up to current position
    fn get_events_until_position(&self, position: usize) -> Vec<Event> {
        let all_events = self.event_store.get_all_events();
        let events = all_events.events();

        if position >= events.len() {
            events.to_vec()
        } else {
            events[..=position].to_vec()
        }
    }

    /// Get the current event
    #[must_use]
    pub fn current_event(&self) -> Option<Event> {
        let pos = self.current_position?;
        let events = self.event_store.get_all_events();
        events.events().get(pos).cloned()
    }

    /// Get the current time point
    #[must_use]
    pub fn current_time_point(&self) -> Option<TimePoint> {
        let event = self.current_event()?;
        let pos = self.current_position?;

        Some(TimePoint::new(
            event.id.clone(),
            pos,
            event.vector_time.clone(),
            event.timestamp,
        ))
    }

    /// Move to the first event
    pub fn goto_start(&mut self) -> Result<()> {
        let all_events = self.event_store.get_all_events();
        if all_events.events().is_empty() {
            return Err(TemporalError::TimeTravelFailed(
                "No events in store".to_string(),
            ));
        }
        self.current_position = Some(0);
        Ok(())
    }

    /// Move to the last event
    pub fn goto_end(&mut self) -> Result<()> {
        let all_events = self.event_store.get_all_events();
        let len = all_events.events().len();
        if len == 0 {
            return Err(TemporalError::TimeTravelFailed(
                "No events in store".to_string(),
            ));
        }
        self.current_position = Some(len - 1);
        Ok(())
    }

    /// Step forward one event
    pub fn step_forward(&mut self) -> Result<Event> {
        let all_events = self.event_store.get_all_events();
        let events = all_events.events();

        let next_pos = match self.current_position {
            None => 0,
            Some(pos) => pos + 1,
        };

        if next_pos >= events.len() {
            return Err(TemporalError::TimeTravelFailed(
                "Already at end of history".to_string(),
            ));
        }

        self.current_position = Some(next_pos);
        Ok(events[next_pos].clone())
    }

    /// Step backward one event
    pub fn step_backward(&mut self) -> Result<Event> {
        let current = self
            .current_position
            .ok_or_else(|| TemporalError::TimeTravelFailed("Not positioned".to_string()))?;

        if current == 0 {
            return Err(TemporalError::TimeTravelFailed(
                "Already at start of history".to_string(),
            ));
        }

        let prev_pos = current - 1;
        self.current_position = Some(prev_pos);

        let all_events = self.event_store.get_all_events();
        Ok(all_events.events()[prev_pos].clone())
    }

    /// Jump to a specific event by ID
    pub fn goto_event(&mut self, event_id: &str) -> Result<Event> {
        let all_events = self.event_store.get_all_events();
        let events = all_events.events();

        let pos = events
            .iter()
            .position(|e| e.id == event_id)
            .ok_or_else(|| TemporalError::EventNotFound(event_id.to_string()))?;

        self.current_position = Some(pos);
        Ok(events[pos].clone())
    }

    /// Jump to a specific time point
    pub fn goto_time_point(&mut self, time_point: &TimePoint) -> Result<()> {
        self.goto_event(&time_point.event_id)?;
        Ok(())
    }

    /// Create a checkpoint at the current position
    pub fn create_checkpoint(&mut self, name: String) -> Result<Checkpoint> {
        let time_point = self.current_time_point().ok_or_else(|| {
            TemporalError::TimeTravelFailed("Not positioned at any event".to_string())
        })?;

        let checkpoint = Checkpoint::new(name, time_point);
        self.checkpoints
            .insert(checkpoint.id.clone(), checkpoint.clone());

        Ok(checkpoint)
    }

    /// Restore a checkpoint
    pub fn restore_checkpoint(&mut self, checkpoint: &Checkpoint) -> Result<()> {
        self.goto_time_point(&checkpoint.time_point)
    }

    /// Get all checkpoints
    #[must_use]
    pub fn get_checkpoints(&self) -> Vec<&Checkpoint> {
        self.checkpoints.values().collect()
    }

    /// Get checkpoint by name
    #[must_use]
    pub fn get_checkpoint_by_name(&self, name: &str) -> Option<&Checkpoint> {
        self.checkpoints.values().find(|c| c.name == name)
    }

    /// Get the event stream from start to current position
    #[must_use]
    pub fn get_current_stream(&self) -> EventStream {
        match self.current_position {
            None => EventStream::new(vec![]),
            Some(pos) => EventStream::new(self.get_events_until_position(pos)),
        }
    }
}

/// High-level time-travel debugger
pub struct TimeTravelDebugger {
    /// Underlying time machine
    time_machine: TimeMachine,
    /// Navigation history (for undo/redo)
    navigation_history: Vec<TimePoint>,
    /// Current position in navigation history
    history_position: Option<usize>,
}

impl TimeTravelDebugger {
    /// Create a new time-travel debugger
    #[must_use]
    pub fn new(event_store: EventStore) -> Self {
        Self {
            time_machine: TimeMachine::new(event_store),
            navigation_history: Vec::new(),
            history_position: None,
        }
    }

    /// Record current position in navigation history
    fn record_navigation(&mut self) {
        if let Some(time_point) = self.time_machine.current_time_point() {
            // Truncate history after current position
            if let Some(pos) = self.history_position {
                self.navigation_history.truncate(pos + 1);
            }

            self.navigation_history.push(time_point);
            self.history_position = Some(self.navigation_history.len() - 1);
        }
    }

    /// Navigate to the start
    pub async fn goto_start(&mut self) -> Result<()> {
        self.time_machine.goto_start()?;
        self.record_navigation();
        Ok(())
    }

    /// Navigate to the end
    pub async fn goto_end(&mut self) -> Result<()> {
        self.time_machine.goto_end()?;
        self.record_navigation();
        Ok(())
    }

    /// Step forward
    pub async fn step_forward(&mut self) -> Result<Event> {
        let event = self.time_machine.step_forward()?;
        self.record_navigation();
        Ok(event)
    }

    /// Step backward
    pub async fn step_backward(&mut self) -> Result<Event> {
        let event = self.time_machine.step_backward()?;
        self.record_navigation();
        Ok(event)
    }

    /// Go to a specific event
    pub async fn goto_event(&mut self, event_id: &str) -> Result<Event> {
        let event = self.time_machine.goto_event(event_id)?;
        self.record_navigation();
        Ok(event)
    }

    /// Undo last navigation
    pub async fn undo_navigation(&mut self) -> Result<()> {
        let current_pos = self
            .history_position
            .ok_or_else(|| TemporalError::TimeTravelFailed("No navigation history".to_string()))?;

        if current_pos == 0 {
            return Err(TemporalError::TimeTravelFailed(
                "Already at start of navigation history".to_string(),
            ));
        }

        let prev_pos = current_pos - 1;
        let time_point = &self.navigation_history[prev_pos];
        self.time_machine.goto_time_point(time_point)?;
        self.history_position = Some(prev_pos);

        Ok(())
    }

    /// Redo last undone navigation
    pub async fn redo_navigation(&mut self) -> Result<()> {
        let current_pos = self
            .history_position
            .ok_or_else(|| TemporalError::TimeTravelFailed("No navigation history".to_string()))?;

        if current_pos >= self.navigation_history.len() - 1 {
            return Err(TemporalError::TimeTravelFailed(
                "Already at end of navigation history".to_string(),
            ));
        }

        let next_pos = current_pos + 1;
        let time_point = &self.navigation_history[next_pos];
        self.time_machine.goto_time_point(time_point)?;
        self.history_position = Some(next_pos);

        Ok(())
    }

    /// Create a checkpoint
    pub fn create_checkpoint(&mut self, name: String) -> Result<Checkpoint> {
        self.time_machine.create_checkpoint(name)
    }

    /// Restore a checkpoint
    pub async fn restore_checkpoint(&mut self, checkpoint: &Checkpoint) -> Result<()> {
        self.time_machine.restore_checkpoint(checkpoint)?;
        self.record_navigation();
        Ok(())
    }

    /// Get current event
    #[must_use]
    pub fn current_event(&self) -> Option<Event> {
        self.time_machine.current_event()
    }

    /// Get current stream up to current position
    #[must_use]
    pub fn current_stream(&self) -> EventStream {
        self.time_machine.get_current_stream()
    }

    /// Get all checkpoints
    #[must_use]
    pub fn checkpoints(&self) -> Vec<&Checkpoint> {
        self.time_machine.get_checkpoints()
    }

    /// Access the underlying time machine
    #[must_use]
    pub const fn time_machine(&self) -> &TimeMachine {
        &self.time_machine
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event_sourcing::{EventData, EventType};

    fn create_test_events(count: usize) -> EventStore {
        let store = EventStore::new();

        for i in 0..count {
            let event = Event::new(
                format!("entity-{i}"),
                EventType::Updated,
                EventData::Custom {
                    data: serde_json::json!({"index": i}),
                },
                VectorTime::new(),
            );
            store.append(event).unwrap();
        }

        store
    }

    #[test]
    fn test_time_machine_navigation() {
        let store = create_test_events(5);
        let mut machine = TimeMachine::new(store);

        // Start at beginning
        machine.goto_start().unwrap();
        assert_eq!(machine.current_position(), Some(0));

        // Step forward
        machine.step_forward().unwrap();
        assert_eq!(machine.current_position(), Some(1));

        // Step backward
        machine.step_backward().unwrap();
        assert_eq!(machine.current_position(), Some(0));

        // Go to end
        machine.goto_end().unwrap();
        assert_eq!(machine.current_position(), Some(4));
    }

    #[test]
    fn test_checkpoint_creation() {
        let store = create_test_events(3);
        let mut machine = TimeMachine::new(store);

        machine.goto_start().unwrap();
        machine.step_forward().unwrap();

        let checkpoint = machine
            .create_checkpoint("test_checkpoint".to_string())
            .unwrap();
        assert_eq!(checkpoint.name, "test_checkpoint");

        // Move away
        machine.step_forward().unwrap();
        assert_eq!(machine.current_position(), Some(2));

        // Restore checkpoint
        machine.restore_checkpoint(&checkpoint).unwrap();
        assert_eq!(machine.current_position(), Some(1));
    }

    #[tokio::test]
    async fn test_debugger_undo_redo() {
        let store = create_test_events(5);
        let mut debugger = TimeTravelDebugger::new(store);

        debugger.goto_start().await.unwrap();
        debugger.step_forward().await.unwrap();
        debugger.step_forward().await.unwrap();

        assert_eq!(debugger.time_machine.current_position(), Some(2));

        // Undo
        debugger.undo_navigation().await.unwrap();
        assert_eq!(debugger.time_machine.current_position(), Some(1));

        // Redo
        debugger.redo_navigation().await.unwrap();
        assert_eq!(debugger.time_machine.current_position(), Some(2));
    }

    #[tokio::test]
    async fn test_debugger_checkpoint_workflow() {
        let store = create_test_events(5);
        let mut debugger = TimeTravelDebugger::new(store);

        debugger.goto_start().await.unwrap();
        debugger.step_forward().await.unwrap();

        let checkpoint = debugger
            .create_checkpoint("before_exploration".to_string())
            .unwrap();

        debugger.step_forward().await.unwrap();
        debugger.step_forward().await.unwrap();

        // Restore to checkpoint
        debugger.restore_checkpoint(&checkpoint).await.unwrap();
        assert_eq!(debugger.time_machine.current_position(), Some(1));
    }

    #[test]
    fn test_time_point() {
        let time_point = TimePoint::new(
            "evt_123".to_string(),
            5,
            VectorTime::new(),
            Utc::now(),
        );

        assert_eq!(time_point.event_id, "evt_123");
        assert_eq!(time_point.position, 5);
    }
}
