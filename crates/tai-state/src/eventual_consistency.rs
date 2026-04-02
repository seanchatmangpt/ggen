//! Eventual consistency handling with conflict resolution
//!
//! This module implements:
//! - Last-write-wins (LWW) conflict resolution
//! - Custom conflict resolver traits
//! - Vector clock causality tracking
//! - Lamport clock support
//! - Read-repair pattern for stale data healing
//! - Stale read detection and warnings

use crate::error::{Error, Result};
use crate::types::{StateSnapshot, VectorClockSnapshot};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
use std::sync::Arc;

/// Re-export VectorClock from types
pub use crate::types::VectorClockSnapshot as VectorClock;

/// Trait for custom conflict resolution strategies
pub trait ConflictResolver: Send + Sync {
    /// Resolve a conflict between two state versions
    ///
    /// # Arguments
    ///
    /// * `state1` - First state version
    /// * `state2` - Second state version
    ///
    /// # Returns
    ///
    /// The resolved state, or error if resolution failed
    fn resolve(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> Result<StateSnapshot>;

    /// Check if two states are in conflict (concurrent modifications)
    fn is_conflicted(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> bool;
}

/// Last-write-wins resolver (simple timestamp-based)
#[derive(Debug, Clone)]
pub struct LastWriteWinsResolver;

impl ConflictResolver for LastWriteWinsResolver {
    fn resolve(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> Result<StateSnapshot> {
        let resolved = if state1.timestamp >= state2.timestamp {
            state1.clone()
        } else {
            state2.clone()
        };

        Ok(resolved)
    }

    fn is_conflicted(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> bool {
        // Conflicted if versions are concurrent (different timestamps, same data)
        state1.timestamp != state2.timestamp && state1.version != state2.version
    }
}

/// Vector clock-based resolver (causality-aware)
#[derive(Debug, Clone)]
pub struct CausalityAwareResolver;

impl ConflictResolver for CausalityAwareResolver {
    fn resolve(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> Result<StateSnapshot> {
        // Check causality
        if state1.causality.happened_before(&state2.causality) {
            return Ok(state2.clone());
        }

        if state2.causality.happened_before(&state1.causality) {
            return Ok(state1.clone());
        }

        // Concurrent - use timestamp as tiebreaker
        let resolved = if state1.timestamp >= state2.timestamp {
            state1.clone()
        } else {
            state2.clone()
        };

        Ok(resolved)
    }

    fn is_conflicted(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> bool {
        // Conflicted if neither version caused the other
        state1.causality.concurrent_with(&state2.causality)
    }
}

/// Version-based resolver (highest version wins)
#[derive(Debug, Clone)]
pub struct VersionBasedResolver;

impl ConflictResolver for VersionBasedResolver {
    fn resolve(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> Result<StateSnapshot> {
        let resolved = if state1.version >= state2.version {
            state1.clone()
        } else {
            state2.clone()
        };

        Ok(resolved)
    }

    fn is_conflicted(&self, state1: &StateSnapshot, state2: &StateSnapshot) -> bool {
        // Conflicted if versions differ but neither is clearly newer
        state1.version != state2.version && state1.timestamp != state2.timestamp
    }
}

/// Lamport clock for total ordering in distributed systems
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct LamportClock(u64);

impl LamportClock {
    /// Create a new Lamport clock at initial value
    pub fn new() -> Self {
        LamportClock(0)
    }

    /// Increment the clock for a local event
    pub fn increment(&mut self) {
        self.0 += 1;
    }

    /// Update clock with received message's clock (max + 1)
    pub fn observe(&mut self, other: LamportClock) {
        self.0 = self.0.max(other.0) + 1;
    }

    /// Get current value
    pub fn value(&self) -> u64 {
        self.0
    }
}

impl Default for LamportClock {
    fn default() -> Self {
        Self::new()
    }
}

/// Handler for eventual consistency in distributed scenarios
pub struct EventualConsistency {
    resolver: Box<dyn ConflictResolver>,
    lamport_clock: Arc<AtomicU64>,
    read_repair_enabled: bool,
}

impl EventualConsistency {
    /// Create with last-write-wins resolver
    pub fn new() -> Self {
        EventualConsistency {
            resolver: Box::new(LastWriteWinsResolver),
            lamport_clock: Arc::new(AtomicU64::new(0)),
            read_repair_enabled: true,
        }
    }

    /// Create with custom resolver
    pub fn with_resolver(resolver: Box<dyn ConflictResolver>) -> Self {
        EventualConsistency {
            resolver,
            lamport_clock: Arc::new(AtomicU64::new(0)),
            read_repair_enabled: true,
        }
    }

    /// Disable read repair (data healing)
    pub fn disable_read_repair(mut self) -> Self {
        self.read_repair_enabled = false;
        self
    }

    /// Get next Lamport clock value for event ordering
    pub fn next_lamport_clock(&self) -> u64 {
        self.lamport_clock
            .fetch_add(1, AtomicOrdering::SeqCst)
    }

    /// Observe a Lamport clock value from another replica
    pub fn observe_lamport_clock(&self, clock: u64) {
        let mut current = self.lamport_clock.load(AtomicOrdering::SeqCst);
        loop {
            let new_val = current.saturating_add(1).max(clock + 1);
            match self.lamport_clock.compare_exchange(
                current,
                new_val,
                AtomicOrdering::SeqCst,
                AtomicOrdering::SeqCst,
            ) {
                Ok(_) => break,
                Err(actual) => current = actual,
            }
        }
    }

    /// Resolve conflict between two state versions
    pub fn resolve_conflict(
        &self,
        state1: &StateSnapshot,
        state2: &StateSnapshot,
    ) -> Result<StateSnapshot> {
        if self.resolver.is_conflicted(state1, state2) {
            tracing::warn!(
                state1_version = state1.version,
                state2_version = state2.version,
                "Conflict detected between state versions, resolving..."
            );
        }

        self.resolver.resolve(state1, state2)
    }

    /// Apply read repair pattern to heal stale data
    ///
    /// When reading from a replica, if data is stale compared to known versions,
    /// automatically update it with the latest version.
    pub async fn read_repair(
        &self,
        read_state: &StateSnapshot,
        known_versions: &[StateSnapshot],
    ) -> Result<StateSnapshot> {
        if !self.read_repair_enabled {
            return Ok(read_state.clone());
        }

        let mut resolved = read_state.clone();

        for known in known_versions {
            if self.resolver.is_conflicted(&resolved, known) {
                tracing::info!(
                    read_version = resolved.version,
                    known_version = known.version,
                    "Read repair: healing stale data"
                );
                resolved = self.resolver.resolve(&resolved, known)?;
            }
        }

        Ok(resolved)
    }

    /// Detect and log stale read
    pub fn detect_stale_read(
        &self,
        read_state: &StateSnapshot,
        latest_known: &StateSnapshot,
    ) -> Result<()> {
        if read_state.version < latest_known.version
            && read_state.timestamp < latest_known.timestamp
        {
            tracing::warn!(
                read_version = read_state.version,
                latest_version = latest_known.version,
                version_lag = latest_known.version - read_state.version,
                "Stale read detected"
            );

            return Err(Error::StaleRead {
                context: format!(
                    "Read version {} is behind latest {}",
                    read_state.version, latest_known.version
                ),
            });
        }

        Ok(())
    }

    /// Merge vector clocks (combine causality information)
    pub fn merge_clocks(
        &self,
        clock1: &VectorClockSnapshot,
        clock2: &VectorClockSnapshot,
    ) -> VectorClockSnapshot {
        let mut merged = clock1.clone();
        merged.merge(clock2);
        merged
    }

    /// Check if read is causally consistent with write
    pub fn is_causally_consistent(
        &self,
        write_clock: &VectorClockSnapshot,
        read_clock: &VectorClockSnapshot,
    ) -> bool {
        // Read is consistent if it observed the write's causality
        write_clock.happened_before(read_clock) || write_clock == read_clock
    }

    /// Get statistics about conflict resolution
    pub fn get_stats(&self) -> ConsistencyStats {
        ConsistencyStats {
            lamport_clock: self.lamport_clock.load(AtomicOrdering::SeqCst),
            read_repair_enabled: self.read_repair_enabled,
        }
    }
}

impl Default for EventualConsistency {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about consistency operations
#[derive(Debug, Clone)]
pub struct ConsistencyStats {
    pub lamport_clock: u64,
    pub read_repair_enabled: bool,
}

/// Snapshot of conflict resolution metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConflictMetadata {
    /// States that were in conflict
    pub conflicting_versions: Vec<StateSnapshot>,

    /// Resolved state
    pub resolved_state: StateSnapshot,

    /// Timestamp of resolution
    pub resolved_at: DateTime<Utc>,

    /// Resolution strategy used
    pub strategy: String,

    /// Whether read repair was applied
    pub read_repair_applied: bool,
}

/// History of conflicts for an entity
#[derive(Debug, Clone)]
pub struct ConflictHistory {
    pub entity_id: String,
    pub conflicts: Vec<ConflictMetadata>,
}

impl ConflictHistory {
    /// Create new conflict history
    pub fn new(entity_id: String) -> Self {
        ConflictHistory {
            entity_id,
            conflicts: Vec::new(),
        }
    }

    /// Record a conflict resolution
    pub fn record_resolution(
        &mut self,
        state1: StateSnapshot,
        state2: StateSnapshot,
        resolved: StateSnapshot,
        strategy: String,
    ) {
        self.conflicts.push(ConflictMetadata {
            conflicting_versions: vec![state1, state2],
            resolved_state: resolved,
            resolved_at: Utc::now(),
            strategy,
            read_repair_applied: false,
        });
    }

    /// Get recent conflicts
    pub fn get_recent(&self, limit: usize) -> Vec<&ConflictMetadata> {
        self.conflicts.iter().rev().take(limit).collect()
    }

    /// Check if entity has frequent conflicts
    pub fn has_conflict_storm(&self, threshold: usize, window_secs: i64) -> bool {
        let cutoff = Utc::now() - chrono::Duration::seconds(window_secs);
        self.conflicts
            .iter()
            .filter(|c| c.resolved_at > cutoff)
            .count()
            > threshold
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::StateId;
    use serde_json::json;

    #[test]
    fn test_lamport_clock() {
        let mut clock = LamportClock::new();
        assert_eq!(clock.value(), 0);

        clock.increment();
        assert_eq!(clock.value(), 1);

        let other = LamportClock(10);
        clock.observe(other);
        assert_eq!(clock.value(), 11);
    }

    #[test]
    fn test_vector_clock_causality() {
        let mut clock1 = VectorClockSnapshot::new();
        clock1.increment("node1");

        let mut clock2 = VectorClockSnapshot::new();
        clock2.increment("node1");
        clock2.increment("node1");

        assert!(clock1.happened_before(&clock2));
    }

    #[test]
    fn test_last_write_wins_resolver() {
        let resolver = LastWriteWinsResolver;

        let state1 = StateSnapshot::new(
            StateId::new("state1".to_string()),
            json!({"value": 1}),
            crate::types::ChangeMetadata::new("test", "change1", None),
        );

        tokio::time::sleep(std::time::Duration::from_millis(10));

        let state2 = StateSnapshot::new(
            StateId::new("state2".to_string()),
            json!({"value": 2}),
            crate::types::ChangeMetadata::new("test", "change2", None),
        );

        let resolved = resolver.resolve(&state1, &state2).unwrap();
        assert_eq!(resolved.data, json!({"value": 2}));
    }

    #[test]
    fn test_conflict_history() {
        let mut history = ConflictHistory::new("entity1".to_string());

        let state1 = StateSnapshot::new(
            StateId::new("state1".to_string()),
            json!({"value": 1}),
            crate::types::ChangeMetadata::new("test", "change1", None),
        );

        let state2 = StateSnapshot::new(
            StateId::new("state2".to_string()),
            json!({"value": 2}),
            crate::types::ChangeMetadata::new("test", "change2", None),
        );

        history.record_resolution(
            state1,
            state2,
            StateSnapshot::new(
                StateId::new("resolved".to_string()),
                json!({"value": 2}),
                crate::types::ChangeMetadata::new("test", "resolved", None),
            ),
            "LastWriteWins".to_string(),
        );

        assert_eq!(history.conflicts.len(), 1);
    }
}
