//! Thread-safe FMEA registry for failure tracking.
//!
//! This module provides a global registry for:
//! - Registering failure modes
//! - Recording failure events
//! - Querying failure statistics
//!
//! Design:
//! - Thread-safe singleton using lazy_static
//! - Ring buffer for events (max 1000, prevents unbounded growth)
//! - Zero overhead on success path (no allocations)

use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, RwLock};
use std::time::Instant;

use once_cell::sync::Lazy;

use super::types::{FailureMode, FailureCategory};

/// Global FMEA registry singleton.
///
/// Thread-safe registry initialized lazily on first access.
/// Pre-populated with critical failure modes from catalog.
pub static FMEA_REGISTRY: Lazy<Arc<RwLock<FmeaRegistry>>> = Lazy::new(|| {
    let mut registry = FmeaRegistry::new();
    // Register critical failures (defined in catalog.rs)
    super::catalog::register_critical_failures(&mut registry);
    Arc::new(RwLock::new(registry))
});

/// FMEA registry for tracking failure modes and events.
///
/// Stores:
/// - Registered failure modes (by ID)
/// - Recent failure events (ring buffer, max 1000)
#[derive(Debug)]
pub struct FmeaRegistry {
    /// Registered failure modes (ID → FailureMode).
    failure_modes: HashMap<String, FailureMode>,
    /// Recent failure events (ring buffer).
    events: VecDeque<FailureEvent>,
    /// Maximum events to retain (prevents unbounded growth).
    max_events: usize,
}

impl FmeaRegistry {
    /// Creates a new empty registry.
    pub fn new() -> Self {
        Self {
            failure_modes: HashMap::new(),
            events: VecDeque::new(),
            max_events: 1000,
        }
    }

    /// Registers a failure mode.
    ///
    /// If a failure mode with the same ID exists, it is replaced.
    pub fn register(&mut self, mode: FailureMode) {
        self.failure_modes.insert(mode.id.clone(), mode);
    }

    /// Records a failure event.
    ///
    /// Events are stored in a ring buffer (max 1000).
    /// Oldest events are evicted when buffer is full.
    pub fn record_event(&mut self, event: FailureEvent) {
        if self.events.len() >= self.max_events {
            self.events.pop_front();
        }
        self.events.push_back(event);
    }

    /// Gets a failure mode by ID.
    pub fn get_failure_mode(&self, id: &str) -> Option<&FailureMode> {
        self.failure_modes.get(id)
    }

    /// Returns all registered failure modes.
    pub fn all_failure_modes(&self) -> impl Iterator<Item = &FailureMode> {
        self.failure_modes.values()
    }

    /// Returns all failure modes in a category.
    pub fn failure_modes_by_category(&self, category: FailureCategory) -> impl Iterator<Item = &FailureMode> {
        self.failure_modes.values().filter(move |m| m.category == category)
    }

    /// Returns recent failure events.
    pub fn recent_events(&self, limit: usize) -> impl Iterator<Item = &FailureEvent> {
        self.events.iter().rev().take(limit)
    }

    /// Returns events for a specific failure mode.
    pub fn events_for_mode<'a>(&'a self, mode_id: &'a str) -> impl Iterator<Item = &'a FailureEvent> {
        self.events.iter().filter(move |e| e.mode_id == mode_id)
    }

    /// Returns the total number of events.
    pub fn event_count(&self) -> usize {
        self.events.len()
    }

    /// Returns the number of registered failure modes.
    pub fn failure_mode_count(&self) -> usize {
        self.failure_modes.len()
    }

    /// Clears all events (useful for testing).
    #[cfg(test)]
    pub fn clear_events(&mut self) {
        self.events.clear();
    }
}

impl Default for FmeaRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// A failure event that occurred at runtime.
///
/// Captures:
/// - When (timestamp)
/// - What (failure mode ID, error message)
/// - Where (operation, context)
/// - Why (source error chain)
#[derive(Debug, Clone)]
pub struct FailureEvent {
    /// Failure mode ID (references registered FailureMode).
    pub mode_id: String,
    /// When the failure occurred.
    pub timestamp: Instant,
    /// Error message from the failure.
    pub error_message: String,
    /// Optional context (e.g., file path, user input).
    pub context: Option<String>,
    /// Error source chain (from Error::source()).
    pub source_chain: Vec<String>,
    /// Operation that failed (e.g., "template_generation", "lockfile_save").
    pub operation: String,
    /// Additional metadata (extensible).
    pub metadata: HashMap<String, String>,
}

impl FailureEvent {
    /// Creates a new failure event.
    pub fn new(mode_id: String, operation: String, error_message: String) -> Self {
        Self {
            mode_id,
            timestamp: Instant::now(),
            error_message,
            context: None,
            source_chain: Vec::new(),
            operation,
            metadata: HashMap::new(),
        }
    }

    /// Sets the context.
    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }

    /// Sets the source error chain.
    pub fn with_source_chain(mut self, chain: Vec<String>) -> Self {
        self.source_chain = chain;
        self
    }

    /// Adds metadata.
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}

impl Default for FailureEvent {
    fn default() -> Self {
        Self::new(String::new(), String::new(), String::new())
    }
}
