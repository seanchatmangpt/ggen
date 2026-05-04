//! Process discovery algorithms.
//!
//! This module provides tools for extracting process models from event logs,
//! utilizing the Alpha algorithm from the pictl-algos crate.

use crate::error::Result;
use crate::event_log::EventLog;
use pictl_algos::alpha::discover_alpha;
pub use pictl_types::models::PetriNet;

/// Orchestrator for process mining operations.
pub struct ProcessMiner {
    activity_key: String,
}

impl ProcessMiner {
    /// Create a new process miner.
    #[must_use]
    pub fn new() -> Self {
        Self {
            activity_key: "concept:name".to_string(),
        }
    }

    /// Set the activity key to use for discovery.
    #[must_use]
    pub fn with_activity_key(mut self, key: impl Into<String>) -> Self {
        self.activity_key = key.into();
        self
    }

    /// Discover a Petri net from an event log using the Alpha miner.
    ///
    /// # Errors
    ///
    /// Returns an error if discovery fails.
    pub fn discover_alpha(&self, log: &EventLog) -> Result<PetriNet> {
        discover_alpha(log, &self.activity_key)
            .map_err(|e| crate::error::Error::AlphaPlusPlusDiscovery(e.to_string()))
    }
}

impl Default for ProcessMiner {
    fn default() -> Self {
        Self::new()
    }
}
