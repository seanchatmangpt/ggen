//! Conformance checking algorithms.
//!
//! This module provides tools for validating event logs against process models,
//! utilizing the alignment and token replay algorithms from pictl-algos.

use crate::error::Result;
use crate::event_log::EventLog;
use pictl_algos::conformance::{check_conformance_alignment, check_conformance_token_replay};
pub use pictl_types::conformance::ConformanceResult;
pub use pictl_types::models::PetriNet;

/// Checker for process conformance.
pub struct ConformanceChecker {
    activity_key: String,
}

impl ConformanceChecker {
    /// Create a new conformance checker.
    #[must_use]
    pub fn new() -> Self {
        Self {
            activity_key: "concept:name".to_string(),
        }
    }

    /// Set the activity key to use for conformance.
    #[must_use]
    pub fn with_activity_key(mut self, key: impl Into<String>) -> Self {
        self.activity_key = key.into();
        self
    }

    /// Check conformance using alignment-based algorithm.
    ///
    /// # Errors
    ///
    /// Returns an error if the model or log is invalid.
    pub fn check_alignment(&self, log: &EventLog, net: &PetriNet) -> Result<ConformanceResult> {
        check_conformance_alignment(log, net, &self.activity_key)
            .map_err(|e| crate::error::Error::ConformanceError(e.to_string()))
    }

    /// Check conformance using token replay.
    ///
    /// # Errors
    ///
    /// Returns an error if the model or log is invalid.
    pub fn check_token_replay(&self, log: &EventLog, model: &pictl_types::models::DFG) -> Result<ConformanceResult> {
        check_conformance_token_replay(log, model, &self.activity_key)
            .map_err(|e| crate::error::Error::ConformanceError(e.to_string()))
    }
}

impl Default for ConformanceChecker {
    fn default() -> Self {
        Self::new()
    }
}
