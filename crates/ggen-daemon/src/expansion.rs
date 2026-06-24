use serde::{Deserialize, Serialize};

use crate::catalog::RepoCatalogEntry;

/// Tracks bulk spec bundle expansion across a catalog of repos, recording
/// which bundles have been dispatched and which remain.
#[derive(Debug, Clone)]
pub struct ExpansionPlan {
    pub spec_bundles: Vec<String>,
    pub repos: Vec<RepoCatalogEntry>,
    pub completed: Vec<String>,
    pub pending: Vec<String>,
}

impl ExpansionPlan {
    /// Create a new plan.  All bundles start in `pending`; `completed` is empty.
    pub fn new(bundles: Vec<String>, repos: Vec<RepoCatalogEntry>) -> Self {
        let pending = bundles.clone();
        Self {
            spec_bundles: bundles,
            repos,
            completed: Vec::new(),
            pending,
        }
    }

    /// Remove and return the next pending bundle, or `None` if all have been
    /// dispatched.
    pub fn next_bundle(&mut self) -> Option<String> {
        if self.pending.is_empty() {
            None
        } else {
            Some(self.pending.remove(0))
        }
    }

    /// Record a bundle as having finished dispatching.
    ///
    /// Bundles not in the original plan are silently ignored — `completed` only
    /// ever contains entries from `spec_bundles`.
    pub fn mark_complete(&mut self, bundle: &str) {
        if !self.spec_bundles.iter().any(|b| b == bundle) {
            return;
        }
        self.pending.retain(|b| b != bundle);
        if !self.completed.iter().any(|b| b == bundle) {
            self.completed.push(bundle.to_owned());
        }
    }

    /// Number of bundles not yet dispatched.
    pub fn remaining(&self) -> usize {
        self.pending.len()
    }

    /// Fraction of bundles that have completed dispatching (`0.0` when no
    /// bundles exist).
    pub fn completion_ratio(&self) -> f64 {
        let total = self.spec_bundles.len();
        if total == 0 {
            return 0.0;
        }
        self.completed.len() as f64 / total as f64
    }
}

/// Aggregate statistics across all bundles in an expansion run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpansionSummary {
    pub total_bundles: usize,
    pub total_repos: usize,
    pub total_commits_generated: usize,
    pub total_failures: usize,
}
