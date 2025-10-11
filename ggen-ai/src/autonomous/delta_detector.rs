//! Graph Delta Detection
//!
//! Monitors RDF graphs for changes, computes differential updates,
//! and triggers regeneration pipelines.

use crate::error::{GgenAiError, Result};
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use tracing::{debug, info};

/// Graph delta representing changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphDelta {
    /// Added triples
    pub additions: Vec<String>,
    /// Removed triples
    pub deletions: Vec<String>,
    /// Modified triples (old -> new)
    pub modifications: Vec<(String, String)>,
    /// Delta statistics
    pub stats: DeltaStats,
    /// Timestamp of delta computation
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Delta operation type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum DeltaOperation {
    /// Triple added
    Add,
    /// Triple removed
    Delete,
    /// Triple modified
    Modify,
}

/// Statistics about a graph delta
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeltaStats {
    /// Number of additions
    pub additions_count: usize,
    /// Number of deletions
    pub deletions_count: usize,
    /// Number of modifications
    pub modifications_count: usize,
    /// Total changes
    pub total_changes: usize,
    /// Affected subjects
    pub affected_subjects: HashSet<String>,
    /// Affected predicates
    pub affected_predicates: HashSet<String>,
}

/// Delta detector for RDF graphs
#[derive(Clone)]
pub struct DeltaDetector {
    store: Store,
    baseline: HashMap<String, HashSet<String>>,
    evolution_history: Vec<GraphDelta>,
}

impl std::fmt::Debug for DeltaDetector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeltaDetector")
            .field("store", &"Store")
            .field("baseline", &self.baseline)
            .field("evolution_history", &self.evolution_history)
            .finish()
    }
}

impl DeltaDetector {
    /// Create a new delta detector
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new().map_err(|e| {
                GgenAiError::configuration(format!("Failed to create RDF store: {}", e))
            })?,
            baseline: HashMap::new(),
            evolution_history: Vec::new(),
        })
    }

    /// Set baseline graph state
    pub fn set_baseline(&mut self, triples: &[String]) -> Result<()> {
        debug!("Setting baseline with {} triples", triples.len());

        self.baseline.clear();

        for triple in triples {
            // Parse triple into subject-predicate-object
            let parts: Vec<&str> = triple.split_whitespace().collect();
            if parts.len() >= 3 {
                let subject = parts[0].to_string();
                let predicate_object = parts[1..].join(" ");

                self.baseline
                    .entry(subject)
                    .or_insert_with(HashSet::new)
                    .insert(predicate_object);
            }
        }

        info!("Baseline set with {} subjects", self.baseline.len());
        Ok(())
    }

    /// Compute delta between baseline and new state
    pub fn compute_delta(&mut self, new_triples: &[String]) -> Result<GraphDelta> {
        let start = std::time::Instant::now();

        debug!("Computing delta for {} new triples", new_triples.len());

        // Build new state representation
        let mut new_state: HashMap<String, HashSet<String>> = HashMap::new();
        for triple in new_triples {
            let parts: Vec<&str> = triple.split_whitespace().collect();
            if parts.len() >= 3 {
                let subject = parts[0].to_string();
                let predicate_object = parts[1..].join(" ");

                new_state
                    .entry(subject)
                    .or_insert_with(HashSet::new)
                    .insert(predicate_object);
            }
        }

        let mut additions = Vec::new();
        let mut deletions = Vec::new();
        let modifications = Vec::new(); // Not used yet but kept for future enhancement
        let mut affected_subjects = HashSet::new();
        let mut affected_predicates = HashSet::new();

        // Find additions and modifications
        for (subject, predicates) in &new_state {
            if let Some(baseline_predicates) = self.baseline.get(subject) {
                // Check for new predicates
                for pred_obj in predicates {
                    if !baseline_predicates.contains(pred_obj) {
                        additions.push(format!("{} {}", subject, pred_obj));
                        affected_subjects.insert(subject.clone());

                        // Extract predicate
                        if let Some(pred) = pred_obj.split_whitespace().next() {
                            affected_predicates.insert(pred.to_string());
                        }
                    }
                }
            } else {
                // Entirely new subject
                for pred_obj in predicates {
                    additions.push(format!("{} {}", subject, pred_obj));
                    affected_subjects.insert(subject.clone());

                    if let Some(pred) = pred_obj.split_whitespace().next() {
                        affected_predicates.insert(pred.to_string());
                    }
                }
            }
        }

        // Find deletions
        for (subject, predicates) in &self.baseline {
            if let Some(new_predicates) = new_state.get(subject) {
                // Check for removed predicates
                for pred_obj in predicates {
                    if !new_predicates.contains(pred_obj) {
                        deletions.push(format!("{} {}", subject, pred_obj));
                        affected_subjects.insert(subject.clone());

                        if let Some(pred) = pred_obj.split_whitespace().next() {
                            affected_predicates.insert(pred.to_string());
                        }
                    }
                }
            } else {
                // Entire subject removed
                for pred_obj in predicates {
                    deletions.push(format!("{} {}", subject, pred_obj));
                    affected_subjects.insert(subject.clone());

                    if let Some(pred) = pred_obj.split_whitespace().next() {
                        affected_predicates.insert(pred.to_string());
                    }
                }
            }
        }

        let stats = DeltaStats {
            additions_count: additions.len(),
            deletions_count: deletions.len(),
            modifications_count: modifications.len(),
            total_changes: additions.len() + deletions.len() + modifications.len(),
            affected_subjects,
            affected_predicates,
        };

        let delta = GraphDelta {
            additions,
            deletions,
            modifications,
            stats,
            timestamp: chrono::Utc::now(),
        };

        // Store in history
        self.evolution_history.push(delta.clone());

        info!(
            "Delta computed: {} additions, {} deletions, {} modifications in {:?}",
            delta.stats.additions_count,
            delta.stats.deletions_count,
            delta.stats.modifications_count,
            start.elapsed()
        );

        Ok(delta)
    }

    /// Get evolution history
    pub fn get_history(&self) -> &[GraphDelta] {
        &self.evolution_history
    }

    /// Clear evolution history
    pub fn clear_history(&mut self) {
        self.evolution_history.clear();
    }

    /// Apply delta to baseline (update baseline)
    pub fn apply_delta(&mut self, delta: &GraphDelta) -> Result<()> {
        debug!("Applying delta to baseline");

        // Apply deletions
        for deletion in &delta.deletions {
            let parts: Vec<&str> = deletion.split_whitespace().collect();
            if parts.len() >= 3 {
                let subject = parts[0].to_string();
                let predicate_object = parts[1..].join(" ");

                if let Some(predicates) = self.baseline.get_mut(&subject) {
                    predicates.remove(&predicate_object);
                    if predicates.is_empty() {
                        self.baseline.remove(&subject);
                    }
                }
            }
        }

        // Apply additions
        for addition in &delta.additions {
            let parts: Vec<&str> = addition.split_whitespace().collect();
            if parts.len() >= 3 {
                let subject = parts[0].to_string();
                let predicate_object = parts[1..].join(" ");

                self.baseline
                    .entry(subject)
                    .or_insert_with(HashSet::new)
                    .insert(predicate_object);
            }
        }

        info!("Delta applied to baseline");
        Ok(())
    }

    /// Check if delta has significant changes (threshold-based)
    pub fn is_significant(&self, delta: &GraphDelta, threshold: usize) -> bool {
        delta.stats.total_changes >= threshold
    }
}

impl Default for DeltaDetector {
    fn default() -> Self {
        // For default implementation, we can safely unwrap since Store::new()
        // only fails with memory allocation issues which are unrecoverable anyway
        Self::new().unwrap_or_else(|_e| {
            // In case of error, return a minimal valid instance
            Self {
                store: Store::new().expect("Fatal: cannot allocate RDF store"),
                baseline: HashMap::new(),
                evolution_history: Vec::new(),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delta_detection() {
        let mut detector = DeltaDetector::new().unwrap();

        let baseline = vec![
            "ex:Person1 rdf:type ex:Person .".to_string(),
            "ex:Person1 ex:name \"Alice\" .".to_string(),
        ];

        detector.set_baseline(&baseline).unwrap();

        let new_state = vec![
            "ex:Person1 rdf:type ex:Person .".to_string(),
            "ex:Person1 ex:name \"Alice\" .".to_string(),
            "ex:Person1 ex:age \"30\" .".to_string(), // Addition
        ];

        let delta = detector.compute_delta(&new_state).unwrap();

        assert_eq!(delta.stats.additions_count, 1);
        assert_eq!(delta.stats.deletions_count, 0);
    }

    #[test]
    fn test_apply_delta() {
        let mut detector = DeltaDetector::new().unwrap();

        let baseline = vec!["ex:Person1 rdf:type ex:Person .".to_string()];

        detector.set_baseline(&baseline).unwrap();

        let delta = GraphDelta {
            additions: vec!["ex:Person1 ex:name \"Bob\" .".to_string()],
            deletions: vec![],
            modifications: vec![],
            stats: DeltaStats {
                additions_count: 1,
                deletions_count: 0,
                modifications_count: 0,
                total_changes: 1,
                affected_subjects: HashSet::new(),
                affected_predicates: HashSet::new(),
            },
            timestamp: chrono::Utc::now(),
        };

        detector.apply_delta(&delta).unwrap();

        assert!(detector.baseline.contains_key("ex:Person1"));
    }

    #[test]
    fn test_is_significant() {
        let detector = DeltaDetector::new().unwrap();

        let delta = GraphDelta {
            additions: vec!["ex:Entity1 ex:prop \"value\" .".to_string()],
            deletions: vec![],
            modifications: vec![],
            stats: DeltaStats {
                additions_count: 1,
                deletions_count: 0,
                modifications_count: 0,
                total_changes: 1,
                affected_subjects: HashSet::new(),
                affected_predicates: HashSet::new(),
            },
            timestamp: chrono::Utc::now(),
        };

        assert!(detector.is_significant(&delta, 1));
        assert!(!detector.is_significant(&delta, 5));
    }
}
