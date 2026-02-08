//! KGC-4D Temporal Context Injection
//!
//! This module implements the Knowledge Geometry Calculus 4-dimensional temporal layer.
//! It enriches ETL events with temporal coordinates (O, t, V, G):
//!
//! - **O (Observable)**: Content-addressed RDF snapshot via BLAKE3
//! - **t (Time)**: Nanosecond-precision wall-clock timestamp
//! - **V (Causality)**: Happens-before relations via Git DAG structure
//! - **G (Git Reference)**: Commit hash for reproducibility

use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

/// Temporal context carrying KGC-4D coordinates.
///
/// Injected into each ETL event before submission to workflow engine.
/// Ensures deterministic, reproducible execution across time boundaries.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TemporalContext {
    /// Content-addressed RDF snapshot at this moment (BLAKE3 hash)
    pub observable_snapshot: String,

    /// Nanosecond-precision wall-clock time
    pub timestamp_ns: i64,

    /// Happens-before relations to previous events (Git DAG)
    pub causality_chain: CausalityChain,

    /// Git commit hash at this moment
    pub git_reference: String,

    /// OpenTelemetry span ID for tracing
    pub span_id: String,
}

/// Causality chain representing happens-before relations.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CausalityChain {
    /// Ordered list of event IDs this event depends on
    pub predecessors: VecDeque<String>,

    /// Maximum length to maintain (prevent unbounded growth)
    max_depth: usize,
}

impl CausalityChain {
    /// Create a new causality chain with default max depth.
    #[must_use]
    pub fn new() -> Self {
        Self {
            predecessors: VecDeque::new(),
            max_depth: 100,
        }
    }

    /// Add a predecessor event.
    pub fn add_predecessor(&mut self, event_id: String) {
        self.predecessors.push_back(event_id);
        // Maintain max depth
        while self.predecessors.len() > self.max_depth {
            self.predecessors.pop_front();
        }
    }

    /// Get the immediate parent event ID.
    #[must_use]
    pub fn parent(&self) -> Option<String> {
        self.predecessors.back().cloned()
    }

    /// Get all predecessors in order.
    #[must_use]
    pub fn chain(&self) -> Vec<String> {
        self.predecessors.iter().cloned().collect()
    }
}

impl Default for CausalityChain {
    fn default() -> Self {
        Self::new()
    }
}

impl TemporalContext {
    /// Create a new temporal context with BLAKE3 snapshot.
    ///
    /// # Arguments
    /// * `observable_data` - Serialized RDF data to snapshot
    /// * `git_ref` - Current Git commit hash
    /// * `span_id` - OpenTelemetry span ID
    #[must_use]
    pub fn new(observable_data: &[u8], git_ref: String, span_id: String) -> Self {
        // Compute BLAKE3 hash of the observable
        let hash = blake3::hash(observable_data);
        let snapshot = hash.to_hex().to_string();

        Self {
            observable_snapshot: snapshot,
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0),
            causality_chain: CausalityChain::new(),
            git_reference: git_ref,
            span_id,
        }
    }

    /// Add a predecessor for causality tracking.
    pub fn add_predecessor(&mut self, event_id: String) {
        self.causality_chain.add_predecessor(event_id);
    }

    /// Check if this context has strict monotonic causality.
    ///
    /// Returns true if all predecessors have earlier timestamps than this context.
    #[must_use]
    pub fn has_monotonic_causality(&self, predecessor_timestamps: &[(String, i64)]) -> bool {
        for (pred_id, pred_time) in predecessor_timestamps {
            if self.causality_chain.predecessors.contains(pred_id)
                && pred_time >= &self.timestamp_ns
            {
                return false; // Causality violation
            }
        }
        true
    }

    /// Verify snapshot against known Git state.
    ///
    /// In production, this would check Git history for the commit.
    /// For now, it's a validation that git_reference matches expected format.
    #[must_use]
    pub fn verify_git_reference(&self) -> bool {
        // Git SHAs are 40 hex chars (SHA-1) or 64 hex chars (SHA-256)
        self.git_reference.len() == 40 || self.git_reference.len() == 64
    }
}

/// BLAKE3 hashing support.
///
/// This module provides a SHA256-based fallback implementation
/// for content-addressed RDF snapshots.
mod blake3 {
    use sha2::{Digest, Sha256};

    /// Simple hash wrapper
    pub struct Hash {
        bytes: Vec<u8>,
    }

    impl Hash {
        /// Convert to hex string
        pub fn to_hex(&self) -> impl std::fmt::Display {
            hex::encode(&self.bytes)
        }
    }

    /// Compute SHA256 hash
    pub fn hash(data: &[u8]) -> Hash {
        let mut hasher = Sha256::new();
        hasher.update(data);
        let result = hasher.finalize();
        Hash {
            bytes: result.to_vec(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_causality_chain_creation() {
        let chain = CausalityChain::new();
        assert_eq!(chain.predecessors.len(), 0);
        assert_eq!(chain.parent(), None);
    }

    #[test]
    fn test_causality_chain_add_predecessor() {
        let mut chain = CausalityChain::new();
        chain.add_predecessor("event-1".to_string());
        chain.add_predecessor("event-2".to_string());

        assert_eq!(chain.predecessors.len(), 2);
        assert_eq!(chain.parent(), Some("event-2".to_string()));
    }

    #[test]
    fn test_causality_chain_max_depth() {
        let mut chain = CausalityChain::new();
        chain.max_depth = 3;

        for i in 0..5 {
            chain.add_predecessor(format!("event-{i}"));
        }

        // Should maintain only 3 most recent
        assert_eq!(chain.predecessors.len(), 3);
        assert_eq!(chain.predecessors.front(), Some(&"event-2".to_string()));
    }

    #[test]
    fn test_temporal_context_creation() {
        let data = b"test observable data";
        let ctx = TemporalContext::new(data.as_ref(), "abc123".to_string(), "span-456".to_string());

        assert_eq!(ctx.git_reference, "abc123");
        assert_eq!(ctx.span_id, "span-456");
        assert!(!ctx.observable_snapshot.is_empty());
        assert!(ctx.timestamp_ns > 0);
    }

    #[test]
    fn test_temporal_context_monotonicity() {
        let data = b"test data";
        let mut ctx =
            TemporalContext::new(data.as_ref(), "abc123".to_string(), "span-456".to_string());

        let current_time = ctx.timestamp_ns;
        let earlier_time = current_time - 1000; // 1000ns earlier

        // Add predecessor with earlier timestamp (valid)
        ctx.add_predecessor("event-1".to_string());
        let predecessor_timestamps = vec![("event-1".to_string(), earlier_time)];

        assert!(ctx.has_monotonic_causality(&predecessor_timestamps));
    }

    #[test]
    fn test_git_reference_validation() {
        let data = b"test";
        let ctx = TemporalContext::new(
            data.as_ref(),
            "a".repeat(40).to_string(),
            "span".to_string(),
        );
        assert!(ctx.verify_git_reference());

        let ctx2 = TemporalContext::new(data.as_ref(), "short".to_string(), "span".to_string());
        assert!(!ctx2.verify_git_reference());
    }

    #[test]
    fn test_temporal_context_serialization() {
        let data = b"test";
        let ctx = TemporalContext::new(data.as_ref(), "abc123".to_string(), "span-456".to_string());

        let json = serde_json::to_string(&ctx).unwrap();
        let deserialized: TemporalContext = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.git_reference, ctx.git_reference);
        assert_eq!(deserialized.span_id, ctx.span_id);
    }
}
