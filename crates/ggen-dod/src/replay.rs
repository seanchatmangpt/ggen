//! Time-travel and replay infrastructure: Verify determinism through replay
//!
//! Records kernel executions and replays them to verify:
//! - Same input produces identical output
//! - Determinism proofs hold
//! - No external state leaked in

use crate::error::DoDResult;
use crate::kernel::{Kernel, KernelDecision};
use crate::observation::Observation;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// A recorded kernel execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionRecord {
    /// Unique record ID
    record_id: String,
    /// Input observations (O)
    observations: Vec<Observation>,
    /// Tenant ID
    tenant_id: String,
    /// Output decision
    decision: KernelDecision,
    /// When recorded
    recorded_at: chrono::DateTime<chrono::Utc>,
}

impl ExecutionRecord {
    /// Create a new execution record
    pub fn new(
        observations: Vec<Observation>, tenant_id: impl Into<String>, decision: KernelDecision,
    ) -> Self {
        Self {
            record_id: uuid::Uuid::new_v4().to_string(),
            observations,
            tenant_id: tenant_id.into(),
            decision,
            recorded_at: chrono::Utc::now(),
        }
    }

    /// Get record ID
    pub fn record_id(&self) -> &str {
        &self.record_id
    }

    /// Get observations
    pub fn observations(&self) -> &[Observation] {
        &self.observations
    }

    /// Get decision
    pub fn decision(&self) -> &KernelDecision {
        &self.decision
    }
}

/// Replay result comparing original and replayed execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReplayResult {
    /// Was replay successful?
    success: bool,
    /// Original decision hash
    original_hash: String,
    /// Replay decision hash
    replay_hash: String,
    /// Are hashes identical?
    hashes_match: bool,
    /// Explanation if mismatch
    explanation: String,
}

impl ReplayResult {
    /// Create a successful replay
    pub fn successful(hash: String) -> Self {
        Self {
            success: true,
            original_hash: hash.clone(),
            replay_hash: hash,
            hashes_match: true,
            explanation: "Determinism verified".to_string(),
        }
    }

    /// Create a failed replay
    pub fn failed(original: String, replay: String, explanation: impl Into<String>) -> Self {
        Self {
            success: false,
            original_hash: original,
            replay_hash: replay,
            hashes_match: false,
            explanation: explanation.into(),
        }
    }

    /// Is this deterministic?
    pub fn is_deterministic(&self) -> bool {
        self.hashes_match
    }

    /// Get explanation
    pub fn explanation(&self) -> &str {
        &self.explanation
    }
}

/// Time-travel/replay infrastructure
pub struct ReplayEngine {
    /// Recorded executions
    records: BTreeMap<String, ExecutionRecord>,
    /// Replay results
    results: Vec<ReplayResult>,
}

impl ReplayEngine {
    /// Create a new replay engine
    pub fn new() -> Self {
        Self {
            records: BTreeMap::new(),
            results: Vec::new(),
        }
    }

    /// Record an execution
    pub fn record(mut self, record: ExecutionRecord) -> Self {
        self.records.insert(record.record_id().to_string(), record);
        self
    }

    /// Replay a recorded execution
    pub fn replay(&mut self, kernel: &mut Kernel, record_id: &str) -> DoDResult<ReplayResult> {
        let record = self
            .records
            .get(record_id)
            .ok_or_else(|| {
                crate::error::DoDError::Internal(format!("record not found: {}", record_id))
            })?
            .clone();

        // Replay the decision
        let replay_decision = kernel.decide(
            record.observations().to_vec(),
            record.decision().observations()[0].tenant_id(),
        )?;

        // Compare hashes
        let original_hash = record
            .decision()
            .determinism_hash()
            .ok_or_else(|| crate::error::DoDError::DeterminismViolation)?
            .to_string();

        let replay_hash = replay_decision
            .determinism_hash()
            .ok_or_else(|| crate::error::DoDError::DeterminismViolation)?
            .to_string();

        let result = if original_hash == replay_hash {
            ReplayResult::successful(original_hash)
        } else {
            ReplayResult::failed(original_hash, replay_hash, "determinism hash mismatch")
        };

        self.results.push(result.clone());

        // Verify determinism if hashes match
        if result.is_deterministic() {
            kernel.verify_determinism(&record.decision(), &replay_decision)?;
        } else {
            return Err(crate::error::DoDError::DeterminismViolation);
        }

        Ok(result)
    }

    /// Replay all recorded executions
    pub fn replay_all(&mut self, kernel: &mut Kernel) -> DoDResult<()> {
        let record_ids: Vec<_> = self.records.keys().map(|k| k.clone()).collect();

        for record_id in record_ids {
            self.replay(kernel, &record_id)?;
        }

        Ok(())
    }

    /// Get all replay results
    pub fn results(&self) -> &[ReplayResult] {
        &self.results
    }

    /// Check if all replays are deterministic
    pub fn all_deterministic(&self) -> bool {
        self.results.iter().all(|r| r.is_deterministic())
    }

    /// Get determinism report
    pub fn determinism_report(&self) -> String {
        let total = self.results.len();
        let deterministic = self.results.iter().filter(|r| r.is_deterministic()).count();

        format!(
            "Determinism: {}/{} replays successful ({}%)",
            deterministic,
            total,
            if total > 0 {
                (deterministic * 100) / total
            } else {
                100
            }
        )
    }
}

impl Default for ReplayEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_replay_result_success() {
        let result = ReplayResult::successful("hash123".to_string());
        assert!(result.is_deterministic());
    }

    #[test]
    fn test_replay_result_failure() {
        let result = ReplayResult::failed("hash123".to_string(), "hash456".to_string(), "mismatch");

        assert!(!result.is_deterministic());
    }

    #[test]
    fn test_replay_engine() {
        let engine = ReplayEngine::new();
        assert!(engine.all_deterministic());
    }
}
