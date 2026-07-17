//! Advanced Phases: 9-12
//!
//! Additional hyper-advanced capabilities:
//! - Phase 9: Distributed Consensus
//! - Phase 10: Time-Travel Debugging
//! - Phase 11: Performance Prophet
//! - Phase 12: Quality Metrics Dashboard

use crate::core::receipt::{TestOutcome, TestReceipt};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::time::{Duration, Instant};

// ============================================================================
// Phase 9: Distributed Consensus
// ============================================================================

/// Consensus vote from a verification node
#[derive(Debug, Clone)]
pub struct ConsensusVote {
    /// Node identifier
    pub node_id: String,
    /// Receipt being voted on
    pub receipt_id: String,
    /// Vote (approve/reject)
    pub approved: bool,
    /// Vote timestamp
    pub timestamp: u64,
    /// Vote hash-signature (mock implementation using `DefaultHasher` for testing purposes only;
    /// not a cryptographic signature — use a real signing library such as `ed25519` for
    /// production consensus).
    pub mock_signature: String,
}

/// Distributed consensus for multi-node verification
///
/// Implements Byzantine fault-tolerant consensus for test receipts.
/// **Note:** Vote signatures are mock hash-signatures (`DefaultHasher`) for testing
/// purposes only. Replace with a real signing scheme (e.g., ed25519) for production use.
pub struct DistributedConsensus {
    /// Node identifier
    node_id: String,
    /// Votes collected
    votes: HashMap<String, Vec<ConsensusVote>>,
    /// Consensus threshold (fraction of nodes required)
    threshold: f64,
    /// Total number of nodes
    total_nodes: usize,
}

impl DistributedConsensus {
    /// Create a new distributed consensus instance
    #[must_use]
    pub fn new(node_id: String, total_nodes: usize) -> Self {
        Self {
            node_id,
            votes: HashMap::new(),
            threshold: 0.67, // Byzantine fault tolerance: 2/3 majority
            total_nodes,
        }
    }

    /// Cast a vote for a receipt
    pub fn vote(&mut self, receipt_id: String, approved: bool) -> ConsensusVote {
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        let mut hasher = DefaultHasher::new();
        self.node_id.hash(&mut hasher);
        receipt_id.hash(&mut hasher);
        approved.hash(&mut hasher);
        timestamp.hash(&mut hasher);
        let sig_hash = hasher.finish();
        let vote = ConsensusVote {
            node_id: self.node_id.clone(),
            receipt_id: receipt_id.clone(),
            approved,
            timestamp,
            mock_signature: format!("mock_sig_{}_{:x}", self.node_id, sig_hash),
        };

        self.votes.entry(receipt_id).or_default().push(vote.clone());
        vote
    }

    /// Record a vote from another node
    pub fn record_vote(&mut self, vote: ConsensusVote) {
        self.votes.entry(vote.receipt_id.clone()).or_default().push(vote);
    }

    /// Check if consensus reached for a receipt
    #[must_use]
    pub fn has_consensus(&self, receipt_id: &str) -> Option<bool> {
        let votes = self.votes.get(receipt_id)?;
        #[allow(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            clippy::cast_precision_loss
        )]
        if votes.len() < (self.total_nodes as f64 * self.threshold).ceil() as usize {
            return None; // Not enough votes yet
        }

        let approvals = votes.iter().filter(|v| v.approved).count();
        // Precision loss acceptable for ratio calculation (bounded 0.0-1.0)
        #[allow(clippy::cast_precision_loss)]
        let approval_ratio = approvals as f64 / votes.len() as f64;

        Some(approval_ratio >= self.threshold)
    }

    /// Get consensus status
    #[must_use]
    pub fn consensus_status(&self, receipt_id: &str) -> ConsensusStatus {
        match self.has_consensus(receipt_id) {
            Some(true) => ConsensusStatus::Approved,
            Some(false) => ConsensusStatus::Rejected,
            None => {
                let votes = self.votes.get(receipt_id).map_or(0, Vec::len);
                #[allow(
                    clippy::cast_possible_truncation,
                    clippy::cast_sign_loss,
                    clippy::cast_precision_loss
                )]
                let needed = (self.total_nodes as f64 * self.threshold).ceil() as usize;
                ConsensusStatus::Pending { votes, needed }
            }
        }
    }
}

/// Consensus status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConsensusStatus {
    /// Consensus reached: approved
    Approved,
    /// Consensus reached: rejected
    Rejected,
    /// Consensus pending (votes received, votes needed for quorum)
    Pending {
        /// Number of votes received
        votes: usize,
        /// Number of votes needed for consensus
        needed: usize,
    },
}

// ============================================================================
// Phase 10: Time-Travel Debugging
// ============================================================================

/// Execution snapshot for deterministic replay
#[derive(Debug, Clone)]
pub struct ExecutionSnapshot {
    /// Snapshot identifier
    pub id: String,
    /// Test contract name
    pub contract_name: String,
    /// Execution ticks at snapshot
    pub ticks: u64,
    /// State description (opaque)
    pub state: String,
    /// Timestamp
    pub timestamp: Instant,
}

/// Time-travel debugger for deterministic test replay
pub struct TimeTravelDebugger {
    /// Execution snapshots
    snapshots: Vec<ExecutionSnapshot>,
    /// Current snapshot index
    current_index: usize,
    /// Recording enabled
    recording: bool,
}

impl TimeTravelDebugger {
    /// Create a new time-travel debugger
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn new() -> Self {
        Self { snapshots: Vec::new(), current_index: 0, recording: true }
    }

    /// Start recording snapshots
    #[allow(clippy::missing_const_for_fn)]
    pub fn start_recording(&mut self) {
        self.recording = true;
    }

    /// Stop recording snapshots
    #[allow(clippy::missing_const_for_fn)]
    pub fn stop_recording(&mut self) {
        self.recording = false;
    }

    /// Take a snapshot of current execution state
    pub fn snapshot(&mut self, contract_name: String, ticks: u64, state: String) -> String {
        if !self.recording {
            return String::new();
        }

        let id = format!("snapshot_{contract_name}_{}", self.snapshots.len());
        let snapshot = ExecutionSnapshot {
            id: id.clone(),
            contract_name,
            ticks,
            state,
            timestamp: Instant::now(),
        };

        self.snapshots.push(snapshot);
        id
    }

    /// Replay execution from a snapshot
    #[must_use]
    pub fn replay_from(&mut self, snapshot_id: &str) -> Option<&ExecutionSnapshot> {
        let index = self.snapshots.iter().position(|s| s.id == snapshot_id)?;
        self.current_index = index;
        self.snapshots.get(index)
    }

    /// Step forward one snapshot
    #[must_use]
    pub fn step_forward(&mut self) -> Option<&ExecutionSnapshot> {
        if self.current_index < self.snapshots.len() - 1 {
            self.current_index += 1;
            Some(&self.snapshots[self.current_index])
        } else {
            None
        }
    }

    /// Step backward one snapshot
    #[must_use]
    pub fn step_backward(&mut self) -> Option<&ExecutionSnapshot> {
        if self.current_index > 0 {
            self.current_index -= 1;
            Some(&self.snapshots[self.current_index])
        } else {
            None
        }
    }

    /// Get all snapshots
    #[must_use]
    pub fn snapshots(&self) -> &[ExecutionSnapshot] {
        &self.snapshots
    }

    /// Get current snapshot
    #[must_use]
    pub fn current_snapshot(&self) -> Option<&ExecutionSnapshot> {
        self.snapshots.get(self.current_index)
    }
}

impl Default for TimeTravelDebugger {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Phase 11: Performance Prophet
// ============================================================================

/// Performance prediction
#[derive(Debug, Clone)]
pub struct PerformancePrediction {
    /// Predicted execution time (ticks)
    pub predicted_ticks: u64,
    /// Confidence interval (±ticks)
    pub confidence_interval: u64,
    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,
    /// Prediction basis
    pub basis: String,
}

/// Performance prophet for predictive τ analysis
pub struct PerformanceProphet {
    /// Historical τ measurements
    history: Vec<(String, u64)>, // (contract_name, ticks)
    /// Moving average window size
    window_size: usize,
}

impl PerformanceProphet {
    /// Create a new performance prophet
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Vec initialization is not const
    pub fn new() -> Self {
        Self { history: Vec::new(), window_size: 10 }
    }

    /// Record a performance measurement
    pub fn record(&mut self, contract_name: String, ticks: u64) {
        self.history.push((contract_name, ticks));
    }

    /// Predict performance for a contract
    #[must_use]
    pub fn predict(&self, contract_name: &str) -> PerformancePrediction {
        // Filter history for this contract
        let contract_history: Vec<u64> = self
            .history
            .iter()
            .filter(|(name, _)| name == contract_name)
            .map(|(_, ticks)| *ticks)
            .collect();

        if contract_history.is_empty() {
            // No history - return conservative estimate
            return PerformancePrediction {
                predicted_ticks: 100,
                confidence_interval: 50,
                confidence: 0.0,
                basis: "No historical data".to_string(),
            };
        }

        // Use moving average of recent executions
        let recent: Vec<u64> =
            contract_history.iter().rev().take(self.window_size).copied().collect();

        let sum: u64 = recent.iter().sum();
        let avg = sum / recent.len() as u64;

        // Calculate standard deviation for confidence interval
        #[allow(clippy::cast_precision_loss)]
        let variance: f64 = recent
            .iter()
            .map(|x| {
                #[allow(clippy::cast_precision_loss)]
                let diff = (*x as f64) - (avg as f64);
                diff * diff
            })
            .sum::<f64>()
            / recent.len() as f64;

        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let std_dev = variance.sqrt().ceil() as u64; // Round up to be conservative

        #[allow(clippy::cast_precision_loss)]
        let confidence = (recent.len() as f64 / (recent.len() + 5) as f64).min(1.0);

        PerformancePrediction {
            predicted_ticks: avg,
            confidence_interval: std_dev.max(1), // Minimum interval of 1 tick
            confidence,
            basis: format!("Based on {} recent executions", recent.len()),
        }
    }

    /// Detect performance regressions
    #[must_use]
    pub fn detect_regression(&self, contract_name: &str, current_ticks: u64) -> bool {
        let prediction = self.predict(contract_name);
        // Use 2 sigma (95% confidence interval)
        let upper_bound = prediction.predicted_ticks + (2 * prediction.confidence_interval);
        current_ticks > upper_bound
    }
}

impl Default for PerformanceProphet {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Phase 12: Quality Metrics Dashboard
// ============================================================================

/// Quality metrics aggregator
#[derive(Debug, Clone, Default)]
pub struct QualityMetrics {
    /// Total tests executed
    pub total_tests: usize,
    /// Tests passed
    pub tests_passed: usize,
    /// Tests failed
    pub tests_failed: usize,
    /// Average τ
    pub average_tau: f64,
    /// Maximum τ
    pub max_tau: u64,
    /// Minimum τ
    pub min_tau: u64,
    /// τ violations
    pub tau_violations: usize,
    /// Effect violations
    pub effect_violations: usize,
    /// Coverage percentage
    pub coverage_percent: f64,
    /// Execution time (total)
    pub execution_time: Duration,
}

impl QualityMetrics {
    /// Create new quality metrics
    #[must_use]
    pub fn new() -> Self {
        Self { min_tau: u64::MAX, ..Default::default() }
    }

    /// Update metrics with a test receipt
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for statistical calculations
    pub fn update(&mut self, receipt: &TestReceipt, execution_time: Duration) {
        self.total_tests += 1;
        self.execution_time += execution_time;

        match receipt.result {
            TestOutcome::Pass => self.tests_passed += 1,
            TestOutcome::Fail | TestOutcome::Error => self.tests_failed += 1,
            TestOutcome::Skip => {}
        }

        let ticks = receipt.timing.total_ticks;
        self.max_tau = self.max_tau.max(ticks);
        self.min_tau = self.min_tau.min(ticks);

        // Precision loss acceptable for running average calculation
        #[allow(clippy::cast_precision_loss)]
        let total_tau = self.average_tau * (self.total_tests - 1) as f64;
        #[allow(clippy::cast_precision_loss)]
        let new_avg = (total_tau + ticks as f64) / self.total_tests as f64;
        self.average_tau = new_avg;

        if !receipt.timing.budget_met {
            self.tau_violations += 1;
        }

        // coverage_percent: proxy metric — ratio of passing tests to total tests, as a percentage.
        // This reflects the fraction of tested contract paths that are exercised successfully.
        #[allow(clippy::cast_precision_loss)]
        {
            self.coverage_percent =
                self.tests_passed as f64 / self.total_tests.max(1) as f64 * 100.0;
        }
    }

    /// Get pass rate
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for rate calculations
    pub fn pass_rate(&self) -> f64 {
        if self.total_tests == 0 {
            0.0
        } else {
            self.tests_passed as f64 / self.total_tests as f64
        }
    }

    /// Get failure rate
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for rate calculations
    pub fn failure_rate(&self) -> f64 {
        if self.total_tests == 0 {
            0.0
        } else {
            self.tests_failed as f64 / self.total_tests as f64
        }
    }

    /// Get τ violation rate
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for rate calculations
    pub fn tau_violation_rate(&self) -> f64 {
        if self.total_tests == 0 {
            0.0
        } else {
            self.tau_violations as f64 / self.total_tests as f64
        }
    }

    /// Generate dashboard summary
    #[must_use]
    pub fn dashboard_summary(&self) -> String {
        format!(
            "Quality Metrics Dashboard\n\
             ========================\n\
             Total Tests:      {}\n\
             Passed:           {} ({:.1}%)\n\
             Failed:           {} ({:.1}%)\n\
             Average τ:        {:.1} ticks\n\
             Max τ:            {} ticks\n\
             Min τ:            {} ticks\n\
             τ Violations:     {} ({:.1}%)\n\
             Coverage:         {:.1}%\n\
             Execution Time:   {:?}\n",
            self.total_tests,
            self.tests_passed,
            self.pass_rate() * 100.0,
            self.tests_failed,
            self.failure_rate() * 100.0,
            self.average_tau,
            self.max_tau,
            if self.min_tau == u64::MAX { 0 } else { self.min_tau },
            self.tau_violations,
            self.tau_violation_rate() * 100.0,
            self.coverage_percent,
            self.execution_time,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_distributed_consensus() {
        let mut consensus = DistributedConsensus::new("node1".to_string(), 3);
        let receipt_id = "receipt1".to_string();

        consensus.vote(receipt_id.clone(), true);
        // Byzantine FT threshold is 0.67, so for 3 nodes: ceil(3 * 0.67) = 3 votes needed
        assert_eq!(
            consensus.consensus_status(&receipt_id),
            ConsensusStatus::Pending { votes: 1, needed: 3 }
        );

        let vote2 = ConsensusVote {
            node_id: "node2".to_string(),
            receipt_id: receipt_id.clone(),
            approved: true,
            timestamp: 0,
            mock_signature: "mock_sig_node2".to_string(),
        };
        consensus.record_vote(vote2);

        // Still pending with 2/3 votes
        assert_eq!(
            consensus.consensus_status(&receipt_id),
            ConsensusStatus::Pending { votes: 2, needed: 3 }
        );

        // Add third vote to reach consensus
        let vote3 = ConsensusVote {
            node_id: "node3".to_string(),
            receipt_id: receipt_id.clone(),
            approved: true,
            timestamp: 0,
            mock_signature: "mock_sig_node3".to_string(),
        };
        consensus.record_vote(vote3);

        // Now we have consensus
        assert_eq!(consensus.consensus_status(&receipt_id), ConsensusStatus::Approved);
    }

    #[test]
    fn test_time_travel_debugger() {
        let mut debugger = TimeTravelDebugger::new();
        assert_eq!(debugger.snapshots().len(), 0);

        let _id1 = debugger.snapshot("test1".to_string(), 10, "state1".to_string());
        let id2 = debugger.snapshot("test1".to_string(), 20, "state2".to_string());

        assert_eq!(debugger.snapshots().len(), 2);

        let snapshot = debugger.replay_from(&id2);
        assert!(snapshot.is_some());
        assert_eq!(snapshot.unwrap().ticks, 20);
    }

    #[test]
    fn test_performance_prophet() {
        let mut prophet = PerformanceProphet::new();
        prophet.record("test1".to_string(), 10);
        prophet.record("test1".to_string(), 12);
        prophet.record("test1".to_string(), 11);

        let prediction = prophet.predict("test1");
        assert_eq!(prediction.predicted_ticks, 11);

        assert!(!prophet.detect_regression("test1", 12));
        assert!(prophet.detect_regression("test1", 100));
    }

    #[test]
    fn test_quality_metrics() {
        let mut metrics = QualityMetrics::new();
        assert_eq!(metrics.total_tests, 0);

        let timing = crate::core::receipt::TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let contract = crate::core::contract::TestContract::hot_path("test1", &["module1"]);
        let receipt = TestReceipt::from_contract(&contract, timing, TestOutcome::Pass);

        metrics.update(&receipt, Duration::from_millis(10));
        assert_eq!(metrics.total_tests, 1);
        assert_eq!(metrics.tests_passed, 1);
        assert_eq!(metrics.pass_rate(), 1.0);
    }
}
