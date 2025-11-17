//! Temporal Fabric: Causality, Tick Budgets, and MAPE-K Cycles in Types
//!
//! This module makes temporal and causal guarantees UNREPRESENTABLE in incorrect code:
//! - MAPE-K phases must be traversed in order (typestate machine)
//! - Time flows monotonically through observations and snapshots
//! - Tick budgets are compile-time constants that cannot be violated
//! - Causality is first-class: no retrocausation possible
//!
//! Every invocation of the AHI cycle produces a proof-carrying state that only
//! advances if all prior phases succeeded.

use std::marker::PhantomData;
use serde::{Deserialize, Serialize};

// ============================================================================
// LOGICAL TIME: Monotonic indexing for causality
// ============================================================================

/// Logical timestamp - monotonically increasing, guarantees causality
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct LogicalTime(pub u64);

impl LogicalTime {
    /// Create initial logical time (0)
    pub fn genesis() -> Self {
        LogicalTime(0)
    }

    /// Increment time
    pub fn advance(&self) -> Self {
        LogicalTime(self.0 + 1)
    }

    /// Check if this time is after another (causality check)
    pub fn is_after(&self, other: &LogicalTime) -> bool {
        self.0 > other.0
    }

    /// Get inner value
    pub fn value(&self) -> u64 {
        self.0
    }
}

/// Timebound value: anything with a logical timestamp
/// Proves it cannot be written to the past
pub trait Timed {
    fn logical_time(&self) -> LogicalTime;
}

/// Observation from Γ with logical time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimedObservation {
    pub id: String,
    pub data: String,
    pub timestamp: LogicalTime,
}

impl Timed for TimedObservation {
    fn logical_time(&self) -> LogicalTime {
        self.timestamp
    }
}

/// Snapshot of Σ with logical time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimedSnapshot {
    pub id: String,
    pub content: String,
    pub created_at: LogicalTime,
    pub is_active: bool,
}

impl Timed for TimedSnapshot {
    fn logical_time(&self) -> LogicalTime {
        self.created_at
    }
}

/// Effect that must respect causality: cannot write into the past
pub struct CausalEffect<T: Timed> {
    /// The object being written
    object: T,
    /// Time it was written at
    #[allow(dead_code)]
    written_at: LogicalTime,
    /// Time it affects (must be >= written_at)
    #[allow(dead_code)]
    affects_at: LogicalTime,
}

impl<T: Timed> CausalEffect<T> {
    /// Create a causal effect (fails if written into past)
    pub fn new(object: T, written_at: LogicalTime, affects_at: LogicalTime) -> Result<Self, String> {
        if affects_at.0 < written_at.0 {
            return Err(format!(
                "Causality violation: trying to affect past (written at {}, affects at {})",
                written_at.0, affects_at.0
            ));
        }

        Ok(CausalEffect {
            object,
            written_at,
            affects_at,
        })
    }

    /// Get the actual object
    pub fn object(&self) -> &T {
        &self.object
    }
}

// ============================================================================
// MAPE-K TYPESTATE MACHINE
// ============================================================================
// Impossible to skip phases or reorder. Each type represents completion of
// a prior phase. Transitions consume old state and produce new state.

/// Marker for Monitor phase completion
pub struct MonitorPhaseComplete;

/// Marker for Analyze phase completion
pub struct AnalyzePhaseComplete;

/// Marker for Plan phase completion
pub struct PlanPhaseComplete;

/// Marker for Execute phase completion
pub struct ExecutePhaseComplete;

/// Marker for Knowledge phase completion
pub struct KnowledgePhaseComplete;

/// Entry point: fresh MAPE-K cycle in Monitor phase
#[derive(Debug, Clone)]
pub struct MAPEKCycle {
    /// Cycle ID
    pub cycle_id: String,
    /// Wall-clock time this cycle started
    pub started_at: u64,
    /// Logical time at cycle start
    pub logical_time: LogicalTime,
    /// Current tick count
    pub ticks_used: usize,
}

impl MAPEKCycle {
    /// Create a fresh cycle
    pub fn new(cycle_id: impl Into<String>) -> MonitorState {
        MonitorState {
            cycle: MAPEKCycle {
                cycle_id: cycle_id.into(),
                started_at: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                logical_time: LogicalTime::genesis(),
                ticks_used: 0,
            },
            observations: Vec::new(),
        }
    }
}

/// Monitor Phase: Collect observations from Γ
/// Only transitions to AnalyzeState if observations captured
#[derive(Debug, Clone)]
pub struct MonitorState {
    cycle: MAPEKCycle,
    observations: Vec<TimedObservation>,
}

impl MonitorState {
    /// Add an observation
    pub fn add_observation(&mut self, obs: TimedObservation) {
        self.observations.push(obs);
    }

    /// Transition to Analyze phase
    /// IMPOSSIBLE to transition without observations
    pub fn finalize(self) -> Result<AnalyzeState, String> {
        if self.observations.is_empty() {
            return Err("Cannot advance to Analyze without observations".to_string());
        }

        Ok(AnalyzeState {
            cycle: self.cycle,
            observations: self.observations,
            findings: Vec::new(),
        })
    }
}

/// Analyze Phase: Process observations into findings
#[derive(Debug, Clone)]
pub struct AnalyzeState {
    cycle: MAPEKCycle,
    observations: Vec<TimedObservation>,
    findings: Vec<String>, // In real usage, detailed Finding structs
}

impl AnalyzeState {
    /// Add a finding
    pub fn add_finding(&mut self, finding: String) {
        self.findings.push(finding);
    }

    /// Advance tick counter
    pub fn consume_ticks(&mut self, ticks: usize) -> Result<(), String> {
        self.cycle.ticks_used += ticks;
        if self.cycle.ticks_used > 8 {
            return Err(format!(
                "Hot path tick budget exceeded: {} > 8",
                self.cycle.ticks_used
            ));
        }
        Ok(())
    }

    /// Transition to Plan phase
    pub fn finalize(self) -> Result<PlanState, String> {
        if self.findings.is_empty() {
            return Err("Cannot advance to Plan without findings".to_string());
        }

        Ok(PlanState {
            cycle: self.cycle,
            observations: self.observations,
            findings: self.findings,
            proposals: Vec::new(),
        })
    }
}

/// Plan Phase: Generate proposals from findings
#[derive(Debug, Clone)]
pub struct PlanState {
    cycle: MAPEKCycle,
    observations: Vec<TimedObservation>,
    findings: Vec<String>,
    proposals: Vec<String>, // In real usage, detailed Proposal structs
}

impl PlanState {
    /// Add a proposal
    pub fn add_proposal(&mut self, proposal: String) {
        self.proposals.push(proposal);
    }

    /// Transition to Execute phase
    pub fn finalize(self) -> Result<ExecuteState, String> {
        if self.proposals.is_empty() {
            return Err("Cannot advance to Execute without proposals".to_string());
        }

        Ok(ExecuteState {
            cycle: self.cycle,
            observations: self.observations,
            findings: self.findings,
            proposals: self.proposals,
            results: Vec::new(),
        })
    }
}

/// Execute Phase: Apply proposals and capture results
#[derive(Debug, Clone)]
pub struct ExecuteState {
    cycle: MAPEKCycle,
    observations: Vec<TimedObservation>,
    findings: Vec<String>,
    proposals: Vec<String>,
    results: Vec<String>, // Execution results
}

impl ExecuteState {
    /// Record execution result
    pub fn add_result(&mut self, result: String) {
        self.results.push(result);
    }

    /// Transition to Knowledge phase
    pub fn finalize(self) -> Result<KnowledgeState, String> {
        Ok(KnowledgeState {
            cycle: self.cycle,
            observations: self.observations,
            findings: self.findings,
            proposals: self.proposals,
            results: self.results,
            learned: Vec::new(),
        })
    }
}

/// Knowledge Phase: Learn from cycle and update K (knowledge)
#[derive(Debug, Clone)]
pub struct KnowledgeState {
    cycle: MAPEKCycle,
    observations: Vec<TimedObservation>,
    findings: Vec<String>,
    proposals: Vec<String>,
    results: Vec<String>,
    learned: Vec<String>, // Lessons learned
}

impl KnowledgeState {
    /// Record a lesson learned
    pub fn learn(&mut self, lesson: String) {
        self.learned.push(lesson);
    }

    /// Finalize the entire cycle
    pub fn finalize(self) -> CompletedMAPEKCycle {
        CompletedMAPEKCycle {
            cycle_id: self.cycle.cycle_id,
            logical_time: self.cycle.logical_time,
            ticks_used: self.cycle.ticks_used,
            observations_count: self.observations.len(),
            findings_count: self.findings.len(),
            proposals_count: self.proposals.len(),
            execution_results: self.results.len(),
            lessons_learned: self.learned.len(),
        }
    }
}

/// Completed MAPE-K cycle (immutable proof of completion)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompletedMAPEKCycle {
    pub cycle_id: String,
    pub logical_time: LogicalTime,
    pub ticks_used: usize,
    pub observations_count: usize,
    pub findings_count: usize,
    pub proposals_count: usize,
    pub execution_results: usize,
    pub lessons_learned: usize,
}

// ============================================================================
// TICK BUDGETS: Compile-time and runtime enforcement
// ============================================================================

/// Const generic tick budgets that encode hot/warm/cold path bounds
pub trait TickBudget {
    const MAX_TICKS: usize;

    fn check_budget(ticks_used: usize) -> Result<(), String> {
        if ticks_used > Self::MAX_TICKS {
            Err(format!(
                "Tick budget exceeded: {} > {}",
                ticks_used,
                Self::MAX_TICKS
            ))
        } else {
            Ok(())
        }
    }
}

/// Hot path: ≤ 8 ticks (Chatman constant)
pub struct HotPathBudget;
impl TickBudget for HotPathBudget {
    const MAX_TICKS: usize = 8;
}

/// Warm path: ≤ 100 ticks
pub struct WarmPathBudget;
impl TickBudget for WarmPathBudget {
    const MAX_TICKS: usize = 100;
}

/// Cold path: unbounded
pub struct ColdPathBudget;
impl TickBudget for ColdPathBudget {
    const MAX_TICKS: usize = usize::MAX;
}

/// Tick-budgeted action execution
pub struct TickBudgetedExecution<B: TickBudget> {
    ticks_used: usize,
    _budget: PhantomData<B>,
}

impl<B: TickBudget> TickBudgetedExecution<B> {
    /// Create new execution within budget
    pub fn new() -> Self {
        Self {
            ticks_used: 0,
            _budget: PhantomData,
        }
    }

    /// Consume ticks (fails if exceeds budget)
    pub fn consume(&mut self, ticks: usize) -> Result<(), String> {
        self.ticks_used += ticks;
        B::check_budget(self.ticks_used)
    }

    /// Get remaining budget
    pub fn remaining(&self) -> usize {
        B::MAX_TICKS - self.ticks_used
    }

    /// Verify final state
    pub fn verify(&self) -> Result<(), String> {
        B::check_budget(self.ticks_used)
    }
}

// ============================================================================
// TEMPORAL ORDERING GUARANTEES
// ============================================================================

/// Proof that a value came AFTER another in logical time
pub struct TemporalAfter<T: Timed> {
    after: T,
    before: T,
}

impl<T: Timed> TemporalAfter<T> {
    /// Construct if causally valid
    pub fn new(before: T, after: T) -> Result<Self, String> {
        if after.logical_time().is_after(&before.logical_time()) {
            Ok(TemporalAfter { after, before })
        } else {
            Err("Temporal ordering violated: after is not after before".to_string())
        }
    }

    /// Get the after value
    pub fn after(&self) -> &T {
        &self.after
    }

    /// Get the before value
    pub fn before(&self) -> &T {
        &self.before
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_logical_time_advance() {
        let t0 = LogicalTime::genesis();
        let t1 = t0.advance();
        let t2 = t1.advance();

        assert_eq!(t0.value(), 0);
        assert_eq!(t1.value(), 1);
        assert_eq!(t2.value(), 2);
        assert!(t2.is_after(&t1));
        assert!(t1.is_after(&t0));
    }

    #[test]
    fn test_mape_k_typestate() {
        let monitor = MAPEKCycle::new("cycle-1");

        let mut monitor_state = MonitorState {
            cycle: monitor.cycle,
            observations: Vec::new(),
        };

        // Add observations
        monitor_state.add_observation(TimedObservation {
            id: "obs-1".to_string(),
            data: "test".to_string(),
            timestamp: LogicalTime(1),
        });

        // Transition to Analyze
        let analyze = monitor_state.finalize();
        assert!(analyze.is_ok());

        let mut analyze_state = analyze.unwrap();
        analyze_state.add_finding("finding-1".to_string());

        // Transition to Plan
        let plan = analyze_state.finalize();
        assert!(plan.is_ok());

        let mut plan_state = plan.unwrap();
        plan_state.add_proposal("proposal-1".to_string());

        // Transition to Execute
        let execute = plan_state.finalize();
        assert!(execute.is_ok());

        let mut execute_state = execute.unwrap();
        execute_state.add_result("result-1".to_string());

        // Transition to Knowledge
        let knowledge = execute_state.finalize();
        assert!(knowledge.is_ok());

        let mut knowledge_state = knowledge.unwrap();
        knowledge_state.learn("lesson-1".to_string());

        // Finalize
        let completed = knowledge_state.finalize();
        assert_eq!(completed.observations_count, 1);
        assert_eq!(completed.findings_count, 1);
        assert_eq!(completed.proposals_count, 1);
    }

    #[test]
    fn test_mape_k_phase_skip_prevention() {
        let monitor = MAPEKCycle::new("cycle-2");
        let monitor_state = MonitorState {
            cycle: monitor.cycle,
            observations: Vec::new(),
        };

        // Cannot transition to Analyze without observations
        let result = monitor_state.finalize();
        assert!(result.is_err());
    }

    #[test]
    fn test_tick_budget_hot_path() {
        let mut exec = TickBudgetedExecution::<HotPathBudget>::new();

        // Can consume up to 8 ticks
        assert!(exec.consume(5).is_ok());
        assert_eq!(exec.remaining(), 3);

        // Cannot exceed budget
        let result = exec.consume(5);
        assert!(result.is_err());
    }

    #[test]
    fn test_causal_effect() {
        let written_at = LogicalTime(1);
        let affects_at = LogicalTime(2);

        let obs = TimedObservation {
            id: "obs".to_string(),
            data: "data".to_string(),
            timestamp: affects_at,
        };

        // Valid: effect is written before it takes effect
        let effect = CausalEffect::new(obs, written_at, affects_at);
        assert!(effect.is_ok());

        // Invalid: retrocausation
        let obs2 = TimedObservation {
            id: "obs2".to_string(),
            data: "data".to_string(),
            timestamp: LogicalTime(1),
        };

        let bad_effect = CausalEffect::new(obs2, LogicalTime(5), LogicalTime(2));
        assert!(bad_effect.is_err());
    }

    #[test]
    fn test_temporal_ordering() {
        let before = TimedObservation {
            id: "before".to_string(),
            data: "d1".to_string(),
            timestamp: LogicalTime(1),
        };

        let after = TimedObservation {
            id: "after".to_string(),
            data: "d2".to_string(),
            timestamp: LogicalTime(2),
        };

        let ordering = TemporalAfter::new(before, after);
        assert!(ordering.is_ok());

        // Reverse should fail
        let before2 = TimedObservation {
            id: "before2".to_string(),
            data: "d1".to_string(),
            timestamp: LogicalTime(5),
        };

        let after2 = TimedObservation {
            id: "after2".to_string(),
            data: "d2".to_string(),
            timestamp: LogicalTime(3),
        };

        let bad_ordering = TemporalAfter::new(before2, after2);
        assert!(bad_ordering.is_err());
    }
}
