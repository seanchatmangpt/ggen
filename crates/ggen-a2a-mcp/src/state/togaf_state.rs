//! TogafStateManager - Turn progression and phase tracking for the 70-turn protocol.

use std::collections::{HashMap, HashSet};
use std::ops::RangeInclusive;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use chrono::Utc;
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tracing::{info, instrument};

use super::artifacts::{Artifact, ArtifactRegistry};
use super::error::{StateError, StateResult};

// ---------------------------------------------------------------------------
// TogafPhase
// ---------------------------------------------------------------------------

/// The six TOGAF ADM phases in the FIBO-TOGAF protocol.
///
/// Each phase maps to a contiguous range of turns within the 70-turn
/// collaboration protocol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TogafPhase {
    /// Architecture Vision (Turns 1-8)
    A,
    /// Business Architecture (Turns 9-22)
    B,
    /// Information Systems Architectures (Turns 23-40)
    C,
    /// Technology Architecture (Turns 41-54)
    D,
    /// Opportunities & Solutions (Turns 55-62)
    E,
    /// Migration Planning (Turns 63-70)
    F,
}

impl TogafPhase {
    /// Returns the inclusive turn range for this phase.
    pub fn turn_range(&self) -> RangeInclusive<usize> {
        match self {
            TogafPhase::A => 1..=8,
            TogafPhase::B => 9..=22,
            TogafPhase::C => 23..=40,
            TogafPhase::D => 41..=54,
            TogafPhase::E => 55..=62,
            TogafPhase::F => 63..=70,
        }
    }

    /// Maps a turn number (1-based) to its phase.
    ///
    /// Returns an error if the turn is outside the valid range (1-70).
    pub fn from_turn(turn: usize) -> StateResult<Self> {
        match turn {
            1..=8 => Ok(TogafPhase::A),
            9..=22 => Ok(TogafPhase::B),
            23..=40 => Ok(TogafPhase::C),
            41..=54 => Ok(TogafPhase::D),
            55..=62 => Ok(TogafPhase::E),
            63..=70 => Ok(TogafPhase::F),
            _ => Err(StateError::InvalidTurn(turn, 70)),
        }
    }

    /// The turn at which the ARB gate for this phase sits (the last turn
    /// of the phase).
    pub fn arb_gate(&self) -> usize {
        *self.turn_range().end()
    }

    /// Human-readable label for the ARB reviewer role at the phase gate.
    pub fn arb_reviewer(&self) -> &'static str {
        match self {
            TogafPhase::A => "ChiefArchitect",
            TogafPhase::B => "ChiefArchitect + ComplianceOfficer",
            TogafPhase::C => "ChiefArchitect + ComplianceOfficer",
            TogafPhase::D => "ChiefArchitect",
            TogafPhase::E => "ComplianceOfficer",
            TogafPhase::F => "ChiefArchitect",
        }
    }

    /// Ordered list of all phases.
    pub fn all() -> &'static [TogafPhase] {
        &[
            TogafPhase::A,
            TogafPhase::B,
            TogafPhase::C,
            TogafPhase::D,
            TogafPhase::E,
            TogafPhase::F,
        ]
    }

    /// Returns the next phase, if one exists.
    pub fn next(&self) -> Option<TogafPhase> {
        match self {
            TogafPhase::A => Some(TogafPhase::B),
            TogafPhase::B => Some(TogafPhase::C),
            TogafPhase::C => Some(TogafPhase::D),
            TogafPhase::D => Some(TogafPhase::E),
            TogafPhase::E => Some(TogafPhase::F),
            TogafPhase::F => None,
        }
    }

    /// Display name for the phase.
    pub fn display_name(&self) -> &'static str {
        match self {
            TogafPhase::A => "Architecture Vision",
            TogafPhase::B => "Business Architecture",
            TogafPhase::C => "Information Systems Architectures",
            TogafPhase::D => "Technology Architecture",
            TogafPhase::E => "Opportunities & Solutions",
            TogafPhase::F => "Migration Planning",
        }
    }
}

impl std::fmt::Display for TogafPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// ---------------------------------------------------------------------------
// PhaseStatus
// ---------------------------------------------------------------------------

/// Status of a phase within the protocol.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PhaseStatus {
    /// Phase has not started yet.
    NotStarted,
    /// Phase is currently in progress.
    InProgress,
    /// Phase is awaiting ARB gate approval.
    ArbPending,
    /// ARB gate has been approved for this phase.
    ArbApproved,
    /// Phase is fully complete.
    Completed,
}

impl std::fmt::Display for PhaseStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhaseStatus::NotStarted => write!(f, "NotStarted"),
            PhaseStatus::InProgress => write!(f, "InProgress"),
            PhaseStatus::ArbPending => write!(f, "ArbPending"),
            PhaseStatus::ArbApproved => write!(f, "ArbApproved"),
            PhaseStatus::Completed => write!(f, "Completed"),
        }
    }
}

// ---------------------------------------------------------------------------
// PhaseState
// ---------------------------------------------------------------------------

/// Snapshot of a single phase's progress.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhaseState {
    /// Which phase this state belongs to.
    pub phase: TogafPhase,
    /// The most recently completed turn within this phase (0 = none yet).
    pub current_turn: usize,
    /// Set of turns that have been completed.
    pub completed_turns: HashSet<usize>,
    /// Current lifecycle status of the phase.
    pub status: PhaseStatus,
}

impl PhaseState {
    /// Create a new `PhaseState` for the given phase, starting at `NotStarted`.
    pub fn new(phase: TogafPhase) -> Self {
        Self {
            phase,
            current_turn: 0,
            completed_turns: HashSet::new(),
            status: PhaseStatus::NotStarted,
        }
    }

    /// Returns the total number of turns in this phase.
    pub fn turn_count(&self) -> usize {
        self.phase.turn_range().count()
    }

    /// Returns the number of completed turns.
    pub fn completed_count(&self) -> usize {
        self.completed_turns.len()
    }

    /// Returns true if all turns in this phase are completed.
    pub fn is_all_turns_done(&self) -> bool {
        self.phase
            .turn_range()
            .all(|t| self.completed_turns.contains(&t))
    }
}

// ---------------------------------------------------------------------------
// TurnRecord
// ---------------------------------------------------------------------------

/// Record produced each time a turn is advanced.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TurnRecord {
    /// The turn number that was just completed.
    pub turn: usize,
    /// The phase this turn belongs to.
    pub phase: TogafPhase,
    /// Whether this turn is an ARB gate.
    pub is_arb_gate: bool,
    /// The phase status after this turn.
    pub phase_status: PhaseStatus,
    /// Timestamp when the turn was advanced.
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

// ---------------------------------------------------------------------------
// StateSummary
// ---------------------------------------------------------------------------

/// High-level snapshot of the entire protocol state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateSummary {
    /// Current turn number (the next turn to execute; 1-based).
    pub current_turn: usize,
    /// Total turns in the protocol.
    pub total_turns: usize,
    /// Per-phase status summary.
    pub phases: HashMap<TogafPhase, PhaseStatus>,
    /// Total number of artifacts stored.
    pub artifact_count: usize,
    /// Whether the entire protocol is complete.
    pub is_complete: bool,
}

// ---------------------------------------------------------------------------
// TogafStateManager
// ---------------------------------------------------------------------------

/// Manages the 70-turn FIBO-TOGAF protocol state.
///
/// Uses `AtomicUsize` for the turn counter (lock-free reads) and
/// `Arc<RwLock<>>` for shared mutable state (phases, artifacts).
///
/// # Thread Safety
///
/// The manager is `Clone` and can be shared across tasks. All interior
/// mutation goes through `RwLock` or atomic operations.
#[derive(Clone)]
pub struct TogafStateManager {
    /// Next turn to execute (starts at 1). Uses `SeqCst` ordering for
    /// cross-task consistency.
    current_turn: Arc<AtomicUsize>,
    /// Maximum number of turns in the protocol.
    total_turns: usize,
    /// Per-phase state, guarded by an async `RwLock`.
    phases: Arc<RwLock<HashMap<TogafPhase, PhaseState>>>,
    /// Artifact registry, guarded by an async `RwLock`.
    artifacts: Arc<RwLock<ArtifactRegistry>>,
}

impl TogafStateManager {
    /// Create a new state manager for a protocol with the given total turns.
    pub fn new(total_turns: usize) -> StateResult<Self> {
        if total_turns == 0 {
            return Err(StateError::InvalidTurn(0, 0));
        }

        let phases: HashMap<TogafPhase, PhaseState> = TogafPhase::all()
            .iter()
            .map(|&p| (p, PhaseState::new(p)))
            .collect();

        Ok(Self {
            current_turn: Arc::new(AtomicUsize::new(1)),
            total_turns,
            phases: Arc::new(RwLock::new(phases)),
            artifacts: Arc::new(RwLock::new(ArtifactRegistry::new())),
        })
    }

    /// Create a state manager pre-configured for the standard 70-turn protocol.
    pub fn standard_70() -> Self {
        Self::new(70).expect("Failed to create standard 70-turn state manager")
    }

    /// Returns the current turn number (the next turn to execute).
    ///
    /// This is a lock-free read via `AtomicUsize`.
    pub fn current_turn(&self) -> usize {
        self.current_turn.load(Ordering::SeqCst)
    }

    /// Returns the total number of turns in the protocol.
    pub fn total_turns(&self) -> usize {
        self.total_turns
    }

    /// Returns true if the protocol has been fully completed.
    pub fn is_complete(&self) -> bool {
        self.current_turn.load(Ordering::SeqCst) > self.total_turns
    }

    /// Determine which phase a given turn belongs to.
    ///
    /// For the standard 70-turn protocol, turns 1-8 = Phase A, etc.
    /// For custom `total_turns`, falls back to proportional allocation.
    pub async fn get_phase(&self, turn: usize) -> StateResult<TogafPhase> {
        if turn < 1 || turn > self.total_turns {
            return Err(StateError::InvalidTurn(turn, self.total_turns));
        }

        // Standard 70-turn protocol has fixed phase boundaries.
        if self.total_turns == 70 {
            return TogafPhase::from_turn(turn);
        }

        // For non-standard turn counts, use proportional allocation.
        let phases = TogafPhase::all();
        let turns_per_phase = (self.total_turns as f64 / phases.len() as f64).ceil() as usize;
        let idx = ((turn - 1) / turns_per_phase).min(phases.len() - 1);
        Ok(phases[idx])
    }

    /// Check whether a given turn is an ARB gate.
    ///
    /// An ARB gate sits at the last turn of each phase.
    pub async fn is_arb_gate(&self, turn: usize) -> bool {
        self.get_phase(turn)
            .await
            .map(|phase| phase.arb_gate() == turn)
            .unwrap_or(false)
    }

    /// Advance to the next turn.
    ///
    /// Returns a [`TurnRecord`] describing what happened. If the current
    /// turn is an ARB gate and has not been approved, returns an error.
    ///
    /// # Errors
    ///
    /// - [`StateError::TurnExhausted`] if all turns have been consumed.
    /// - [`StateError::ArbGateNotApproved`] if the current turn is a gate
    ///   and approval has not been recorded.
    #[instrument(skip(self), fields(turn = self.current_turn.load(Ordering::SeqCst)))]
    pub async fn advance_turn(&self) -> StateResult<TurnRecord> {
        let turn = self.current_turn.load(Ordering::SeqCst);

        if turn > self.total_turns {
            return Err(StateError::TurnExhausted(self.total_turns));
        }

        let phase = self.get_phase(turn).await?;
        let is_gate = self.is_arb_gate(turn).await;

        // Check ARB gate approval: block if the current turn is an ARB gate
        // that hasn't been approved.
        if is_gate {
            let phases_guard = self.phases.read().await;
            if let Some(ps) = phases_guard.get(&phase) {
                if ps.status == PhaseStatus::ArbPending {
                    return Err(StateError::ArbGateNotApproved(turn));
                }
            }
        }

        // Check: if this is the first turn of a new phase, verify the
        // previous phase's ARB gate has been approved.
        let range = phase.turn_range();
        if turn == *range.start() && turn > 1 {
            let prev_phase = TogafPhase::from_turn(turn - 1)?;
            let phases_guard = self.phases.read().await;
            if let Some(ps) = phases_guard.get(&prev_phase) {
                if ps.status == PhaseStatus::ArbPending {
                    return Err(StateError::ArbGateNotApproved(prev_phase.arb_gate()));
                }
            }
        }

        // Update phase state.
        {
            let mut phases_guard = self.phases.write().await;

            let ps = phases_guard
                .entry(phase)
                .or_insert_with(|| PhaseState::new(phase));

            // Transition phase status.
            match ps.status {
                PhaseStatus::NotStarted | PhaseStatus::ArbApproved => {
                    ps.status = PhaseStatus::InProgress;
                }
                PhaseStatus::Completed => {
                    return Err(StateError::PhaseNotInProgress(
                        phase.to_string(),
                        PhaseStatus::Completed.to_string(),
                    ));
                }
                PhaseStatus::InProgress | PhaseStatus::ArbPending => {}
            }

            ps.completed_turns.insert(turn);
            ps.current_turn = turn;

            // If this was the last turn of the phase, move to ArbPending
            // (unless it's the very last turn of the protocol).
            if turn == phase.arb_gate() && turn != self.total_turns {
                ps.status = PhaseStatus::ArbPending;
            }

            // If this is the very last turn of the protocol, mark complete.
            if turn == self.total_turns {
                ps.status = PhaseStatus::Completed;
            }
        }

        // Atomically advance the turn counter.
        self.current_turn.store(turn + 1, Ordering::SeqCst);

        let phase_status = {
            let phases_guard = self.phases.read().await;
            phases_guard
                .get(&phase)
                .map(|ps| ps.status.clone())
                .unwrap_or(PhaseStatus::NotStarted)
        };

        let record = TurnRecord {
            turn,
            phase,
            is_arb_gate: is_gate,
            phase_status,
            timestamp: Utc::now(),
        };

        info!(
            turn = record.turn,
            phase = %record.phase,
            is_arb_gate = record.is_arb_gate,
            status = %record.phase_status,
            "Turn advanced"
        );

        Ok(record)
    }

    /// Mark an ARB gate as approved for a given phase, allowing the
    /// protocol to continue past the gate.
    ///
    /// This is typically called by [`ArbApprovalManager`] after all
    /// required reviewers have approved.
    pub async fn approve_arb_gate(&self, turn: usize) -> StateResult<()> {
        let phase = self.get_phase(turn).await?;
        if phase.arb_gate() != turn {
            return Err(StateError::InvalidTurn(turn, phase.arb_gate()));
        }

        let mut phases_guard = self.phases.write().await;

        if let Some(ps) = phases_guard.get_mut(&phase) {
            ps.status = PhaseStatus::ArbApproved;
            info!(turn = turn, phase = %phase, "ARB gate approved");
        }

        Ok(())
    }

    /// Store an artifact produced at a given turn.
    pub async fn store_artifact(&self, turn: usize, artifact: Artifact) -> StateResult<()> {
        if turn < 1 || turn > self.total_turns {
            return Err(StateError::InvalidTurn(turn, self.total_turns));
        }

        let mut artifacts_guard = self.artifacts.write().await;
        artifacts_guard.insert(artifact);
        Ok(())
    }

    /// Retrieve all artifacts produced during a given phase.
    pub async fn get_artifacts_for_phase(&self, phase: TogafPhase) -> StateResult<Vec<Artifact>> {
        let artifacts_guard = self.artifacts.read().await;
        Ok(artifacts_guard
            .get_for_phase(phase)
            .into_iter()
            .cloned()
            .collect())
    }

    /// Check whether a phase has been completed.
    pub async fn is_phase_complete(&self, phase: TogafPhase) -> bool {
        let phases_guard = self.phases.read().await;
        phases_guard
            .get(&phase)
            .map(|ps| ps.status == PhaseStatus::Completed)
            .unwrap_or(false)
    }

    /// Get a snapshot of the current phase state.
    pub async fn get_phase_state(&self, phase: TogafPhase) -> StateResult<PhaseState> {
        let phases_guard = self.phases.read().await;
        phases_guard.get(&phase).cloned().ok_or_else(|| {
            StateError::PhaseNotInProgress(phase.to_string(), "NotStarted".to_string())
        })
    }

    /// Produce a summary of the entire protocol state.
    pub async fn summary(&self) -> StateSummary {
        let current_turn = self.current_turn.load(Ordering::SeqCst);
        let phases_guard = self.phases.read().await;
        let artifacts_guard = self.artifacts.read().await;

        let mut phase_summary = HashMap::new();
        for (&phase, ps) in phases_guard.iter() {
            phase_summary.insert(phase, ps.status.clone());
        }

        let artifact_count = artifacts_guard.count();

        StateSummary {
            current_turn,
            total_turns: self.total_turns,
            phases: phase_summary,
            artifact_count,
            is_complete: current_turn > self.total_turns,
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn togaf_phase_turn_ranges() {
        assert_eq!(TogafPhase::A.turn_range(), 1..=8);
        assert_eq!(TogafPhase::B.turn_range(), 9..=22);
        assert_eq!(TogafPhase::C.turn_range(), 23..=40);
        assert_eq!(TogafPhase::D.turn_range(), 41..=54);
        assert_eq!(TogafPhase::E.turn_range(), 55..=62);
        assert_eq!(TogafPhase::F.turn_range(), 63..=70);
    }

    #[test]
    fn togaf_phase_from_turn() {
        assert_eq!(TogafPhase::from_turn(1).unwrap(), TogafPhase::A);
        assert_eq!(TogafPhase::from_turn(8).unwrap(), TogafPhase::A);
        assert_eq!(TogafPhase::from_turn(9).unwrap(), TogafPhase::B);
        assert_eq!(TogafPhase::from_turn(22).unwrap(), TogafPhase::B);
        assert_eq!(TogafPhase::from_turn(23).unwrap(), TogafPhase::C);
        assert_eq!(TogafPhase::from_turn(40).unwrap(), TogafPhase::C);
        assert_eq!(TogafPhase::from_turn(41).unwrap(), TogafPhase::D);
        assert_eq!(TogafPhase::from_turn(54).unwrap(), TogafPhase::D);
        assert_eq!(TogafPhase::from_turn(55).unwrap(), TogafPhase::E);
        assert_eq!(TogafPhase::from_turn(62).unwrap(), TogafPhase::E);
        assert_eq!(TogafPhase::from_turn(63).unwrap(), TogafPhase::F);
        assert_eq!(TogafPhase::from_turn(70).unwrap(), TogafPhase::F);
    }

    #[test]
    fn togaf_phase_from_turn_invalid() {
        assert!(TogafPhase::from_turn(0).is_err());
        assert!(TogafPhase::from_turn(71).is_err());
        assert!(TogafPhase::from_turn(100).is_err());
    }

    #[test]
    fn togaf_phase_arb_gates() {
        // ARB gates are at the last turn of each phase.
        assert_eq!(TogafPhase::A.arb_gate(), 8);
        assert_eq!(TogafPhase::B.arb_gate(), 22);
        assert_eq!(TogafPhase::C.arb_gate(), 40);
        assert_eq!(TogafPhase::D.arb_gate(), 54);
        assert_eq!(TogafPhase::E.arb_gate(), 62);
        assert_eq!(TogafPhase::F.arb_gate(), 70);
    }

    #[test]
    fn togaf_phase_next() {
        assert_eq!(TogafPhase::A.next(), Some(TogafPhase::B));
        assert_eq!(TogafPhase::B.next(), Some(TogafPhase::C));
        assert_eq!(TogafPhase::C.next(), Some(TogafPhase::D));
        assert_eq!(TogafPhase::D.next(), Some(TogafPhase::E));
        assert_eq!(TogafPhase::E.next(), Some(TogafPhase::F));
        assert_eq!(TogafPhase::F.next(), None);
    }

    #[tokio::test]
    async fn state_manager_initial_state() {
        let mgr = TogafStateManager::standard_70();
        assert_eq!(mgr.current_turn(), 1);
        assert_eq!(mgr.total_turns(), 70);
        assert!(!mgr.is_complete());
    }

    #[tokio::test]
    async fn state_manager_advance_single_turn() {
        let mgr = TogafStateManager::standard_70();
        let record = mgr.advance_turn().await.unwrap();
        assert_eq!(record.turn, 1);
        assert_eq!(record.phase, TogafPhase::A);
        assert!(!record.is_arb_gate);
        assert_eq!(mgr.current_turn(), 2);
    }

    #[tokio::test]
    async fn state_manager_detects_arb_gate() {
        let mgr = TogafStateManager::standard_70();

        // Advance to turn 8 (last turn of Phase A = ARB gate).
        for _ in 1..8 {
            let _ = mgr.advance_turn().await.unwrap();
        }
        let record = mgr.advance_turn().await.unwrap();
        assert_eq!(record.turn, 8);
        assert!(record.is_arb_gate);
        // Turn 8 is the last turn of the 70-turn protocol's Phase A but
        // NOT the last turn overall, so the phase should be ArbPending.
        assert_eq!(record.phase_status, PhaseStatus::ArbPending);
    }

    #[tokio::test]
    async fn state_manager_blocks_at_arb_gate() {
        let mgr = TogafStateManager::standard_70();

        // Advance through Phase A to the ARB gate at turn 8.
        for _ in 1..8 {
            let _ = mgr.advance_turn().await.unwrap();
        }
        let _ = mgr.advance_turn().await.unwrap(); // Turn 8

        // Turn 8 is now ArbPending. Trying to advance to turn 9 should fail.
        let result = mgr.advance_turn().await;
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::ArbGateNotApproved(8) => {} // expected
            other => panic!("Expected ArbGateNotApproved(8), got: {:?}", other),
        }
    }

    #[tokio::test]
    async fn state_manager_approve_gate_and_continue() {
        let mgr = TogafStateManager::standard_70();

        // Advance to the ARB gate at turn 8.
        for _ in 1..8 {
            let _ = mgr.advance_turn().await.unwrap();
        }
        let _ = mgr.advance_turn().await.unwrap(); // Turn 8

        // Approve the gate.
        mgr.approve_arb_gate(8).await.unwrap();

        // Now we should be able to advance to turn 9.
        let record = mgr.advance_turn().await.unwrap();
        assert_eq!(record.turn, 9);
        assert_eq!(record.phase, TogafPhase::B);
    }

    #[tokio::test]
    async fn state_manager_turn_exhausted() {
        let mgr = TogafStateManager::new(3).unwrap();

        // Turn 1
        mgr.advance_turn().await.unwrap();
        // Turn 2
        mgr.advance_turn().await.unwrap();
        // Turn 3 (last) - need to approve since it's a gate
        mgr.advance_turn().await.unwrap();

        // Turn 4 should fail.
        let result: StateResult<TurnRecord> = mgr.advance_turn().await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn state_manager_store_and_retrieve_artifact() {
        let mgr = TogafStateManager::standard_70();

        let artifact = Artifact::new_test(1, TogafPhase::A, "StakeholderMap");
        mgr.store_artifact(1, artifact.clone()).await.unwrap();

        let artifacts = mgr.get_artifacts_for_phase(TogafPhase::A).await.unwrap();
        assert_eq!(artifacts.len(), 1);
        assert_eq!(artifacts[0].name, "StakeholderMap");
    }

    #[tokio::test]
    async fn state_manager_summary() {
        let mgr = TogafStateManager::standard_70();
        mgr.advance_turn().await.unwrap();

        let summary = mgr.summary().await;
        assert_eq!(summary.current_turn, 2);
        assert_eq!(summary.total_turns, 70);
        assert!(!summary.is_complete);
        // Phase A should be InProgress.
        assert_eq!(
            summary.phases.get(&TogafPhase::A),
            Some(&PhaseStatus::InProgress)
        );
    }

    #[tokio::test]
    async fn state_manager_get_phase_state() {
        let mgr = TogafStateManager::standard_70();
        mgr.advance_turn().await.unwrap();

        let ps = mgr.get_phase_state(TogafPhase::A).await.unwrap();
        assert_eq!(ps.phase, TogafPhase::A);
        assert_eq!(ps.status, PhaseStatus::InProgress);
        assert!(ps.completed_turns.contains(&1));
    }

    #[tokio::test]
    async fn state_manager_non_standard_turns() {
        // 12 turns, 2 per phase.
        let mgr = TogafStateManager::new(12).unwrap();
        assert_eq!(mgr.total_turns(), 12);

        let phase_1 = mgr.get_phase(1).await.unwrap();
        let phase_6 = mgr.get_phase(12).await.unwrap();
        // With 12 turns / 6 phases = 2 per phase.
        assert_eq!(phase_1, TogafPhase::A);
        assert_eq!(phase_6, TogafPhase::F);
    }
}
