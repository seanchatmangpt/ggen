//! PDCA (Plan-Do-Check-Act) cycle tracking.
//!
//! This module provides structures and functionality for tracking improvements
//! through the Deming PDCA cycle.

use crate::{Improvement, KaizenError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// State in the PDCA cycle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PdcaState {
    /// Planning phase - defining the improvement.
    Plan,
    /// Doing phase - implementing the improvement.
    Do,
    /// Checking phase - measuring results.
    Check,
    /// Acting phase - standardizing or iterating.
    Act,
    /// Completed cycle.
    Completed,
}

impl PdcaState {
    /// Gets the next valid state in the cycle.
    pub fn next(&self) -> Option<Self> {
        match self {
            PdcaState::Plan => Some(PdcaState::Do),
            PdcaState::Do => Some(PdcaState::Check),
            PdcaState::Check => Some(PdcaState::Act),
            PdcaState::Act => Some(PdcaState::Completed),
            PdcaState::Completed => None,
        }
    }

    /// Checks if transition to target state is valid.
    pub fn can_transition_to(&self, target: &PdcaState) -> bool {
        match (self, target) {
            (PdcaState::Plan, PdcaState::Do) => true,
            (PdcaState::Do, PdcaState::Check) => true,
            (PdcaState::Check, PdcaState::Act) => true,
            (PdcaState::Act, PdcaState::Completed) => true,
            (PdcaState::Act, PdcaState::Plan) => true, // Allow iteration
            _ => false,
        }
    }
}

/// A phase in the PDCA cycle with associated data.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PdcaPhase {
    /// The state this phase represents.
    pub state: PdcaState,
    /// When this phase started.
    pub started_at: DateTime<Utc>,
    /// When this phase completed.
    pub completed_at: Option<DateTime<Utc>>,
    /// Notes for this phase.
    pub notes: String,
    /// Objectives for this phase.
    pub objectives: Vec<String>,
    /// Outcomes achieved.
    pub outcomes: Vec<String>,
}

impl PdcaPhase {
    /// Creates a new PDCA phase.
    pub fn new(state: PdcaState, notes: String) -> Self {
        Self {
            state,
            started_at: Utc::now(),
            completed_at: None,
            notes,
            objectives: Vec::new(),
            outcomes: Vec::new(),
        }
    }

    /// Marks the phase as completed.
    pub fn complete(&mut self) {
        self.completed_at = Some(Utc::now());
    }

    /// Checks if the phase is completed.
    pub fn is_completed(&self) -> bool {
        self.completed_at.is_some()
    }

    /// Adds an objective to the phase.
    pub fn add_objective(&mut self, objective: String) {
        self.objectives.push(objective);
    }

    /// Adds an outcome to the phase.
    pub fn add_outcome(&mut self, outcome: String) {
        self.outcomes.push(outcome);
    }

    /// Calculates the duration of the phase.
    pub fn duration(&self) -> Option<chrono::Duration> {
        self.completed_at
            .map(|end| end.signed_duration_since(self.started_at))
    }
}

/// A complete PDCA cycle for an improvement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PdcaCycle {
    /// The improvement being tracked.
    pub improvement: Improvement,
    /// Current state in the cycle.
    pub current_state: PdcaState,
    /// History of phases in this cycle.
    pub phases: Vec<PdcaPhase>,
    /// Cycle number (for iterations).
    pub cycle_number: u32,
    /// When the cycle started.
    pub started_at: DateTime<Utc>,
    /// When the cycle completed.
    pub completed_at: Option<DateTime<Utc>>,
}

impl PdcaCycle {
    /// Creates a new PDCA cycle for an improvement.
    pub fn new(improvement: Improvement, cycle_number: u32) -> Self {
        Self {
            improvement,
            current_state: PdcaState::Plan,
            phases: Vec::new(),
            cycle_number,
            started_at: Utc::now(),
            completed_at: None,
        }
    }

    /// Starts a new phase in the cycle.
    pub fn start_phase(&mut self, state: PdcaState, notes: String) -> Result<()> {
        if !self.current_state.can_transition_to(&state) {
            return Err(KaizenError::InvalidTransition(format!(
                "Cannot transition from {:?} to {:?}",
                self.current_state, state
            )));
        }

        // Complete current phase if it exists
        if let Some(current_phase) = self.phases.last_mut() {
            if !current_phase.is_completed() {
                current_phase.complete();
            }
        }

        let phase = PdcaPhase::new(state, notes);
        self.phases.push(phase);
        self.current_state = state;

        Ok(())
    }

    /// Gets the current phase.
    pub fn current_phase(&self) -> Option<&PdcaPhase> {
        self.phases.last()
    }

    /// Gets a mutable reference to the current phase.
    pub fn current_phase_mut(&mut self) -> Option<&mut PdcaPhase> {
        self.phases.last_mut()
    }

    /// Advances to the next state in the cycle.
    pub fn advance(&mut self, notes: String) -> Result<()> {
        let next_state = self
            .current_state
            .next()
            .ok_or_else(|| KaizenError::InvalidTransition("Cycle already completed".to_string()))?;

        self.start_phase(next_state, notes)
    }

    /// Completes the current cycle.
    pub fn complete(&mut self) -> Result<()> {
        if self.current_state != PdcaState::Act {
            return Err(KaizenError::InvalidTransition(
                "Can only complete from Act state".to_string(),
            ));
        }

        if let Some(phase) = self.phases.last_mut() {
            if !phase.is_completed() {
                phase.complete();
            }
        }

        self.current_state = PdcaState::Completed;
        self.completed_at = Some(Utc::now());

        Ok(())
    }

    /// Starts a new iteration cycle (from Act back to Plan).
    pub fn iterate(&mut self, notes: String) -> Result<()> {
        if self.current_state != PdcaState::Act {
            return Err(KaizenError::InvalidTransition(
                "Can only iterate from Act state".to_string(),
            ));
        }

        self.start_phase(PdcaState::Plan, notes)
    }

    /// Checks if the cycle is completed.
    pub fn is_completed(&self) -> bool {
        self.completed_at.is_some()
    }

    /// Calculates the total cycle duration.
    pub fn total_duration(&self) -> Option<chrono::Duration> {
        self.completed_at
            .map(|end| end.signed_duration_since(self.started_at))
    }

    /// Gets the duration for a specific state.
    pub fn state_duration(&self, state: PdcaState) -> chrono::Duration {
        self.phases
            .iter()
            .filter(|p| p.state == state)
            .filter_map(|p| p.duration())
            .fold(chrono::Duration::zero(), |acc, d| acc + d)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Category, Priority};

    fn create_test_improvement() -> Improvement {
        Improvement::new(
            "IMP-001".to_string(),
            "Test Improvement".to_string(),
            "Test Description".to_string(),
            Category::Performance,
            Priority::High,
            "owner@example.com".to_string(),
        )
    }

    #[test]
    fn test_pdca_state_transitions() {
        // Arrange & Act & Assert
        assert_eq!(PdcaState::Plan.next(), Some(PdcaState::Do));
        assert_eq!(PdcaState::Do.next(), Some(PdcaState::Check));
        assert_eq!(PdcaState::Check.next(), Some(PdcaState::Act));
        assert_eq!(PdcaState::Act.next(), Some(PdcaState::Completed));
        assert_eq!(PdcaState::Completed.next(), None);
    }

    #[test]
    fn test_can_transition() {
        // Arrange & Act & Assert
        assert!(PdcaState::Plan.can_transition_to(&PdcaState::Do));
        assert!(PdcaState::Do.can_transition_to(&PdcaState::Check));
        assert!(PdcaState::Check.can_transition_to(&PdcaState::Act));
        assert!(PdcaState::Act.can_transition_to(&PdcaState::Completed));
        assert!(PdcaState::Act.can_transition_to(&PdcaState::Plan));

        // Invalid transitions
        assert!(!PdcaState::Plan.can_transition_to(&PdcaState::Check));
        assert!(!PdcaState::Completed.can_transition_to(&PdcaState::Plan));
    }

    #[test]
    fn test_phase_creation() {
        // Arrange & Act
        let phase = PdcaPhase::new(PdcaState::Plan, "Planning phase".to_string());

        // Assert
        assert_eq!(phase.state, PdcaState::Plan);
        assert_eq!(phase.notes, "Planning phase");
        assert!(!phase.is_completed());
        assert!(phase.objectives.is_empty());
    }

    #[test]
    fn test_phase_completion() {
        // Arrange
        let mut phase = PdcaPhase::new(PdcaState::Plan, "Planning".to_string());

        // Act
        phase.complete();

        // Assert
        assert!(phase.is_completed());
        assert!(phase.completed_at.is_some());
        assert!(phase.duration().is_some());
    }

    #[test]
    fn test_phase_objectives_and_outcomes() {
        // Arrange
        let mut phase = PdcaPhase::new(PdcaState::Plan, "Planning".to_string());

        // Act
        phase.add_objective("Reduce build time".to_string());
        phase.add_objective("Improve test coverage".to_string());
        phase.add_outcome("Built optimization plan".to_string());

        // Assert
        assert_eq!(phase.objectives.len(), 2);
        assert_eq!(phase.outcomes.len(), 1);
    }

    #[test]
    fn test_cycle_creation() {
        // Arrange
        let improvement = create_test_improvement();

        // Act
        let cycle = PdcaCycle::new(improvement, 1);

        // Assert
        assert_eq!(cycle.cycle_number, 1);
        assert_eq!(cycle.current_state, PdcaState::Plan);
        assert!(cycle.phases.is_empty());
        assert!(!cycle.is_completed());
    }

    #[test]
    fn test_cycle_phase_progression() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act & Assert - Plan to Do
        assert!(cycle
            .start_phase(PdcaState::Do, "Starting Do phase".to_string())
            .is_ok());
        assert_eq!(cycle.current_state, PdcaState::Do);
        assert_eq!(cycle.phases.len(), 1);

        // Act & Assert - Do to Check
        assert!(cycle
            .start_phase(PdcaState::Check, "Starting Check phase".to_string())
            .is_ok());
        assert_eq!(cycle.current_state, PdcaState::Check);
        assert_eq!(cycle.phases.len(), 2);
    }

    #[test]
    fn test_cycle_invalid_transition() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act
        let result = cycle.start_phase(PdcaState::Act, "Invalid jump".to_string());

        // Assert
        assert!(result.is_err());
        assert_eq!(cycle.current_state, PdcaState::Plan);
    }

    #[test]
    fn test_cycle_advance() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act
        assert!(cycle.advance("Moving to Do".to_string()).is_ok());
        assert!(cycle.advance("Moving to Check".to_string()).is_ok());
        assert!(cycle.advance("Moving to Act".to_string()).is_ok());

        // Assert
        assert_eq!(cycle.current_state, PdcaState::Act);
        assert_eq!(cycle.phases.len(), 3);
    }

    #[test]
    fn test_cycle_completion() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act - Advance to Act state
        cycle.advance("Do".to_string()).ok();
        cycle.advance("Check".to_string()).ok();
        cycle.advance("Act".to_string()).ok();

        // Complete the cycle
        let result = cycle.complete();

        // Assert
        assert!(result.is_ok());
        assert!(cycle.is_completed());
        assert_eq!(cycle.current_state, PdcaState::Completed);
        assert!(cycle.completed_at.is_some());
    }

    #[test]
    fn test_cycle_iteration() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act - Advance to Act state
        cycle.advance("Do".to_string()).ok();
        cycle.advance("Check".to_string()).ok();
        cycle.advance("Act".to_string()).ok();

        // Iterate
        let result = cycle.iterate("Starting new iteration".to_string());

        // Assert
        assert!(result.is_ok());
        assert_eq!(cycle.current_state, PdcaState::Plan);
        assert_eq!(cycle.phases.len(), 4); // Plan, Do, Check, Act from first + new Plan
    }

    #[test]
    fn test_current_phase() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act
        cycle.start_phase(PdcaState::Do, "Test phase".to_string()).ok();

        // Assert
        let current = cycle.current_phase();
        assert!(current.is_some());
        assert_eq!(current.map(|p| &p.notes), Some(&"Test phase".to_string()));
    }

    #[test]
    fn test_state_duration() {
        // Arrange
        let improvement = create_test_improvement();
        let mut cycle = PdcaCycle::new(improvement, 1);

        // Act
        cycle.advance("Do".to_string()).ok();
        if let Some(phase) = cycle.phases.last_mut() {
            phase.complete();
        }

        // Assert
        let duration = cycle.state_duration(PdcaState::Do);
        assert!(duration >= chrono::Duration::zero());
    }
}
