//! TOGAF State Management for FIBO-TOGAF A2A Protocol
//!
//! This module provides the state management infrastructure for the 70-turn
//! FIBO-TOGAF architecture development protocol. It includes:
//!
//! - [`TogafStateManager`] - Turn progression and phase tracking
//! - [`HandoffProtocol`] - Phase-to-phase handoff validation
//! - [`ArbApprovalManager`] - ARB gate approval workflow
//! - [`ArtifactRegistry`] - Artifact storage and retrieval
//!
//! The protocol follows the TOGAF Architecture Development Method (ADM)
//! with six phases (A-F) spanning 70 turns, with ARB gates at each
//! phase transition point.

pub mod arb_gates;
pub mod artifacts;
pub mod error;
pub mod handoff;
pub mod togaf_state;

// Re-export primary types
pub use arb_gates::{
    ArbApproval, ArbApprovalManager, ArbGate, ArbGateSummary, ApprovalCriterion,
    ApprovalDecision, ApprovalResponse, ApprovalStatus, StakeholderRole,
};
pub use artifacts::{Artifact, ArtifactRegistry, ArtifactType};
pub use error::StateError;
pub use handoff::{
    ArbApprovalValidator, ArtifactCompletenessValidator, FiboConsistencyValidator,
    FiboValidationResult, HandoffPackage, HandoffProtocol, HandoffResult, HandoffStatus,
    HandoffValidator, ValidationResult,
};
pub use togaf_state::{PhaseState, PhaseStatus, StateSummary, TogafPhase, TogafStateManager, TurnRecord};
