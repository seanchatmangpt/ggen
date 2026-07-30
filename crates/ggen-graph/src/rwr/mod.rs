//! Ross-Weill-Robertson Level-5 foundation-for-execution machinery.
//!
//! This module mechanizes the complete maturity contract across operating model,
//! architecture core diagram, digitized platform, engagement model, value
//! realization, machinery, automation, autonomics, receipt, and replay.

pub mod automatic;
pub mod autonomic;
pub mod evidence;
pub mod execution;
pub mod matrix;
pub mod reference;

pub use automatic::{
    read_committed_payload, AdmittedTrigger, AndonLevel, AndonSignal,
    AutomaticOperationReceipt, AutomaticReplayVerifier, AutomaticRuntime,
    AutonomicOperationsController, AutonomicOperationsReceipt, AutonomicResource, CircuitBreaker,
    ConsequenceObserver, ErrorBudget, FilesystemConsequenceObserver, KnowledgeHook,
    ManufacturedIntent, ObservationError, OperationsCapability, OperationsError, OperationsGovernor,
    OperationsProofSurface, PostconditionFailure, RetryPolicy, Route, Trigger,
    TriggerAdmissionPolicy, TriggerRouter, ALL_OPERATIONS_CAPABILITIES, OPERATIONS_VERSION,
    REQUIRED_OPERATIONS_PROOF_SURFACES,
};
pub use autonomic::{AutonomicController, AutonomicCycleReceipt, AutonomicError, ManagedCell};
pub use evidence::{
    AssessmentReceipt, DimensionAssessment, EvidenceError, EvidenceLedger, EvidenceOutcome,
    EvidenceRecord, GallState, MaturityAssessment,
};
pub use execution::{
    Action, ActuationReceipt, ExecutionError, ExecutionGrant, ExecutionPolicy, FilesystemActuator,
    FoundationMachine, ReplayVerifier,
};
pub use matrix::{
    all_contracts, contract, Dimension, DimensionContract, EvidenceSurface, MaturityLevel,
    RwrDomain, ALL_DIMENSIONS, MATRIX_VERSION,
};
pub use reference::{ReferenceError, ReferenceFoundation, ReferenceProof};
