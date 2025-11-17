pub mod constitution;
pub mod control_loop;
pub mod delta_proposer;
#[cfg(test)]
pub mod e2e_example;
pub mod pattern_miner;
pub mod promotion;
/// Ontology System Module
///
/// This module provides the autonomous ontology management system (Σ) including:
/// - Meta-ontology definitions (Σ²)
/// - Snapshot versioning and management
/// - Change proposals and validation (ΔΣ)
/// - Pattern mining and drift detection
/// - Closed-loop evolution control
/// - Lock-free atomic promotion
pub mod sigma_runtime;
pub mod validators;

pub use sigma_runtime::{
    PerformanceMetrics,
    SigmaOverlay,
    SigmaReceipt,
    SigmaRuntime,
    SigmaSnapshot,
    SigmaSnapshotId,
    SnapshotMetadata,
    // ValidationResult excluded to avoid conflict with rdf::ValidationResult
    // Use ontology::sigma_runtime::ValidationResult for ontology-specific validation
    TestResult,
};

pub use pattern_miner::{
    MinerConfig, Observation, ObservationSource, OntologyStats, Pattern, PatternMiner, PatternType,
    ProposedChange,
};

pub use delta_proposer::{
    DeltaSigmaProposal, DeltaSigmaProposer, MockLLMProposer, ProposerConfig, RealLLMProposer,
};

pub use validators::{
    CompositeValidator, DynamicValidator, Invariant, MockDynamicValidator,
    MockPerformanceValidator, MockStaticValidator, PerformanceValidator, StaticValidator,
    ValidationContext, ValidationEvidence, ValidatorResult,
};

pub use promotion::{AtomicSnapshotPromoter, PromotionMetrics, PromotionResult, SnapshotGuard};

pub use control_loop::{AutonomousControlLoop, ControlLoopConfig, IterationTelemetry, LoopState};

pub use constitution::{
    AtomicPromotionCheck, Constitution, ConstitutionValidation, GuardSoundnessCheck,
    ImmutabilityCheck, InvariantCheck, InvariantResult, NoRetrocausationCheck,
    ProjectionDeterminismCheck, SLOPreservationCheck, TypeSoundnessCheck,
};
