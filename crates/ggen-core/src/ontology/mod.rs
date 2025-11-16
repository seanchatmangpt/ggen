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
pub mod pattern_miner;
pub mod delta_proposer;
pub mod validators;
pub mod promotion;
pub mod control_loop;
pub mod constitution;
#[cfg(test)]
pub mod e2e_example;

pub use sigma_runtime::{
    SigmaSnapshotId, SigmaSnapshot, SnapshotMetadata,
    SigmaOverlay, SigmaReceipt, SigmaRuntime,
    ValidationResult, TestResult, PerformanceMetrics,
};

pub use pattern_miner::{
    Pattern, PatternType, PatternMiner, MinerConfig,
    Observation, ObservationSource, ProposedChange,
    OntologyStats,
};

pub use delta_proposer::{
    DeltaSigmaProposal, DeltaSigmaProposer, ProposerConfig,
    MockLLMProposer, RealLLMProposer,
};

pub use validators::{
    ValidationEvidence, Invariant, ValidationContext,
    StaticValidator, DynamicValidator, PerformanceValidator,
    CompositeValidator, ValidatorResult,
    MockStaticValidator, MockDynamicValidator, MockPerformanceValidator,
};

pub use promotion::{
    AtomicSnapshotPromoter, SnapshotGuard, PromotionResult, PromotionMetrics,
};

pub use control_loop::{
    AutonomousControlLoop, ControlLoopConfig, LoopState, IterationTelemetry,
};

pub use constitution::{
    InvariantCheck, InvariantResult, Constitution, ConstitutionValidation,
    NoRetrocausationCheck, TypeSoundnessCheck, GuardSoundnessCheck,
    ProjectionDeterminismCheck, SLOPreservationCheck, ImmutabilityCheck,
    AtomicPromotionCheck,
};
