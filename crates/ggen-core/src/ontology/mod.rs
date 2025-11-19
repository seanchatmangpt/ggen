pub mod constitution;
pub mod control_loop;
pub mod delta_proposer;
#[cfg(test)]
pub mod e2e_example;
pub mod pattern_miner;
pub mod promotion;
/// Autonomous Ontology Management System Module
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

/// Semantic Ontology Extraction System
///
/// This module provides RDF/OWL ontology extraction and transformation into
/// strongly-typed Rust schema representations for code generation:
/// - SPARQL-based class and property extraction
/// - Type mapping (TypeScript, GraphQL, SQL, React)
/// - Relationship derivation and cardinality constraints
/// - Snapshot testing for schema extraction
pub mod error;
pub mod extractor;
pub mod schema;

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

pub use error::{OntologyError, OntologyResult};
pub use extractor::OntologyExtractor;
pub use schema::*;
