//! Executable enterprise architecture machinery for the ggen ecosystem.
//!
//! This package is now a CLI and compatibility facade. Generic ggen Building
//! Block law and the Fortune 5 runtime kernel are owned by the canonical
//! `ggen-architecture` dependency. The facade deliberately stops at bounded
//! intent generation and performs no direct actuation.

pub mod autonomic;
pub mod capacity;
pub mod doctor;
pub mod error;
pub mod fortune5;
pub mod level5_crown;
pub mod model;
pub mod receipt;
pub mod registry;
pub mod state;

pub use autonomic::{
    ArchitectureIntent, AutonomicController, AutonomicCycle, Diagnosis, IntentKind, Stimulus,
};
pub use capacity::{
    CapacityEnvelope, CapacityFinding, CapacityLevel, CapacityPolicy, CapacitySample,
    WorkloadVector,
};
pub use doctor::{DoctorFinding, DoctorReport, DoctorStatus};
pub use error::{ArchitectureError, Result};
pub use fortune5::{
    ControlEvidence, DimensionAssessment, Fortune5Assessment, Fortune5AutonomicPlan,
    Fortune5Catalog, Fortune5Dimension, Fortune5Domain, Fortune5Finding, Fortune5Intent,
    Fortune5IntentKind, Fortune5Policy, Fortune5Program, ProofKind, ProofObligation,
};
pub use ggen_architecture_kernel::{
    profiles, ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, BuildingBlockViolation, CompositionReceipt, EvidenceKind,
    EvidenceObligation, EvidenceReceipt, ObligationId, Port, PortDirection, PortId, PortKind,
    ProfileId, RealizationBinding, RealizationId, ResourceCeiling, ResourceClaim,
    Standing as BuildingBlockStanding, SubstitutionAssessment,
};
pub use level5_crown::{
    CrownEvidence, CrownFinding, LevelFiveCrownAssessment, LevelFiveCrownProgram,
    OperationalGuards, ReleaseTruth, SlaGovernor, TaxonomyProfileClosure,
};
pub use model::{ArchitectureAsset, AssetKind, LifecycleState, Severity, Standing, TransitionStep};
pub use registry::{ArchitectureRegistry, ImpactReport, RegistryViolation};
pub use state::{ArchitectureState, AutonomicPolicy};
