//! Executable enterprise architecture machinery for the ggen ecosystem.
//!
//! This package is now a CLI and compatibility facade. Generic ggen Building
//! Block law, enterprise graph calculus, and the Fortune 5 runtime kernel are
//! owned by the canonical `ggen-architecture` dependency. The facade deliberately
//! stops at deterministic construction and bounded intent generation and performs
//! no direct actuation.

pub mod autonomic;
pub mod capacity;
pub mod doctor;
pub mod error;
pub mod fortune5;
// The Level-5 crown module is a compatibility facade over an externally
// documented 21/99/63 contract. Its public data carriers intentionally retain
// field-shaped wire names, and its local fixtures use expectation messages.
// Keep those two debts fenced here rather than weakening crate-wide lint law.
#[allow(missing_docs, clippy::expect_used)]
pub mod level5_crown;
pub mod model;
pub mod receipt;
pub mod registry;
pub mod self_play;
pub mod spg;
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
    profiles, public_vocabulary, ArchitectureFacet, ArchitectureLayer, ArchitectureView,
    ArchitectureViewpoint, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, BuildingBlockViolation, CompositionReceipt, ElementId,
    EnterpriseArchitectureError, EnterpriseArchitectureModel, EnterpriseArchitectureReceipt,
    EnterpriseArchitectureViolation, EnterpriseElement, EnterpriseElementKind,
    EnterpriseImpactReport, EnterpriseRelation, EnterpriseRelationKind, EnterpriseTransition,
    EnterpriseWorkPackage, EvidenceKind, EvidenceObligation, EvidenceReceipt, GovernanceAssessment,
    GovernanceFinding, GovernanceSeverity, ObligationId, Port, PortDirection, PortId, PortKind,
    ProfileId, RealizationBinding, RealizationId, RelationId, ResourceCeiling, ResourceClaim,
    Standing as BuildingBlockStanding, SubstitutionAssessment, TraceHop, TransitionId,
    WorkPackageId, ENTERPRISE_ARCHITECTURE_RECEIPT_SCHEMA,
};
pub use level5_crown::{
    CrownEvidence, CrownFinding, LevelFiveCrownAssessment, LevelFiveCrownProgram,
    OperationalGuards, ReleaseTruth, SlaGovernor, TaxonomyProfileClosure,
};
pub use model::{ArchitectureAsset, AssetKind, LifecycleState, Severity, Standing, TransitionStep};
pub use registry::{ArchitectureRegistry, ImpactReport, RegistryViolation};
pub use self_play::{
    demo_scenario, run_scenario, run_suite, verify_report, verify_suite, ActionSpec, ActorPolicy,
    ActorRole, Comparison, GameState, Metric, MetricConstraint, MetricEffect, MoveReceipt,
    SelfPlayDoctorFinding, SelfPlayDoctorReport, SelfPlayDoctorStatus, SelfPlayReport,
    SelfPlayScenario, SelfPlayStanding, SelfPlayViolation, UseCaseKind,
};
pub use spg::{
    apply_rewrite as apply_spg_rewrite, canonical_graph_bytes as spg_canonical_graph_bytes,
    compile_projection, from_json as spg_from_json, graph_digest as spg_graph_digest,
    plan_rewrite as plan_spg_rewrite, replay_rewrite as replay_spg_rewrite,
    rewrite_plan_digest as spg_rewrite_plan_digest, semantic_diff as spg_semantic_diff,
    validate as validate_spg, ProjectionEnvelope, SpgDiff, SpgEdge, SpgError, SpgExactSubject,
    SpgGraph, SpgNode, SpgReplayReceipt, SpgRewriteOperation, SpgRewritePlan,
};
pub use state::{ArchitectureState, AutonomicPolicy};
