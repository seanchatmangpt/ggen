#![forbid(unsafe_code)]
//! Canonical, deterministic, IO-free architecture kernel for ggen.
//!
//! The generic ggen Building Block calculus is the semantic authority. The
//! existing Fortune 5 runtime law is preserved as a profile module. This crate
//! may manufacture bounded intents, but it cannot actuate external systems.

pub mod building_block;
pub mod certification;
pub mod self_play;

#[path = "lib.rs"]
pub mod fortune5_kernel;

/// Architecture profiles layered over the generic building-block calculus.
pub mod profiles {
    /// Fortune 5 operational controls, policies, intents, and receipts.
    pub use crate::fortune5_kernel as fortune5;
}

pub use building_block::{
    ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, BuildingBlockViolation, CompositionReceipt, EvidenceKind,
    EvidenceObligation, EvidenceReceipt, LifecycleState, ObligationId, Port, PortDirection, PortId,
    PortKind, ProfileId, RealizationBinding, RealizationId, ResourceCeiling, ResourceClaim,
    Standing, SubstitutionAssessment,
};
pub use certification::{
    generate_rebuild_roadmap, simulate_tai_rebuild, tai_case_study, CertificationAward,
    CertificationLevel, CertificationProgram, CertificationRefusal, CertificationRequirement,
    CertificationRequirementKind, EvidenceLedger, RebuildRoadmap, RequirementReceipt,
    RoadmapAction, RoadmapStep, TaiCaseStudy, TaiCaseStudyRefusal, TaiRebuildReceipt,
    TargetArchitectureInstance, CERTIFICATION_AWARD_SCHEMA, GBB_CERTIFICATION_PROGRAM_ID,
    REBUILD_ROADMAP_SCHEMA, TAI_CASE_STUDY_BROKER, TAI_CASE_STUDY_EXAMPLE, TAI_CASE_STUDY_ID,
    TAI_CASE_STUDY_PACK, TAI_CASE_STUDY_VERSION, TAI_REBUILD_RECEIPT_SCHEMA,
};
pub use self_play::{
    run_scenario, run_suite, verify_report, verify_suite, ActionSpec, ActorPolicy, ActorRole,
    Comparison, GameState, ManifestAction, ManifestActionAuthority, ManifestActionEvidence,
    ManifestEffect, ManifestGuard, ManifestMetricValue, ManifestPolicy, ManifestPolicyAuthority,
    ManifestPolicyWeight, ManifestScenario, ManifestScenarioConstraint, ManifestScenarioUseCase,
    Metric, MetricConstraint, MetricEffect, MoveReceipt, SelfPlayManifest, SelfPlayReport,
    SelfPlayScenario, SelfPlayStanding, SelfPlayViolation, UseCaseKind, SELF_PLAY_RECEIPT_SCHEMA,
    SELF_PLAY_REPORT_SCHEMA, SELF_PLAY_REQUIRED_BROKER,
};

/// Receipt schema retained by the Fortune 5 profile.
pub const RECEIPT_SCHEMA: &str = fortune5_kernel::RECEIPT_SCHEMA;
/// Intent schema retained by the Fortune 5 profile.
pub const INTENT_SCHEMA: &str = fortune5_kernel::INTENT_SCHEMA;
/// The only lawful downstream actuator address.
pub const REQUIRED_BROKER: &str = fortune5_kernel::REQUIRED_BROKER;
