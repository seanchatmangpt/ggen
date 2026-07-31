//! Evidence-backed certification law for canonical ggen Building Blocks.
//!
//! Every award is bound to the executable `TAI-REBUILD-001` case study and to
//! the admitted seven-day ggen standards profile. The pure kernel assesses
//! evidence and deterministic reconstruction receipts; the GitHub verifier
//! executes the ontology-authored TAI manufacturing pack.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::building_block::{
    BuildingBlockId, BuildingBlockRefusal, CompositionReceipt, EvidenceReceipt, LifecycleState,
    ProfileId, Standing,
};

mod case_study;
mod program;
mod roadmap;
mod standards;
mod testing;

pub use case_study::{
    tai_case_study, TaiCaseStudy, TaiCaseStudyRefusal, TAI_CASE_STUDY_BROKER,
    TAI_CASE_STUDY_EXAMPLE, TAI_CASE_STUDY_ID, TAI_CASE_STUDY_PACK, TAI_CASE_STUDY_VERSION,
};
pub use program::CertificationProgram;
pub use roadmap::{generate_rebuild_roadmap, simulate_tai_rebuild};
pub use standards::{
    seven_day_standards_profile, StandardAuthority, StandardCheckpoint, StandardStatus,
    StandardsProfile, StandardsProfileRefusal, GGEN_SEVEN_DAY_STANDARDS_ID,
    GGEN_SEVEN_DAY_STANDARDS_VERSION,
};
pub use testing::{
    testing_bblock_protocol, TestingBblockProtocol, TestingBblockRefusal, TestingBblockStanding,
    TestingSuite, TestingSuiteKind, TestingSuiteStatus, TESTING_BBLOCK_PROTOCOL_ID,
    TESTING_BBLOCK_PROTOCOL_VERSION,
};

pub const CERTIFICATION_AWARD_SCHEMA: &str = "ggen.building-block.certification-award.v2";
pub const REBUILD_ROADMAP_SCHEMA: &str = "ggen.building-block.rebuild-roadmap.v2";
pub const TAI_REBUILD_RECEIPT_SCHEMA: &str = "ggen.building-block.tai-rebuild-receipt.v2";
pub const GBB_CERTIFICATION_PROGRAM_ID: &str = "ggen-bblocks-certification-tai-v26.7.31";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CertificationLevel {
    Gbb100Foundation,
    Gbb200Builder,
    Gbb300Composer,
    Gbb400Governor,
    Gbb500TaiManufacturer,
}

impl CertificationLevel {
    #[must_use]
    pub const fn code(self) -> &'static str {
        match self {
            Self::Gbb100Foundation => "GBB-100",
            Self::Gbb200Builder => "GBB-200",
            Self::Gbb300Composer => "GBB-300",
            Self::Gbb400Governor => "GBB-400",
            Self::Gbb500TaiManufacturer => "GBB-500",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CertificationRequirementKind {
    Fence,
    Diagnose,
    Construct,
    EvidenceClosure,
    Compose,
    RefuseInvalidComposition,
    Substitute,
    Govern,
    GenerateRoadmap,
    RebuildTai,
    RecoverFailure,
    TeachTransfer,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationRequirement {
    pub id: String,
    pub level: CertificationLevel,
    pub kind: CertificationRequirementKind,
    pub outcome: String,
    pub falsifier: String,
    pub artifact_schema: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RequirementReceipt {
    pub requirement_id: String,
    pub candidate_id: String,
    pub case_study_id: String,
    pub standards_profile_id: String,
    pub standards_profile_digest: String,
    pub positive_witness_digest: String,
    pub negative_falsifier_digest: String,
    pub independent_verifier_digest: String,
    pub receipt_verifier_digest: String,
    pub replay_digest: String,
    pub artifact_digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationAward {
    pub schema: String,
    pub program_id: String,
    pub program_version: String,
    pub case_study_id: String,
    pub standards_profile_id: String,
    pub standards_profile_digest: String,
    pub candidate_id: String,
    pub level: CertificationLevel,
    pub requirement_ids: Vec<String>,
    pub receipt_digests: BTreeMap<String, String>,
    pub digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TargetArchitectureInstance {
    pub id: String,
    pub case_study_id: String,
    pub standards_profile_id: String,
    pub standards_profile_digest: String,
    pub roots: BTreeSet<BuildingBlockId>,
    #[serde(default)]
    pub required_profiles: BTreeSet<ProfileId>,
    #[serde(default)]
    pub expected_composition_digest: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RoadmapAction {
    Identify,
    Qualify,
    Admit,
    Activate,
    Reactivate,
    SelectRealization,
    CloseEvidence,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoadmapStep {
    pub sequence: u32,
    pub block_id: BuildingBlockId,
    pub action: RoadmapAction,
    pub rationale: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RebuildRoadmap {
    pub schema: String,
    pub target_id: String,
    pub case_study_id: String,
    pub standards_profile_id: String,
    pub standards_profile_digest: String,
    pub composition: CompositionReceipt,
    pub steps: Vec<RoadmapStep>,
    pub digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaiRebuildReceipt {
    pub schema: String,
    pub candidate_id: String,
    pub target_id: String,
    pub case_study_id: String,
    pub standards_profile_id: String,
    pub standards_profile_digest: String,
    pub composition_digest: String,
    pub order: Vec<BuildingBlockId>,
    pub profiles: BTreeSet<ProfileId>,
    pub evidence_digests: BTreeMap<BuildingBlockId, BTreeSet<String>>,
    pub standing: Standing,
    pub digest: String,
}

#[derive(Debug, Error)]
pub enum CertificationRefusal {
    #[error("certification candidate identity is required")]
    CandidateMissing,
    #[error("TAI case-study mismatch: expected `{expected}`, observed `{observed}`")]
    CaseStudyMismatch { expected: String, observed: String },
    #[error("standards profile mismatch: expected `{expected}`, observed `{observed}`")]
    StandardsProfileMismatch { expected: String, observed: String },
    #[error("standards profile digest mismatch: expected `{expected}`, observed `{observed}`")]
    StandardsProfileDigestMismatch { expected: String, observed: String },
    #[error("certification requirement `{0}` has no receipt")]
    RequirementReceiptMissing(String),
    #[error("receipt candidate mismatch: expected `{expected}`, observed `{observed}`")]
    ReceiptCandidateMismatch { expected: String, observed: String },
    #[error("receipt requirement mismatch: expected `{expected}`, observed `{observed}`")]
    ReceiptRequirementMismatch { expected: String, observed: String },
    #[error("requirement `{requirement}` is missing `{surface}` evidence")]
    ReceiptSurfaceMissing {
        requirement: String,
        surface: String,
    },
    #[error("target architecture instance identity is required")]
    TargetIdentityMissing,
    #[error("target architecture instance requires at least one root block")]
    TargetRootsMissing,
    #[error("composition digest mismatch: expected `{expected}`, observed `{observed}`")]
    CompositionDigestMismatch { expected: String, observed: String },
    #[error("required profile `{0:?}` is absent from the target composition")]
    RequiredProfileMissing(ProfileId),
    #[error("composed block `{0:?}` is absent from the registry")]
    ComposedBlockMissing(BuildingBlockId),
    #[error("target contains block `{block:?}` in terminal lifecycle `{lifecycle:?}`")]
    TargetContainsRetiredBlock {
        block: BuildingBlockId,
        lifecycle: LifecycleState,
    },
    #[error("target rebuild roadmap has {0} unresolved step(s)")]
    RoadmapIncomplete(usize),
    #[error(transparent)]
    StandardsProfile(#[from] StandardsProfileRefusal),
    #[error(transparent)]
    BuildingBlock(#[from] BuildingBlockRefusal),
    #[error("certification receipt serialization failed: {0}")]
    Serialization(#[from] serde_json::Error),
}

pub(crate) fn digest<T: Serialize>(value: &T) -> Result<String, CertificationRefusal> {
    let bytes = serde_json::to_vec(value)?;
    Ok(format!("urn:blake3:{}", blake3::hash(&bytes).to_hex()))
}

pub type EvidenceLedger = BTreeMap<BuildingBlockId, BTreeSet<EvidenceReceipt>>;

#[cfg(test)]
mod tests;
