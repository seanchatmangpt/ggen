//! Evidence-backed certification law for ggen Building Blocks.
//!
//! Certification is cumulative and execution-backed. The final credential
//! requires a deterministic roadmap generated from Building Block state and a
//! replay-stable simulation of rebuilding a Target Architecture Instance.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::building_block::{
    BuildingBlockId, BuildingBlockRefusal, CompositionReceipt, EvidenceReceipt, LifecycleState,
    ProfileId, Standing,
};

mod program;
mod roadmap;

pub use program::CertificationProgram;
pub use roadmap::{generate_rebuild_roadmap, simulate_tai_rebuild};

/// Canonical schema for cumulative certification awards.
pub const CERTIFICATION_AWARD_SCHEMA: &str = "ggen.building-block.certification-award.v1";
/// Canonical schema for GBB-generated rebuild roadmaps.
pub const REBUILD_ROADMAP_SCHEMA: &str = "ggen.building-block.rebuild-roadmap.v1";
/// Canonical schema for simulated TAI rebuild receipts.
pub const TAI_REBUILD_RECEIPT_SCHEMA: &str = "ggen.building-block.tai-rebuild-receipt.v1";
/// Stable identity of the first ggen Building Blocks certification program.
pub const GBB_CERTIFICATION_PROGRAM_ID: &str = "ggen-bblocks-certification-v26.7.30";

/// Cumulative certification level.
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
    /// Public credential code.
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

/// Executable requirement kind.
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

/// One cumulative requirement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationRequirement {
    pub id: String,
    pub level: CertificationLevel,
    pub kind: CertificationRequirementKind,
    pub outcome: String,
    pub falsifier: String,
    pub artifact_schema: String,
}

/// Complete receipt for one certification requirement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RequirementReceipt {
    pub requirement_id: String,
    pub candidate_id: String,
    pub positive_witness_digest: String,
    pub negative_falsifier_digest: String,
    pub independent_verifier_digest: String,
    pub receipt_verifier_digest: String,
    pub replay_digest: String,
    pub artifact_digest: String,
}

/// Deterministic cumulative certification award.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationAward {
    pub schema: String,
    pub program_id: String,
    pub program_version: String,
    pub candidate_id: String,
    pub level: CertificationLevel,
    pub requirement_ids: Vec<String>,
    pub receipt_digests: BTreeMap<String, String>,
    pub digest: String,
}

/// Target Architecture Instance reconstructed from registered GBB roots.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TargetArchitectureInstance {
    pub id: String,
    pub roots: BTreeSet<BuildingBlockId>,
    #[serde(default)]
    pub required_profiles: BTreeSet<ProfileId>,
    #[serde(default)]
    pub expected_composition_digest: Option<String>,
}

/// Work action generated from current GBB state.
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

/// One deterministic roadmap step.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoadmapStep {
    pub sequence: u32,
    pub block_id: BuildingBlockId,
    pub action: RoadmapAction,
    pub rationale: String,
}

/// Roadmap generated directly from GBB and evidence state.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RebuildRoadmap {
    pub schema: String,
    pub target_id: String,
    pub composition: CompositionReceipt,
    pub steps: Vec<RoadmapStep>,
    pub digest: String,
}

/// Receipt proving one complete bounded TAI rebuild simulation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaiRebuildReceipt {
    pub schema: String,
    pub candidate_id: String,
    pub target_id: String,
    pub composition_digest: String,
    pub order: Vec<BuildingBlockId>,
    pub profiles: BTreeSet<ProfileId>,
    pub evidence_digests: BTreeMap<BuildingBlockId, BTreeSet<String>>,
    pub standing: Standing,
    pub digest: String,
}

/// Typed fail-closed certification and TAI-rebuild refusals.
#[derive(Debug, Error)]
pub enum CertificationRefusal {
    #[error("certification candidate identity is required")]
    CandidateMissing,
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
