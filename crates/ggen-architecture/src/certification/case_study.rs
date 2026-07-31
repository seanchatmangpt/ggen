use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Stable programme identity authored by the TAI enterprise-rebuild pack.
pub const TAI_CASE_STUDY_ID: &str = "TAI-REBUILD-001";
/// Exact case-study version.
pub const TAI_CASE_STUDY_VERSION: &str = "26.7.31";
/// Ontology-authored source pack.
pub const TAI_CASE_STUDY_PACK: &str = "packs/tai-enterprise-rebuild-pack";
/// Executable manufacturing consumer.
pub const TAI_CASE_STUDY_EXAMPLE: &str = "examples/tai-enterprise-rebuild";
/// Only lawful future external actuator address.
pub const TAI_CASE_STUDY_BROKER: &str = "BRCE";

/// Canonical TAI case-study boundary used by every certification level.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaiCaseStudy {
    pub id: String,
    pub version: String,
    pub observed_enterprise: String,
    pub target_enterprise: String,
    pub source_pack: String,
    pub source_example: String,
    pub root_broker: String,
    pub governing_equation: String,
    pub building_block_families: Vec<String>,
    pub reconstruction_phases: Vec<String>,
    pub certification_horizons: Vec<String>,
    pub counterfactual_scenarios: Vec<String>,
    pub recent_capability_rails: u16,
    pub exclusions: Vec<String>,
}

impl TaiCaseStudy {
    /// Validate the cardinality and constitutional boundary shared with the
    /// ontology-authored `TAI-REBUILD-001` pack.
    pub fn validate(&self) -> Result<(), TaiCaseStudyRefusal> {
        if self.id != TAI_CASE_STUDY_ID {
            return Err(TaiCaseStudyRefusal::IdentityMismatch(self.id.clone()));
        }
        if self.version != TAI_CASE_STUDY_VERSION {
            return Err(TaiCaseStudyRefusal::VersionMismatch(self.version.clone()));
        }
        if self.source_pack != TAI_CASE_STUDY_PACK || self.source_example != TAI_CASE_STUDY_EXAMPLE
        {
            return Err(TaiCaseStudyRefusal::SourceBoundaryMismatch);
        }
        if self.root_broker != TAI_CASE_STUDY_BROKER {
            return Err(TaiCaseStudyRefusal::BrokerMismatch(
                self.root_broker.clone(),
            ));
        }
        for (surface, observed, expected) in [
            (
                "building-block families",
                self.building_block_families.len(),
                9,
            ),
            ("reconstruction phases", self.reconstruction_phases.len(), 7),
            (
                "certification horizons",
                self.certification_horizons.len(),
                10,
            ),
            (
                "counterfactual scenarios",
                self.counterfactual_scenarios.len(),
                7,
            ),
            (
                "recent capability rails",
                usize::from(self.recent_capability_rails),
                18,
            ),
        ] {
            if observed != expected {
                return Err(TaiCaseStudyRefusal::CardinalityMismatch {
                    surface: surface.to_string(),
                    expected,
                    observed,
                });
            }
        }
        Ok(())
    }
}

/// Return the admitted certification view of the executable TAI rebuild pack.
#[must_use]
pub fn tai_case_study() -> TaiCaseStudy {
    TaiCaseStudy {
        id: TAI_CASE_STUDY_ID.to_string(),
        version: TAI_CASE_STUDY_VERSION.to_string(),
        observed_enterprise:
            "Technology Applications, Inc. historical systems-of-systems enterprise".to_string(),
        target_enterprise: "Automated Technical Capability Company".to_string(),
        source_pack: TAI_CASE_STUDY_PACK.to_string(),
        source_example: TAI_CASE_STUDY_EXAMPLE.to_string(),
        root_broker: TAI_CASE_STUDY_BROKER.to_string(),
        governing_equation: "O → O* → μ → A → R".to_string(),
        building_block_families: [
            "mission",
            "contract",
            "capability",
            "certification",
            "organization",
            "technology",
            "total-quality-leadership",
            "evidence",
            "economics",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        reconstruction_phases: [
            "TAI-0-founder-and-first-mission",
            "TAI-1-repeatable-engineering-services",
            "TAI-2-adjacent-engineering-expansion",
            "TAI-3-environmental-science-and-technology",
            "TAI-4-information-technology",
            "TAI-5-systems-integration-and-communications",
            "TAI-6-institutional-enterprise",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        certification_horizons: [
            "systems-engineering",
            "lifecycle-logistics",
            "software-and-information-technology",
            "modeling-and-simulation",
            "operations-support",
            "systems-integration",
            "telecommunications-and-network-engineering",
            "self-paced-training",
            "environmental-chemical-and-biological-analysis",
            "advanced-research-instrumentation",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        counterfactual_scenarios: [
            "historical",
            "rebuilt-today",
            "post-agi",
            "delayed-contract",
            "unavailable-certification",
            "failed-inspection",
            "founder-unavailable",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        recent_capability_rails: 18,
        exclusions: [
            "no claim that historical TAI has been recreated",
            "no claim that a contract exists or was awarded",
            "no self-awarded issuer-recognized certification",
            "no direct cloud, laboratory, field, or production actuation",
            "no LLM calls in the deterministic case-study runtime",
            "no generated outputs committed by hand",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
    }
}

/// Typed refusal from case-study admission.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum TaiCaseStudyRefusal {
    #[error("TAI case-study identity mismatch: `{0}`")]
    IdentityMismatch(String),
    #[error("TAI case-study version mismatch: `{0}`")]
    VersionMismatch(String),
    #[error("TAI case-study source pack or executable example is detached")]
    SourceBoundaryMismatch,
    #[error("TAI case-study broker mismatch: `{0}`")]
    BrokerMismatch(String),
    #[error(
        "TAI case-study {surface} cardinality mismatch: expected {expected}, observed {observed}"
    )]
    CardinalityMismatch {
        surface: String,
        expected: usize,
        observed: usize,
    },
}
