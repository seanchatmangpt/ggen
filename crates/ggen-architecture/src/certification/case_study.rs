use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::building_block::{
    ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRefusal, BuildingBlockRegistry, EvidenceKind, EvidenceObligation, EvidenceReceipt,
    LifecycleState, ObligationId, Port, PortDirection, PortId, PortKind, ProfileId,
    RealizationBinding, RealizationId, ResourceCeiling, ResourceClaim, Standing,
};

use super::{EvidenceLedger, TargetArchitectureInstance};

/// Stable identity of the canonical TAI certification case study.
pub const TAI_CASE_STUDY_ID: &str = "tai-enterprise-reconstruction-v26.7.30";
/// Root Building Block for the reconstructed enterprise.
pub const TAI_ENTERPRISE_ROOT_ID: &str = "tai-enterprise";
/// Stable target identity for the Automated Technical Capability Company.
pub const TAI_TARGET_ID: &str = "tai-automated-technical-capability-company";

const TAI_FACET_IDS: [&str; 9] = [
    "tai-mission",
    "tai-contract",
    "tai-organization",
    "tai-capability",
    "tai-certification",
    "tai-technology",
    "tai-quality",
    "tai-evidence",
    "tai-economics",
];

/// State supplied to a certification candidate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TaiCaseStudyState {
    /// Intentionally incomplete enterprise requiring a generated roadmap.
    Examination,
    /// Fully active, realized, and evidence-closed enterprise.
    Complete,
}

/// Canonical narrative and semantic boundary for the TAI case study.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaiCaseStudy {
    pub id: String,
    pub title: String,
    pub observed_enterprise: String,
    pub target_enterprise: String,
    pub mission: String,
    pub governing_equation: String,
    pub root_broker: String,
    pub enterprise_lifecycle: Vec<String>,
    pub manufacturing_line: Vec<String>,
    pub capability_domains: BTreeSet<String>,
    pub enterprise_facets: BTreeSet<BuildingBlockId>,
    pub root_block: BuildingBlockId,
}

impl TaiCaseStudy {
    /// Bind the case study to the exact composition digest of one admitted registry.
    pub fn target(
        &self, registry: &BuildingBlockRegistry,
    ) -> Result<TargetArchitectureInstance, BuildingBlockRefusal> {
        let roots = BTreeSet::from([self.root_block.clone()]);
        let composition = registry.compose(&roots)?;
        Ok(TargetArchitectureInstance {
            id: TAI_TARGET_ID.to_string(),
            roots,
            required_profiles: required_profiles(),
            expected_composition_digest: Some(composition.digest),
        })
    }
}

/// Return the canonical case-study definition synthesized from TAI doctrine.
#[must_use]
pub fn tai_case_study() -> TaiCaseStudy {
    TaiCaseStudy {
        id: TAI_CASE_STUDY_ID.to_string(),
        title: "TAI: systems-of-systems enterprise reconstruction".to_string(),
        observed_enterprise: "historical TAI systems-of-systems practice across engineering, lifecycle logistics, software, simulation, operations support, training, communications, environmental analysis, and research instrumentation".to_string(),
        target_enterprise: "Automated Technical Capability Company".to_string(),
        mission: "Convert admitted customer mission into installed, operated, supported, replaceable, and retireable technical capability with zero unreceipted actuation.".to_string(),
        governing_equation: "O → O* → μ → A → R".to_string(),
        root_broker: "BRCE".to_string(),
        enterprise_lifecycle: [
            "concept",
            "design",
            "build",
            "simulate",
            "inspect",
            "operate",
            "support",
            "document",
            "verify",
            "train",
            "maintain",
            "decommission",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        manufacturing_line: [
            "customer-mission",
            "critical-to-quality",
            "work-order",
            "shape",
            "route",
            "generation",
            "build",
            "replay",
            "admission",
            "receipt",
            "host-fit",
            "installation",
            "operation",
            "support",
            "replacement",
            "retirement",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        capability_domains: [
            "systems-engineering-and-integration",
            "lifecycle-logistics-and-supportability",
            "software-and-civilization-logic",
            "simulation-and-modeling",
            "operations-support",
            "telecommunications-and-network-engineering",
            "training-and-deterministic-self-play",
            "environmental-chemical-and-biological-analysis",
            "advanced-research-instrumentation",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        enterprise_facets: TAI_FACET_IDS
            .into_iter()
            .map(BuildingBlockId::new)
            .collect(),
        root_block: BuildingBlockId::new(TAI_ENTERPRISE_ROOT_ID),
    }
}

/// Manufacture the admitted TAI Building Block registry for an examination state.
pub fn tai_case_study_registry(
    state: TaiCaseStudyState,
) -> Result<BuildingBlockRegistry, BuildingBlockRefusal> {
    let specs = [
        BlockSpec {
            id: "tai-mission",
            capability: "convert customer mission into admitted critical-to-quality demand",
            dependencies: &[],
            behaviors: &["bind mission, outcomes, exclusions, and lifecycle horizon"],
        },
        BlockSpec {
            id: "tai-contract",
            capability: "compile mission, constraints, standards, and outcomes into deterministic work orders",
            dependencies: &["tai-mission"],
            behaviors: &["preserve customer voice as CTQ and route only admitted work"],
        },
        BlockSpec {
            id: "tai-organization",
            capability: "bind accountable roles, value streams, and bounded authority",
            dependencies: &["tai-mission", "tai-contract"],
            behaviors: &["separate cognition, admission, verification, and actuation authority"],
        },
        BlockSpec {
            id: "tai-quality",
            capability: "operate Total Quality Leadership through TPS, Jidoka, falsifiers, and continuous improvement",
            dependencies: &["tai-mission", "tai-contract"],
            behaviors: &["stop on evidence failure and preserve the rejected observation"],
        },
        BlockSpec {
            id: "tai-technology",
            capability: "manufacture full solution architecture, software, infrastructure, networks, simulations, and tools",
            dependencies: &["tai-contract", "tai-quality"],
            behaviors: &["protect data in flight and at rest with post-quantum-ready controls"],
        },
        BlockSpec {
            id: "tai-economics",
            capability: "bind cost, capacity, resource, replacement, and retirement ceilings",
            dependencies: &["tai-mission", "tai-contract"],
            behaviors: &["refuse capability whose lifecycle cost or resource claim exceeds its ceiling"],
        },
        BlockSpec {
            id: "tai-capability",
            capability: "compose mission capability across industry domains and the complete system lifecycle",
            dependencies: &[
                "tai-organization",
                "tai-technology",
                "tai-quality",
                "tai-economics",
            ],
            behaviors: &["build capability rather than detached artifacts"],
        },
        BlockSpec {
            id: "tai-evidence",
            capability: "observe consequence and preserve provenance, process evidence, receipt, and deterministic replay",
            dependencies: &["tai-capability", "tai-quality"],
            behaviors: &["bind observed consequence to the exact admitted action"],
        },
        BlockSpec {
            id: "tai-certification",
            capability: "compile bodies of knowledge into evidence-backed internal capability standing",
            dependencies: &["tai-capability", "tai-evidence"],
            behaviors: &["keep external certification orthogonal to internal standing"],
        },
        BlockSpec {
            id: TAI_ENTERPRISE_ROOT_ID,
            capability: "reconstruct the Automated Technical Capability Company as one governed enterprise",
            dependencies: &TAI_FACET_IDS,
            behaviors: &["generate the enterprise roadmap from admitted Building Block closure"],
        },
    ];

    let mut registry = BuildingBlockRegistry::new();
    for spec in specs {
        let mut block = make_block(spec);
        if state == TaiCaseStudyState::Examination {
            apply_examination_gap(&mut block);
        }
        registry.register(block)?;
    }
    Ok(registry)
}

/// Manufacture evidence appropriate to the requested case-study state.
#[must_use]
pub fn tai_case_study_evidence(
    registry: &BuildingBlockRegistry, state: TaiCaseStudyState,
) -> EvidenceLedger {
    registry
        .blocks
        .iter()
        .filter_map(|(id, block)| {
            let receipts = match state {
                TaiCaseStudyState::Complete => complete_evidence(block),
                TaiCaseStudyState::Examination => examination_evidence(block),
            };
            (!receipts.is_empty()).then(|| (id.clone(), receipts))
        })
        .collect()
}

struct BlockSpec<'a> {
    id: &'a str,
    capability: &'a str,
    dependencies: &'a [&'a str],
    behaviors: &'a [&'a str],
}

fn make_block(spec: BlockSpec<'_>) -> BuildingBlock {
    let block_id = BuildingBlockId::new(spec.id);
    let input = PortId::new(format!("{}-input", spec.id));
    let output = PortId::new(format!("{}-evidence", spec.id));
    let authority = Authority::new("read:admitted-graph");
    let realization_id = RealizationId::new(format!("{}-realization-v1", spec.id));
    let obligation_id = ObligationId::new(format!("{}-proof", spec.id));
    let realization = RealizationBinding {
        id: realization_id.clone(),
        realizes: block_id.clone(),
        passport_id: format!("urn:tai:passport:{}", spec.id),
        passport_digest: format!("urn:blake3:tai-passport-{}", spec.id),
        provided_ports: BTreeSet::from([output.clone()]),
        authorities: BTreeSet::from([authority.clone()]),
        resources: ResourceClaim {
            memory_bytes: 1_048_576,
            cpu_millis: 1_000,
            output_bytes: 1_048_576,
            broker_intents: 0,
        },
    };
    let obligation = EvidenceObligation {
        id: obligation_id.clone(),
        positive_witness: format!("cargo test tai_positive_{}", spec.id),
        negative_falsifier: format!("cargo test tai_negative_{}", spec.id),
        independent_verifier: "cargo test certification::tests".to_string(),
        receipt_verifier: "ggen receipt verify".to_string(),
        replay: "cargo test tai_complete_enterprise_rebuild_is_replay_stable".to_string(),
    };
    BuildingBlock {
        id: block_id,
        version: "26.7.30".to_string(),
        owner: "tai-architecture-board".to_string(),
        lifecycle: LifecycleState::Active,
        standing: Standing::Unknown,
        architecture: ArchitectureFacet {
            capability: spec.capability.to_string(),
            requirements: BTreeSet::from([
                format!("TAI-{}-CAPABILITY", spec.id.to_uppercase()),
                "TAI-ZERO-UNRECEIPTED-ACTUATION".to_string(),
            ]),
            constraints: [
                "BRCE is the only lawful downstream actuation address",
                "lifecycle and standing remain orthogonal",
                "admission precedes authority",
                "all consequential boundaries require receipt and replay",
            ]
            .into_iter()
            .map(str::to_string)
            .collect(),
            quality_attributes: [
                "deterministic",
                "replayable",
                "supportable",
                "replaceable",
                "retireable",
            ]
            .into_iter()
            .map(str::to_string)
            .collect(),
            permitted_authorities: BTreeSet::from([authority.clone()]),
        },
        contract: BuildingBlockContract {
            behavior: spec
                .behaviors
                .iter()
                .map(|value| (*value).to_string())
                .collect(),
            required_inputs: BTreeSet::from([input.clone()]),
            promised_outputs: BTreeSet::from([output.clone()]),
            resource_ceiling: ResourceCeiling {
                memory_bytes: 2_097_152,
                cpu_millis: 2_000,
                output_bytes: 2_097_152,
                broker_intents: 0,
            },
            authority_ceiling: BTreeSet::from([authority]),
        },
        ports: BTreeMap::from([
            (
                input.clone(),
                Port {
                    id: input,
                    direction: PortDirection::Input,
                    kind: PortKind::Data,
                    schema: "ggen.admitted-observation.v1".to_string(),
                    required: true,
                },
            ),
            (
                output.clone(),
                Port {
                    id: output,
                    direction: PortDirection::Output,
                    kind: PortKind::Evidence,
                    schema: "ggen.receipted-capability.v1".to_string(),
                    required: true,
                },
            ),
        ]),
        dependencies: spec
            .dependencies
            .iter()
            .map(|dependency| BuildingBlockId::new(*dependency))
            .collect(),
        realizations: BTreeMap::from([(realization_id.clone(), realization)]),
        selected_realization: Some(realization_id),
        profiles: profiles_for(spec.id),
        incompatible_profiles: BTreeSet::new(),
        obligations: BTreeMap::from([(obligation_id, obligation)]),
        exclusions: [
            "direct external actuation",
            "certificate-implies-standing",
            "silent constraint deletion",
            "unbounded vendor-specific implementation",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        provenance: format!("urn:ggen:tai-case-study:{}", spec.id),
    }
}

fn apply_examination_gap(block: &mut BuildingBlock) {
    match block.id.as_str() {
        "tai-contract" => block.lifecycle = LifecycleState::Identified,
        "tai-organization" => block.lifecycle = LifecycleState::Admitted,
        "tai-technology" => {
            block.lifecycle = LifecycleState::Qualified;
            block.selected_realization = None;
        }
        "tai-capability" => block.lifecycle = LifecycleState::Discovered,
        "tai-evidence" => block.lifecycle = LifecycleState::Deprecated,
        TAI_ENTERPRISE_ROOT_ID => block.lifecycle = LifecycleState::Admitted,
        _ => {}
    }
}

fn examination_evidence(block: &BuildingBlock) -> BTreeSet<EvidenceReceipt> {
    match block.id.as_str() {
        "tai-mission" | "tai-quality" | "tai-economics" | "tai-evidence" => {
            complete_evidence(block)
        }
        "tai-organization" | "tai-certification" => {
            evidence_for(block, &[EvidenceKind::PositiveWitness])
        }
        _ => BTreeSet::new(),
    }
}

fn complete_evidence(block: &BuildingBlock) -> BTreeSet<EvidenceReceipt> {
    evidence_for(
        block,
        &[
            EvidenceKind::PositiveWitness,
            EvidenceKind::NegativeFalsifier,
            EvidenceKind::IndependentVerifier,
            EvidenceKind::ReceiptVerifier,
            EvidenceKind::Replay,
        ],
    )
}

fn evidence_for(block: &BuildingBlock, kinds: &[EvidenceKind]) -> BTreeSet<EvidenceReceipt> {
    block
        .obligations
        .keys()
        .flat_map(|obligation_id| {
            kinds.iter().copied().map(|kind| EvidenceReceipt {
                obligation_id: obligation_id.clone(),
                kind,
                digest: format!("urn:blake3:{}-{kind:?}", block.id.as_str()),
            })
        })
        .collect()
}

fn profiles_for(id: &str) -> BTreeSet<ProfileId> {
    let mut profiles = BTreeSet::from([ProfileId::new("core"), ProfileId::new("tai-case-study")]);
    if id == TAI_ENTERPRISE_ROOT_ID {
        profiles.extend(required_profiles());
    }
    profiles
}

fn required_profiles() -> BTreeSet<ProfileId> {
    ["core", "tai-case-study", "brce", "rwr-level5", "fortune5"]
        .into_iter()
        .map(ProfileId::new)
        .collect()
}
