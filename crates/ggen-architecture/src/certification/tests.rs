use std::collections::{BTreeMap, BTreeSet};

use crate::building_block::{
    ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, EvidenceKind, EvidenceObligation, EvidenceReceipt, LifecycleState,
    ObligationId, Port, PortDirection, PortId, PortKind, ProfileId, RealizationBinding,
    RealizationId, ResourceCeiling, ResourceClaim, Standing,
};

use super::*;

fn standards_digest() -> String {
    seven_day_standards_profile()
        .digest()
        .expect("canonical seven-day standards profile")
}

fn block(id: &str, dependency: Option<&str>) -> BuildingBlock {
    let block_id = BuildingBlockId::new(id);
    let input = PortId::new(format!("{id}-input"));
    let output = PortId::new(format!("{id}-output"));
    let authority = Authority::new("read:admitted-graph");
    let realization_id = RealizationId::new(format!("{id}-realization"));
    let obligation_id = ObligationId::new(format!("{id}-proof"));
    let realization = RealizationBinding {
        id: realization_id.clone(),
        realizes: block_id.clone(),
        passport_id: format!("urn:tai:passport:{id}"),
        passport_digest: format!("urn:blake3:tai-passport-{id}"),
        provided_ports: BTreeSet::from([output.clone()]),
        authorities: BTreeSet::from([authority.clone()]),
        resources: ResourceClaim {
            memory_bytes: 64,
            cpu_millis: 10,
            output_bytes: 1_024,
            broker_intents: 0,
        },
    };
    let obligation = EvidenceObligation {
        id: obligation_id.clone(),
        positive_witness: "cargo test positive".to_string(),
        negative_falsifier: "cargo test negative".to_string(),
        independent_verifier: "cargo test certification::tests".to_string(),
        receipt_verifier: "ggen receipt verify".to_string(),
        replay: "cargo test certification::tests".to_string(),
    };
    BuildingBlock {
        id: block_id,
        version: TAI_CASE_STUDY_VERSION.to_string(),
        owner: "tai-architecture-board".to_string(),
        lifecycle: LifecycleState::Active,
        standing: Standing::Unknown,
        architecture: ArchitectureFacet {
            capability: format!("manufacture TAI {id}"),
            requirements: BTreeSet::from([format!("TAI-{id}")]),
            constraints: BTreeSet::from(["zero unreceipted actuation".to_string()]),
            quality_attributes: BTreeSet::from(["deterministic".to_string()]),
            permitted_authorities: BTreeSet::from([authority.clone()]),
        },
        contract: BuildingBlockContract {
            behavior: BTreeSet::from([format!("reconstruct {id}")]),
            required_inputs: BTreeSet::from([input.clone()]),
            promised_outputs: BTreeSet::from([output.clone()]),
            resource_ceiling: ResourceCeiling {
                memory_bytes: 128,
                cpu_millis: 20,
                output_bytes: 2_048,
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
        dependencies: dependency
            .map(|value| BTreeSet::from([BuildingBlockId::new(value)]))
            .unwrap_or_default(),
        realizations: BTreeMap::from([(realization_id.clone(), realization)]),
        selected_realization: Some(realization_id),
        profiles: BTreeSet::from([ProfileId::new("core"), ProfileId::new("tai-case-study")]),
        incompatible_profiles: BTreeSet::new(),
        obligations: BTreeMap::from([(obligation_id, obligation)]),
        exclusions: BTreeSet::from(["direct actuation".to_string()]),
        provenance: format!("urn:tai-rebuild:{id}"),
    }
}

fn evidence(block: &BuildingBlock) -> BTreeSet<EvidenceReceipt> {
    block
        .obligations
        .keys()
        .flat_map(|obligation_id| {
            [
                EvidenceKind::PositiveWitness,
                EvidenceKind::NegativeFalsifier,
                EvidenceKind::IndependentVerifier,
                EvidenceKind::ReceiptVerifier,
                EvidenceKind::Replay,
            ]
            .into_iter()
            .map(|kind| EvidenceReceipt {
                obligation_id: obligation_id.clone(),
                kind,
                digest: format!("urn:blake3:{}-{kind:?}", block.id.as_str()),
            })
        })
        .collect()
}

fn requirement_receipt(id: &str) -> RequirementReceipt {
    RequirementReceipt {
        requirement_id: id.to_string(),
        candidate_id: "candidate-1".to_string(),
        case_study_id: TAI_CASE_STUDY_ID.to_string(),
        standards_profile_id: GGEN_SEVEN_DAY_STANDARDS_ID.to_string(),
        standards_profile_digest: standards_digest(),
        positive_witness_digest: format!("urn:blake3:{id}-positive"),
        negative_falsifier_digest: format!("urn:blake3:{id}-negative"),
        independent_verifier_digest: format!("urn:blake3:{id}-independent"),
        receipt_verifier_digest: format!("urn:blake3:{id}-receipt"),
        replay_digest: format!("urn:blake3:{id}-replay"),
        artifact_digest: format!("urn:blake3:{id}-artifact"),
    }
}

fn complete_registry() -> (BuildingBlockRegistry, BuildingBlock, BuildingBlock) {
    let foundation = block("foundation", None);
    let application = block("application", Some("foundation"));
    let mut registry = BuildingBlockRegistry::new();
    registry
        .register(application.clone())
        .expect("valid TAI root");
    registry
        .register(foundation.clone())
        .expect("valid TAI dependency");
    (registry, foundation, application)
}

fn target(root: &BuildingBlock) -> TargetArchitectureInstance {
    TargetArchitectureInstance {
        id: "tai-automated-technical-capability-company".to_string(),
        case_study_id: TAI_CASE_STUDY_ID.to_string(),
        standards_profile_id: GGEN_SEVEN_DAY_STANDARDS_ID.to_string(),
        standards_profile_digest: standards_digest(),
        roots: BTreeSet::from([root.id.clone()]),
        required_profiles: BTreeSet::from([
            ProfileId::new("core"),
            ProfileId::new("tai-case-study"),
        ]),
        expected_composition_digest: None,
    }
}

#[test]
fn tai_case_study_preserves_source_and_exact_cardinality() {
    let case_study = tai_case_study();
    assert!(case_study.validate().is_ok());
    assert_eq!(case_study.id, TAI_CASE_STUDY_ID);
    assert_eq!(case_study.version, TAI_CASE_STUDY_VERSION);
    assert_eq!(case_study.source_pack, TAI_CASE_STUDY_PACK);
    assert_eq!(case_study.source_example, TAI_CASE_STUDY_EXAMPLE);
    assert_eq!(case_study.root_broker, "BRCE");
    assert_eq!(case_study.building_block_families.len(), 9);
    assert_eq!(case_study.reconstruction_phases.len(), 7);
    assert_eq!(case_study.certification_horizons.len(), 10);
    assert_eq!(case_study.counterfactual_scenarios.len(), 7);
    assert_eq!(case_study.recent_capability_rails, 18);
}

#[test]
fn seven_day_standards_are_exact_and_pending_work_cannot_promote() {
    let profile = seven_day_standards_profile();
    assert!(profile.validate().is_ok());
    assert_eq!(profile.id, GGEN_SEVEN_DAY_STANDARDS_ID);
    assert_eq!(profile.version, GGEN_SEVEN_DAY_STANDARDS_VERSION);
    assert_eq!(profile.checkpoints.len(), 18);
    assert_eq!(profile.promoting_checkpoints().len(), 17);
    assert!(profile.checkpoints.iter().any(|checkpoint| {
        checkpoint.id == "STD-18-CI-CONTROL-PLANE-G0"
            && checkpoint.status == StandardStatus::PendingCheckpoint
    }));
    assert!(profile.digest().is_ok_and(|value| value.starts_with("urn:blake3:")));
}

#[test]
fn standards_digest_changes_when_law_changes() {
    let original = seven_day_standards_profile();
    let mut changed = original.clone();
    changed.checkpoints[0].law.push_str(" changed");
    assert_ne!(
        original.digest().expect("original standards digest"),
        changed.digest().expect("changed standards digest")
    );
}

#[test]
fn programme_has_five_levels_twelve_requirements_and_sixty_surfaces() {
    let program = CertificationProgram::ggen_bblocks_v1();
    assert_eq!(program.case_study_id, TAI_CASE_STUDY_ID);
    assert_eq!(program.standards_profile_id, GGEN_SEVEN_DAY_STANDARDS_ID);
    assert_eq!(program.requirements.len(), 12);
    assert_eq!(
        program
            .requirements_through(CertificationLevel::Gbb100Foundation)
            .len(),
        2
    );
    assert_eq!(
        program
            .requirements_through(CertificationLevel::Gbb500TaiManufacturer)
            .len(),
        12
    );
    assert_eq!(12 * 5, 60);
}

#[test]
fn award_binds_candidate_case_and_exact_standards_profile() {
    let program = CertificationProgram::ggen_bblocks_v1();
    let receipts = program
        .requirements_through(CertificationLevel::Gbb100Foundation)
        .into_iter()
        .map(|requirement| (requirement.id.clone(), requirement_receipt(&requirement.id)))
        .collect::<BTreeMap<_, _>>();
    let award = program
        .assess(
            "candidate-1",
            CertificationLevel::Gbb100Foundation,
            &receipts,
        )
        .expect("complete TAI portfolio");
    assert_eq!(award.case_study_id, TAI_CASE_STUDY_ID);
    assert_eq!(award.standards_profile_id, GGEN_SEVEN_DAY_STANDARDS_ID);
    assert_eq!(award.standards_profile_digest, standards_digest());
    assert!(award.digest.starts_with("urn:blake3:"));
}

#[test]
fn award_refuses_missing_replay_detached_case_and_stale_standards() {
    let program = CertificationProgram::ggen_bblocks_v1();
    let mut receipts = program
        .requirements_through(CertificationLevel::Gbb100Foundation)
        .into_iter()
        .map(|requirement| (requirement.id.clone(), requirement_receipt(&requirement.id)))
        .collect::<BTreeMap<_, _>>();
    receipts
        .get_mut("GBB-100-FENCE")
        .expect("canonical requirement")
        .replay_digest
        .clear();
    assert!(matches!(
        program.assess(
            "candidate-1",
            CertificationLevel::Gbb100Foundation,
            &receipts
        ),
        Err(CertificationRefusal::ReceiptSurfaceMissing { surface, .. })
            if surface == "replay"
    ));

    let mut receipt = requirement_receipt("GBB-100-FENCE");
    receipt.case_study_id = "detached-case".to_string();
    receipts.insert("GBB-100-FENCE".to_string(), receipt);
    assert!(matches!(
        program.assess(
            "candidate-1",
            CertificationLevel::Gbb100Foundation,
            &receipts
        ),
        Err(CertificationRefusal::CaseStudyMismatch { .. })
    ));

    let mut receipt = requirement_receipt("GBB-100-FENCE");
    receipt.standards_profile_digest = "urn:blake3:stale".to_string();
    receipts.insert("GBB-100-FENCE".to_string(), receipt);
    assert!(matches!(
        program.assess(
            "candidate-1",
            CertificationLevel::Gbb100Foundation,
            &receipts
        ),
        Err(CertificationRefusal::StandardsProfileDigestMismatch { .. })
    ));
}

#[test]
fn gbb_state_generates_dependency_ordered_tai_roadmap() {
    let foundation = block("foundation", None);
    let mut application = block("application", Some("foundation"));
    application.lifecycle = LifecycleState::Qualified;
    application.selected_realization = None;
    let mut registry = BuildingBlockRegistry::new();
    registry
        .register(application.clone())
        .expect("valid TAI root");
    registry
        .register(foundation.clone())
        .expect("valid TAI dependency");
    let ledger = BTreeMap::from([(foundation.id.clone(), evidence(&foundation))]);
    let roadmap = generate_rebuild_roadmap(&registry, &target(&application), &ledger)
        .expect("generated TAI roadmap");
    assert_eq!(
        roadmap.composition.order,
        vec![foundation.id.clone(), application.id.clone()]
    );
    assert_eq!(roadmap.case_study_id, TAI_CASE_STUDY_ID);
    assert_eq!(roadmap.standards_profile_id, GGEN_SEVEN_DAY_STANDARDS_ID);
    assert_eq!(roadmap.standards_profile_digest, standards_digest());
    assert_eq!(
        roadmap
            .steps
            .iter()
            .map(|step| step.action)
            .collect::<Vec<_>>(),
        vec![
            RoadmapAction::Admit,
            RoadmapAction::Activate,
            RoadmapAction::SelectRealization,
            RoadmapAction::CloseEvidence,
        ]
    );
}

#[test]
fn complete_tai_rebuild_is_alive_and_replay_stable() {
    let (registry, foundation, application) = complete_registry();
    let ledger = BTreeMap::from([
        (foundation.id.clone(), evidence(&foundation)),
        (application.id.clone(), evidence(&application)),
    ]);
    let target = target(&application);
    let first = simulate_tai_rebuild(&registry, &target, "candidate-1", &ledger)
        .expect("complete TAI rebuild");
    let second = simulate_tai_rebuild(&registry, &target, "candidate-1", &ledger)
        .expect("stable TAI replay");
    assert_eq!(first, second);
    assert_eq!(first.case_study_id, TAI_CASE_STUDY_ID);
    assert_eq!(first.standards_profile_id, GGEN_SEVEN_DAY_STANDARDS_ID);
    assert_eq!(first.standards_profile_digest, standards_digest());
    assert_eq!(first.standing, Standing::Alive);
    assert_eq!(first.order, vec![foundation.id, application.id]);
}

#[test]
fn detached_target_stale_standards_and_terminal_lifecycle_are_refused() {
    let (registry, foundation, application) = complete_registry();
    let ledger = BTreeMap::from([
        (foundation.id.clone(), evidence(&foundation)),
        (application.id.clone(), evidence(&application)),
    ]);
    let mut detached = target(&application);
    detached.case_study_id = "detached-case".to_string();
    assert!(matches!(
        generate_rebuild_roadmap(&registry, &detached, &ledger),
        Err(CertificationRefusal::CaseStudyMismatch { .. })
    ));

    let mut stale = target(&application);
    stale.standards_profile_digest = "urn:blake3:stale".to_string();
    assert!(matches!(
        generate_rebuild_roadmap(&registry, &stale, &ledger),
        Err(CertificationRefusal::StandardsProfileDigestMismatch { .. })
    ));

    let mut retired = block("retired", None);
    retired.lifecycle = LifecycleState::Retired;
    let mut retired_registry = BuildingBlockRegistry::new();
    retired_registry
        .register(retired.clone())
        .expect("valid terminal block declaration");
    assert!(matches!(
        generate_rebuild_roadmap(&retired_registry, &target(&retired), &BTreeMap::new()),
        Err(CertificationRefusal::TargetContainsRetiredBlock { .. })
    ));
}
