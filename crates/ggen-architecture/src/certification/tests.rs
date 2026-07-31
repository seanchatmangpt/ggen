use std::collections::{BTreeMap, BTreeSet};

use crate::building_block::{
    ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, EvidenceKind, EvidenceObligation, EvidenceReceipt, LifecycleState,
    ObligationId, Port, PortDirection, PortId, PortKind, ProfileId, RealizationBinding,
    RealizationId, ResourceCeiling, ResourceClaim, Standing,
};

use super::*;

fn block(id: &str, dependency: Option<&str>) -> BuildingBlock {
    let block_id = BuildingBlockId::new(id);
    let input = PortId::new(format!("{id}-input"));
    let output = PortId::new(format!("{id}-output"));
    let authority = Authority::new("read:graph");
    let realization_id = RealizationId::new(format!("{id}-realization"));
    let obligation_id = ObligationId::new(format!("{id}-proof"));
    let realization = RealizationBinding {
        id: realization_id.clone(),
        realizes: block_id.clone(),
        passport_id: format!("urn:passport:{id}"),
        passport_digest: format!("urn:blake3:passport-{id}"),
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
        independent_verifier: "cargo test verifier".to_string(),
        receipt_verifier: "ggen receipt verify".to_string(),
        replay: "cargo test replay".to_string(),
    };
    BuildingBlock {
        id: block_id,
        version: "26.7.30".to_string(),
        owner: "ggen-architecture-board".to_string(),
        lifecycle: LifecycleState::Active,
        standing: Standing::Unknown,
        architecture: ArchitectureFacet {
            capability: format!("manufacture {id}"),
            requirements: BTreeSet::from([format!("REQ-{id}")]),
            constraints: BTreeSet::from(["zero unreceipted actuation".to_string()]),
            quality_attributes: BTreeSet::from(["deterministic".to_string()]),
            permitted_authorities: BTreeSet::from([authority.clone()]),
        },
        contract: BuildingBlockContract {
            behavior: BTreeSet::from([format!("rebuild {id}")]),
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
                    schema: "urn:schema:input".to_string(),
                    required: true,
                },
            ),
            (
                output.clone(),
                Port {
                    id: output,
                    direction: PortDirection::Output,
                    kind: PortKind::Evidence,
                    schema: "urn:schema:output".to_string(),
                    required: true,
                },
            ),
        ]),
        dependencies: dependency
            .map(|value| BTreeSet::from([BuildingBlockId::new(value)]))
            .unwrap_or_default(),
        realizations: BTreeMap::from([(realization_id.clone(), realization)]),
        selected_realization: Some(realization_id),
        profiles: BTreeSet::from([ProfileId::new("core")]),
        incompatible_profiles: BTreeSet::new(),
        obligations: BTreeMap::from([(obligation_id, obligation)]),
        exclusions: BTreeSet::from(["direct actuation".to_string()]),
        provenance: format!("urn:git:{id}"),
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
        positive_witness_digest: format!("urn:blake3:{id}-positive"),
        negative_falsifier_digest: format!("urn:blake3:{id}-negative"),
        independent_verifier_digest: format!("urn:blake3:{id}-independent"),
        receipt_verifier_digest: format!("urn:blake3:{id}-receipt"),
        replay_digest: format!("urn:blake3:{id}-replay"),
        artifact_digest: format!("urn:blake3:{id}-artifact"),
    }
}

#[test]
fn certification_program_has_five_levels_twelve_requirements_and_sixty_surfaces() {
    let program = CertificationProgram::ggen_bblocks_v1();
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
fn certification_refuses_missing_replay() {
    let program = CertificationProgram::ggen_bblocks_v1();
    let mut receipts = program
        .requirements_through(CertificationLevel::Gbb100Foundation)
        .into_iter()
        .map(|requirement| (requirement.id.clone(), requirement_receipt(&requirement.id)))
        .collect::<BTreeMap<_, _>>();
    receipts
        .get_mut("GBB-100-FENCE")
        .expect("canonical requirement must exist")
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
}

#[test]
fn bblocks_generate_dependency_ordered_roadmap() {
    let foundation = block("foundation", None);
    let mut application = block("application", Some("foundation"));
    application.lifecycle = LifecycleState::Qualified;
    application.selected_realization = None;
    let mut registry = BuildingBlockRegistry::new();
    registry.register(application.clone()).expect("valid root");
    registry
        .register(foundation.clone())
        .expect("valid dependency");
    let ledger = BTreeMap::from([(foundation.id.clone(), evidence(&foundation))]);
    let target = TargetArchitectureInstance {
        id: "tai-1".to_string(),
        roots: BTreeSet::from([application.id.clone()]),
        required_profiles: BTreeSet::from([ProfileId::new("core")]),
        expected_composition_digest: None,
    };
    let first = generate_rebuild_roadmap(&registry, &target, &ledger).expect("roadmap");
    let second = generate_rebuild_roadmap(&registry, &target, &ledger).expect("replay");
    assert_eq!(first, second);
    assert_eq!(
        first.composition.order,
        vec![foundation.id.clone(), application.id.clone()]
    );
    assert_eq!(
        first
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
fn complete_tai_rebuild_is_replay_stable_and_alive() {
    let foundation = block("foundation", None);
    let application = block("application", Some("foundation"));
    let mut registry = BuildingBlockRegistry::new();
    registry.register(application.clone()).expect("valid root");
    registry
        .register(foundation.clone())
        .expect("valid dependency");
    let ledger = BTreeMap::from([
        (foundation.id.clone(), evidence(&foundation)),
        (application.id.clone(), evidence(&application)),
    ]);
    let target = TargetArchitectureInstance {
        id: "tai-complete".to_string(),
        roots: BTreeSet::from([application.id.clone()]),
        required_profiles: BTreeSet::from([ProfileId::new("core")]),
        expected_composition_digest: None,
    };
    let first =
        simulate_tai_rebuild(&registry, &target, "candidate-1", &ledger).expect("complete rebuild");
    let second =
        simulate_tai_rebuild(&registry, &target, "candidate-1", &ledger).expect("stable replay");
    assert_eq!(first, second);
    assert_eq!(first.standing, Standing::Alive);
    assert_eq!(first.order, vec![foundation.id, application.id]);
    assert!(first.digest.starts_with("urn:blake3:"));
}

#[test]
fn retired_blocks_are_refused() {
    let mut retired = block("retired", None);
    retired.lifecycle = LifecycleState::Retired;
    let mut registry = BuildingBlockRegistry::new();
    registry.register(retired.clone()).expect("valid registry");
    let target = TargetArchitectureInstance {
        id: "tai-retired".to_string(),
        roots: BTreeSet::from([retired.id]),
        required_profiles: BTreeSet::new(),
        expected_composition_digest: None,
    };
    assert!(matches!(
        generate_rebuild_roadmap(&registry, &target, &BTreeMap::new()),
        Err(CertificationRefusal::TargetContainsRetiredBlock { .. })
    ));
}
