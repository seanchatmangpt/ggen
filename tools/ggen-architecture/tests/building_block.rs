//! Cross-package proofs for the canonical ggen Building Block facade.

use std::collections::{BTreeMap, BTreeSet};

use ggen_architecture::profiles::fortune5::REQUIRED_BROKER;
use ggen_architecture::{
    ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, BuildingBlockStanding, EvidenceKind, EvidenceObligation,
    EvidenceReceipt, ObligationId, Port, PortDirection, PortId, PortKind, ProfileId,
    RealizationBinding, RealizationId, ResourceClaim, ResourceCeiling,
};

fn admitted_block() -> BuildingBlock {
    let input = PortId::from("graph");
    let output = PortId::from("receipt");
    let authority = Authority::from("read:graph");
    let id = BuildingBlockId::from("deterministic-knowledge-projection");
    let realization_id = RealizationId::from("ggen-engine-26.7.61");
    let obligation_id = ObligationId::from("projection-e2e");

    BuildingBlock {
        id: id.clone(),
        version: "26.7.61".to_string(),
        owner: "ggen-core-team".to_string(),
        lifecycle: ggen_architecture_kernel::LifecycleState::Admitted,
        standing: BuildingBlockStanding::Unknown,
        architecture: ArchitectureFacet {
            capability: "deterministic knowledge projection".to_string(),
            requirements: BTreeSet::from(["zero-unreceipted-actuation".to_string()]),
            constraints: BTreeSet::from(["io-free-kernel".to_string()]),
            quality_attributes: BTreeSet::from(["byte-idempotent".to_string()]),
            permitted_authorities: BTreeSet::from([authority.clone()]),
        },
        contract: BuildingBlockContract {
            behavior: BTreeSet::from(["project admitted RDF into owned artifacts".to_string()]),
            required_inputs: BTreeSet::from([input.clone()]),
            promised_outputs: BTreeSet::from([output.clone()]),
            resource_ceiling: ResourceCeiling {
                memory_bytes: 67_108_864,
                cpu_millis: 30_000,
                output_bytes: 10_485_760,
                broker_intents: 0,
            },
            authority_ceiling: BTreeSet::from([authority.clone()]),
        },
        ports: BTreeMap::from([
            (
                input.clone(),
                Port {
                    id: input,
                    direction: PortDirection::Input,
                    kind: PortKind::Data,
                    schema: "application/n-quads".to_string(),
                    required: true,
                },
            ),
            (
                output.clone(),
                Port {
                    id: output.clone(),
                    direction: PortDirection::Output,
                    kind: PortKind::Evidence,
                    schema: "ggen.receipt.v1".to_string(),
                    required: true,
                },
            ),
        ]),
        dependencies: BTreeSet::new(),
        realizations: BTreeMap::from([(
            realization_id.clone(),
            RealizationBinding {
                id: realization_id.clone(),
                realizes: id,
                passport_id: "urn:ggen:passport:ggen-engine:26.7.61".to_string(),
                passport_digest: "urn:blake3:admitted-passport".to_string(),
                provided_ports: BTreeSet::from([output]),
                authorities: BTreeSet::from([authority]),
                resources: ResourceClaim {
                    memory_bytes: 33_554_432,
                    cpu_millis: 10_000,
                    output_bytes: 5_242_880,
                    broker_intents: 0,
                },
            },
        )]),
        selected_realization: Some(realization_id),
        profiles: BTreeSet::from([ProfileId::from("ggen-core-development")]),
        incompatible_profiles: BTreeSet::new(),
        obligations: BTreeMap::from([(
            obligation_id.clone(),
            EvidenceObligation {
                id: obligation_id,
                positive_witness: "ggen sync run".to_string(),
                negative_falsifier: "ggen sync run malformed-input".to_string(),
                independent_verifier: "ggen receipt verify".to_string(),
                receipt_verifier: "ggen receipt verify".to_string(),
                replay: "ggen sync run && diff".to_string(),
            },
        )]),
        exclusions: BTreeSet::from(["direct external actuation".to_string()]),
        provenance: "urn:git:seanchatmangpt/ggen:ee6f31ec".to_string(),
    }
}

#[test]
fn facade_exposes_one_canonical_building_block_kernel() {
    let block = admitted_block();
    assert!(block.validate().is_empty());
    assert_eq!(REQUIRED_BROKER, "BRCE");
}

#[test]
fn composition_is_deterministic_and_receipted() {
    let block = admitted_block();
    let root = block.id.clone();
    let mut registry = BuildingBlockRegistry::new();
    assert!(registry.register(block).is_ok());
    let roots = BTreeSet::from([root]);
    let first = registry.compose(&roots);
    let second = registry.compose(&roots);
    assert!(matches!(
        (&first, &second),
        (Ok(left), Ok(right)) if left == right && left.digest.starts_with("urn:blake3:")
    ));
}

#[test]
fn alive_requires_witness_falsifier_verifier_receipt_and_replay() {
    let block = admitted_block();
    let obligation = ObligationId::from("projection-e2e");
    let receipts: BTreeSet<EvidenceReceipt> = [
        EvidenceKind::PositiveWitness,
        EvidenceKind::NegativeFalsifier,
        EvidenceKind::IndependentVerifier,
        EvidenceKind::ReceiptVerifier,
        EvidenceKind::Replay,
    ]
    .into_iter()
    .map(|kind| EvidenceReceipt {
        obligation_id: obligation.clone(),
        kind,
        digest: format!("urn:blake3:{kind:?}"),
    })
    .collect();
    assert_eq!(
        block.evidence_standing(&receipts),
        BuildingBlockStanding::Alive
    );
}
