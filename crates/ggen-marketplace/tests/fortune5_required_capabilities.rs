#![allow(clippy::expect_used, clippy::panic)]
use ggen_marketplace::marketplace::fortune5::{
    Fortune5Capability, Fortune5EvidenceLedger, Fortune5EvidenceOutcome, Fortune5EvidenceRecord,
    Fortune5ProofSurface, Fortune5Reference, Fortune5Standing, ALL_FORTUNE5_CAPABILITIES,
    REQUIRED_PROOF_SURFACES,
};
use std::fs;

#[test]
fn full_fortune5_contract_crosses_real_boundaries() {
    let root = tempfile::tempdir().expect("temp root");
    let reference = Fortune5Reference::new(root.path().join("proof"));
    let proof = reference.prove_all().expect("Fortune-5 proof");

    proof.verify().expect("proof replay");
    let assessment = &proof.assessment_receipt.assessment;
    assert_eq!(assessment.standing, Fortune5Standing::Alive);
    assert!(assessment.is_complete_contract());
    assert_eq!(assessment.capabilities.len(), 19);
    assert_eq!(assessment.satisfied_surface_count(), 57);
    assert!(assessment.open_obligations().is_empty());
    assert_eq!(proof.ledger.records().len(), 57);

    let witness_count = fs::read_dir(reference.root().join("witnesses"))
        .expect("witness root")
        .map(|entry| {
            fs::read_dir(entry.expect("capability directory").path())
                .expect("surface directory")
                .count()
        })
        .sum::<usize>();
    assert_eq!(witness_count, 57);
}

#[test]
fn one_missing_surface_prevents_crown_promotion() {
    let mut ledger = Fortune5EvidenceLedger::new();
    let capability = Fortune5Capability::InstallTruth;
    for (epoch, surface) in REQUIRED_PROOF_SURFACES.iter().copied().take(2).enumerate() {
        ledger
            .admit(Fortune5EvidenceRecord::observed(
                format!("partial-{epoch}"),
                capability,
                surface,
                Fortune5EvidenceOutcome::Pass,
                format!("partial/{epoch}.json"),
                epoch as u64 + 1,
                b"observed",
            ))
            .expect("admit partial evidence");
    }

    let assessment = ledger.assess();
    assert_ne!(assessment.standing, Fortune5Standing::Alive);
    assert!(assessment
        .open_obligations()
        .contains(&(capability, Fortune5ProofSurface::ReceiptReplay)));
}

#[test]
fn evidence_tamper_and_duplicate_identity_fail_closed() {
    let mut ledger = Fortune5EvidenceLedger::new();
    let record = Fortune5EvidenceRecord::observed(
        "evidence-1",
        Fortune5Capability::ProofTruth,
        Fortune5ProofSurface::PositiveExecution,
        Fortune5EvidenceOutcome::Pass,
        "proof.json",
        1,
        b"proof",
    );
    ledger.admit(record.clone()).expect("first admission");
    assert!(ledger.admit(record.clone()).is_err());

    let mut tampered = record;
    tampered.artifact_digest[0] ^= 0xff;
    let mut fresh = Fortune5EvidenceLedger::new();
    assert!(fresh.admit(tampered).is_err());
}

#[test]
fn independent_reference_roots_replay_to_same_crown_digest() {
    let left_root = tempfile::tempdir().expect("left root");
    let right_root = tempfile::tempdir().expect("right root");
    let left = Fortune5Reference::new(left_root.path().join("proof"))
        .prove_all()
        .expect("left proof");
    let right = Fortune5Reference::new(right_root.path().join("proof"))
        .prove_all()
        .expect("right proof");

    assert_eq!(left.ledger.root_digest(), right.ledger.root_digest());
    assert_eq!(
        left.assessment_receipt.receipt_digest,
        right.assessment_receipt.receipt_digest
    );
    assert_eq!(
        ALL_FORTUNE5_CAPABILITIES.len() * REQUIRED_PROOF_SURFACES.len(),
        57
    );
}
