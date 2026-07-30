use ggen_graph::rwr::{
    Action, AutonomicController, Dimension, ExecutionError, ExecutionPolicy, FilesystemActuator,
    FoundationMachine, GallState, ManagedCell, ReferenceFoundation, ReplayVerifier, ALL_DIMENSIONS,
};
use std::fs::File;
use std::io::Read;

#[test]
fn full_reference_foundation_closes_every_rwr_dimension() -> Result<(), Box<dyn std::error::Error>>
{
    let temp = tempfile::tempdir()?;
    let proof_root = temp.path().join("rwr-level5-proof");
    let foundation = ReferenceFoundation::new(&proof_root);

    let proof = foundation.prove_all()?;
    proof.verify()?;

    assert_eq!(
        proof.assessment_receipt.assessment.standing,
        GallState::Alive
    );
    assert!(proof.assessment_receipt.assessment.is_complete_matrix());
    assert_eq!(
        proof.assessment_receipt.assessment.dimensions.len(),
        ALL_DIMENSIONS.len()
    );
    assert_eq!(proof.ledger.records().len(), ALL_DIMENSIONS.len() * 3);
    assert!(proof
        .assessment_receipt
        .assessment
        .dimensions
        .iter()
        .all(|assessment| assessment.standing == GallState::Alive));

    for record in proof.ledger.records() {
        let artifact_path = proof_root.join("actuation").join(&record.source);
        assert!(
            artifact_path.is_file(),
            "missing external evidence: {artifact_path:?}"
        );
        let mut bytes = Vec::new();
        File::open(artifact_path)?.read_to_end(&mut bytes)?;
        let observed: [u8; 32] = blake3::hash(&bytes).into();
        assert_eq!(record.artifact_digest, observed);
    }

    Ok(())
}

#[test]
fn policy_refusal_and_receipt_replay_are_real_boundaries() -> Result<(), Box<dyn std::error::Error>>
{
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let narrow_machine =
        FoundationMachine::new(ExecutionPolicy::new([Dimension::ProcessIntegration], 64));

    let refused = Action::new(
        "governance-bypass",
        Dimension::EnterpriseGovernance,
        b"bypass".to_vec(),
    );
    assert!(matches!(
        narrow_machine.derive_grant(&refused),
        Err(ExecutionError::DimensionRefused(
            Dimension::EnterpriseGovernance
        ))
    ));

    let full_machine = FoundationMachine::new(ExecutionPolicy::full_level5(64));
    let admitted = Action::new(
        "receipted-action",
        Dimension::ReceiptReplay,
        b"real-consequence".to_vec(),
    );
    let grant = full_machine.derive_grant(&admitted)?;
    let receipt = actuator.actuate(&grant, &admitted)?;
    actuator.verify_committed(&receipt)?;

    let mut replay = ReplayVerifier::default();
    replay.verify_once(&receipt)?;
    assert!(matches!(
        replay.verify_once(&receipt),
        Err(ExecutionError::ReceiptReplayRefused)
    ));
    assert!(matches!(
        actuator.actuate(&grant, &admitted),
        Err(ExecutionError::ReplayRefused(id)) if id == "receipted-action"
    ));

    Ok(())
}

#[test]
fn autonomic_controller_repairs_degraded_state_and_receipts_the_cycle(
) -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let actuator = FilesystemActuator::new(temp.path().join("actuation"));
    let machine = FoundationMachine::new(ExecutionPolicy::full_level5(128));

    let initial = Action::new(
        "managed-cell-initial",
        Dimension::AutonomicClosure,
        b"state=degraded".to_vec(),
    );
    let initial_grant = machine.derive_grant(&initial)?;
    let initial_receipt = actuator.actuate(&initial_grant, &initial)?;

    let mut cell = ManagedCell::new(b"state=healthy".to_vec(), initial_receipt.action_id);
    let cycle = AutonomicController::new(3).converge(&mut cell, &machine, &actuator)?;
    cycle.verify()?;

    assert!(cycle.converged);
    assert_eq!(cycle.cycles, 1);
    assert_eq!(cycle.actuation_receipts.len(), 1);
    assert_eq!(
        cycle.actuation_receipts[0].dimension,
        Dimension::AutonomicClosure
    );

    let final_path = actuator
        .root()
        .join("committed")
        .join(cell.current_action_id())
        .join("artifact.bin");
    let mut final_state = Vec::new();
    File::open(final_path)?.read_to_end(&mut final_state)?;
    assert_eq!(final_state, b"state=healthy");

    Ok(())
}
