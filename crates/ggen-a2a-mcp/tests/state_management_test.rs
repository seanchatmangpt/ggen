//! Chicago TDD integration tests for TogafStateManager, HandoffProtocol,
//! ArbApprovalManager, and ArtifactRegistry.
//!
//! Tests use real collaborators (no mocks) following Chicago TDD.

use ggen_a2a_mcp::state::{
    ApprovalCriterion, ApprovalDecision, ApprovalResponse, ApprovalStatus, ArbApprovalManager,
    ArbApprovalValidator, Artifact, ArtifactCompletenessValidator, ArtifactRegistry, ArtifactType,
    FiboConsistencyValidator, FiboValidationResult, HandoffPackage, HandoffProtocol, HandoffStatus,
    HandoffValidator, PhaseState, PhaseStatus, PhaseStatus::*, StakeholderRole, StateError,
    StateSummary, TogafPhase, TogafStateManager,
};

// ===========================================================================
// Helpers
// ===========================================================================

/// Build a PhaseState with the given status and completed turns.
fn phase_state(phase: TogafPhase, status: PhaseStatus) -> PhaseState {
    let mut ps = PhaseState::new(phase);
    let is_not_started = status == PhaseStatus::NotStarted;
    ps.status = status;
    if !is_not_started {
        ps.completed_turns = phase.turn_range().collect();
        ps.current_turn = phase.arb_gate();
    }
    ps
}

/// Build a test artifact with optional FIBO concepts.
fn fibo_artifact(turn: usize, phase: TogafPhase, name: &str, concepts: Vec<&str>) -> Artifact {
    Artifact::new(
        format!("{}-{}", turn, name.to_lowercase().replace(' ', "-")),
        turn,
        phase,
        ArtifactType::Other(name.to_string()),
        name,
        serde_json::json!({"turn": turn}),
    )
    .with_fibo_concepts(concepts.iter().map(|s| s.to_string()).collect())
}

/// Build a typed artifact.
fn typed_artifact(
    id: &str, turn: usize, phase: TogafPhase, name: &str, artifact_type: ArtifactType,
    concepts: Vec<&str>,
) -> Artifact {
    Artifact::new(
        id,
        turn,
        phase,
        artifact_type,
        name,
        serde_json::json!({"name": name}),
    )
    .with_fibo_concepts(concepts.iter().map(|s| s.to_string()).collect())
}

// ===========================================================================
// TogafPhase unit tests
// ===========================================================================

#[test]
fn phase_turn_ranges() {
    assert_eq!(TogafPhase::A.turn_range(), 1..=8);
    assert_eq!(TogafPhase::B.turn_range(), 9..=22);
    assert_eq!(TogafPhase::C.turn_range(), 23..=40);
    assert_eq!(TogafPhase::D.turn_range(), 41..=54);
    assert_eq!(TogafPhase::E.turn_range(), 55..=62);
    assert_eq!(TogafPhase::F.turn_range(), 63..=70);
}

#[test]
fn phase_from_turn() {
    assert_eq!(TogafPhase::from_turn(1).unwrap(), TogafPhase::A);
    assert_eq!(TogafPhase::from_turn(8).unwrap(), TogafPhase::A);
    assert_eq!(TogafPhase::from_turn(9).unwrap(), TogafPhase::B);
    assert_eq!(TogafPhase::from_turn(22).unwrap(), TogafPhase::B);
    assert_eq!(TogafPhase::from_turn(23).unwrap(), TogafPhase::C);
    assert_eq!(TogafPhase::from_turn(40).unwrap(), TogafPhase::C);
    assert_eq!(TogafPhase::from_turn(41).unwrap(), TogafPhase::D);
    assert_eq!(TogafPhase::from_turn(54).unwrap(), TogafPhase::D);
    assert_eq!(TogafPhase::from_turn(55).unwrap(), TogafPhase::E);
    assert_eq!(TogafPhase::from_turn(62).unwrap(), TogafPhase::E);
    assert_eq!(TogafPhase::from_turn(63).unwrap(), TogafPhase::F);
    assert_eq!(TogafPhase::from_turn(70).unwrap(), TogafPhase::F);
}

#[test]
fn phase_from_turn_invalid() {
    assert!(TogafPhase::from_turn(0).is_err());
    assert!(TogafPhase::from_turn(71).is_err());
    assert!(TogafPhase::from_turn(100).is_err());
}

#[test]
fn phase_arb_gates() {
    assert_eq!(TogafPhase::A.arb_gate(), 8);
    assert_eq!(TogafPhase::B.arb_gate(), 22);
    assert_eq!(TogafPhase::C.arb_gate(), 40);
    assert_eq!(TogafPhase::D.arb_gate(), 54);
    assert_eq!(TogafPhase::E.arb_gate(), 62);
    assert_eq!(TogafPhase::F.arb_gate(), 70);
}

#[test]
fn phase_next() {
    assert_eq!(TogafPhase::A.next(), Some(TogafPhase::B));
    assert_eq!(TogafPhase::B.next(), Some(TogafPhase::C));
    assert_eq!(TogafPhase::E.next(), Some(TogafPhase::F));
    assert_eq!(TogafPhase::F.next(), None);
}

// ===========================================================================
// TogafStateManager tests
// ===========================================================================

#[tokio::test]
async fn state_manager_initial_state() {
    let mgr = TogafStateManager::standard_70();
    assert_eq!(mgr.current_turn(), 1);
    assert_eq!(mgr.total_turns(), 70);
    assert!(!mgr.is_complete());
}

#[tokio::test]
async fn state_manager_advance_single_turn() {
    let mgr = TogafStateManager::standard_70();
    let record = mgr.advance_turn().await.unwrap();
    assert_eq!(record.turn, 1);
    assert_eq!(record.phase, TogafPhase::A);
    assert!(!record.is_arb_gate);
    assert_eq!(mgr.current_turn(), 2);
}

#[tokio::test]
async fn state_manager_detects_arb_gate() {
    let mgr = TogafStateManager::standard_70();
    for _ in 1..8 {
        let _ = mgr.advance_turn().await.unwrap();
    }
    let record = mgr.advance_turn().await.unwrap();
    assert_eq!(record.turn, 8);
    assert!(record.is_arb_gate);
    assert_eq!(record.phase_status, PhaseStatus::ArbPending);
}

#[tokio::test]
async fn state_manager_blocks_at_arb_gate() {
    let mgr = TogafStateManager::standard_70();
    for _ in 1..8 {
        let _ = mgr.advance_turn().await.unwrap();
    }
    let _ = mgr.advance_turn().await.unwrap(); // Turn 8 -- ArbPending

    let result = mgr.advance_turn().await;
    assert!(result.is_err());
    match result.unwrap_err() {
        StateError::ArbGateNotApproved(8) => {}
        other => panic!("Expected ArbGateNotApproved(8), got: {:?}", other),
    }
}

#[tokio::test]
async fn state_manager_approve_gate_and_continue() {
    let mgr = TogafStateManager::standard_70();
    for _ in 1..8 {
        let _ = mgr.advance_turn().await.unwrap();
    }
    let _ = mgr.advance_turn().await.unwrap(); // Turn 8

    mgr.approve_arb_gate(8).await.unwrap();

    let record = mgr.advance_turn().await.unwrap();
    assert_eq!(record.turn, 9);
    assert_eq!(record.phase, TogafPhase::B);
}

#[tokio::test]
async fn state_manager_turn_exhausted() {
    let mgr = TogafStateManager::new(3);
    mgr.advance_turn().await.unwrap();
    mgr.advance_turn().await.unwrap();
    mgr.advance_turn().await.unwrap(); // Turn 3 (last)

    let result = mgr.advance_turn().await;
    assert!(result.is_err());
}

#[tokio::test]
async fn state_manager_clone_shares_state() {
    let mgr = TogafStateManager::new(5);
    let mgr2 = mgr.clone();

    mgr.advance_turn().await.unwrap();
    assert_eq!(mgr.current_turn(), 2);
    assert_eq!(mgr2.current_turn(), 2);
}

#[tokio::test]
async fn state_manager_get_phase_for_each_turn() {
    let mgr = TogafStateManager::standard_70();
    for turn in 1..=70 {
        let phase = mgr.get_phase(turn).await.unwrap();
        let range = phase.turn_range();
        assert!(range.contains(&turn), "Turn {} not in {:?}", turn, range);
    }
}

#[tokio::test]
async fn state_manager_get_phase_invalid() {
    let mgr = TogafStateManager::standard_70();
    assert!(mgr.get_phase(0).await.is_err());
    assert!(mgr.get_phase(71).await.is_err());
}

#[tokio::test]
async fn state_manager_is_arb_gate() {
    let mgr = TogafStateManager::standard_70();
    for turn in [8, 22, 40, 54, 62, 70] {
        assert!(mgr.is_arb_gate(turn).await, "Turn {} should be gate", turn);
    }
    for turn in [1, 5, 10, 30, 50, 60, 65] {
        assert!(
            !mgr.is_arb_gate(turn).await,
            "Turn {} should not be gate",
            turn
        );
    }
}

#[tokio::test]
async fn state_manager_phase_status_transitions() {
    let mgr = TogafStateManager::standard_70();

    // NotStarted
    let ps = mgr.get_phase_state(TogafPhase::A).await.unwrap();
    assert_eq!(ps.status, PhaseStatus::NotStarted);

    // InProgress after first turn
    mgr.advance_turn().await.unwrap();
    let ps = mgr.get_phase_state(TogafPhase::A).await.unwrap();
    assert_eq!(ps.status, PhaseStatus::InProgress);

    // ArbPending after gate turn
    for _ in 2..8 {
        mgr.advance_turn().await.unwrap();
    }
    mgr.advance_turn().await.unwrap(); // Turn 8
    let ps = mgr.get_phase_state(TogafPhase::A).await.unwrap();
    assert_eq!(ps.status, PhaseStatus::ArbPending);

    // ArbApproved after approval
    mgr.approve_arb_gate(8).await.unwrap();
    let ps = mgr.get_phase_state(TogafPhase::A).await.unwrap();
    assert_eq!(ps.status, PhaseStatus::ArbApproved);
}

#[tokio::test]
async fn state_manager_completed_turns_tracked() {
    let mgr = TogafStateManager::standard_70();
    mgr.advance_turn().await.unwrap();
    mgr.advance_turn().await.unwrap();
    mgr.advance_turn().await.unwrap();

    let ps = mgr.get_phase_state(TogafPhase::A).await.unwrap();
    assert_eq!(ps.completed_count(), 3);
    assert!(ps.completed_turns.contains(&1));
    assert!(ps.completed_turns.contains(&3));
    assert!(!ps.completed_turns.contains(&4));
}

#[tokio::test]
async fn state_manager_store_and_retrieve_artifacts() {
    let mgr = TogafStateManager::standard_70();

    let artifact = Artifact::new(
        "sm-1",
        1,
        TogafPhase::A,
        ArtifactType::StakeholderMap,
        "Stakeholder Map",
        serde_json::json!({"stakeholders": ["exec", "regulator"]}),
    )
    .with_fibo_concepts(vec!["fibo-fnd:LegalPerson".to_string()]);

    mgr.store_artifact(1, artifact).await.unwrap();

    let artifacts = mgr.get_artifacts_for_phase(TogafPhase::A).await.unwrap();
    assert_eq!(artifacts.len(), 1);
    assert_eq!(artifacts[0].artifact_type, ArtifactType::StakeholderMap);

    let phase_b = mgr.get_artifacts_for_phase(TogafPhase::B).await.unwrap();
    assert!(phase_b.is_empty());
}

#[tokio::test]
async fn state_manager_store_artifact_invalid_turn() {
    let mgr = TogafStateManager::standard_70();
    let artifact = fibo_artifact(0, TogafPhase::A, "test", vec![]);
    assert!(mgr.store_artifact(0, artifact).await.is_err());
}

#[tokio::test]
async fn state_manager_summary() {
    let mgr = TogafStateManager::standard_70();
    mgr.advance_turn().await.unwrap();
    mgr.advance_turn().await.unwrap();
    mgr.store_artifact(1, fibo_artifact(1, TogafPhase::A, "a1", vec![]))
        .await
        .unwrap();

    let summary = mgr.summary().await;
    assert_eq!(summary.current_turn, 3);
    assert_eq!(summary.total_turns, 70);
    assert!(!summary.is_complete);
    assert_eq!(summary.artifact_count, 1);
    assert_eq!(
        summary.phases.get(&TogafPhase::A),
        Some(&PhaseStatus::InProgress)
    );
}

// ===========================================================================
// ArtifactRegistry tests
// ===========================================================================

#[test]
fn registry_insert_and_get() {
    let mut reg = ArtifactRegistry::new();
    let artifact = typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "SM",
        ArtifactType::StakeholderMap,
        vec![],
    );
    reg.insert(artifact);
    assert_eq!(reg.count(), 1);
    assert!(reg.contains("a1"));
    assert!(reg.get("nonexistent").is_none());
}

#[test]
fn registry_index_by_turn() {
    let mut reg = ArtifactRegistry::new();
    reg.insert(typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "A1",
        ArtifactType::Other("X".into()),
        vec![],
    ));
    reg.insert(typed_artifact(
        "a2",
        1,
        TogafPhase::A,
        "A2",
        ArtifactType::Other("Y".into()),
        vec![],
    ));
    reg.insert(typed_artifact(
        "a3",
        2,
        TogafPhase::A,
        "A3",
        ArtifactType::Other("Z".into()),
        vec![],
    ));

    assert_eq!(reg.get_for_turn(1).len(), 2);
    assert_eq!(reg.get_for_turn(2).len(), 1);
    assert!(reg.get_for_turn(3).is_empty());
}

#[test]
fn registry_index_by_phase() {
    let mut reg = ArtifactRegistry::new();
    reg.insert(typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "A1",
        ArtifactType::Other("X".into()),
        vec![],
    ));
    reg.insert(typed_artifact(
        "a2",
        9,
        TogafPhase::B,
        "A2",
        ArtifactType::Other("Y".into()),
        vec![],
    ));

    assert_eq!(reg.get_for_phase(TogafPhase::A).len(), 1);
    assert_eq!(reg.get_for_phase(TogafPhase::B).len(), 1);
    assert!(reg.get_for_phase(TogafPhase::C).is_empty());
}

#[test]
fn registry_replace() {
    let mut reg = ArtifactRegistry::new();
    reg.insert(typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "V1",
        ArtifactType::Other("V1".into()),
        vec![],
    ));

    let v2 = typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "V2",
        ArtifactType::Other("V2".into()),
        vec![],
    );
    reg.insert(v2);

    assert_eq!(reg.count(), 1);
    assert_eq!(reg.get("a1").unwrap().name, "V2");
}

#[test]
fn registry_remove() {
    let mut reg = ArtifactRegistry::new();
    reg.insert(typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "A1",
        ArtifactType::Other("X".into()),
        vec![],
    ));
    reg.insert(typed_artifact(
        "a2",
        2,
        TogafPhase::B,
        "A2",
        ArtifactType::Other("Y".into()),
        vec![],
    ));

    assert!(reg.remove("a1").is_some());
    assert_eq!(reg.count(), 1);
    assert!(reg.get_for_turn(1).is_empty());
}

#[test]
fn registry_by_type() {
    let mut reg = ArtifactRegistry::new();
    reg.insert(typed_artifact(
        "a1",
        1,
        TogafPhase::A,
        "SM",
        ArtifactType::StakeholderMap,
        vec![],
    ));
    reg.insert(typed_artifact(
        "a2",
        2,
        TogafPhase::A,
        "AV",
        ArtifactType::ArchitectureVision,
        vec![],
    ));
    reg.insert(typed_artifact(
        "a3",
        3,
        TogafPhase::A,
        "SM2",
        ArtifactType::StakeholderMap,
        vec![],
    ));

    assert_eq!(reg.get_by_type(&ArtifactType::StakeholderMap).len(), 2);
    assert_eq!(reg.get_by_type(&ArtifactType::ArchitectureVision).len(), 1);
    assert!(reg.get_by_type(&ArtifactType::TechnologyCatalog).is_empty());
}

#[test]
fn registry_fibo_concepts() {
    let mut reg = ArtifactRegistry::new();
    reg.insert(fibo_artifact(
        1,
        TogafPhase::A,
        "A1",
        vec!["fibo-fnd:LegalPerson"],
    ));
    reg.insert(fibo_artifact(
        2,
        TogafPhase::B,
        "A2",
        vec!["fibo-fnd:LegalPerson", "fibo-lcc:LoanContract"],
    ));

    let concepts = reg.all_fibo_concepts();
    assert_eq!(concepts.len(), 2);
    assert!(concepts.contains("fibo-fnd:LegalPerson"));
    assert!(concepts.contains("fibo-lcc:LoanContract"));
}

#[test]
fn registry_empty() {
    let reg = ArtifactRegistry::new();
    assert_eq!(reg.count(), 0);
    assert!(reg.all().is_empty());
    assert!(reg.all_fibo_concepts().is_empty());
}

// ===========================================================================
// HandoffValidator tests
// ===========================================================================

#[tokio::test]
async fn arb_approval_validator_passes() {
    let validator = ArbApprovalValidator::new();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let result = validator.validate(&ps, &[], TogafPhase::B).await.unwrap();
    assert!(result.passed);
}

#[tokio::test]
async fn arb_approval_validator_fails_pending() {
    let validator = ArbApprovalValidator::new();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbPending);
    let result = validator.validate(&ps, &[], TogafPhase::B).await.unwrap();
    assert!(!result.passed);
}

#[tokio::test]
async fn artifact_completeness_passes() {
    let validator = ArtifactCompletenessValidator::new(2);
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts = vec![
        fibo_artifact(1, TogafPhase::A, "A1", vec![]),
        fibo_artifact(2, TogafPhase::A, "A2", vec![]),
    ];
    let result = validator
        .validate(&ps, &artifacts, TogafPhase::B)
        .await
        .unwrap();
    assert!(result.passed);
}

#[tokio::test]
async fn artifact_completeness_fails() {
    let validator = ArtifactCompletenessValidator::new(3);
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts = vec![fibo_artifact(1, TogafPhase::A, "A1", vec![])];
    let result = validator
        .validate(&ps, &artifacts, TogafPhase::B)
        .await
        .unwrap();
    assert!(!result.passed);
}

#[tokio::test]
async fn fibo_consistency_no_concepts() {
    let validator = FiboConsistencyValidator::new();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts = vec![fibo_artifact(1, TogafPhase::A, "A1", vec![])];
    let result = validator
        .validate(&ps, &artifacts, TogafPhase::B)
        .await
        .unwrap();
    assert!(result.passed);
}

#[tokio::test]
async fn fibo_consistency_with_concepts() {
    let validator = FiboConsistencyValidator::new();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifact = fibo_artifact(
        1,
        TogafPhase::A,
        "A1",
        vec!["fibo-fnd:LegalPerson", "fibo-lcc:LoanContract"],
    );
    let result = validator
        .validate(&ps, &[artifact], TogafPhase::B)
        .await
        .unwrap();
    assert!(result.passed);
    assert!(result.message.contains("2 unique FIBO concepts"));
}

// ===========================================================================
// HandoffProtocol tests
// ===========================================================================

#[tokio::test]
async fn handoff_accepts_valid() {
    let protocol = HandoffProtocol::with_standard_validators();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts = vec![
        fibo_artifact(1, TogafPhase::A, "SM", vec!["fibo-fnd:LegalPerson"]),
        fibo_artifact(3, TogafPhase::A, "Vision", vec!["fibo-fnd:Organization"]),
    ];

    let result = protocol
        .validate_handoff(&ps, &artifacts, TogafPhase::B)
        .await;
    assert_eq!(result.status, HandoffStatus::Accepted);
    assert!(result.validations.iter().all(|v| v.passed));
}

#[tokio::test]
async fn handoff_rejects_no_approval() {
    let protocol = HandoffProtocol::with_standard_validators();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbPending);
    let artifacts = vec![fibo_artifact(1, TogafPhase::A, "A1", vec![])];

    let result = protocol
        .validate_handoff(&ps, &artifacts, TogafPhase::B)
        .await;
    assert!(matches!(result.status, HandoffStatus::Rejected(_)));
}

#[tokio::test]
async fn handoff_rejects_no_artifacts() {
    let protocol = HandoffProtocol::with_standard_validators();
    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts: Vec<Artifact> = vec![];

    let result = protocol
        .validate_handoff(&ps, &artifacts, TogafPhase::B)
        .await;
    assert!(matches!(result.status, HandoffStatus::Rejected(_)));
}

#[tokio::test]
async fn handoff_custom_validator_chain() {
    let protocol = HandoffProtocol::new()
        .add_validator(Box::new(ArbApprovalValidator::new()))
        .add_validator(Box::new(ArtifactCompletenessValidator::new(3)));

    let ps = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts = vec![
        fibo_artifact(1, TogafPhase::A, "A1", vec![]),
        fibo_artifact(2, TogafPhase::A, "A2", vec![]),
    ];

    let result = protocol
        .validate_handoff(&ps, &artifacts, TogafPhase::B)
        .await;
    assert!(matches!(result.status, HandoffStatus::Rejected(_)));
}

#[tokio::test]
async fn handoff_sequential_phases() {
    let protocol = HandoffProtocol::with_standard_validators();

    // Phase A -> B
    let ps_a = phase_state(TogafPhase::A, PhaseStatus::ArbApproved);
    let artifacts_a = vec![
        fibo_artifact(1, TogafPhase::A, "SM", vec!["fibo-fnd:LegalPerson"]),
        fibo_artifact(3, TogafPhase::A, "Vision", vec!["fibo-fnd:Organization"]),
    ];
    let result_a = protocol
        .validate_handoff(&ps_a, &artifacts_a, TogafPhase::B)
        .await;
    assert_eq!(result_a.status, HandoffStatus::Accepted);

    // Phase B -> C
    let ps_b = phase_state(TogafPhase::B, PhaseStatus::ArbApproved);
    let artifacts_b = vec![
        fibo_artifact(9, TogafPhase::B, "CapMap", vec!["fibo-fnd:LegalPerson"]),
        fibo_artifact(
            11,
            TogafPhase::B,
            "ProductMap",
            vec!["fibo-lcc:LoanContract"],
        ),
    ];
    let result_b = protocol
        .validate_handoff(&ps_b, &artifacts_b, TogafPhase::C)
        .await;
    assert_eq!(result_b.status, HandoffStatus::Accepted);

    // Phase C -> D
    let ps_c = phase_state(TogafPhase::C, PhaseStatus::ArbApproved);
    let artifacts_c = vec![fibo_artifact(
        23,
        TogafPhase::C,
        "DataCatalog",
        vec!["fibo-fnd:LegalPerson"],
    )];
    let result_c = protocol
        .validate_handoff(&ps_c, &artifacts_c, TogafPhase::D)
        .await;
    assert_eq!(result_c.status, HandoffStatus::Accepted);
}

#[tokio::test]
async fn handoff_package_fibo_consistency() {
    let pkg =
        HandoffPackage::new(TogafPhase::A, TogafPhase::B, vec![]).with_fibo_validations(vec![
            FiboValidationResult {
                concept: "fibo-fnd:LegalPerson".to_string(),
                is_consistent: true,
                details: "OK".to_string(),
            },
            FiboValidationResult {
                concept: "fibo-unknown:Bad".to_string(),
                is_consistent: false,
                details: "Not found".to_string(),
            },
        ]);
    assert!(!pkg.all_fibo_consistent());

    let pkg_ok =
        HandoffPackage::new(TogafPhase::A, TogafPhase::B, vec![]).with_fibo_validations(vec![
            FiboValidationResult {
                concept: "fibo-fnd:LegalPerson".to_string(),
                is_consistent: true,
                details: "OK".to_string(),
            },
        ]);
    assert!(pkg_ok.all_fibo_consistent());
}

// ===========================================================================
// ArbApprovalManager tests
// ===========================================================================

#[tokio::test]
async fn standard_gates_exist() {
    let mgr = ArbApprovalManager::with_standard_gates();
    let turns = mgr.gate_turns().await;

    for turn in [8, 22, 40, 54, 62, 70] {
        assert!(turns.contains(&turn), "Missing gate at turn {}", turn);
    }
}

#[tokio::test]
async fn standard_gates_correct_reviewers() {
    let mgr = ArbApprovalManager::with_standard_gates();

    let gate_a = mgr.get_gate(8).await.unwrap();
    assert_eq!(gate_a.phase, TogafPhase::A);
    assert_eq!(
        gate_a.required_reviewers,
        vec![StakeholderRole::ChiefArchitect]
    );

    let gate_b = mgr.get_gate(22).await.unwrap();
    assert!(gate_b
        .required_reviewers
        .contains(&StakeholderRole::ChiefArchitect));
    assert!(gate_b
        .required_reviewers
        .contains(&StakeholderRole::ComplianceOfficer));

    let gate_e = mgr.get_gate(62).await.unwrap();
    assert_eq!(
        gate_e.required_reviewers,
        vec![StakeholderRole::ComplianceOfficer]
    );
}

#[tokio::test]
async fn stakeholder_checkpoints() {
    let mgr = ArbApprovalManager::with_standard_gates();
    let turns = mgr.gate_turns().await;
    for turn in [10, 25, 45, 65] {
        assert!(turns.contains(&turn), "Missing checkpoint at turn {}", turn);
    }
}

#[tokio::test]
async fn single_reviewer_approval() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(8).await.unwrap();
    assert!(!mgr.is_approved(8).await);

    mgr.submit_response(
        8,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "Vision is solid",
        ),
    )
    .await
    .unwrap();
    assert!(mgr.is_approved(8).await);
}

#[tokio::test]
async fn single_reviewer_rejection() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(8).await.unwrap();

    mgr.submit_response(
        8,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Rejected("Too vague".to_string()),
            "Need metrics",
        ),
    )
    .await
    .unwrap();

    assert!(!mgr.is_approved(8).await);
    let approval = mgr.get_approval(8).await.unwrap();
    assert!(matches!(approval.status, ApprovalStatus::Rejected(_)));
}

#[tokio::test]
async fn multi_reviewer_sequential() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(22).await.unwrap();

    mgr.submit_response(
        22,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "OK",
        ),
    )
    .await
    .unwrap();
    assert!(!mgr.is_approved(22).await);

    mgr.submit_response(
        22,
        ApprovalResponse::new(
            StakeholderRole::ComplianceOfficer,
            ApprovalDecision::Approved,
            "Compliant",
        ),
    )
    .await
    .unwrap();
    assert!(mgr.is_approved(22).await);
}

#[tokio::test]
async fn multi_reviewer_partial_rejection() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(22).await.unwrap();

    mgr.submit_response(
        22,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "OK",
        ),
    )
    .await
    .unwrap();

    mgr.submit_response(
        22,
        ApprovalResponse::new(
            StakeholderRole::ComplianceOfficer,
            ApprovalDecision::Rejected("FIBO gaps".to_string()),
            "Missing mappings",
        ),
    )
    .await
    .unwrap();

    assert!(!mgr.is_approved(22).await);
}

#[tokio::test]
async fn unexpected_reviewer() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(8).await.unwrap();

    let result = mgr
        .submit_response(
            8,
            ApprovalResponse::new(
                StakeholderRole::BusinessOwner,
                ApprovalDecision::Approved,
                "Not mine",
            ),
        )
        .await;
    assert!(result.is_err());
    match result.unwrap_err() {
        StateError::UnexpectedReviewer(8, role) => assert!(role.contains("Business")),
        other => panic!("Expected UnexpectedReviewer, got: {:?}", other),
    }
}

#[tokio::test]
async fn duplicate_response() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(8).await.unwrap();

    mgr.submit_response(
        8,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "First",
        ),
    )
    .await
    .unwrap();

    let result = mgr
        .submit_response(
            8,
            ApprovalResponse::new(
                StakeholderRole::ChiefArchitect,
                ApprovalDecision::Approved,
                "Dup",
            ),
        )
        .await;
    assert!(result.is_err());
    match result.unwrap_err() {
        StateError::DuplicateResponse(_, 8) => {}
        other => panic!("Expected DuplicateResponse, got: {:?}", other),
    }
}

#[tokio::test]
async fn gate_not_found() {
    let mgr = ArbApprovalManager::new();
    assert!(mgr.request_approval(99).await.is_err());
    // check_gate returns Ok(false) for non-existent gates -- not an error.
    assert!(!mgr.check_gate(99).await.unwrap());
    assert!(mgr.get_gate(99).await.is_err());
}

#[tokio::test]
async fn summary_shows_progress() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(8).await.unwrap();
    mgr.submit_response(
        8,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "OK",
        ),
    )
    .await
    .unwrap();

    mgr.request_approval(22).await.unwrap();
    mgr.submit_response(
        22,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "OK",
        ),
    )
    .await
    .unwrap();

    let summaries = mgr.summary().await;
    let turn8 = summaries.iter().find(|s| s.turn == 8).unwrap();
    assert_eq!(turn8.status, ApprovalStatus::Approved);
    assert!(turn8.missing_reviewers.is_empty());

    let turn22 = summaries.iter().find(|s| s.turn == 22).unwrap();
    assert_eq!(turn22.status, ApprovalStatus::Pending);
    assert_eq!(turn22.responded_count, 1);
    assert_eq!(turn22.required_count, 2);
}

#[tokio::test]
async fn needs_revision_blocks() {
    let mgr = ArbApprovalManager::with_standard_gates();
    mgr.request_approval(8).await.unwrap();

    mgr.submit_response(
        8,
        ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::NeedsRevision("Add metrics".to_string()),
            "Revise",
        ),
    )
    .await
    .unwrap();

    assert!(!mgr.is_approved(8).await);
    let approval = mgr.get_approval(8).await.unwrap();
    assert!(matches!(approval.status, ApprovalStatus::Rejected(_)));
}

// ===========================================================================
// Full protocol integration tests
// ===========================================================================

#[tokio::test]
async fn advance_through_all_70_turns() {
    let mgr = TogafStateManager::standard_70();
    // Gate turns are at the end of each phase (except the last).
    let gate_turns = [8, 22, 40, 54, 62];

    for turn in 1..=70 {
        match mgr.advance_turn().await {
            Ok(_record) => {
                // If we just completed a gate turn, approve it before the next.
                if gate_turns.contains(&turn) {
                    mgr.approve_arb_gate(turn).await.unwrap();
                }
            }
            Err(StateError::ArbGateNotApproved(gate_turn)) => {
                // The first turn of a new phase is blocked because the previous
                // phase's ARB gate hasn't been approved yet.
                mgr.approve_arb_gate(gate_turn).await.unwrap();
                mgr.advance_turn().await.unwrap();
            }
            Err(e) => {
                panic!("Turn {} failed unexpectedly: {:?}", turn, e);
            }
        }
    }

    assert!(mgr.is_complete());
    assert_eq!(mgr.current_turn(), 71);
}

#[tokio::test]
async fn full_70_turn_arb_approval_sequence() {
    let mgr = ArbApprovalManager::with_standard_gates();
    let gate_turns = mgr.gate_turns().await;

    for turn in &gate_turns {
        mgr.request_approval(*turn).await.unwrap();
        let gate = mgr.get_gate(*turn).await.unwrap();

        for reviewer in &gate.required_reviewers {
            mgr.submit_response(
                *turn,
                ApprovalResponse::new(reviewer.clone(), ApprovalDecision::Approved, "Approved"),
            )
            .await
            .unwrap();
        }

        assert!(
            mgr.is_approved(*turn).await,
            "Gate at turn {} should be approved",
            turn
        );
    }

    let summary = mgr.summary().await;
    assert!(summary.iter().all(|s| s.status == ApprovalStatus::Approved));
}
