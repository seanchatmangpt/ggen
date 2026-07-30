//! Integration proofs for lifecycle, impact, capacity, receipts, Fortune 5, and zero actuation.

use std::{collections::BTreeMap, error::Error};

use ggen_architecture::{
    ArchitectureAsset, ArchitectureRegistry, ArchitectureState, AssetKind, AutonomicController,
    AutonomicPolicy, CapacityEnvelope, CapacityLevel, CapacityPolicy, CapacitySample, DoctorReport,
    DoctorStatus, Fortune5Assessment, Fortune5AutonomicPlan, Fortune5Catalog, Fortune5IntentKind,
    Fortune5Program, LifecycleState, Standing, Stimulus, WorkloadVector,
};

fn active_asset(id: &str, kind: AssetKind) -> ArchitectureAsset {
    let mut asset = ArchitectureAsset::new(id, id, kind);
    asset.lifecycle = LifecycleState::Active;
    asset.standing = Standing::Alive;
    asset.owner = Some("architecture-board".to_string());
    asset.version = Some("26.7.30".to_string());
    asset
}

fn base_state() -> Result<ArchitectureState, Box<dyn Error>> {
    let mut registry = ArchitectureRegistry::new();
    registry.register(active_asset("ontology-registry", AssetKind::Ontology))?;
    registry.register(active_asset("ggen-engine", AssetKind::Component))?;
    registry.register(active_asset("distribution-pack", AssetKind::Pack))?;
    registry.add_dependency("ggen-engine", "ontology-registry")?;
    registry.add_dependency("distribution-pack", "ggen-engine")?;

    Ok(ArchitectureState {
        schema_version: 1,
        name: "test-enterprise".to_string(),
        registry,
        capacity_samples: Vec::new(),
        capacity_policy: CapacityPolicy::default(),
        autonomic_policy: AutonomicPolicy::default(),
    })
}

#[test]
fn lifecycle_state_machine_refuses_skipped_promotion() -> Result<(), Box<dyn Error>> {
    let mut registry = ArchitectureRegistry::new();
    registry.register(ArchitectureAsset::new(
        "candidate",
        "Candidate",
        AssetKind::Ontology,
    ))?;

    assert!(registry
        .transition("candidate", LifecycleState::Active)
        .is_err());
    registry.transition("candidate", LifecycleState::Identified)?;
    registry.transition("candidate", LifecycleState::Qualified)?;
    registry.transition("candidate", LifecycleState::Admitted)?;
    registry.transition("candidate", LifecycleState::Active)?;
    assert_eq!(
        registry.asset("candidate")?.lifecycle,
        LifecycleState::Active
    );
    Ok(())
}

#[test]
fn impact_analysis_returns_transitive_dependents_in_dependency_order() -> Result<(), Box<dyn Error>>
{
    let state = base_state()?;
    let impact = state.registry.impact_report("ontology-registry")?;

    assert_eq!(
        impact.ordered_revalidation,
        vec![
            "ontology-registry".to_string(),
            "ggen-engine".to_string(),
            "distribution-pack".to_string()
        ]
    );
    assert_eq!(impact.affected.len(), 3);
    Ok(())
}

#[test]
fn dependency_cycle_is_a_registry_refusal() -> Result<(), Box<dyn Error>> {
    let mut registry = ArchitectureRegistry::new();
    registry.register(active_asset("left", AssetKind::Component))?;
    registry.register(active_asset("right", AssetKind::Component))?;
    registry.add_dependency("left", "right")?;
    registry.add_dependency("right", "left")?;

    assert!(registry
        .validate()
        .iter()
        .any(|finding| finding.code == "EA-REG-005"));
    Ok(())
}

#[test]
fn capacity_envelope_detects_warning_refusal_and_nonlinear_knee() {
    let policy = CapacityPolicy {
        warn_elapsed_ms: 50,
        refuse_elapsed_ms: 150,
        warn_memory_bytes: 1_000,
        refuse_memory_bytes: 10_000,
        max_documents: None,
        max_quads: None,
        knee_slope_ratio: 2.0,
    };
    let samples = vec![
        sample("small", 10, 10, 100),
        sample("warning", 20, 60, 200),
        sample("refusal", 30, 200, 300),
    ];
    let envelope = CapacityEnvelope::analyze(&samples, &policy);

    assert_eq!(envelope.first_warning.as_deref(), Some("warning"));
    assert_eq!(envelope.first_refusal.as_deref(), Some("refusal"));
    assert_eq!(envelope.first_knee.as_deref(), Some("refusal"));
    assert_eq!(envelope.latest_level, CapacityLevel::Refuse);
}

#[test]
fn doctor_refuses_direct_autonomic_actuation() -> Result<(), Box<dyn Error>> {
    let mut state = base_state()?;
    state.autonomic_policy.direct_actuation_allowed = true;
    let report = DoctorReport::analyze(&state)?;

    assert_eq!(report.status, DoctorStatus::Refused);
    assert!(report
        .findings
        .iter()
        .any(|finding| finding.code == "EA-AUTO-001"));
    Ok(())
}

#[test]
fn autonomic_cycle_emits_intents_but_performs_no_actuation() -> Result<(), Box<dyn Error>> {
    let mut state = base_state()?;
    state.capacity_policy.warn_elapsed_ms = 25;
    state.capacity_policy.refuse_elapsed_ms = 100;
    let stimuli = vec![
        Stimulus::AssetChanged {
            asset_id: "ontology-registry".to_string(),
        },
        Stimulus::Capacity {
            sample: sample("observed-warning", 40, 50, 500),
        },
    ];

    let cycle = AutonomicController::new(&state).run_cycle("sequence-1", stimuli)?;

    assert!(!cycle.actuation_performed);
    assert!(!cycle.intents.is_empty());
    assert!(cycle
        .intents
        .iter()
        .all(|intent| !intent.required_capabilities.is_empty()));
    assert_eq!(cycle.cycle_id, cycle.receipt_hash);
    Ok(())
}

#[test]
fn receipt_is_deterministic_for_identical_cycle() -> Result<(), Box<dyn Error>> {
    let state = base_state()?;
    let stimuli = vec![Stimulus::AssetChanged {
        asset_id: "ggen-engine".to_string(),
    }];

    let left = AutonomicController::new(&state).run_cycle("sequence-2", stimuli.clone())?;
    let right = AutonomicController::new(&state).run_cycle("sequence-2", stimuli)?;
    assert_eq!(left.receipt_hash, right.receipt_hash);
    Ok(())
}

#[test]
fn canonical_repository_state_parses_and_has_closed_dependencies() -> Result<(), Box<dyn Error>> {
    let state: ArchitectureState =
        serde_json::from_str(include_str!("../../../architecture/ggen-enterprise.json"))?;
    assert!(state.registry.validate().is_empty());
    assert!(state.registry.assets.len() >= 10);
    Ok(())
}

#[test]
fn fortune5_catalog_is_conjunctive_twenty_one_by_three() {
    let catalog = Fortune5Catalog::canonical();

    assert!(catalog.validate().is_empty());
    assert_eq!(catalog.dimensions.len(), 21);
    assert_eq!(catalog.obligations().count(), 63);
    assert!(catalog
        .dimensions
        .iter()
        .all(|dimension| dimension.obligations.len() == 3));
}

#[test]
fn synthetic_fortune5_program_proves_the_assessment_machinery() -> Result<(), Box<dyn Error>> {
    let program: Fortune5Program = serde_json::from_str(include_str!(
        "../../../architecture/fortune5/synthetic-level5.json"
    ))?;
    let assessment = Fortune5Assessment::assess(&program)?;

    assert!(assessment.synthetic);
    assert!(assessment.level_five_ready);
    assert_eq!(assessment.maturity_level, 5);
    assert_eq!(assessment.alive_dimensions, 21);
    assert_eq!(assessment.passed_obligations, 63);
    assert!(assessment.findings.is_empty());
    Ok(())
}

#[test]
fn synthetic_fortune5_level_five_cannot_authorize_promotion() -> Result<(), Box<dyn Error>> {
    let program: Fortune5Program = serde_json::from_str(include_str!(
        "../../../architecture/fortune5/synthetic-level5.json"
    ))?;
    let assessment = Fortune5Assessment::assess(&program)?;
    let plan = Fortune5AutonomicPlan::plan(&assessment)?;

    assert!(!plan.actuation_performed);
    assert_eq!(plan.intents.len(), 1);
    assert_eq!(plan.intents[0].kind, Fortune5IntentKind::Reverify);
    assert!(plan
        .intents
        .iter()
        .all(|intent| !intent.required_capabilities.is_empty()));
    Ok(())
}

#[test]
fn fortune5_segregation_violation_blocks_and_plans_repair() -> Result<(), Box<dyn Error>> {
    let mut program: Fortune5Program = serde_json::from_str(include_str!(
        "../../../architecture/fortune5/synthetic-level5.json"
    ))?;
    if let Some(first) = program.evidence.first_mut() {
        first.approver.clone_from(&first.producer);
    }
    let assessment = Fortune5Assessment::assess(&program)?;
    let plan = Fortune5AutonomicPlan::plan(&assessment)?;

    assert!(!assessment.level_five_ready);
    assert_eq!(assessment.standing, Standing::Blocked);
    assert!(assessment
        .findings
        .iter()
        .any(|finding| finding.code == "F5-SOD-001"));
    assert!(plan
        .intents
        .iter()
        .any(|intent| intent.kind == Fortune5IntentKind::BlockPromotion));
    assert!(plan
        .intents
        .iter()
        .any(|intent| intent.kind == Fortune5IntentKind::RepairSegregation));
    assert!(!plan.actuation_performed);
    Ok(())
}

fn sample(label: &str, documents: u64, elapsed_ms: u64, memory: u64) -> CapacitySample {
    CapacitySample {
        label: label.to_string(),
        workload: WorkloadVector {
            documents,
            quads: documents.saturating_mul(10),
            blank_nodes: 0,
            rules: 0,
            shapes: 0,
            templates: 0,
            projections: 0,
        },
        elapsed_ms,
        peak_memory_bytes: memory,
        phase_ms: BTreeMap::new(),
    }
}
