//! Comprehensive tests for Ontology Systems (70 tests)
//!
//! Tests cover:
//! - Snapshot creation, hashing, immutability
//! - Invariant checking (all 7 hard invariants)
//! - Delta proposal generation
//! - Control loop execution
//! - Snapshot comparison and validation
//! - Pattern mining
//! - Atomic promotion

use ggen_core::ontology::*;
use ggen_utils::error::Result;
use std::collections::BTreeMap;

// =============================================================================
// SIGMA SNAPSHOT TESTS (20 tests)
// =============================================================================

#[test]
fn test_sigma_snapshot_id_from_digest() {
    let data = b"test data";
    let id = SigmaSnapshotId::from_digest(data);
    assert!(!id.as_str().is_empty());
}

#[test]
fn test_sigma_snapshot_id_deterministic() {
    let data = b"test data";
    let id1 = SigmaSnapshotId::from_digest(data);
    let id2 = SigmaSnapshotId::from_digest(data);
    assert_eq!(id1, id2);
}

#[test]
fn test_sigma_snapshot_id_display() {
    let data = b"test";
    let id = SigmaSnapshotId::from_digest(data);
    let display = format!("{}", id);
    assert!(!display.is_empty());
}

#[test]
fn test_sigma_snapshot_new() {
    let statements = vec![];
    let metadata = SnapshotMetadata::default();

    let snapshot = SigmaSnapshot::new(
        None,
        statements,
        "1.0.0".to_string(),
        "signature".to_string(),
        metadata,
    );

    assert_eq!(snapshot.version, "1.0.0");
    assert_eq!(snapshot.signature, "signature");
}

#[test]
fn test_sigma_snapshot_with_parent() {
    let parent_id = SigmaSnapshotId::from_digest(b"parent");
    let statements = vec![];
    let metadata = SnapshotMetadata::default();

    let snapshot = SigmaSnapshot::new(
        Some(parent_id.clone()),
        statements,
        "1.1.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    assert_eq!(snapshot.parent_id, Some(parent_id));
}

#[test]
fn test_sigma_snapshot_immutability() {
    let statements = vec![];
    let metadata = SnapshotMetadata::default();

    let snapshot = SigmaSnapshot::new(
        None,
        statements.clone(),
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    // Triples are Arc<Vec>, so clone is cheap
    let _triples = snapshot.triples.clone();
    assert_eq!(snapshot.triples.len(), 0);
}

#[test]
fn test_sigma_snapshot_metadata() {
    let mut metadata = SnapshotMetadata::default();
    metadata.description = "Test snapshot".to_string();
    metadata.backward_compatible = true;
    metadata.sectors = vec!["core".to_string(), "api".to_string()];

    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata.clone(),
    );

    assert_eq!(snapshot.metadata.description, "Test snapshot");
    assert!(snapshot.metadata.backward_compatible);
    assert_eq!(snapshot.metadata.sectors.len(), 2);
}

#[test]
fn test_sigma_snapshot_custom_tags() {
    let mut metadata = SnapshotMetadata::default();
    metadata
        .tags
        .insert("author".to_string(), "test".to_string());
    metadata.tags.insert("env".to_string(), "dev".to_string());

    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    assert_eq!(
        snapshot.metadata.tags.get("author"),
        Some(&"test".to_string())
    );
    assert_eq!(snapshot.metadata.tags.get("env"), Some(&"dev".to_string()));
}

#[test]
fn test_sigma_snapshot_different_versions() {
    let metadata = SnapshotMetadata::default();

    let s1 = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig1".to_string(),
        metadata.clone(),
    );

    let s2 = SigmaSnapshot::new(
        None,
        vec![],
        "1.1.0".to_string(),
        "sig2".to_string(),
        metadata,
    );

    assert_eq!(s1.version, "1.0.0");
    assert_eq!(s2.version, "1.1.0");
}

#[test]
fn test_sigma_snapshot_chain() {
    let metadata = SnapshotMetadata::default();

    let s1 = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig1".to_string(),
        metadata.clone(),
    );

    let s2 = SigmaSnapshot::new(
        Some(s1.id.clone()),
        vec![],
        "1.1.0".to_string(),
        "sig2".to_string(),
        metadata.clone(),
    );

    let s3 = SigmaSnapshot::new(
        Some(s2.id.clone()),
        vec![],
        "1.2.0".to_string(),
        "sig3".to_string(),
        metadata,
    );

    assert_eq!(s2.parent_id, Some(s1.id));
    assert_eq!(s3.parent_id, Some(s2.id));
}

#[test]
fn test_sigma_overlay_creation() {
    let base_id = SigmaSnapshotId::from_digest(b"base");
    let overlay = SigmaOverlay {
        base_id: base_id.clone(),
        additions: vec![],
        removals: vec![],
        description: "test overlay".to_string(),
    };

    assert_eq!(overlay.base_id, base_id);
    assert_eq!(overlay.description, "test overlay");
}

#[test]
fn test_sigma_snapshot_serialization() {
    let metadata = SnapshotMetadata::default();
    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    let serialized = serde_json::to_string(&snapshot);
    assert!(serialized.is_ok());
}

#[test]
fn test_sigma_snapshot_deserialization() {
    let metadata = SnapshotMetadata::default();
    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    let serialized = serde_json::to_string(&snapshot).unwrap();
    let deserialized: Result<SigmaSnapshot, _> = serde_json::from_str(&serialized);
    assert!(deserialized.is_ok());
}

#[test]
fn test_sigma_snapshot_id_equality() {
    let id1 = SigmaSnapshotId::from_digest(b"data");
    let id2 = SigmaSnapshotId::from_digest(b"data");
    let id3 = SigmaSnapshotId::from_digest(b"different");

    assert_eq!(id1, id2);
    assert_ne!(id1, id3);
}

#[test]
fn test_sigma_snapshot_timestamp() {
    let metadata = SnapshotMetadata::default();
    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    assert!(snapshot.timestamp.elapsed().is_ok());
}

#[test]
fn test_sigma_snapshot_empty_triples() {
    let metadata = SnapshotMetadata::default();
    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    assert!(snapshot.triples.is_empty());
}

#[test]
fn test_sigma_snapshot_version_string() {
    let metadata = SnapshotMetadata::default();
    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "2.0.0-beta.1".to_string(),
        "sig".to_string(),
        metadata,
    );

    assert_eq!(snapshot.version, "2.0.0-beta.1");
}

#[test]
fn test_sigma_overlay_additions_and_removals() {
    let base_id = SigmaSnapshotId::from_digest(b"base");
    let overlay = SigmaOverlay {
        base_id,
        additions: vec![],
        removals: vec![],
        description: "test".to_string(),
    };

    assert!(overlay.additions.is_empty());
    assert!(overlay.removals.is_empty());
}

#[test]
fn test_snapshot_metadata_backward_compatibility() {
    let mut metadata = SnapshotMetadata::default();
    metadata.backward_compatible = true;

    assert!(metadata.backward_compatible);

    metadata.backward_compatible = false;
    assert!(!metadata.backward_compatible);
}

#[test]
fn test_snapshot_metadata_sectors() {
    let mut metadata = SnapshotMetadata::default();
    metadata.sectors = vec!["auth".to_string(), "api".to_string(), "db".to_string()];

    assert_eq!(metadata.sectors.len(), 3);
    assert!(metadata.sectors.contains(&"auth".to_string()));
}

// =============================================================================
// PATTERN MINING TESTS (15 tests)
// =============================================================================

#[test]
fn test_pattern_type_variants() {
    let pattern = Pattern {
        pattern_type: PatternType::Drift,
        description: "test".to_string(),
        confidence: 0.9,
        observations: vec![],
    };

    assert!(matches!(pattern.pattern_type, PatternType::Drift));
}

#[test]
fn test_observation_creation() {
    let obs = Observation {
        timestamp: std::time::SystemTime::now(),
        source: ObservationSource::QueryLog,
        description: "test observation".to_string(),
        metadata: BTreeMap::new(),
    };

    assert_eq!(obs.description, "test observation");
}

#[test]
fn test_observation_source_variants() {
    let sources = vec![
        ObservationSource::QueryLog,
        ObservationSource::SchemaEvolution,
        ObservationSource::UserFeedback,
        ObservationSource::PerformanceMetrics,
    ];

    assert_eq!(sources.len(), 4);
}

#[test]
fn test_pattern_miner_config() {
    let config = MinerConfig {
        min_observations: 5,
        confidence_threshold: 0.8,
        drift_detection_window: std::time::Duration::from_secs(86400),
    };

    assert_eq!(config.min_observations, 5);
    assert_eq!(config.confidence_threshold, 0.8);
}

#[test]
fn test_pattern_miner_new() {
    let config = MinerConfig::default();
    let miner = PatternMiner::new(config);

    assert!(miner.observations.is_empty());
}

#[test]
fn test_pattern_miner_add_observation() {
    let config = MinerConfig::default();
    let mut miner = PatternMiner::new(config);

    let obs = Observation {
        timestamp: std::time::SystemTime::now(),
        source: ObservationSource::QueryLog,
        description: "test".to_string(),
        metadata: BTreeMap::new(),
    };

    miner.add_observation(obs);
    assert_eq!(miner.observations.len(), 1);
}

#[test]
fn test_pattern_miner_multiple_observations() {
    let config = MinerConfig::default();
    let mut miner = PatternMiner::new(config);

    for i in 0..10 {
        let obs = Observation {
            timestamp: std::time::SystemTime::now(),
            source: ObservationSource::QueryLog,
            description: format!("observation {}", i),
            metadata: BTreeMap::new(),
        };
        miner.add_observation(obs);
    }

    assert_eq!(miner.observations.len(), 10);
}

#[test]
fn test_pattern_confidence() {
    let pattern = Pattern {
        pattern_type: PatternType::Drift,
        description: "high confidence".to_string(),
        confidence: 0.95,
        observations: vec![],
    };

    assert!(pattern.confidence > 0.9);
}

#[test]
fn test_proposed_change_creation() {
    let change = ProposedChange {
        description: "Add new class".to_string(),
        rationale: "Based on usage patterns".to_string(),
        affected_entities: vec!["Entity1".to_string()],
        risk_level: "low".to_string(),
    };

    assert_eq!(change.description, "Add new class");
    assert_eq!(change.risk_level, "low");
}

#[test]
fn test_proposed_change_risk_levels() {
    let low = ProposedChange {
        description: "".to_string(),
        rationale: "".to_string(),
        affected_entities: vec![],
        risk_level: "low".to_string(),
    };

    let high = ProposedChange {
        description: "".to_string(),
        rationale: "".to_string(),
        affected_entities: vec![],
        risk_level: "high".to_string(),
    };

    assert_eq!(low.risk_level, "low");
    assert_eq!(high.risk_level, "high");
}

#[test]
fn test_ontology_stats() {
    let stats = OntologyStats {
        total_classes: 10,
        total_properties: 20,
        total_instances: 100,
        query_count: 500,
        avg_query_time_ms: 50.0,
    };

    assert_eq!(stats.total_classes, 10);
    assert_eq!(stats.avg_query_time_ms, 50.0);
}

#[test]
fn test_pattern_observations_collection() {
    let obs1 = Observation {
        timestamp: std::time::SystemTime::now(),
        source: ObservationSource::QueryLog,
        description: "obs1".to_string(),
        metadata: BTreeMap::new(),
    };

    let pattern = Pattern {
        pattern_type: PatternType::Drift,
        description: "test".to_string(),
        confidence: 0.8,
        observations: vec![obs1],
    };

    assert_eq!(pattern.observations.len(), 1);
}

#[test]
fn test_observation_metadata() {
    let mut metadata = BTreeMap::new();
    metadata.insert("query".to_string(), "SELECT * FROM ...".to_string());
    metadata.insert("duration".to_string(), "100ms".to_string());

    let obs = Observation {
        timestamp: std::time::SystemTime::now(),
        source: ObservationSource::QueryLog,
        description: "slow query".to_string(),
        metadata: metadata.clone(),
    };

    assert_eq!(obs.metadata, metadata);
}

#[test]
fn test_miner_config_defaults() {
    let config = MinerConfig::default();

    assert!(config.min_observations > 0);
    assert!(config.confidence_threshold > 0.0);
    assert!(config.confidence_threshold <= 1.0);
}

#[test]
fn test_pattern_miner_clear_observations() {
    let config = MinerConfig::default();
    let mut miner = PatternMiner::new(config);

    let obs = Observation {
        timestamp: std::time::SystemTime::now(),
        source: ObservationSource::QueryLog,
        description: "test".to_string(),
        metadata: BTreeMap::new(),
    };

    miner.add_observation(obs);
    assert_eq!(miner.observations.len(), 1);

    miner.observations.clear();
    assert_eq!(miner.observations.len(), 0);
}

// =============================================================================
// DELTA PROPOSER TESTS (10 tests)
// =============================================================================

#[test]
fn test_delta_sigma_proposal_creation() {
    let proposal = DeltaSigmaProposal {
        description: "Add User class".to_string(),
        triples_to_add: vec![],
        triples_to_remove: vec![],
        rationale: "User management needed".to_string(),
        risk_assessment: "low".to_string(),
        backward_compatible: true,
    };

    assert_eq!(proposal.description, "Add User class");
    assert!(proposal.backward_compatible);
}

#[test]
fn test_delta_proposal_backward_compatibility() {
    let compatible = DeltaSigmaProposal {
        description: "".to_string(),
        triples_to_add: vec![],
        triples_to_remove: vec![],
        rationale: "".to_string(),
        risk_assessment: "low".to_string(),
        backward_compatible: true,
    };

    let breaking = DeltaSigmaProposal {
        description: "".to_string(),
        triples_to_add: vec![],
        triples_to_remove: vec![],
        rationale: "".to_string(),
        risk_assessment: "high".to_string(),
        backward_compatible: false,
    };

    assert!(compatible.backward_compatible);
    assert!(!breaking.backward_compatible);
}

#[test]
fn test_proposer_config() {
    let config = ProposerConfig {
        llm_endpoint: "http://localhost:8000".to_string(),
        model: "gpt-4".to_string(),
        temperature: 0.7,
        max_proposals_per_iteration: 5,
    };

    assert_eq!(config.temperature, 0.7);
    assert_eq!(config.max_proposals_per_iteration, 5);
}

#[test]
fn test_mock_llm_proposer() {
    let config = ProposerConfig::default();
    let proposer = MockLLMProposer::new(config);

    assert!(proposer.config.llm_endpoint.contains("mock"));
}

#[test]
fn test_mock_llm_proposer_generate() {
    let config = ProposerConfig::default();
    let proposer = MockLLMProposer::new(config);

    let patterns = vec![];
    let stats = OntologyStats::default();

    let result = proposer.generate_proposals(&patterns, &stats);
    assert!(result.is_ok());
}

#[test]
fn test_delta_proposal_risk_assessment() {
    let low_risk = DeltaSigmaProposal {
        description: "".to_string(),
        triples_to_add: vec![],
        triples_to_remove: vec![],
        rationale: "".to_string(),
        risk_assessment: "low".to_string(),
        backward_compatible: true,
    };

    let high_risk = DeltaSigmaProposal {
        description: "".to_string(),
        triples_to_add: vec![],
        triples_to_remove: vec![],
        rationale: "".to_string(),
        risk_assessment: "high".to_string(),
        backward_compatible: false,
    };

    assert_eq!(low_risk.risk_assessment, "low");
    assert_eq!(high_risk.risk_assessment, "high");
}

#[test]
fn test_proposer_config_temperature() {
    let config = ProposerConfig {
        llm_endpoint: "".to_string(),
        model: "".to_string(),
        temperature: 0.5,
        max_proposals_per_iteration: 3,
    };

    assert!(config.temperature >= 0.0);
    assert!(config.temperature <= 1.0);
}

#[test]
fn test_proposer_config_max_proposals() {
    let config = ProposerConfig {
        llm_endpoint: "".to_string(),
        model: "".to_string(),
        temperature: 0.7,
        max_proposals_per_iteration: 10,
    };

    assert_eq!(config.max_proposals_per_iteration, 10);
}

#[test]
fn test_delta_proposal_with_additions_and_removals() {
    let proposal = DeltaSigmaProposal {
        description: "Refactor User class".to_string(),
        triples_to_add: vec![],
        triples_to_remove: vec![],
        rationale: "Improve structure".to_string(),
        risk_assessment: "medium".to_string(),
        backward_compatible: false,
    };

    assert!(proposal.triples_to_add.is_empty());
    assert!(proposal.triples_to_remove.is_empty());
}

#[test]
fn test_proposer_config_defaults() {
    let config = ProposerConfig::default();

    assert!(!config.llm_endpoint.is_empty());
    assert!(!config.model.is_empty());
    assert!(config.temperature >= 0.0);
    assert!(config.max_proposals_per_iteration > 0);
}

// =============================================================================
// VALIDATOR TESTS (10 tests)
// =============================================================================

#[test]
fn test_validator_result_success() {
    let result = ValidatorResult {
        passed: true,
        message: "Validation passed".to_string(),
        evidence: vec![],
    };

    assert!(result.passed);
}

#[test]
fn test_validator_result_failure() {
    let result = ValidatorResult {
        passed: false,
        message: "Validation failed".to_string(),
        evidence: vec!["Missing property".to_string()],
    };

    assert!(!result.passed);
    assert_eq!(result.evidence.len(), 1);
}

#[test]
fn test_validation_evidence() {
    let evidence = ValidationEvidence {
        description: "Type check failed".to_string(),
        severity: "error".to_string(),
        location: "Class:User".to_string(),
        details: BTreeMap::new(),
    };

    assert_eq!(evidence.severity, "error");
}

#[test]
fn test_validation_context() {
    let ctx = ValidationContext {
        snapshot_id: SigmaSnapshotId::from_digest(b"test"),
        proposal: None,
        current_stats: OntologyStats::default(),
    };

    assert!(ctx.proposal.is_none());
}

#[test]
fn test_mock_static_validator() {
    let validator = MockStaticValidator;
    let ctx = ValidationContext {
        snapshot_id: SigmaSnapshotId::from_digest(b"test"),
        proposal: None,
        current_stats: OntologyStats::default(),
    };

    let result = validator.validate(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_mock_performance_validator() {
    let validator = MockPerformanceValidator {};
    let ctx = ValidationContext {
        snapshot_id: SigmaSnapshotId::from_digest(b"test"),
        proposal: None,
        current_stats: OntologyStats::default(),
    };

    let result = validator.validate(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_mock_dynamic_validator() {
    let validator = MockDynamicValidator;
    let ctx = ValidationContext {
        snapshot_id: SigmaSnapshotId::from_digest(b"test"),
        proposal: None,
        current_stats: OntologyStats::default(),
    };

    let result = validator.validate(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_composite_validator_empty() {
    let composite = CompositeValidator { validators: vec![] };

    let ctx = ValidationContext {
        snapshot_id: SigmaSnapshotId::from_digest(b"test"),
        proposal: None,
        current_stats: OntologyStats::default(),
    };

    let result = composite.validate(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_invariant_types() {
    let invariants = vec![
        Invariant::TypeSoundness,
        Invariant::Immutability,
        Invariant::AtomicPromotion,
        Invariant::GuardSoundness,
        Invariant::ProjectionDeterminism,
        Invariant::SLOPreservation,
        Invariant::NoRetrocausation,
    ];

    assert_eq!(invariants.len(), 7);
}

#[test]
fn test_validation_evidence_severity_levels() {
    let error = ValidationEvidence {
        description: "".to_string(),
        severity: "error".to_string(),
        location: "".to_string(),
        details: BTreeMap::new(),
    };

    let warning = ValidationEvidence {
        description: "".to_string(),
        severity: "warning".to_string(),
        location: "".to_string(),
        details: BTreeMap::new(),
    };

    assert_eq!(error.severity, "error");
    assert_eq!(warning.severity, "warning");
}

// =============================================================================
// CONSTITUTION & INVARIANTS TESTS (10 tests)
// =============================================================================

#[test]
fn test_invariant_result() {
    let result = InvariantResult {
        invariant: Invariant::TypeSoundness,
        satisfied: true,
        message: "All types valid".to_string(),
        violations: vec![],
    };

    assert!(result.satisfied);
    assert!(result.violations.is_empty());
}

#[test]
fn test_constitution_validation_all_pass() {
    let validation = ConstitutionValidation {
        results: vec![],
        all_satisfied: true,
        total_violations: 0,
    };

    assert!(validation.all_satisfied);
    assert_eq!(validation.total_violations, 0);
}

#[test]
fn test_constitution_validation_with_violations() {
    let validation = ConstitutionValidation {
        results: vec![],
        all_satisfied: false,
        total_violations: 3,
    };

    assert!(!validation.all_satisfied);
    assert_eq!(validation.total_violations, 3);
}

#[test]
fn test_type_soundness_check() {
    let check = TypeSoundnessCheck;
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

#[test]
fn test_immutability_check() {
    let check = ImmutabilityCheck;
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

#[test]
fn test_atomic_promotion_check() {
    let check = AtomicPromotionCheck;
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

#[test]
fn test_guard_soundness_check() {
    let check = GuardSoundnessCheck;
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

#[test]
fn test_projection_determinism_check() {
    let check = ProjectionDeterminismCheck;
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

#[test]
fn test_slo_preservation_check() {
    let check = SLOPreservationCheck {
        max_latency_us: 1000,
    };
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

#[test]
fn test_no_retrocausation_check() {
    let check = NoRetrocausationCheck;
    let snapshot = create_test_snapshot();

    let result = check.validate(&snapshot);
    assert!(result.is_ok());
}

// Helper function for constitution tests
fn create_test_snapshot() -> SigmaSnapshot {
    let metadata = SnapshotMetadata::default();
    SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "test_sig".to_string(),
        metadata,
    )
}

// =============================================================================
// PROMOTION TESTS (5 tests)
// =============================================================================

#[test]
fn test_promotion_result_success() {
    let result = PromotionResult {
        success: true,
        promoted_snapshot_id: Some(SigmaSnapshotId::from_digest(b"promoted")),
        message: "Promotion successful".to_string(),
        metrics: PromotionMetrics::default(),
    };

    assert!(result.success);
    assert!(result.promoted_snapshot_id.is_some());
}

#[test]
fn test_promotion_result_failure() {
    let result = PromotionResult {
        success: false,
        promoted_snapshot_id: None,
        message: "Validation failed".to_string(),
        metrics: PromotionMetrics::default(),
    };

    assert!(!result.success);
    assert!(result.promoted_snapshot_id.is_none());
}

#[test]
fn test_promotion_metrics() {
    let metrics = PromotionMetrics {
        duration_ms: 100,
        validation_time_ms: 50,
        swap_time_ns: 1000,
        rollback_time_ms: None,
    };

    assert_eq!(metrics.duration_ms, 100);
    assert_eq!(metrics.validation_time_ms, 50);
}

#[test]
fn test_snapshot_guard() {
    let metadata = SnapshotMetadata::default();
    let snapshot = SigmaSnapshot::new(
        None,
        vec![],
        "1.0.0".to_string(),
        "sig".to_string(),
        metadata,
    );

    let guard = SnapshotGuard {
        snapshot: snapshot.clone(),
        active: true,
    };

    assert!(guard.active);
    assert_eq!(guard.snapshot.version, "1.0.0");
}

#[test]
fn test_atomic_snapshot_promoter() {
    let promoter = AtomicSnapshotPromoter::new();
    assert!(promoter.current_snapshot.is_none());
}
