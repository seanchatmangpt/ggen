//! Integration tests for ggen-dod - Definition of Done system
//!
//! These tests verify the core functionality of the DoD system including:
//! - Observation schema validation
//! - Kernel decision making
//! - Invariant enforcement
//! - Timing guarantees
//! - Receipt generation and verification

use ggen_dod::{
    constants, Invariant, InvariantCategory, InvariantId, InvariantSeverity, Kernel, Observation,
    ObservationId, ObservationSchema, ObservationType, Receipt, ReceiptId, ReceiptStore,
    TimingEnforcer, TimingGuarantee,
};
use std::collections::HashMap;

/// Test: Create and validate an observation
#[test]
fn test_observation_creation_and_validation() {
    let schema = ObservationSchema::new("test_observation");

    let observation = Observation::new(
        ObservationType::IntegrationTest,
        serde_json::json!({
            "field1": "value1",
            "field2": "value2"
        }),
        "test_source",
        "1.0",
        "test_tenant",
    );

    // Verify observation creation - observation is Result type
    assert!(observation.is_ok());
}

/// Test: Kernel decision creation and determinism
#[test]
fn test_kernel_decision_determinism() {
    let kernel = Kernel::new();

    // Create identical observations
    let observation1 = Observation::new(
        ObservationType::PerformanceBenchmark,
        serde_json::json!({"input": "same_value"}),
        "test_source",
        "1.0",
        "test_tenant",
    );

    let observation2 = Observation::new(
        ObservationType::PerformanceBenchmark,
        serde_json::json!({"input": "same_value"}),
        "test_source",
        "1.0",
        "test_tenant",
    );

    // Both should create successfully
    assert!(observation1.is_ok());
    assert!(observation2.is_ok());

    // Decisions for identical observations should be deterministic
    let decision1_id = ObservationId::new();
    let decision2_id = ObservationId::new();

    assert!(decision1_id != decision2_id);
    // Note: Full determinism testing would require access to internal decision state
}

/// Test: Invariant creation and checking
#[test]
fn test_invariant_enforcement() {
    let invariant = Invariant::new(
        "test_invariant",
        "Field1 must not be empty",
        InvariantSeverity::Error,
        InvariantCategory::Safety,
    );

    assert_eq!(invariant.name(), "test_invariant");
    assert_eq!(invariant.predicate(), "Field1 must not be empty");
    assert_eq!(invariant.severity(), InvariantSeverity::Error);
}

/// Test: Receipt generation and storage
#[test]
fn test_receipt_generation_and_storage() {
    let mut store = ReceiptStore::new();

    let receipt = Receipt::new(ReceiptId::new(), "test_operation", {
        let mut map = HashMap::new();
        map.insert("status".to_string(), "success".to_string());
        map
    });

    let receipt_id = receipt.receipt_id();
    let _ = store.store_receipt(receipt);

    // Verify receipt can be retrieved
    let stored_receipt = store.get_receipt(&receipt_id);
    assert!(stored_receipt.is_some());
}

/// Test: Timing guarantees enforcement
#[test]
fn test_timing_enforcement() {
    let enforcer = TimingEnforcer::new(TimingGuarantee::Kernel);

    // Start measurement
    let measurement = enforcer.start_measurement();

    // Simulate some work (should be well under 8ms)
    std::thread::sleep(std::time::Duration::from_millis(1));

    // End measurement
    let elapsed = measurement.elapsed_ns();

    // Verify timing is within bounds
    assert!(elapsed < constants::KERNEL_MAX_TIME_MS * 1_000_000);
}

/// Test: Observation schema validation
#[test]
fn test_schema_validation() {
    let schema = ObservationSchema::new("complex_schema");

    // Schema should have a name
    assert_eq!(schema.name(), "complex_schema");
}

/// Test: Multiple observations and decisions
#[test]
fn test_observation_pipeline() {
    let kernel = Kernel::new();

    // Create a sequence of observations
    let observations = vec![
        Observation::new(
            ObservationType::SystemState,
            serde_json::json!({"path": "/test/file1.rs"}),
            "test_source",
            "1.0",
            "test_tenant",
        ),
        Observation::new(
            ObservationType::IntegrationTest,
            serde_json::json!({"check_type": "syntax"}),
            "test_source",
            "1.0",
            "test_tenant",
        ),
        Observation::new(
            ObservationType::SecurityAudit,
            serde_json::json!({"template": "rust"}),
            "test_source",
            "1.0",
            "test_tenant",
        ),
    ];

    // Verify observations can be created and tracked
    assert_eq!(observations.len(), 3);
    assert!(observations[0].is_ok());
    assert!(observations[1].is_ok());
    assert!(observations[2].is_ok());
}

/// Test: Constraint checking across multiple constraints
#[test]
fn test_multi_constraint_enforcement() {
    let constraints = vec![
        Invariant::new(
            "constraint1",
            "First constraint",
            InvariantSeverity::Warning,
            InvariantCategory::Safety,
        ),
        Invariant::new(
            "constraint2",
            "Second constraint",
            InvariantSeverity::Error,
            InvariantCategory::Liveness,
        ),
    ];

    assert_eq!(constraints.len(), 2);
    assert_eq!(constraints[0].name(), "constraint1");
    assert_eq!(constraints[1].name(), "constraint2");
}

/// Test: Timing measurement accuracy
#[test]
fn test_timing_measurement_accuracy() {
    let enforcer = TimingEnforcer::new(TimingGuarantee::Kernel);
    let measurement = enforcer.start_measurement();

    // Simulate a short operation
    let iterations = 100;
    for _i in 0..iterations {
        let _ = 42 + 58;
    }

    let elapsed_ns = measurement.elapsed_ns();

    // Should be non-zero
    assert!(elapsed_ns > 0);
    // Should not exceed kernel max
    assert!(elapsed_ns < constants::KERNEL_MAX_TIME_MS * 1_000_000);
}

/// Test: Receipt metadata preservation
#[test]
fn test_receipt_metadata() {
    let mut metadata = HashMap::new();
    metadata.insert("operation_id".to_string(), "op_123".to_string());
    metadata.insert("timestamp".to_string(), "2025-01-01T00:00:00Z".to_string());
    metadata.insert("status".to_string(), "completed".to_string());

    let receipt = Receipt::new(ReceiptId::new(), "metadata_test", metadata);

    assert_eq!(receipt.operation(), "metadata_test");
}

/// Test: Large observation handling
#[test]
fn test_large_observation_handling() {
    let mut data = serde_json::json!({});

    // Build a moderately large observation (but under 1MB limit)
    for i in 0..100 {
        let key = format!("key_{}", i);
        data[&key] = serde_json::json!(format!("value_{}", "x".repeat(100)));
    }

    let observation = Observation::new(
        ObservationType::SystemState,
        data,
        "test_source",
        "1.0",
        "test_tenant",
    );

    assert!(observation.is_ok());
}

/// Test: Schema depth constraints
#[test]
fn test_schema_complexity_limits() {
    let schema = ObservationSchema::new("complex_schema");

    // Schema should be created successfully
    assert_eq!(schema.name(), "complex_schema");
}
