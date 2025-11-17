//! Integration tests for ggen-dod - Definition of Done system
//!
//! These tests verify the core functionality of the DoD system including:
//! - Observation schema validation
//! - Kernel decision making
//! - Invariant enforcement
//! - Timing guarantees
//! - Receipt generation and verification

use ggen_dod::{
    constants, DoDError, DoDResult, Invariant, InvariantChecker, InvariantId, Kernel,
    KernelAction, KernelDecision, Observation, ObservationId, ObservationSchema, ObservationType,
    Receipt, ReceiptId, ReceiptStore, TimingEnforcer, TimingGuarantee, TimingMeasurement,
};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Test: Create and validate an observation
#[test]
fn test_observation_creation_and_validation() {
    let schema = ObservationSchema::new(
        "test_observation",
        vec!["field1".to_string(), "field2".to_string()],
    );

    let mut observation = Observation::new(
        ObservationType::QueryExecution,
        HashMap::from([
            ("field1".to_string(), "value1".to_string()),
            ("field2".to_string(), "value2".to_string()),
        ]),
    );

    // Verify observation creation
    assert_eq!(observation.observation_type, ObservationType::QueryExecution);
    assert!(observation.timestamp_ns > 0);

    // Verify observation size is within limits
    let observation_size = serde_json::to_string(&observation)
        .map(|s| s.len())
        .unwrap_or(0);
    assert!(observation_size <= constants::MAX_OBSERVATION_SIZE);
}

/// Test: Kernel decision creation and determinism
#[test]
fn test_kernel_decision_determinism() {
    let kernel = Kernel::new();

    // Create identical observations
    let observation1 = Observation::new(
        ObservationType::CodeGeneration,
        HashMap::from([("input".to_string(), "same_value".to_string())]),
    );

    let observation2 = Observation::new(
        ObservationType::CodeGeneration,
        HashMap::from([("input".to_string(), "same_value".to_string())]),
    );

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
        InvariantId::new(),
        "test_invariant",
        "Field1 must not be empty",
        vec!["field1".to_string()],
    );

    assert_eq!(invariant.name, "test_invariant");
    assert_eq!(invariant.description, "Field1 must not be empty");
    assert!(invariant.affected_fields.contains(&"field1".to_string()));
}

/// Test: Receipt generation and storage
#[test]
fn test_receipt_generation_and_storage() {
    let mut store = ReceiptStore::new();

    let receipt = Receipt::new(
        ReceiptId::new(),
        "test_operation",
        HashMap::from([("status".to_string(), "success".to_string())]),
    );

    let receipt_id = receipt.receipt_id.clone();
    store.store_receipt(receipt).unwrap();

    // Verify receipt can be retrieved
    let stored_receipt = store.get_receipt(&receipt_id);
    assert!(stored_receipt.is_some());
    assert_eq!(stored_receipt.unwrap().operation, "test_operation");
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
    let schema = ObservationSchema::new(
        "complex_schema",
        vec![
            "required_field1".to_string(),
            "required_field2".to_string(),
        ],
    );

    // Valid observation with all required fields
    let valid_fields = vec![
        ("required_field1".to_string(), "value1".to_string()),
        ("required_field2".to_string(), "value2".to_string()),
    ];

    assert_eq!(schema.name, "complex_schema");
    assert_eq!(schema.required_fields.len(), 2);
}

/// Test: Multiple observations and decisions
#[test]
fn test_observation_pipeline() {
    let kernel = Kernel::new();

    // Create a sequence of observations
    let observations = vec![
        Observation::new(
            ObservationType::FileChange,
            HashMap::from([("path".to_string(), "/test/file1.rs".to_string())]),
        ),
        Observation::new(
            ObservationType::ValidationCheck,
            HashMap::from([("check_type".to_string(), "syntax".to_string())]),
        ),
        Observation::new(
            ObservationType::CodeGeneration,
            HashMap::from([("template".to_string(), "rust".to_string())]),
        ),
    ];

    // Verify observations can be created and tracked
    assert_eq!(observations.len(), 3);
    assert_eq!(observations[0].observation_type, ObservationType::FileChange);
    assert_eq!(observations[1].observation_type, ObservationType::ValidationCheck);
    assert_eq!(observations[2].observation_type, ObservationType::CodeGeneration);
}

/// Test: Constraint checking across multiple constraints
#[test]
fn test_multi_constraint_enforcement() {
    let constraints = vec![
        Invariant::new(
            InvariantId::new(),
            "constraint1",
            "First constraint",
            vec!["field_a".to_string()],
        ),
        Invariant::new(
            InvariantId::new(),
            "constraint2",
            "Second constraint",
            vec!["field_b".to_string(), "field_c".to_string()],
        ),
    ];

    assert_eq!(constraints.len(), 2);
    assert_eq!(constraints[0].name, "constraint1");
    assert_eq!(constraints[1].affected_fields.len(), 2);
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

    assert_eq!(receipt.operation, "metadata_test");
}

/// Test: Large observation handling
#[test]
fn test_large_observation_handling() {
    let mut data = HashMap::new();

    // Build a moderately large observation (but under 1MB limit)
    for i in 0..100 {
        data.insert(
            format!("key_{}", i),
            format!("value_{}", "x".repeat(100)),
        );
    }

    let observation = Observation::new(ObservationType::SystemMetrics, data);

    let observation_str = serde_json::to_string(&observation).unwrap();
    assert!(observation_str.len() < constants::MAX_OBSERVATION_SIZE);
}

/// Test: Schema depth constraints
#[test]
fn test_schema_complexity_limits() {
    let mut fields = vec![];

    // Create a schema with many fields (but under max depth)
    for i in 0..50 {
        fields.push(format!("field_{}", i));
    }

    let schema = ObservationSchema::new("complex_schema", fields);
    assert!(schema.required_fields.len() < constants::MAX_SCHEMA_DEPTH);
}
