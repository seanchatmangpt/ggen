//! Governance End-to-End Tests
//!
//! Tests the proof gates and constitutional law enforcement.

#![cfg(feature = "integration")]

use ggen_core::pipeline_engine::intent::ManufacturingIntent;
use ggen_core::pipeline_engine::pipeline::{PipelineConfig, StagedPipeline};
use ggen_core::pipeline_engine::proof_gate::{ProofGateType, ProofGateValidator};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_ethos_conformant_gate_e2e() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let base_path = temp_dir.path();

    // a. Create an ontology
    let ontology_path = base_path.join("ontology.ttl");
    fs::write(
        &ontology_path,
        r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        foaf:Person a rdfs:Class ;
            rdfs:label "Person" .
    "#,
    )
    .expect("Failed to write ontology");

    // b. Create a ManufacturingIntent with an objective
    let intent = ManufacturingIntent::new("Verify process compliance for User creation")
        .with_criterion("process-fitness", "Formal alignment with reference model");

    // c. Create a valid .ggen/audit/latest_events.json log
    let audit_dir = base_path.join(".ggen/audit");
    fs::create_dir_all(&audit_dir).expect("Failed to create audit dir");
    let log_path = audit_dir.join("latest_events.json");

    // Valid log for Alpha miner: A -> B -> C
    // Alpha miner uses "concept:name" as activity key in ProofGateValidator
    let log_json = r#"{
        "traces": [
            {
                "case_id": "case_1",
                "events": [
                    { "attributes": { "concept:name": { "tag": "String", "value": "A" } } },
                    { "attributes": { "concept:name": { "tag": "String", "value": "B" } } },
                    { "attributes": { "concept:name": { "tag": "String", "value": "C" } } }
                ]
            }
        ],
        "attributes": {}
    }"#;
    fs::write(&log_path, log_json).expect("Failed to write log");

    // d. Run the pipeline and assert that the EthosConformant gate PASSES
    // We use a real pipeline run to get a valid BuildReceipt
    let config = PipelineConfig::new("compliance-test", "1.0.0")
        .with_base_path(base_path)
        .with_ontology("ontology.ttl")
        .with_output_dir("output");

    let mut pipeline = StagedPipeline::new(config).expect("Failed to create pipeline");

    // We need to bypass vocabulary validation for example.org or use standard ggen prefixes
    // Actually, StagedPipeline::new uses standard vocabularies.
    // Let's use ggen prefix to be safe or add example.org.

    let receipt = pipeline.run().expect("Pipeline failed");

    // The EthosConformant gate currently looks at ".ggen/audit/latest_events.json" relative to CWD.
    // In a real E2E test, we must ensure we are in the correct directory.
    let original_dir = std::env::current_dir().expect("Failed to get current dir");
    std::env::set_current_dir(base_path).expect("Failed to set current dir");

    let validator = ProofGateValidator::new(intent.clone());
    let reports = validator.validate(&receipt);

    let ethos_report = reports
        .iter()
        .find(|r| r.gate_type == ProofGateType::EthosConformant)
        .expect("EthosConformant gate not found");

    // Verification: Assert it passes and reports fitness
    assert!(
        ethos_report.passed,
        "EthosConformant gate should pass. Message: {}",
        ethos_report.message
    );
    assert!(
        ethos_report.message.contains("fitness"),
        "Should report fitness. Message: {}",
        ethos_report.message
    );
    // Alpha discovery on A->B->C and alignment of the same trace should yield 1.0 fitness
    assert!(
        ethos_report.message.contains("1.00"),
        "Fitness should be 1.00 for perfectly matching log. Message: {}",
        ethos_report.message
    );

    // e. Delete the log and assert that the gate FAILS
    fs::remove_file(&log_path).expect("Failed to delete log");

    let reports_fail = validator.validate(&receipt);
    let ethos_report_fail = reports_fail
        .iter()
        .find(|r| r.gate_type == ProofGateType::EthosConformant)
        .expect("EthosConformant gate not found");

    assert!(
        !ethos_report_fail.passed,
        "EthosConformant gate should fail after log deletion"
    );
    assert!(
        ethos_report_fail
            .message
            .contains("FAILED: No runtime audit logs found"),
        "Should report missing logs. Message: {}",
        ethos_report_fail.message
    );

    // Restore original directory
    std::env::set_current_dir(original_dir).expect("Failed to restore current dir");
}
