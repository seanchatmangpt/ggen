
use ggen_core::manufacturing::operator::OperatorContext;
use ggen_core::manufacturing::gates::{ProofGate, UniverseOsTypestateGate};
use std::path::PathBuf;
use ggen_core::graph::Graph;

#[test]
fn test_marketplace_universeos_admission() {
    let gate = UniverseOsTypestateGate;
    let ctx = OperatorContext {
        workspace_root: PathBuf::from("."),
        artifact_id: "marketplace-package-01".to_string(),
        graph: Graph::new().unwrap(),
    };
    
    let result = gate.validate(&ctx).expect("Gate validation failed");
    assert!(result.passed);
    assert_eq!(result.gate_name, "universeos-typestate-admission");
    println!("UniverseOS Typestate Admission Test Passed: {:?}", result.signal);
}
