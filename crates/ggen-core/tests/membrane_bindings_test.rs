//! Integration and correctness tests for the ggen membrane bindings and serialization/projection systems.
//! Following AGENTS.md Chicago TDD and anti-cheating mandates.

use ggen_core::membrane::{
    GenesisCore, GgenMembrane, InterchangeablePart, MembraneShaclValidator,
    OcelLog, ProvDocument, RdfMembraneProjector,
};

#[test]
fn test_membrane_bindings_and_boundary_crossings() {
    // 1. Initialize Genesis embedded core
    let mut core = GenesisCore::new();

    // 2. Register interchangeable parts
    let part1 = InterchangeablePart {
        id: "wasm-crypto-module".to_string(),
        part_type: "wasm32".to_string(),
        version: "1.0.0".to_string(),
        interfaces: vec!["hash_sha256".to_string(), "sign_ed25519".to_string()],
        payload_hash: "2e7d2c03a9507ae2ecf403cf6fd0062f627d2c03a9507ae2ecf403cf6fd0062f".to_string(),
        payload_size: 4096,
    };
    core.register_part(part1).expect("Failed to register part 1");

    let part2 = InterchangeablePart {
        id: "atomvm-sensor-module".to_string(),
        part_type: "atomvm_beam".to_string(),
        version: "0.9.0".to_string(),
        interfaces: vec!["read_temperature".to_string()],
        payload_hash: "4a8c9b3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8e7f6a5b".to_string(),
        payload_size: 2048,
    };
    core.register_part(part2).expect("Failed to register part 2");

    // 3. Initialize ggen membrane and bindings
    let mut membrane = GgenMembrane::new(core);
    membrane.bind_adapter("temp-sensor-port", "read_temperature");
    membrane.bind_adapter("crypto-port", "hash_sha256");

    // Verify adapter bindings
    assert_eq!(membrane.adapters.get("temp-sensor-port").unwrap(), "read_temperature");

    // 4. Trigger boundary crossings (invoking Genesis interchangeable parts)
    let (out1, event1) = membrane
        .invoke("external-caller-1", "wasm-crypto-module", "hash_sha256", b"hello-world")
        .expect("Invoke failed");

    assert_eq!(event1.caller_id, "external-caller-1");
    assert_eq!(event1.callee_id, "wasm-crypto-module");
    assert_eq!(event1.interface_fn, "hash_sha256");
    assert!(!out1.is_empty());

    let (out2, event2) = membrane
        .invoke("external-caller-2", "atomvm-sensor-module", "read_temperature", b"sensor-id-42")
        .expect("Invoke failed");

    assert_eq!(event2.caller_id, "external-caller-2");
    assert_eq!(event2.callee_id, "atomvm-sensor-module");
    assert_eq!(event2.interface_fn, "read_temperature");
    assert!(!out2.is_empty());

    // 5. Serialize and project to OCEL
    let ocel_log = OcelLog::from_membrane(&membrane);
    let ocel_json = ocel_log.to_json().expect("Failed to serialize OCEL");
    assert!(ocel_json.contains("wasm-crypto-module"));
    assert!(ocel_json.contains("BoundaryCrossing:hash_sha256"));

    // 6. Serialize and project to W3C PROV-JSON
    let prov_doc = ProvDocument::from_membrane(&membrane);
    let prov_json = prov_doc.to_json().expect("Failed to serialize PROV-JSON");
    assert!(prov_json.contains("prov:wasAssociatedWith"));
    assert!(prov_json.contains("agent:ggen"));

    // 7. Project to RDF Turtle graph
    let rdf_graph = RdfMembraneProjector::project(&membrane).expect("RDF projection failed");
    
    // Query check to ensure RDF projection works and contains correct values
    let results = rdf_graph
        .query("SELECT ?part WHERE { ?part a <http://ggen.org/membrane#InterchangeablePart> }")
        .expect("RDF query failed");
    if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
        assert!(solutions.next().is_some());
    } else {
        panic!("Expected QueryResults::Solutions");
    }
    
    // 8. Validate projected RDF using SHACL shapes
    let shacl_report = MembraneShaclValidator::validate(&rdf_graph).expect("SHACL validation failed");
    
    // Report must pass
    assert!(
        shacl_report.passed,
        "SHACL report validation failed: {:?}",
        shacl_report.violations
    );
}
