//! Tests for the Genesis-bearing interchangeable part architecture in ggen-graph.

use ggen_graph::{GenesisCore, OuterMembrane, AdapterLayer, ProjectionLayer};
use ggen_graph::graph::quad::parse_nquad;
use ggen_graph::ocel::{OcelLog, OcelObject, OcelEvent};
use serde_json::json;

#[test]
fn test_genesis_core_and_transaction() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange: Create Genesis Core and sample quads
    let core = GenesisCore::new()?;
    let q1 = parse_nquad("<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> .")?;
    let q2 = parse_nquad("<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> .")?;

    let hash_init = core.graph().state_hash()?;

    // Act: Create delta to insert Alice
    let delta = ggen_graph::RdfDelta::new(vec![q1.to_string()], vec![]);
    let receipt = core.execute_transaction(&delta)?;

    // Assert: Verify state changes and receipt truth
    assert_eq!(receipt.pre_state_hash, hash_init);
    assert_ne!(receipt.post_state_hash, hash_init);
    assert!(core.graph().contains_quad(&q1)?);
    assert!(!core.graph().contains_quad(&q2)?);
    receipt.verify()?;

    Ok(())
}

#[test]
fn test_outer_membrane_sanitization_and_injection_blocks() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let membrane = OuterMembrane::new();

    // Act & Assert: Empty inputs
    let res_empty = membrane.admit_input("  ");
    assert!(res_empty.is_err());
    assert_eq!(
        res_empty.unwrap_err().to_string(),
        "Other error: Empty input rejected by outer membrane"
    );

    // Act & Assert: Injection attacks (DROP GRAPH)
    let res_drop = membrane.admit_input("DROP GRAPH <http://example.org/g>");
    assert!(res_drop.is_err());
    assert!(res_drop.unwrap_err().to_string().contains("Security violation"));

    // Act & Assert: Injection attacks (DELETE WHERE)
    let res_delete = membrane.admit_input("DELETE WHERE { ?s ?p ?o }");
    assert!(res_delete.is_err());
    assert!(res_delete.unwrap_err().to_string().contains("Security violation"));

    // Act & Assert: Valid query is admitted
    let res_valid = membrane.admit_input("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
    assert!(res_valid.is_ok());

    // Act & Assert: Quad validation
    let valid_quad = parse_nquad("<http://example.org/s> <http://example.org/p> \"o\" <http://example.org/g> .")?;
    assert!(membrane.validate_quads(&[valid_quad]).is_ok());

    Ok(())
}

#[test]
fn test_adapter_layer_json_rpc() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange: Valid JSON-RPC payload
    let req = json!({
        "jsonrpc": "2.0",
        "method": "apply_delta",
        "params": {
            "additions": [
                "<http://example.org/a> <http://example.org/b> \"c\" <http://example.org/g> ."
            ],
            "deletions": []
        },
        "id": 1
    });

    // Act
    let delta = AdapterLayer::adapt_json_rpc(&req)?;

    // Assert
    assert_eq!(delta.additions.len(), 1);
    assert_eq!(delta.deletions.len(), 0);
    assert_eq!(
        delta.additions[0],
        "<http://example.org/a> <http://example.org/b> \"c\" <http://example.org/g> ."
    );

    // Act & Assert: Missing params
    let malformed_req = json!({
        "jsonrpc": "2.0",
        "method": "apply_delta",
        "id": 1
    });
    assert!(AdapterLayer::adapt_json_rpc(&malformed_req).is_err());

    // Act & Assert: Unsupported method
    let bad_method_req = json!({
        "jsonrpc": "2.0",
        "method": "delete_all",
        "params": {
            "additions": [],
            "deletions": []
        },
        "id": 1
    });
    assert!(AdapterLayer::adapt_json_rpc(&bad_method_req).is_err());

    Ok(())
}

#[test]
fn test_projection_layer_ocel() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange: Setup a Genesis core and target logs
    let core = GenesisCore::new()?;
    let log = OcelLog {
        objects: vec![
            OcelObject {
                id: "obj-1".to_string(),
                r#type: "CustomType".to_string(),
                attributes: std::collections::HashMap::new(),
            }
        ],
        events: vec![
            OcelEvent {
                id: "ev-1".to_string(),
                activity: "PerformAction".to_string(),
                timestamp: chrono::DateTime::parse_from_rfc3339("2026-05-27T06:00:00Z").unwrap().into(),
                objects: vec![],
                attributes: std::collections::HashMap::new(),
            }
        ],
    };

    // Act: Project log into state
    ProjectionLayer::project_ocel_to_state(&core, &log)?;

    // Act: Project state back to log
    let extracted = ProjectionLayer::project_state_to_ocel(&core)?;

    // Assert: Verify mapping equivalence
    assert_eq!(extracted.objects.len(), 1);
    assert_eq!(extracted.objects[0].id, "obj-1");
    assert_eq!(extracted.objects[0].r#type, "CustomType");
    assert_eq!(extracted.events.len(), 1);
    assert_eq!(extracted.events[0].id, "ev-1");
    assert_eq!(extracted.events[0].activity, "PerformAction");

    Ok(())
}
