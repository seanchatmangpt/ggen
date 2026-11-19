//! Mutual frontier-shape court: Elixir SemanticJira SoT
//! (`{"work_orders":[...]}` flat maps, dependency key `"type"`) must
//! decode and frontier on Rust `ggen-sjira` without a Rust envelope wrap.
//!
//! Elixir probe (from ggen_igniter checkout):
//!
//! ```elixir
//! path = "/Users/sac/ggen/crates/ggen-sjira/tests/fixtures/golden_frontier_orders.json"
//! %{"work_orders" => orders} = Jason.decode!(File.read!(path))
//! Enum.each(orders, fn o -> {:ok, _} = GgenIgniter.SemanticJira.admit_work_order(o) end)
//! ```

use ggen_sjira::{admit_work_order, frontier, orders_from_json, AuthorityIndex, Evidence};
use serde_json::Value;
use std::collections::BTreeMap;

const GOLDEN: &str = include_str!("fixtures/golden_frontier_orders.json");

fn origin_iri() -> &'static str {
    "https://ggen-igniter.dev/ontology/semantic-jira#objective-code-work-authority"
}

fn authority_index() -> AuthorityIndex {
    let mut index = AuthorityIndex::new();
    index.insert(origin_iri().to_string(), "sha256:fixture".to_string());
    index
}

#[test]
fn elixir_sot_orders_decode_without_rust_envelope() {
    let raw: Value = serde_json::from_str(GOLDEN).expect("golden_frontier_orders.json parses");
    let inputs = orders_from_json(&raw).expect("Elixir SoT must decode on Rust");
    assert_eq!(inputs.len(), 2);
    assert!(
        inputs[0].dependencies.is_empty(),
        "flat SoT keeps deps on the order"
    );
    assert!(
        inputs[0].path_scope.is_empty(),
        "flat SoT keeps path_scope on the order"
    );
    assert_eq!(inputs[0].work_order["identity"], "SJ-FRONT-A");
    assert_eq!(
        inputs[1].work_order["dependencies"][0]["type"],
        "requiresReceipt"
    );
}

#[test]
fn elixir_sot_orders_each_admit_on_rust() {
    let raw: Value = serde_json::from_str(GOLDEN).unwrap();
    let orders = raw["work_orders"].as_array().unwrap();
    for order in orders {
        let admitted = admit_work_order(order).unwrap_or_else(|e| {
            panic!("admit refused for {}: {e}", order["identity"]);
        });
        assert_eq!(admitted.authority, "NONE");
    }
}

#[test]
fn elixir_sot_frontier_blocks_dependent_until_evidence() {
    let raw: Value = serde_json::from_str(GOLDEN).unwrap();
    let inputs = orders_from_json(&raw).unwrap();
    let index = authority_index();

    let result = frontier(&inputs, &[], &BTreeMap::new(), Some(&index));
    let eligible: Vec<_> = result
        .eligible
        .iter()
        .map(|c| c.identity.as_str())
        .collect();
    assert_eq!(eligible, vec!["SJ-FRONT-A"]);
    assert_eq!(result.blocked.len(), 1);
    assert_eq!(result.blocked[0].identity.as_deref(), Some("SJ-FRONT-B"));
    assert_eq!(result.blocked[0].reason, "dependencies_unsatisfied");
    assert_eq!(result.blocked[0].dependencies, vec!["SJ-FRONT-A"]);
    assert_eq!(result.eligible[0].path_scope, vec!["lib/".to_string()]);

    let mut evidence = BTreeMap::new();
    evidence.insert(
        "SJ-FRONT-A".to_string(),
        Evidence {
            standing: Some("ALIVE".to_string()),
            receipt_digest: None,
        },
    );
    let unlocked = frontier(&inputs, &[], &evidence, Some(&index));
    let eligible: Vec<_> = unlocked
        .eligible
        .iter()
        .map(|c| c.identity.as_str())
        .collect();
    assert_eq!(eligible, vec!["SJ-FRONT-A", "SJ-FRONT-B"]);
}

#[test]
fn legacy_rust_envelope_still_decodes() {
    let envelope = serde_json::json!([
        {
            "work_order": {
                "identity": "SJ-LEGACY-1",
                "title": "t",
                "description": "d",
                "subject": "src/x.rs",
                "repository": "owner/repo",
                "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "standing": "UNKNOWN",
                "evidence_ceiling": "UNVERIFIED",
                "promotion_rule": "courts",
                "replay_identity": "semantic-jira:legacy:1",
                "origin_authority": origin_iri(),
                "projections": ["jira"],
                "required_courts": ["c1"],
                "required_evidence": ["e1"],
                "acceptance": ["a1"],
                "falsifiers": ["f1"],
                "path_scope": ["lib/"]
            },
            "dependencies": [
                {"kind": "requiresSemanticIdentity", "upstream": "UP"}
            ],
            "path_scope": ["lib/"],
            "concurrency_key": "global"
        }
    ]);
    let inputs = orders_from_json(&envelope).expect("legacy envelope must still decode");
    assert_eq!(inputs.len(), 1);
    assert_eq!(inputs[0].dependencies.len(), 1);
    assert_eq!(inputs[0].dependencies[0].kind, "requiresSemanticIdentity");
}
