//! Chicago frontier tests — port of the igniter frontier CASES over real
//! serde payloads: eligible/blocked partitions, standing projection from
//! event sequences (last transition wins), illegal-skip refusal, and
//! schedule conflict fencing. Real collaborators (serde, sha2), state
//! assertions on returned values; no mocks.

use ggen_sjira::{
    frontier, project_standing, schedule, ActiveLease, AuthorityIndex, DependencyEdge, Evidence,
    FrontierInput, TransitionEvent,
};
use serde_json::{json, Value};
use std::collections::BTreeMap;

fn valid_order() -> Value {
    json!({
        "identity": "XAAS-26922-21",
        "title": "factory that learns its own yield",
        "description": "planner cost model mined from OCEL outcomes",
        "subject": "sa2a_loop.exs",
        "repository": "seanchatmangpt/xaas",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts-then-receipt",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://graph.test/auth/XAAS-26922-21",
        "projections": ["jira", "fond"],
        "required_courts": ["admission-court"],
        "required_evidence": ["exact_head_verification_evidence"],
        "acceptance": ["cargo test -p ggen-sjira"],
        "falsifiers": ["changing an OCEL outcome does not change plan ranking"]
    })
}

fn order_at(repository: &str, identity: &str) -> Value {
    let mut v = valid_order();
    let obj = v.as_object_mut().unwrap();
    obj["identity"] = json!(identity);
    obj["repository"] = json!(repository);
    v
}

fn input(order: Value) -> FrontierInput {
    FrontierInput {
        work_order: order,
        dependencies: vec![],
        path_scope: vec!["lib/".to_string()],
        concurrency_key: "key-default".to_string(),
    }
}

fn authority_with(order: &Value) -> AuthorityIndex {
    let mut index = AuthorityIndex::new();
    index.insert(
        order["origin_authority"].as_str().unwrap().to_string(),
        "sha256:aa".to_string(),
    );
    index
}

#[test]
fn eligible_when_unknown_resolved_and_dependencies_satisfied() {
    let order = valid_order();
    let index = authority_with(&order);
    let orders = vec![input(order.clone())];

    let result = frontier(&orders, &[], &BTreeMap::new(), Some(&index));
    assert!(result.blocked.is_empty(), "blocked: {:?}", result.blocked);
    assert_eq!(result.eligible.len(), 1);
    let candidate = &result.eligible[0];
    assert_eq!(candidate.identity, "XAAS-26922-21");
    assert_eq!(candidate.authority, "NONE");
    assert_eq!(candidate.origin_admission_digest, "sha256:aa");
    assert!(candidate.work_order_digest.starts_with("sha256:"));
    assert_ne!(candidate.work_order_digest, candidate.definition_digest);
    assert!(result.standings.is_empty());
}

#[test]
fn blocked_when_standing_projected_away_from_unknown() {
    let order = valid_order();
    let index = authority_with(&order);
    let orders = vec![input(order.clone())];
    let events = vec![TransitionEvent {
        work_order_id: "XAAS-26922-21".to_string(),
        from_standing: Some("UNKNOWN".to_string()),
        to_standing: "PARTIAL_ALIVE".to_string(),
        seq: 1,
        receipt_digest: None,
    }];

    let result = frontier(&orders, &events, &BTreeMap::new(), Some(&index));
    assert!(result.eligible.is_empty());
    assert_eq!(result.blocked.len(), 1);
    assert_eq!(result.blocked[0].identity.as_deref(), Some("XAAS-26922-21"));
    assert_eq!(result.blocked[0].reason, "standing=PARTIAL_ALIVE");
}

#[test]
fn blocked_when_origin_not_in_index() {
    let order = valid_order();
    let index = AuthorityIndex::new();
    let orders = vec![input(order)];

    let result = frontier(&orders, &[], &BTreeMap::new(), Some(&index));
    assert!(result.eligible.is_empty());
    assert_eq!(result.blocked[0].reason, "origin_not_admitted");
}

#[test]
fn fails_closed_when_authority_index_unavailable() {
    let order = valid_order();
    let orders = vec![input(order)];

    let result = frontier(&orders, &[], &BTreeMap::new(), None);
    assert!(result.eligible.is_empty());
    assert_eq!(result.blocked.len(), 1);
    assert_eq!(
        result.blocked[0].identity,
        Some("XAAS-26922-21".to_string())
    );
    assert_eq!(result.blocked[0].reason, "authority_index_unavailable");
}

#[test]
fn dependencies_unsatisfied_blocks_with_upstream_names() {
    let order = valid_order();
    let index = authority_with(&order);

    let downstream = FrontierInput {
        dependencies: vec![DependencyEdge {
            kind: "requiresSemanticIdentity".to_string(),
            upstream: "UPSTREAM-1".to_string(),
            required_standing: Some("PARTIAL_ALIVE".to_string()),
            required_receipt_digest: Some("sha256:deadbeef".to_string()),
        }],
        ..input(order.clone())
    };

    // No evidence at all: blocked, upstream named.
    let result = frontier(std::slice::from_ref(&downstream), &[], &BTreeMap::new(), Some(&index));
    assert!(result.eligible.is_empty());
    assert_eq!(result.blocked[0].reason, "dependencies_unsatisfied");
    assert_eq!(result.blocked[0].dependencies, vec!["UPSTREAM-1"]);

    // Evidence present but receipt digest mismatched.
    let mut ev = BTreeMap::new();
    ev.insert(
        "UPSTREAM-1".to_string(),
        Evidence {
            standing: Some("PARTIAL_ALIVE".to_string()),
            receipt_digest: Some("sha256:aaaa".to_string()),
        },
    );
    let result = frontier(std::slice::from_ref(&downstream), &[], &ev, Some(&index));
    assert_eq!(result.blocked[0].reason, "dependencies_unsatisfied");

    // Standing mismatch also blocks.
    let mut ev_bad = ev.clone();
    ev_bad.insert(
        "UPSTREAM-1".to_string(),
        Evidence {
            standing: Some("ALIVE".to_string()),
            receipt_digest: Some("sha256:deadbeef".to_string()),
        },
    );
    let result = frontier(std::slice::from_ref(&downstream), &[], &ev_bad, Some(&index));
    assert_eq!(result.blocked[0].reason, "dependencies_unsatisfied");

    // Matching evidence: eligible.
    ev.insert(
        "UPSTREAM-1".to_string(),
        Evidence {
            standing: Some("PARTIAL_ALIVE".to_string()),
            receipt_digest: Some("sha256:deadbeef".to_string()),
        },
    );
    let result = frontier(&[downstream], &[], &ev, Some(&index));
    assert_eq!(result.eligible.len(), 1, "blocked: {:?}", result.blocked);
}

#[test]
fn logged_transition_evidence_overrides_caller_evidence() {
    let order = valid_order();
    let index = authority_with(&order);

    let downstream = FrontierInput {
        dependencies: vec![DependencyEdge {
            kind: "requiresReceipt".to_string(),
            upstream: "UPSTREAM-1".to_string(),
            required_standing: Some("PARTIAL_ALIVE".to_string()),
            required_receipt_digest: Some("sha256:fromlog".to_string()),
        }],
        ..input(order.clone())
    };

    // Caller evidence would satisfy nothing (wrong digest); the logged
    // transition overrides it and the edge becomes satisfied.
    let mut caller = BTreeMap::new();
    caller.insert(
        "UPSTREAM-1".to_string(),
        Evidence {
            standing: Some("ALIVE".to_string()),
            receipt_digest: Some("sha256:stale".to_string()),
        },
    );
    let events = vec![TransitionEvent {
        work_order_id: "UPSTREAM-1".to_string(),
        from_standing: Some("UNKNOWN".to_string()),
        to_standing: "PARTIAL_ALIVE".to_string(),
        seq: 1,
        receipt_digest: Some("sha256:fromlog".to_string()),
    }];

    let result = frontier(&[downstream], &events, &caller, Some(&index));
    assert_eq!(result.eligible.len(), 1, "blocked: {:?}", result.blocked);
    // The logged standing also lands in the standings projection.
    assert_eq!(
        result.standings.get("UPSTREAM-1").map(String::as_str),
        Some("PARTIAL_ALIVE")
    );
}

#[test]
fn standing_projection_last_transition_wins() {
    let events = vec![
        TransitionEvent {
            work_order_id: "XAAS-26922-21".to_string(),
            from_standing: Some("UNKNOWN".to_string()),
            to_standing: "PARTIAL_ALIVE".to_string(),
            seq: 1,
            receipt_digest: None,
        },
        TransitionEvent {
            work_order_id: "XAAS-26922-21".to_string(),
            from_standing: Some("PARTIAL_ALIVE".to_string()),
            to_standing: "ALIVE".to_string(),
            seq: 2,
            receipt_digest: None,
        },
        // Out-of-order seq: seq 3 still wins over seq 2 regardless of vec
        // position — seq orders the log.
        TransitionEvent {
            work_order_id: "XAAS-26922-21".to_string(),
            from_standing: Some("ALIVE".to_string()),
            to_standing: "BLOCKED".to_string(),
            seq: 3,
            receipt_digest: None,
        },
    ];

    let standings = project_standing(&events).expect("well-formed log projects");
    assert_eq!(
        standings.get("XAAS-26922-21").map(String::as_str),
        Some("BLOCKED"),
        "chain tip = latest logged to, not the vec-last element"
    );
}

#[test]
fn illegal_unknown_to_alive_skip_refuses_projection() {
    let events = vec![TransitionEvent {
        work_order_id: "XAAS-26922-21".to_string(),
        from_standing: Some("UNKNOWN".to_string()),
        to_standing: "ALIVE".to_string(),
        seq: 1,
        receipt_digest: None,
    }];

    let err = project_standing(&events).expect_err("UNKNOWN -> ALIVE skip must refuse");
    let msg = err.to_string();
    assert!(msg.contains("illegal_progression"), "got: {msg}");
    assert!(msg.contains("PARTIAL_ALIVE"), "remediation named: {msg}");
}

#[test]
fn frontier_fails_closed_on_illegal_skip() {
    let order = valid_order();
    let index = authority_with(&order);
    let events = vec![TransitionEvent {
        work_order_id: "OTHER-1".to_string(),
        from_standing: Some("UNKNOWN".to_string()),
        to_standing: "ALIVE".to_string(),
        seq: 1,
        receipt_digest: None,
    }];

    let result = frontier(&[input(order)], &events, &BTreeMap::new(), Some(&index));
    assert!(result.eligible.is_empty(), "selection must fail closed");
    assert_eq!(result.blocked.len(), 1);
    assert_eq!(result.blocked[0].reason, "malformed_transition_log");
    assert_eq!(result.blocked[0].identity, None);
}

#[test]
fn schedule_fences_on_concurrency_key_collision() {
    let order_a = order_at("seanchatmangpt/xaas", "ORDER-A");
    let order_b = order_at("seanchatmangpt/xaas", "ORDER-B");
    let index = authority_with(&order_a);

    let mut in_b = input(order_b);
    in_b.concurrency_key = "key-b".to_string();
    in_b.path_scope = vec!["priv/".to_string()];
    let mut in_a = input(order_a);
    in_a.concurrency_key = "key-a".to_string();

    // Active lease holding key-a: only ORDER-B survives fencing.
    let leases = vec![ActiveLease {
        status: Some("active".to_string()),
        repository: "other/repo".to_string(),
        scope: vec![],
        concurrency_key: "key-a".to_string(),
    }];
    let result = schedule(
        &[in_a.clone(), in_b.clone()],
        &[],
        &BTreeMap::new(),
        Some(&index),
        &leases,
    );
    let ids: Vec<&str> = result
        .eligible
        .iter()
        .map(|c| c.identity.as_str())
        .collect();
    assert_eq!(ids, vec!["ORDER-B"], "key-a collision must fence ORDER-A");
    let blocked_ids: Vec<&str> = result
        .blocked
        .iter()
        .filter_map(|b| b.identity.as_deref())
        .collect();
    assert_eq!(blocked_ids, vec!["ORDER-A"]);
    assert_eq!(result.blocked[0].reason, "conflicting_active_lease");

    // Released lease (status not active): nothing fences.
    let released = vec![ActiveLease {
        status: Some("released".to_string()),
        repository: "other/repo".to_string(),
        scope: vec![],
        concurrency_key: "key-a".to_string(),
    }];
    let result = schedule(
        &[in_a, in_b],
        &[],
        &BTreeMap::new(),
        Some(&index),
        &released,
    );
    assert_eq!(result.eligible.len(), 2);
    assert!(result.blocked.is_empty());
}
