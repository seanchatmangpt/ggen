//! Chicago admission tests over real serde_json payloads — port of the
//! igniter admission CASES (valid admit, missing-field refusal, bad-regex
//! refusal, closed-standing admit, opaque replay_identity, digest
//! determinism). Real collaborators (serde, sha2), state-based assertions
//! on the returned values; no mocks.

use ggen_sjira::{admit_work_order, definition_digest, Refusal, Standing, WorkOrder};
use serde_json::{json, Value};

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
        "replay_identity": "semantic-jira:align:1",
        "origin_authority": "https://graph.test/auth/XAAS-26922-21",
        "projections": ["jira", "fond"],
        "required_courts": ["admission-court"],
        "required_evidence": ["exact_head_verification_evidence"],
        "acceptance": ["cargo test -p ggen-sjira"],
        "falsifiers": ["changing an OCEL outcome does not change plan ranking"],
        "path_scope": ["lib/"]
    })
}

#[test]
fn valid_order_is_admitted_with_authority_none() {
    let admitted = admit_work_order(&valid_order()).expect("valid order must admit");

    assert_eq!(admitted.authority, "NONE");
    assert_eq!(admitted.work_order.standing, Standing::Unknown);
    assert_eq!(admitted.work_order.identity, "XAAS-26922-21");
    assert_eq!(admitted.work_order.path_scope, vec!["lib/".to_string()]);
    assert_eq!(admitted.work_order.authority_requirement, "NONE");
    assert!(admitted.work_order_digest.starts_with("sha256:"));
    assert!(admitted.definition_digest.starts_with("sha256:"));
    // snapshot digest != definition digest: standing is excluded from the
    // definition take.
    assert_ne!(admitted.work_order_digest, admitted.definition_digest);
}

#[test]
fn missing_field_is_refused() {
    let mut raw = valid_order();
    raw.as_object_mut().unwrap().remove("promotion_rule");

    match admit_work_order(&raw) {
        Err(Refusal::Deserialization(msg)) => assert!(
            msg.contains("promotion_rule"),
            "refusal must name the missing field: {msg}"
        ),
        other => panic!("expected Deserialization refusal naming promotion_rule, got {other:?}"),
    }
}

#[test]
fn elixir_optional_extras_are_stripped_not_refused() {
    // Elixir admit keeps extras then stamps authority/admitted; Rust strips
    // unknowns via serde (no deny_unknown_fields) so the same payloads admit.
    let mut raw = valid_order();
    raw["authority"] = json!("NONE");
    raw["admitted"] = json!(true);
    raw["dimensions"] = json!({"observed": false});
    raw["candidate_sha"] = json!("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");

    let admitted = admit_work_order(&raw).expect("Elixir extras must not refuse admit");
    assert_eq!(admitted.authority, "NONE");
    assert_eq!(
        admitted.work_order.candidate_sha.as_deref(),
        Some("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    );
}

#[test]
fn empty_path_scope_is_refused() {
    let mut raw = valid_order();
    raw["path_scope"] = json!([]);

    match admit_work_order(&raw) {
        Err(Refusal::InvalidPathScope { .. }) => {}
        other => panic!("expected InvalidPathScope, got {other:?}"),
    }
}

#[test]
fn absolute_path_scope_is_refused() {
    let mut raw = valid_order();
    raw["path_scope"] = json!(["/abs"]);

    match admit_work_order(&raw) {
        Err(Refusal::InvalidPathScope { .. }) => {}
        other => panic!("expected InvalidPathScope, got {other:?}"),
    }
}

#[test]
fn bad_base_sha_regex_is_refused() {
    let mut raw = valid_order();
    raw["base_sha"] = json!("NOT_A_SHA");

    match admit_work_order(&raw) {
        Err(Refusal::InvalidSha { field, found }) => {
            assert_eq!(field, "base_sha");
            assert_eq!(found, "NOT_A_SHA");
        }
        other => panic!("expected InvalidSha refusal, got {other:?}"),
    }
}

#[test]
fn bad_repository_regex_is_refused() {
    let mut raw = valid_order();
    raw["repository"] = json!("no-slash-here");

    match admit_work_order(&raw) {
        Err(Refusal::InvalidRepository { value }) => assert_eq!(value, "no-slash-here"),
        other => panic!("expected InvalidRepository refusal, got {other:?}"),
    }
}

#[test]
fn bare_local_name_origin_authority_is_refused() {
    let mut raw = valid_order();
    raw["origin_authority"] = json!("some_admitted_authority");

    match admit_work_order(&raw) {
        Err(Refusal::InvalidOriginAuthority { value }) => {
            assert_eq!(value, "some_admitted_authority");
        }
        other => panic!("expected InvalidOriginAuthority refusal, got {other:?}"),
    }
}

#[test]
fn opaque_replay_identity_is_admitted() {
    // Elixir: replay_identity is required non-blank, not @digest-shaped.
    let mut raw = valid_order();
    raw["replay_identity"] = json!("replay-001");

    let admitted = admit_work_order(&raw).expect("opaque replay_identity must admit");
    assert_eq!(admitted.work_order.replay_identity, "replay-001");
}

#[test]
fn blank_replay_identity_is_refused() {
    let mut raw = valid_order();
    raw["replay_identity"] = json!("");

    match admit_work_order(&raw) {
        Err(Refusal::MalformedWorkOrder(msg)) => assert!(msg.contains("replay_identity")),
        other => panic!("expected MalformedWorkOrder for blank replay_identity, got {other:?}"),
    }
}

#[test]
fn closed_standing_partial_alive_is_admitted() {
    let mut raw = valid_order();
    raw["standing"] = json!("PARTIAL_ALIVE");

    let admitted = admit_work_order(&raw).expect("Elixir admits any closed standing");
    assert_eq!(admitted.work_order.standing, Standing::PartialAlive);
}

#[test]
fn refused_standing_vocabulary_is_admittable() {
    let mut raw = valid_order();
    raw["standing"] = json!("REFUSED(bad evidence)");

    let admitted = admit_work_order(&raw).expect("REFUSED(...) is closed standing vocab");
    assert_eq!(
        admitted.work_order.standing,
        Standing::Refused("bad evidence".to_string())
    );
}

#[test]
fn digest_is_deterministic_and_key_order_independent() {
    let a = admit_work_order(&valid_order()).unwrap();
    let mut reordered = serde_json::Map::new();
    for (k, v) in valid_order().as_object().unwrap() {
        reordered.insert(k.clone(), v.clone());
    }
    let mut keys: Vec<String> = reordered.keys().cloned().collect();
    keys.sort();
    keys.reverse();
    let mut flipped = serde_json::Map::new();
    for k in keys {
        flipped.insert(k.clone(), reordered[&k].clone());
    }
    let b = admit_work_order(&Value::Object(flipped)).unwrap();

    assert_eq!(a.work_order_digest, b.work_order_digest);
    assert_eq!(a.definition_digest, b.definition_digest);
}

#[test]
fn standing_change_moves_snapshot_but_not_definition_digest() {
    let admitted = admit_work_order(&valid_order()).unwrap();

    let mut promoted = valid_order();
    promoted["standing"] = json!("PARTIAL_ALIVE");
    let promoted_order: WorkOrder =
        serde_json::from_value(promoted.clone()).expect("non-UNKNOWN standing still deserializes");
    let def_after = definition_digest(&promoted_order);
    // snapshot digest is over the full payload (standing included)
    let snapshot_after = ggen_sjira::digest_of(&promoted);

    assert_eq!(admitted.definition_digest, def_after);
    assert_ne!(admitted.work_order_digest, snapshot_after);
}

#[test]
fn remediation_text_is_present_on_refusals() {
    let mut raw = valid_order();
    raw["origin_authority"] = json!("local_name");
    let err = admit_work_order(&raw).unwrap_err().to_string();
    assert!(
        err.contains("remediation"),
        "refusal carries remediation: {err}"
    );
}
