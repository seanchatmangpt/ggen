//! Chicago admission tests over real serde_json payloads — port of the
//! igniter admission CASES (valid admit, missing-field refusal, bad-regex
//! refusal, standing-not-unknown refusal, digest determinism). Real
//! collaborators (serde, sha2), state-based assertions on the returned
//! values; no mocks.

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
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://graph.test/auth/XAAS-26922-21",
        "projections": ["jira", "fond"],
        "required_courts": ["admission-court"],
        "required_evidence": ["exact_head_verification_evidence"],
        "acceptance": ["cargo test -p ggen-sjira"],
        "falsifiers": ["changing an OCEL outcome does not change plan ranking"]
    })
}

#[test]
fn valid_order_is_admitted_with_authority_none() {
    let admitted = admit_work_order(&valid_order()).expect("valid order must admit");

    assert_eq!(admitted.authority, "NONE");
    assert_eq!(admitted.work_order.standing, Standing::Unknown);
    assert_eq!(admitted.work_order.identity, "XAAS-26922-21");
    assert!(admitted
        .work_order_digest
        .starts_with("sha256:"));
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
fn unknown_field_is_refused() {
    let mut raw = valid_order();
    raw["authority"] = json!("NONE");

    match admit_work_order(&raw) {
        Err(Refusal::Deserialization(msg)) => {
            assert!(msg.contains("authority"), "refusal must name the unknown field: {msg}");
        }
        other => panic!("expected Deserialization refusal for unknown field, got {other:?}"),
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
fn bad_replay_digest_is_refused() {
    let mut raw = valid_order();
    raw["replay_identity"] = json!("replay-001");

    match admit_work_order(&raw) {
        Err(Refusal::DigestMismatch { field, found }) => {
            assert_eq!(field, "replay_identity");
            assert_eq!(found, "replay-001");
        }
        other => panic!("expected DigestMismatch refusal, got {other:?}"),
    }
}

#[test]
fn standing_not_unknown_is_refused() {
    let mut raw = valid_order();
    raw["standing"] = json!("PARTIAL_ALIVE");

    match admit_work_order(&raw) {
        Err(Refusal::StandingNotUnknown { identity, found }) => {
            assert_eq!(identity, "XAAS-26922-21");
            assert_eq!(found, "PARTIAL_ALIVE");
        }
        other => panic!("expected StandingNotUnknown refusal, got {other:?}"),
    }
}

#[test]
fn refused_standing_vocabulary_is_legal_but_not_admittable() {
    let mut raw = valid_order();
    raw["standing"] = json!("REFUSED(bad evidence)");

    match admit_work_order(&raw) {
        Err(Refusal::StandingNotUnknown { found, .. }) => {
            assert_eq!(found, "REFUSED(bad evidence)");
        }
        other => panic!("expected StandingNotUnknown refusal, got {other:?}"),
    }
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
    assert!(err.contains("remediation"), "refusal carries remediation: {err}");
}
