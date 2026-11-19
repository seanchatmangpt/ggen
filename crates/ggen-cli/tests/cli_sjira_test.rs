//! Chicago tests for the `ggen sjira` CLI surface over the real kernel:
//! real files (TempDir), real kernel calls, real ledger IO — no mocks.
//! Assertions land on observable state: return values, file contents,
//! typed refusal text.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests

use std::path::PathBuf;

use clap_noun_verb::NounVerbError;
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};
use tempfile::TempDir;

use ggen_cli_lib::cmds::sjira;

// ── fixtures ─────────────────────────────────────────────────────────────

/// Kernel-valid work order (same field set the ggen-sjira unit tests
/// admit successfully).
fn valid_order(identity: &str) -> Value {
    json!({
        "identity": identity,
        "title": "t",
        "description": "d",
        "subject": format!("src/{identity}.rs"),
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://example.test/auth/X1",
        "projections": ["jira"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": ["f1"],
        "path_scope": ["lib/"]
    })
}

fn write_file(dir: &TempDir, name: &str, v: &Value) -> PathBuf {
    let p = dir.path().join(name);
    std::fs::write(&p, serde_json::to_vec_pretty(v).unwrap()).unwrap();
    p
}

fn order_entry(order: Value, deps: Value) -> Value {
    json!({
        "work_order": order,
        "dependencies": deps,
        "path_scope": [],
        "concurrency_key": "global",
    })
}

/// Two-order backlog: o1 independent, o2 requires upstream o1.
fn backlog() -> Value {
    json!([
        order_entry(valid_order("O1"), json!([])),
        order_entry(
            valid_order("O2"),
            json!([
                {"kind": "requiresSemanticIdentity", "upstream": "O1"}
            ])
        )
    ])
}

/// One StrategicObjective authority node + its trust-root pin, serialized
/// exactly as the execute verb reads it from disk.
fn authority_graph() -> Value {
    use ggen_sjira::authority::{
        admission_digest, AuthorityGraph, AuthorityNode, TrustRootCandidate,
    };
    use std::collections::BTreeMap;

    let mut statements: BTreeMap<String, Vec<String>> = BTreeMap::new();
    statements.insert(
        "sj:objective".to_string(),
        vec!["ship the loop".to_string()],
    );
    let mut node = AuthorityNode {
        iri: "https://example.test/auth/X1".to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements,
    };
    // judge() requires exactly one `admissionDigest` statement equal to
    // admission_digest(&node); the digest law EXCLUDES that statement, so
    // insert it after computing and the value stays stable.
    let digest = admission_digest(&node);
    node.statements
        .insert("admissionDigest".to_string(), vec![digest.clone()]);
    let graph = AuthorityGraph {
        authorities: vec![node],
        trust_roots: vec![TrustRootCandidate {
            node: "https://example.test/trust/r1".to_string(),
            admission_digest: digest.clone(),
            authority_iri: "https://example.test/auth/X1".to_string(),
            source_locator: format!("git:owner/repo@{}:priv/ontology.ttl", "a".repeat(40)),
        }],
    };
    serde_json::to_value(graph).unwrap()
}

/// frontier's --authority file shape: the map-membership index
/// (IRI → admission digest), projected from the same node.
fn authority_index() -> Value {
    use ggen_sjira::authority::{admission_digest, AuthorityNode};
    use std::collections::BTreeMap;

    let mut statements: BTreeMap<String, Vec<String>> = BTreeMap::new();
    statements.insert(
        "sj:objective".to_string(),
        vec!["ship the loop".to_string()],
    );
    let node = AuthorityNode {
        iri: "https://example.test/auth/X1".to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements,
    };
    let digest = admission_digest(&node);
    json!({ "https://example.test/auth/X1": digest })
}

// ── admit ────────────────────────────────────────────────────────────────

#[test]
fn admit_accepts_valid_order() {
    let dir = TempDir::new().unwrap();
    let f = write_file(&dir, "order.json", &valid_order("X1"));
    let out = sjira::admit(f).expect("admit must succeed");
    assert_eq!(out["admitted"], json!(true));
    assert_eq!(out["authority"], json!("NONE"));
    assert_eq!(out["identity"], json!("X1"));
    assert_eq!(out["standing"], json!("UNKNOWN"));
}

#[test]
fn admit_refuses_bad_standing() {
    let dir = TempDir::new().unwrap();
    let mut order = valid_order("bad");
    order["standing"] = json!("TOTALLY_BOGUS_STANDING");
    let f = write_file(&dir, "bad.json", &order);
    let err: NounVerbError = sjira::admit(f).expect_err("bogus standing must refuse");
    let msg = err.to_string();
    assert!(msg.contains("REFUSED:SEMANTIC_JIRA"), "{msg}");
    assert!(msg.contains("invalid standing"), "{msg}");
}

#[test]
fn admit_refuses_missing_file() {
    let err = sjira::admit(PathBuf::from("/nonexistent/order.json")).unwrap_err();
    assert!(err.to_string().contains("cannot read"));
}

#[test]
fn admit_refuses_non_json() {
    let dir = TempDir::new().unwrap();
    let p = dir.path().join("bad.json");
    std::fs::write(&p, "not json at all").unwrap();
    let err = sjira::admit(p).unwrap_err();
    assert!(err.to_string().contains("not valid JSON"));
}

// ── frontier ─────────────────────────────────────────────────────────────

#[test]
fn frontier_without_authority_uses_canonical_index() {
    // Hands-free CLI default = embedded pack index (Elixir index_from/1 parity).
    // Non-pack origins are blocked as origin_not_admitted — not
    // authority_index_unavailable (that remains a kernel-None path only).
    let dir = TempDir::new().unwrap();
    let f = write_file(&dir, "orders.json", &backlog());
    let out = sjira::frontier(f, None, None, None).unwrap();
    let blocked = out["blocked"].as_array().unwrap();
    assert_eq!(blocked.len(), 2, "frontier dump: {out}");
    for b in blocked {
        assert_eq!(b["reason"], json!("origin_not_admitted"));
    }
    assert_eq!(out["eligible_count"], json!(0));
}

#[test]
fn frontier_hands_free_admits_canonical_code_work_origin() {
    // Friday twin: Mix eligible=[FRI-FMT-A] with pack objective-code-work-authority
    // and no --authority-graph; Rust CLI must match without --authority-path.
    let dir = TempDir::new().unwrap();
    let mut a = valid_order("FRI-FMT-A");
    a["origin_authority"] = json!(ggen_sjira::OBJECTIVE_CODE_WORK_AUTHORITY);
    a["path_scope"] = json!(["lib"]);
    let mut b = valid_order("FRI-FMT-B");
    b["origin_authority"] = json!(ggen_sjira::OBJECTIVE_CODE_WORK_AUTHORITY);
    b["path_scope"] = json!(["test"]);
    b["dependencies"] = json!([{
        "type": "requiresReceipt",
        "upstream": "FRI-FMT-A",
        "required_standing": "ALIVE"
    }]);
    let orders = json!({"work_orders": [a, b]});
    let f = write_file(&dir, "orders.json", &orders);
    let out = sjira::frontier(f, None, None, None).unwrap();
    assert_eq!(out["eligible_count"], json!(1), "frontier dump: {out}");
    let eligible = out["eligible"].as_array().unwrap();
    assert_eq!(eligible[0]["identity"], json!("FRI-FMT-A"));
    let blocked = out["blocked"].as_array().unwrap();
    assert_eq!(blocked.len(), 1);
    assert_eq!(blocked[0]["identity"], json!("FRI-FMT-B"));
    assert_eq!(blocked[0]["reason"], json!("dependencies_unsatisfied"));
}

#[test]
fn frontier_eligible_with_authority() {
    let dir = TempDir::new().unwrap();
    let index = authority_index();
    let orders_f = write_file(&dir, "orders.json", &backlog());
    let index_f = write_file(&dir, "index.json", &index);
    let out = sjira::frontier(orders_f, None, None, Some(index_f)).unwrap();
    // o1 is eligible; o2 requires upstream evidence o1 has not stamped yet.
    assert_eq!(out["eligible_count"], json!(1), "frontier dump: {out}");
    let blocked = out["blocked"].as_array().unwrap();
    assert_eq!(blocked.len(), 1);
    assert_eq!(blocked[0]["identity"], json!("O2"));
    assert_eq!(blocked[0]["reason"], json!("dependencies_unsatisfied"));
}

/// Elixir SoT drop-in: `{"work_orders":[...]}` with deps keyed `"type"`
/// (no Rust `{work_order,...}` wrap). Identities are B1-valid.
#[test]
fn frontier_accepts_elixir_work_orders_envelope() {
    let dir = TempDir::new().unwrap();
    let index = authority_index();
    let mut a = valid_order("SJ-O1");
    a.as_object_mut()
        .unwrap()
        .insert("path_scope".into(), json!(["lib/"]));
    let mut b = valid_order("SJ-O2");
    {
        let obj = b.as_object_mut().unwrap();
        obj.insert("path_scope".into(), json!(["test/"]));
        obj.insert(
            "dependencies".into(),
            json!([{"type": "requiresSemanticIdentity", "upstream": "SJ-O1"}]),
        );
    }
    let elixir_sot = json!({"work_orders": [a, b]});
    let orders_f = write_file(&dir, "orders.json", &elixir_sot);
    let index_f = write_file(&dir, "index.json", &index);
    let out = sjira::frontier(orders_f, None, None, Some(index_f)).unwrap();
    assert_eq!(out["eligible_count"], json!(1), "result: {out}");
    assert_eq!(out["eligible"][0]["identity"], json!("SJ-O1"));
    assert_eq!(out["blocked"][0]["identity"], json!("SJ-O2"));
    assert_eq!(
        out["blocked"][0]["reason"],
        json!("dependencies_unsatisfied")
    );
}

#[test]
fn frontier_events_unlock_dependents() {
    // With a stamped o1 transition on the log, o2 becomes eligible —
    // the event-sourced projection is what opens the dependent.
    let dir = TempDir::new().unwrap();
    let index = authority_index();
    let orders_f = write_file(&dir, "orders.json", &backlog());
    let index_f = write_file(&dir, "index.json", &index);
    let events = json!([
        {"work_order_id": "O1", "from_standing": "UNKNOWN",
         "to_standing": "PARTIAL_ALIVE", "seq": 1,
         "receipt_digest": "sha256:".to_owned() + &"c".repeat(64)}
    ]);
    let events_f = write_file(&dir, "events.json", &events);
    let out = sjira::frontier(orders_f, Some(events_f), None, Some(index_f)).unwrap();
    // The chain tip wins: o1 is now PARTIAL_ALIVE so it leaves the
    // frontier (blocked `standing=PARTIAL_ALIVE`); its stamped evidence
    // is exactly what unlocks the dependent o2.
    assert_eq!(out["eligible_count"], json!(1));
    let blocked = out["blocked"].as_array().unwrap();
    assert_eq!(blocked.len(), 1);
    assert_eq!(blocked[0]["identity"], json!("O1"));
    assert_eq!(blocked[0]["reason"], json!("standing=PARTIAL_ALIVE"));
    assert_eq!(out["standings"]["O1"], json!("PARTIAL_ALIVE"));
}

// ── execute over a real ledger (Chicago: assert the file really grew) ────

#[test]
fn execute_runs_two_order_backlog() {
    let dir = TempDir::new().unwrap();
    let orders_f = write_file(&dir, "orders.json", &backlog());
    let ledger = dir.path().join("ledger.json");
    let auth_f = write_file(&dir, "auth.json", &authority_graph());
    let out = sjira::execute(orders_f, ledger.clone(), auth_f, None, None, None, None).unwrap();
    assert_eq!(out["executed"], json!(2), "result: {out}");
    assert!(out["iterations"].as_u64().expect("iterations is a number") >= 2);
    // Chicago: the REAL ledger on disk must carry both identities. The
    // kernel materializes a missing ledger path as a DIRECTORY ledger
    // (ledger::kind defaults to dir for a fresh path), so read every
    // file under it and assert on the concatenated bytes.
    assert!(
        ledger.is_dir(),
        "fresh ledger path must materialize as a dir"
    );
    let mut ledger_body = String::new();
    let mut entries: Vec<_> = std::fs::read_dir(&ledger)
        .unwrap()
        .map(|e| e.unwrap().path())
        .collect();
    entries.sort();
    for f in entries {
        ledger_body.push_str(&std::fs::read_to_string(f).unwrap());
    }
    assert!(ledger_body.contains("O1"));
    assert!(ledger_body.contains("O2"));
    assert_eq!(out["standings"]["O1"], json!("PARTIAL_ALIVE"));
}

#[test]
fn execute_refuses_tampered_ledger_before_anything_runs() {
    let dir = TempDir::new().unwrap();
    let orders_f = write_file(&dir, "orders.json", &backlog());
    let ledger = dir.path().join("ledger.json");
    std::fs::write(&ledger, "not ndjson bytes").unwrap();
    let auth_f = write_file(&dir, "auth.json", &authority_graph());
    let result = sjira::execute(orders_f, ledger, auth_f, None, None, None, None);
    let err = match result {
        Err(e) => e,
        Ok(v) => panic!("expected a typed refusal, got Ok: {v}"),
    };
    assert!(err.to_string().contains("ledger_refused"), "{err}");
}

// ── lease-admit ──────────────────────────────────────────────────────────

fn signed_lease_pair() -> (Value, Value) {
    use ggen_sjira::lease::{self, SovereignLease};

    let k1 = SigningKey::from_bytes(&[7u8; 32]);
    let k2 = SigningKey::from_bytes(&[11u8; 32]);
    let lease = SovereignLease {
        lease_id: "lease-1".into(),
        subject_iri: "https://example.test/subject".into(),
        shapes: vec!["shape-a".into()],
        ontology_digest: "sha256:".to_owned() + &"a".repeat(64),
        granted_at: "2026-11-03T00:00:00Z".into(),
        signatures: vec![],
    };
    let sig1 = lease::sign_record(&lease, "signer-1", &k1);
    let sig2 = lease::sign_record(&lease, "signer-2", &k2);
    let mut lease_value = serde_json::to_value(&lease).unwrap();
    lease_value["signatures"] = serde_json::to_value(vec![sig1, sig2]).unwrap();
    let keys = json!({
        "signer-1": hex_of(&k1.verifying_key()),
        "signer-2": hex_of(&k2.verifying_key()),
    });
    (lease_value, keys)
}

fn hex_of(key: &ed25519_dalek::VerifyingKey) -> String {
    use std::fmt::Write as _;
    let mut out = String::with_capacity(64);
    for b in key.as_bytes() {
        let _ = write!(out, "{b:02x}");
    }
    out
}

#[test]
fn lease_admit_accepts_two_signer_lease() {
    let (lease, keys) = signed_lease_pair();
    let dir = TempDir::new().unwrap();
    let lf = write_file(&dir, "lease.json", &lease);
    let kf = write_file(&dir, "keys.json", &keys);
    let out = sjira::lease_admit(lf, "sha256:".to_owned() + &"b".repeat(64), kf).unwrap();
    assert_eq!(out["admitted"], json!(true));
    assert_eq!(
        out["verified_signers"],
        json!(["signer-1", "signer-2"]),
        "kernel sorts+dedups signers; assert sorted order"
    );
}

#[test]
fn lease_admit_refuses_single_signature() {
    let (mut lease, keys) = signed_lease_pair();
    lease["signatures"].as_array_mut().unwrap().truncate(1);
    let dir = TempDir::new().unwrap();
    let lf = write_file(&dir, "lease.json", &lease);
    let kf = write_file(&dir, "keys.json", &keys);
    let err = sjira::lease_admit(lf, "sha256:".to_owned() + &"b".repeat(64), kf).unwrap_err();
    assert!(err.to_string().contains("insufficient signatures"), "{err}");
}

#[test]
fn lease_admit_refuses_unchanged_digest() {
    let (lease, keys) = signed_lease_pair();
    let dir = TempDir::new().unwrap();
    let lf = write_file(&dir, "lease.json", &lease);
    let kf = write_file(&dir, "keys.json", &keys);
    // Present digest equals the lease's granted-over digest — a no-op
    // evolution the kernel must refuse.
    let present = "sha256:".to_owned() + &"a".repeat(64);
    let err = sjira::lease_admit(lf, present, kf).unwrap_err();
    assert!(err.to_string().contains("shape digest unchanged"), "{err}");
}

#[test]
fn lease_admit_refuses_bad_key_hex() {
    let (lease, mut keys) = signed_lease_pair();
    keys["signer-1"] = json!("zzzz");
    let dir = TempDir::new().unwrap();
    let err = sjira::lease_admit(
        write_file(&dir, "lease.json", &lease),
        "sha256:".to_owned() + &"b".repeat(64),
        write_file(&dir, "keys.json", &keys),
    )
    .unwrap_err();
    assert!(err.to_string().contains("not valid lowercase hex"), "{err}");
}

// ── epoch-check ──────────────────────────────────────────────────────────

#[test]
fn epoch_check_accepts_no_epoch_field() {
    let dir = TempDir::new().unwrap();
    let f = write_file(&dir, "order.json", &valid_order("X1"));
    let out = sjira::epoch_check(f, None).unwrap();
    assert_eq!(out["epoch"], json!("ok"));
}

#[test]
fn epoch_check_refuses_missing_watermark_carries_exit5_marker() {
    let dir = TempDir::new().unwrap();
    let mut order = valid_order("X1");
    order["epoch"] = json!("e1");
    let f = write_file(&dir, "order.json", &order);
    let err = sjira::epoch_check(f, None).unwrap_err();
    let msg = err.to_string();
    // src/main.rs maps REFUSED_STALE_LEASE to exit 5 — the documented
    // Andon class for a stale epoch. The marker must survive Display.
    assert!(msg.contains("REFUSED_STALE_LEASE"), "{msg}");
    assert!(msg.contains("EPOCH_WATERMARK_UNAVAILABLE"), "{msg}");
}

#[test]
fn epoch_check_refuses_legacy_edit() {
    let dir = TempDir::new().unwrap();
    let mut order = valid_order("X1");
    order["epoch"] = json!("e1");
    order["plan_touches"] = json!(["src/X1.rs"]);
    let wm = json!({ "files": [{"path": "src/X1.rs"}] });
    let order_f = write_file(&dir, "order.json", &order);
    let wm_f = write_file(&dir, "watermark.json", &wm);
    let err = sjira::epoch_check(order_f, Some(wm_f)).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("EPOCH_LEGACY_EDIT"), "{msg}");
    assert!(msg.contains("REFUSED_STALE_LEASE"), "{msg}");
}

#[test]
fn epoch_check_accepts_fresh_plan() {
    let dir = TempDir::new().unwrap();
    let mut order = valid_order("X1");
    order["epoch"] = json!("e1");
    order["plan_touches"] = json!(["src/X1.rs"]);
    order["manufacture_plan"] = json!({"src/X1.rs": "generated"});
    let wm = json!({ "files": [{"path": "src/X1.rs"}] });
    let order_f = write_file(&dir, "order.json", &order);
    let wm_f = write_file(&dir, "watermark.json", &wm);
    let out = sjira::epoch_check(order_f, Some(wm_f)).unwrap();
    assert_eq!(out["epoch"], json!("ok"));
}

// ── reconcile (hands-free canonical authority) ───────────────────────────

fn reconcile_receipt(order: &Value) -> Value {
    let admitted = ggen_sjira::admit_work_order(order).expect("fixture order admits");
    json!({
        "definition_digest": admitted.definition_digest,
        "snapshot_digest": format!("sha256:{}", "a".repeat(64)),
        "target": "PARTIAL_ALIVE",
        "evidence": {
            "subject": order["subject"],
            "repository": order["repository"],
            "base_sha": order["base_sha"],
            "court_results": {"c1": {"passed": true}},
            "evidence_types": ["e1"],
            "acceptance_results": {"a1": true},
            "falsifier_results": {"f1": "survived"}
        }
    })
}

#[test]
fn reconcile_hands_free_with_canonical_pack_origin() {
    use ggen_sjira::authority::OBJECTIVE_CODE_WORK_AUTHORITY;

    let dir = TempDir::new().unwrap();
    let mut order = valid_order("X1");
    order["origin_authority"] = json!(OBJECTIVE_CODE_WORK_AUTHORITY);
    order["evidence_ceiling"] = json!("VERIFIED");
    let receipt = reconcile_receipt(&order);
    let order_f = write_file(&dir, "order.json", &order);
    let receipt_f = write_file(&dir, "receipt.json", &receipt);
    let ledger = dir.path().join("ledger");
    let out = sjira::reconcile(order_f, receipt_f, ledger.clone(), None, None).unwrap();
    assert_eq!(out["status"], json!("appended"));
    assert_eq!(out["event"]["identity"], json!("X1"));
    assert_eq!(out["event"]["to"], json!("PARTIAL_ALIVE"));
    assert!(ledger.is_dir(), "ledger must materialize as a dir");
}

#[test]
fn reconcile_hands_free_refuses_unknown_origin() {
    let dir = TempDir::new().unwrap();
    let mut order = valid_order("X1");
    order["evidence_ceiling"] = json!("VERIFIED");
    // origin is https://example.test/auth/X1 — not in the canonical pack.
    let receipt = reconcile_receipt(&order);
    let err = sjira::reconcile(
        write_file(&dir, "order.json", &order),
        write_file(&dir, "receipt.json", &receipt),
        dir.path().join("ledger"),
        None,
        None,
    )
    .unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("REFUSED:SEMANTIC_JIRA"), "{msg}");
    assert!(
        msg.contains("origin_not_admitted") || msg.contains("AuthorityNotAdmitted"),
        "{msg}"
    );
}
