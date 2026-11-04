//! DS-9: the descriptor→xaas-receipt loop witnessed END-TO-END, without a
//! real xaas fabric. This test plays the external executor honestly: it
//! mutates nothing between contract and receipt, and synthesizes execution
//! evidence strictly per the contract's own stated obligations — the
//! contract's `bridge.requires` lists name the courts/acceptance/falsifiers
//! the receipt must witness, and `fabric_verifier.steps` carries one pass
//! step per required court. The mapped receipt is then append-stamped into
//! a real on-disk ledger (the reconcile-append hop; the full reconcile.rs
//! Slice-3 compose is deferred — disclosed: this loop closes through
//! `ledger::append` + `ledger::fetch` verify directly).
//!
//! Standing: PARTIAL_ALIVE by construction — the executor here is a
//! simulation of the fabric, so this file witnesses the LOOP (contract →
//! evidence → receipt → verify → ledger), never a real-fabric integration.
//!
//! Chicago: real frontier/admit/digest/146+ tests machinery — real
//! frontier/admit/digest/ledger collaborators, state assertions on returned
//! values and on-disk ledger files, no mocks.

use ggen_sjira::descriptor::{
    build_xaas_contract, receipt_digest, receipt_from_xaas, DescriptorOpts, ReceiptRefusal,
};
use ggen_sjira::frontier::AuthorityIndex;
use ggen_sjira::ledger::{self, AppendOpts};
use serde_json::{json, Map, Value};
use tempfile::TempDir;

const HEAD: &str = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";

// ── fixtures ────────────────────────────────────────────────────────────────

fn pins(pinned: &[(&str, &str)]) -> AuthorityIndex {
    pinned
        .iter()
        .map(|(iri, digest)| (iri.to_string(), digest.to_string()))
        .collect()
}

fn order_json(identity: &str) -> Value {
    json!({
        "identity": identity,
        "title": "ship the kernel",
        "description": "ordered gate pipeline synthesis",
        "subject": "src/x.rs",
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": format!("https://graph.test/auth/{}", identity),
        "projections": ["receipt"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": ["f1"]
    })
}

fn two_admitted_orders() -> (Vec<Value>, AuthorityIndex) {
    let orders = vec![order_json("X1"), order_json("X2")];
    let authority = pins(&[
        ("https://graph.test/auth/X1", "sha256:aa"),
        ("https://graph.test/auth/X2", "sha256:bb"),
    ]);
    (orders, authority)
}

fn contract_opts(authority: &AuthorityIndex) -> DescriptorOpts<'_> {
    let mut o = DescriptorOpts::default();
    o.authority = Some(authority);
    o.verifier_suite = Some("cargo".to_string());
    o.execution_repo_alias = Some("ggen-main".to_string());
    o
}

// ── the honest simulated executor ───────────────────────────────────────────

/// Seals a receipt: `receipt_digest` = digest over the receipt minus its own
/// digest key — the sealing law the verifier recomputes.
fn seal(body: &mut Value) -> Value {
    let digest = receipt_digest(body);
    body.as_object_mut()
        .expect("receipt body is an object")
        .insert("receipt_digest".into(), json!(digest));
    body.clone()
}

fn str_list(v: &Value) -> Vec<String> {
    v.as_array()
        .map(|a| {
            a.iter()
                .filter_map(|s| s.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default()
}

/// The simulated fabric executor: reads its obligations from the contract
/// value itself (bridge.requires), never from the fixture. Mutates nothing.
fn executor_receipt(contract: &Value, outcome: &str) -> Value {
    let bridge = contract["bridge"].clone();
    let requires = &bridge["requires"];
    let courts = str_list(&requires["courts"]);
    let acceptance = str_list(&requires["acceptance"]);
    let falsifiers = str_list(&requires["falsifiers"]);
    let steps: Vec<Value> = courts
        .iter()
        .map(|c| json!({"id": c, "status": "pass"}))
        .collect();

    let mut body = json!({
        "identity": bridge["identity"],
        "final_head": HEAD,
        "outcome": outcome,
        "head_verified": true,
        "bridge": bridge,
        "fabric_verifier": {
            "status": "pass",
            "steps": steps,
            "court_receipt": {
                "acceptance_results": bool_map(&acceptance),
                "falsifier_results": survived_map(&falsifiers),
                "evidence_ceiling": "VERIFIED"
            }
        }
    });
    seal(&mut body)
}

fn bool_map(names: &[String]) -> Value {
    let mut m = Map::new();
    for n in names {
        m.insert(n.clone(), Value::Bool(true));
    }
    Value::Object(m)
}

fn survived_map(names: &[String]) -> Value {
    let mut m = Map::new();
    for n in names {
        m.insert(n.clone(), json!("survived"));
    }
    Value::Object(m)
}

// ── the loop, witnessed end-to-end over two admitted orders ─────────────────

#[test]
fn loop_over_two_orders_contract_to_ledger_append() {
    let (orders, authority) = two_admitted_orders();
    let o = contract_opts(&authority);

    let dir = TempDir::new().unwrap();
    let ledger_path = dir.path().join("ledger");
    let mut expected_seq = 0u64;

    for identity in ["X1", "X2"] {
        // 1. contract from the admitted order (real frontier + admit)
        let contract = build_xaas_contract(&orders, &[], identity, &o)
            .expect("admitted order builds a contract");

        // 2. external execution — the executor reads its obligations from
        //    the contract value itself, not from the fixture
        let receipt = executor_receipt(&contract, "alive");

        // 3. digest round-trip: the sealed digest recomputes over the
        //    receipt minus its digest key
        let stored = receipt["receipt_digest"].as_str().unwrap();
        assert_eq!(receipt_digest(&receipt), stored);

        let required_evidence: Vec<String> = orders
            .iter()
            .find(|o| o["identity"] == json!(identity))
            .map(|o| str_list(&o["required_evidence"]))
            .unwrap();
        let mapped = receipt_from_xaas(&receipt, &contract["bridge"], Some(&required_evidence))
            .expect("honest execution evidence verifies");

        // digest round-trip: the digest receipt_from_xaas recomputed over
        // the sealed receipt is exactly the digest the executor stamped
        assert_eq!(mapped["receipt_digest"], json!(stored));

        // state assertions on the mapped receipt
        assert_eq!(mapped["identity"], json!(identity));
        assert_eq!(mapped["target"], "ALIVE");
        assert_eq!(mapped["observed_execution"], true);
        assert_eq!(mapped["candidate_sha"], json!(HEAD));
        assert_eq!(mapped["court_results"]["c1"]["passed"], true);
        assert!(mapped["evidence_types"]
            .as_array()
            .unwrap()
            .contains(&json!("exact_head_verification")));

        // 4. reconcile-append via ledger::append (Slice-3 compose deferred —
        //    disclosed). The event commits to the mapped receipt.
        expected_seq += 1;
        let mut event = Map::new();
        event.insert("type".into(), json!("xaas_receipt"));
        event.insert("work_order_id".into(), json!(identity));
        event.insert("receipt".into(), mapped.clone());
        let appended = ledger::append(&ledger_path, &event, &AppendOpts::default())
            .expect("mapped receipt appends to the ledger");
        assert_eq!(appended.status, ledger::AppendStatus::Appended);
        assert_eq!(appended.event["seq"], json!(expected_seq));
    }

    // 5. replay: fetch the real on-disk ledger — every event_digest
    //    recomputes (ledger verify), seq stamped 1,2
    let events = ledger::fetch(&ledger_path).expect("ledger verifies");
    assert_eq!(events.len(), 2);
    for (i, e) in events.iter().enumerate() {
        assert_eq!(e["seq"], json!((i + 1) as u64));
        assert_eq!(e["type"], json!("xaas_receipt"));
        let r = &e["receipt"];
        assert_eq!(r["target"], "ALIVE");
        assert_eq!(r["observed_execution"], true);
        // the carried sealed digest is a valid sha256:<64 hex> receipt class
        let stored = r["receipt_digest"].as_str().unwrap();
        assert!(stored.starts_with("sha256:") && stored.len() == 71);
    }
    // distinct contracts -> distinct receipts -> distinct carried digests
    assert_ne!(
        events[0]["receipt"]["receipt_digest"],
        events[1]["receipt"]["receipt_digest"]
    );
}

// ── the refusal matrix, witnessed END-TO-END (contract → tamper → refuse) ──

/// Builds a real contract for X1 (real frontier/admit machinery), executes
/// honestly, then applies `tamper` to the sealed receipt and RE-SEALS.
/// Re-sealing isolates the target refusal from ReceiptDigestMismatch: the
/// tampered receipt is internally consistent — it is refused on the named
/// law, not on a stale digest.
fn contract_receipt_then_tamper<F: FnOnce(&mut Value)>(tamper: F) -> (Value, Value) {
    let (orders, authority) = two_admitted_orders();
    let o = contract_opts(&authority);
    let contract = build_xaas_contract(&orders, &[], "X1", &o).expect("contract builds");
    let mut receipt = executor_receipt(&contract, "alive");
    tamper(&mut receipt);
    let sealed = seal(&mut receipt);
    (contract, sealed)
}

#[test]
fn tampered_bridge_one_flipped_byte_refuses_bridge_mismatch() {
    let (contract, receipt) = contract_receipt_then_tamper(|r| {
        // flip one byte inside the receipt's embedded bridge copy
        r["bridge"]["subject"] = json!("src/y.rs");
    });
    let err = receipt_from_xaas(&receipt, &contract["bridge"], None).unwrap_err();
    assert_eq!(err, ReceiptRefusal::BridgeMismatch);
}

#[test]
fn alive_without_verifier_pass_refuses_end_to_end() {
    let (contract, receipt) = contract_receipt_then_tamper(|r| {
        r["fabric_verifier"]["status"] = json!("fail");
    });
    let err = receipt_from_xaas(&receipt, &contract["bridge"], None).unwrap_err();
    assert_eq!(err, ReceiptRefusal::AliveWithoutVerifierPass);
}

#[test]
fn unsupported_outcome_refuses_end_to_end() {
    let (contract, receipt) = contract_receipt_then_tamper(|r| {
        r["outcome"] = json!("teleported");
    });
    let err = receipt_from_xaas(&receipt, &contract["bridge"], None).unwrap_err();
    assert_eq!(err, ReceiptRefusal::UnsupportedOutcome("teleported".to_string()));
}
