//! Chicago tests for the execution loop: real file ledgers under
//! pid-unique temp roots, real blake3 journal, real authority graph —
//! state assertions on returned values and on-disk ledger bytes. No mocks.

use ggen_sjira::authority::{admission_digest, AuthorityGraph, AuthorityNode};
use ggen_sjira::execute::{check_promotion, run, ExecuteOpts, EXECUTION_CEILING, EXECUTION_FROM};
use ggen_sjira::{DependencyEdge, FrontierInput};

use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::path::PathBuf;

fn temp_ledger_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("ggen-sjira-execute-{}-{tag}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp ledger dir");
    dir
}

/// A pinned StrategicObjective authority node (real authority.rs law:
/// digest stamps idempotently and build_pinned pins it).
fn auth_node(iri: &str) -> AuthorityNode {
    let mut statements: BTreeMap<String, Vec<String>> = BTreeMap::new();
    statements.insert("sj:title".to_string(), vec!["ship the kernel".to_string()]);
    let bare = AuthorityNode {
        iri: iri.to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements,
    };
    let digest = admission_digest(&bare);
    let mut stamped: BTreeMap<String, Vec<String>> = BTreeMap::new();
    stamped.insert("sj:title".to_string(), vec!["ship the kernel".to_string()]);
    stamped.insert("admissionDigest".to_string(), vec![digest]);
    AuthorityNode {
        iri: iri.to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements: stamped,
    }
}

fn auth_graph(iris: &[&str]) -> AuthorityGraph {
    AuthorityGraph {
        authorities: iris.iter().map(|iri| auth_node(iri)).collect(),
        trust_roots: iris
            .iter()
            .map(|iri| ggen_sjira::authority::TrustRootCandidate {
                node: format!("https://graph.test/trust/{iri}"),
                authority_iri: iri.to_string(),
                admission_digest: admission_digest(&auth_node(iri)),
                // A real durable locator under the CE23-9 grammar.
                source_locator:
                    "git:owner/repo@0123456789abcdef0123456789abcdef01234567:priv/ontology.ttl"
                        .to_string(),
            })
            .collect(),
    }
}

fn order_json(identity: &str, origin: &str) -> Value {
    json!({
        "identity": identity,
        "title": "t",
        "description": "d",
        "subject": "src/x.rs",
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": origin,
        "projections": ["receipt"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": ["f1"]
    })
}

fn input(identity: &str, origin: &str) -> FrontierInput {
    FrontierInput {
        work_order: order_json(identity, origin),
        dependencies: Vec::new(),
        path_scope: vec!["lib/".to_string()],
        concurrency_key: format!("key-{identity}"),
    }
}

fn with_dep(mut fi: FrontierInput, dep: DependencyEdge) -> FrontierInput {
    fi.dependencies.push(dep);
    fi
}

fn dep_on(upstream: &str, standing: &str) -> DependencyEdge {
    DependencyEdge {
        kind: "requiresSemanticIdentity".to_string(),
        upstream: upstream.to_string(),
        required_standing: Some(standing.to_string()),
        required_receipt_digest: None,
    }
}

const AUTH_A: &str = "https://graph.test/auth/OA";
const AUTH_B: &str = "https://graph.test/auth/OB";
const AUTH_C: &str = "https://graph.test/auth/OC";

// ── 1. a 2-order graph executes to PARTIAL_ALIVE receipts ─────────────────

#[test]
fn two_order_graph_executes_to_partial_alive_receipts() {
    let dir = temp_ledger_dir("two-order");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A, AUTH_B]);

    // OB depends on OA reaching PARTIAL_ALIVE: it must NOT be executable in
    // pass 1; it becomes eligible only from OA's on-disk receipt evidence.
    let oa = input("OA", AUTH_A);
    let ob = with_dep(input("OB", AUTH_B), dep_on("OA", "PARTIAL_ALIVE"));

    let out = run(&[oa, ob], &ledger, &graph, &ExecuteOpts::default())
        .expect("the two-order graph must execute to fixed point");

    // Both orders executed, in dependency order, at the honest ceiling.
    let ids: Vec<&str> = out.plans.iter().map(|p| p.identity.as_str()).collect();
    assert_eq!(ids, vec!["OA", "OB"], "dependency order");
    for plan in &out.plans {
        assert_eq!(plan.target_standing, EXECUTION_CEILING);
        assert_eq!(plan.target_standing, "PARTIAL_ALIVE");
        assert!(!plan.head_verified, "head verification is fabric-only");
        assert_eq!(plan.authority, "NONE");
        assert!(plan.plan_digest.starts_with("sha256:"));
        assert!(plan.projections.contains(&"receipt".to_string()));
        assert_eq!(plan.required_courts, vec!["c1".to_string()]);
    }

    // The real ledger holds exactly two legal transitions, digest-verified
    // by fetch itself.
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 2);
    for (i, event) in on_disk.iter().enumerate() {
        assert_eq!(
            event["work_order_id"],
            json!(["OA", "OB"][i]),
            "append order"
        );
        assert_eq!(event["from_standing"], json!(EXECUTION_FROM));
        assert_eq!(event["to_standing"], json!("PARTIAL_ALIVE"));
        let digest = event["receipt_digest"].as_str().unwrap();
        assert_eq!(
            digest, out.plans[i].plan_digest,
            "the transition records the plan digest as receipt evidence"
        );
    }

    // The blake3 journal chains every execution.
    assert_eq!(out.journal.len(), 2);
    assert_eq!(out.journal.verify_chain(), Ok(()));
    assert_eq!(out.journal.admitted_count(), 2);
    assert_eq!(out.journal.refused_count(), 0);

    // Final standings projected from the real ledger.
    assert_eq!(
        out.standings.get("OA").map(String::as_str),
        Some("PARTIAL_ALIVE")
    );
    assert_eq!(
        out.standings.get("OB").map(String::as_str),
        Some("PARTIAL_ALIVE")
    );
    assert_eq!(out.iterations, 2);
    // No event the loop wrote ever targets ALIVE (the fabric-only ceiling).
    for event in &out.events {
        assert_ne!(event["to_standing"], json!("ALIVE"));
    }
}

// ── 2. blocked orders stay blocked, with reasons ──────────────────────────

#[test]
fn blocked_orders_stay_blocked_with_reasons() {
    let dir = temp_ledger_dir("blocked");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    // OC's origin authority is NOT in the graph: frontier blocks it.
    // OD is already PARTIAL_ALIVE on disk: standing-blocked.
    let oc = input("OC", "https://graph.test/auth/unknown");
    let od = input("OD", AUTH_A);

    // Seed OD's real transition through the same ledger API the loop uses.
    let mut seed = serde_json::Map::new();
    seed.insert("work_order_id".to_string(), json!("OD"));
    seed.insert("from_standing".to_string(), json!("UNKNOWN"));
    seed.insert("to_standing".to_string(), json!("PARTIAL_ALIVE"));
    seed.insert("receipt_digest".to_string(), json!("sha256:seed"));
    let appended =
        ggen_sjira::ledger::append(&ledger, &seed, &Default::default()).expect("seed append");
    assert_eq!(appended.status, ggen_sjira::ledger::AppendStatus::Appended);

    let out = run(&[oc, od], &ledger, &graph, &ExecuteOpts::default())
        .expect("all-blocked is a COMPLETED run, not a refusal");

    assert!(out.plans.is_empty(), "nothing may execute");
    assert!(out.events.is_empty());
    assert_eq!(out.journal.len(), 0);
    // The ledger is byte-unchanged by the run: still only the seed event.
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(
        on_disk.len(),
        1,
        "the append is the LAST write; a blocked run appends nothing"
    );

    let by_id: BTreeMap<String, String> = out
        .blocked
        .iter()
        .filter_map(|b| b.identity.clone().map(|i| (i, b.reason.clone())))
        .collect();
    assert_eq!(
        by_id.get("OC").map(String::as_str),
        Some("origin_not_admitted"),
        "unknown origin authority blocks with the frontier reason"
    );
    assert_eq!(
        by_id.get("OD").map(String::as_str),
        Some("standing=PARTIAL_ALIVE"),
        "already-promoted orders stay blocked with the kernel's own reason"
    );
}

// ── 3. max-iterations refuses typed ───────────────────────────────────────

#[test]
fn max_iterations_refuses_typed_exhaustion() {
    let dir = temp_ledger_dir("exhausted");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A, AUTH_B, AUTH_C]);

    // Chain OA -> OB -> OC needs three passes; a one-pass budget cannot
    // empty the frontier.
    let oa = input("OA", AUTH_A);
    let ob = with_dep(input("OB", AUTH_B), dep_on("OA", "PARTIAL_ALIVE"));
    let oc = with_dep(input("OC", AUTH_C), dep_on("OB", "PARTIAL_ALIVE"));

    let err = run(
        &[oa, ob, oc],
        &ledger,
        &graph,
        &ExecuteOpts {
            max_iterations: 1,
            lease: None,
            epoch_watermark: None,
            epoch_extensions: Default::default(),
        },
    )
    .expect_err("a one-pass budget over a three-deep chain must refuse");

    assert_eq!(err.reason, "exhausted");
    assert_eq!(err.broken_term, "mu_on_O");
    assert_eq!(err.hop, "frontier");
    assert!(err.standing.starts_with("REFUSED"));
    assert!(
        err.detail.contains("iteration budget"),
        "remediation named: {}",
        err.detail
    );

    // The refusal is honest about what DID happen: OA executed before the
    // budget ran out, and its receipt is on disk.
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 1);
    assert_eq!(on_disk[0]["work_order_id"], json!("OA"));
    assert_eq!(on_disk[0]["to_standing"], json!("PARTIAL_ALIVE"));
}

// ── 4. the standing ceiling is a typed law ────────────────────────────────

#[test]
fn promotion_ceiling_is_typed() {
    assert!(check_promotion("UNKNOWN", "PARTIAL_ALIVE").is_ok());

    for bad in [
        ("UNKNOWN", "ALIVE"),
        ("PARTIAL_ALIVE", "ALIVE"),
        ("UNKNOWN", "UNKNOWN"),
        ("PARTIAL_ALIVE", "PARTIAL_ALIVE"),
        ("ALIVE", "PARTIAL_ALIVE"),
    ] {
        let err =
            check_promotion(bad.0, bad.1).expect_err("only UNKNOWN -> PARTIAL_ALIVE is writable");
        assert_eq!(err.reason, "standing_ceiling");
        assert_eq!(err.broken_term, "R_missing_standing");
        assert_eq!(err.hop, "reconcile");
        assert!(
            err.detail.contains("external-fabric-only"),
            "the ceiling law is named: {}",
            err.detail
        );
    }
}

// ── 5. a tampered ledger refuses typed BEFORE anything runs ───────────────

#[test]
fn tampered_ledger_refuses_at_frontier() {
    let dir = temp_ledger_dir("tampered");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let mut event = serde_json::Map::new();
    event.insert("work_order_id".to_string(), json!("OA"));
    event.insert("from_standing".to_string(), json!("UNKNOWN"));
    event.insert("to_standing".to_string(), json!("PARTIAL_ALIVE"));
    event.insert("receipt_digest".to_string(), json!("sha256:seed"));
    ggen_sjira::ledger::append(&ledger, &event, &Default::default()).expect("seed");

    // Tamper with the real file on disk: the recorded digest no longer
    // recomputes over the content.
    let body = std::fs::read_to_string(&ledger).unwrap();
    let tampered = body.replace("PARTIAL_ALIVE", "ALIVE");
    std::fs::write(&ledger, tampered).unwrap();

    let oa = input("OA", AUTH_A);
    let err = run(&[oa], &ledger, &graph, &ExecuteOpts::default())
        .expect_err("a tampered ledger must refuse before anything runs");

    assert_eq!(err.hop, "frontier");
    assert_eq!(err.reason, "ledger_refused");
    assert_eq!(err.broken_term, "R_missing_replay");
    assert!(err.standing == "REFUSED");
}

// ── 6. replay is idempotent: a re-run appends nothing new ─────────────────

#[test]
fn rerun_is_idempotent_already_recorded() {
    let dir = temp_ledger_dir("replay");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let oa = input("OA", AUTH_A);
    let first =
        run(&[oa.clone()], &ledger, &graph, &ExecuteOpts::default()).expect("first run executes");
    assert_eq!(first.plans.len(), 1);

    // Second run: OA is already PARTIAL_ALIVE on disk, so the frontier is
    // empty and nothing new appends.
    let second =
        run(&[oa], &ledger, &graph, &ExecuteOpts::default()).expect("replay run completes");
    assert!(second.plans.is_empty());
    assert_eq!(
        second.blocked[0].reason, "standing=PARTIAL_ALIVE",
        "the executed order is standing-blocked on replay"
    );
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 1, "replay must not double-append");
}
