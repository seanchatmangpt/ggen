//! Chicago tests for the completion driver: real file ledgers under
//! pid-unique temp roots, real authority graph, real execution chain —
//! state assertions on the returned `(exit, receipt)` and on-disk ledger
//! bytes. No mocks.

use ggen_sjira::authority::{admission_digest, AuthorityGraph, AuthorityNode};
use ggen_sjira::complete::{run, sanitize, CompleteOpts, DEFAULT_MAX};
use ggen_sjira::{DependencyEdge, FrontierInput};

use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::path::PathBuf;

fn temp_ledger_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "ggen-sjira-complete-{}-{tag}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp ledger dir");
    dir
}

/// A pinned StrategicObjective authority node (real authority.rs law:
/// digest stamps idempotently and build_pinned pins it).
fn auth_node(iri: &str) -> AuthorityNode {
    let mut statements: BTreeMap<String, Vec<String>> = BTreeMap::new();
    statements.insert(
        "sj:title".to_string(),
        vec!["ship the kernel".to_string()],
    );
    let bare = AuthorityNode {
        iri: iri.to_string(),
        types: vec!["StrategicObjective".to_string()],
        statements,
    };
    let digest = admission_digest(&bare);
    let mut stamped: BTreeMap<String, Vec<String>> = BTreeMap::new();
    stamped.insert(
        "sj:title".to_string(),
        vec!["ship the kernel".to_string()],
    );
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

// ── 1. a two-order backlog closes both ────────────────────────────────────

#[test]
fn two_order_backlog_closes_both() {
    let dir = temp_ledger_dir("two-order");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A, AUTH_B]);

    let oa = input("OA", AUTH_A);
    let ob = with_dep(input("OB", AUTH_B), dep_on("OA", "PARTIAL_ALIVE"));

    let (exit, receipt) = run(&[oa, ob], &ledger, Some(&graph), &CompleteOpts::default());

    assert_eq!(exit, 0, "the frontier must empty: {receipt}");
    assert_eq!(receipt["status"], json!("complete"));
    assert_eq!(receipt["closed"], json!(["OA", "OB"]), "close order");
    assert_eq!(receipt["closed_count"], json!(2));
    assert_eq!(receipt["orders"].as_array().map(Vec::len), Some(2));
    assert_eq!(receipt["blocked"], json!([]));
    assert_eq!(receipt["blocked_count"], json!(0));

    // The real ledger holds exactly one event per closed order.
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 2, "one ledger event per closed order");
    assert_eq!(on_disk[0]["work_order_id"], json!("OA"));
    assert_eq!(on_disk[1]["work_order_id"], json!("OB"));
    assert_eq!(on_disk[1]["to_standing"], json!("PARTIAL_ALIVE"));

    // The per-order receipts are the executed plans, in close order.
    assert_eq!(receipt["orders"][0]["identity"], json!("OA"));
    assert_eq!(receipt["orders"][1]["identity"], json!("OB"));
    for order in receipt["orders"].as_array().unwrap() {
        assert_eq!(order["target_standing"], json!("PARTIAL_ALIVE"));
        assert!(order["plan_digest"]
            .as_str()
            .unwrap()
            .starts_with("sha256:"));
    }
}

fn with_dep(mut fi: FrontierInput, dep: DependencyEdge) -> FrontierInput {
    fi.dependencies.push(dep);
    fi
}

// ── 2. a refusal stops the loop with the ledger byte-unchanged ────────────
//
// Through this composition the per-order refusal branch is structurally
// unreachable (the driver and the executor re-derive the identical
// frontier), so the loop-stops-on-refusal law is exercised through the
// reachable typed refusal at the frontier hop: a tampered ledger refuses
// before anything ran. See complete.rs's module doc.

#[test]
fn refusal_stops_loop_and_ledger_is_byte_unchanged() {
    let dir = temp_ledger_dir("refusal-stop");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    // Seed one real transition, then tamper with the file on disk: the
    // recorded digest no longer recomputes over the content.
    let mut event = serde_json::Map::new();
    event.insert("work_order_id".to_string(), json!("OA"));
    event.insert("from_standing".to_string(), json!("UNKNOWN"));
    event.insert("to_standing".to_string(), json!("PARTIAL_ALIVE"));
    event.insert("receipt_digest".to_string(), json!("sha256:seed"));
    ggen_sjira::ledger::append(&ledger, &event, &Default::default()).expect("seed");
    let before = std::fs::read(&ledger).expect("read ledger bytes");
    let tampered = String::from_utf8(before.clone())
        .unwrap()
        .replace("PARTIAL_ALIVE", "ALIVE");
    std::fs::write(&ledger, tampered).unwrap();

    let oa = input("OA", AUTH_A);
    let ob = input("OB", AUTH_A);
    let (exit, receipt) = run(&[oa, ob], &ledger, Some(&graph), &CompleteOpts::default());

    assert_eq!(exit, 1);
    assert_eq!(receipt["status"], json!("refused"));
    assert_eq!(receipt["reason"], json!("frontier_refused"));
    assert!(
        receipt["detail"]
            .as_str()
            .unwrap()
            .contains("refused before anything ran"),
        "the refusal names the frontier hop: {}",
        receipt["detail"]
    );
    assert!(receipt.get("closed").is_none(), "nothing closed");

    // The tampered file is byte-unchanged by the run: the loop wrote
    // nothing.
    let after = std::fs::read(&ledger).expect("read ledger bytes");
    assert_eq!(
        before.len() + "ALIVE".len() - "PARTIAL_ALIVE".len(),
        after.len()
    );
    assert!(
        String::from_utf8_lossy(&after).contains("\"ALIVE\""),
        "tampered bytes still on disk, untouched by the run"
    );
}

// ── 3. --max 1 -> max_iterations_exhausted with remaining_eligible ────────

#[test]
fn max_one_refuses_with_remaining_eligible() {
    let dir = temp_ledger_dir("max-one");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A, AUTH_B]);

    let oa = input("OA", AUTH_A);
    let ob = with_dep(input("OB", AUTH_B), dep_on("OA", "PARTIAL_ALIVE"));

    let opts = CompleteOpts {
        max_iterations: Some(1),
        ..Default::default()
    };
    let (exit, receipt) = run(&[oa, ob], &ledger, Some(&graph), &opts);

    assert_eq!(exit, 1);
    assert_eq!(receipt["status"], json!("refused"));
    assert_eq!(receipt["reason"], json!("max_iterations_exhausted"));
    assert_eq!(receipt["remaining_eligible"], json!(1), "OB remains");
    assert_eq!(receipt["closed"], json!(["OA"]));
    assert!(
        receipt["detail"]
            .as_str()
            .unwrap()
            .contains("--max 1 iterations reached"),
        "{}",
        receipt["detail"]
    );

    // The budget exhaustion is honest about what DID happen: OA's
    // transition is on disk; the ledger holds exactly the closed orders.
    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 1);
    assert_eq!(on_disk[0]["work_order_id"], json!("OA"));
}

// ── 4. exit-2 mutual exclusion, before any ledger IO ─────────────────────

#[test]
fn pack_dir_and_packs_root_are_mutually_exclusive() {
    let dir = temp_ledger_dir("exit-two");
    let ledger = dir.join("ledger.ndjson");

    let opts = CompleteOpts {
        pack_dir: Some(dir.join("pack")),
        packs_root: Some(dir.join("packs")),
        ..Default::default()
    };
    let (exit, receipt) = run(&[input("OA", AUTH_A)], &ledger, None, &opts);

    assert_eq!(exit, 2, "a broken invocation is not a refusal of any order");
    assert_eq!(receipt["status"], json!("invalid_invocation"));
    assert_eq!(
        receipt["reason"],
        json!("--pack-dir and --packs-root are mutually exclusive")
    );
    assert!(
        !ledger.exists(),
        "exit 2 fires before any ledger IO"
    );
}

// ── 5. no authority graph -> authority_index_unavailable (exit 1) ─────────

#[test]
fn missing_authority_graph_refuses_typed() {
    let dir = temp_ledger_dir("no-authority");
    let ledger = dir.join("ledger.ndjson");
    let (exit, receipt) = run(&[input("OA", AUTH_A)], &ledger, None, &CompleteOpts::default());

    assert_eq!(exit, 1);
    assert_eq!(receipt["status"], json!("refused"));
    assert_eq!(
        receipt["reason"],
        json!("authority_index_unavailable")
    );
    assert!(!ledger.exists(), "nothing was written");
}

// ── 6. per-order pack resolution is computed and surfaced ─────────────────

#[test]
fn packs_root_resolves_target_pack_per_order() {
    let dir = temp_ledger_dir("pack-resolution");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let oa = input("OA", AUTH_A);

    let opts = CompleteOpts {
        packs_root: Some(dir.join("packs")),
        target_packs: BTreeMap::from([(
            "OA".to_string(),
            "smalt-substrate-pack".to_string(),
        )]),
        ..Default::default()
    };
    let (exit, receipt) = run(
        &[oa, input("OB", AUTH_A)],
        &ledger,
        Some(&graph),
        &opts,
    );

    assert_eq!(exit, 0, "{}", receipt);
    assert_eq!(receipt["closed"], json!(["OA", "OB"]));
    assert_eq!(
        receipt["orders"][0]["resolved_pack_dir"],
        json!((dir.join("packs").join("smalt-substrate-pack")).to_string_lossy()),
        "OA names target_pack: <root>/<target_pack>"
    );
    assert!(
        receipt["orders"][1].get("resolved_pack_dir").is_none(),
        "OB names no target_pack: no resolution surfaced"
    );
}

// ── 7. sanitize: the per-order receipt file name law ──────────────────────

#[test]
fn sanitize_replaces_everything_outside_the_class() {
    assert_eq!(sanitize("plain-identity_1.0"), "plain-identity_1.0");
    assert_eq!(sanitize("sj:OA/one"), "sj_OA_one");
    assert_eq!(sanitize("a b"), "a_b");
    assert_eq!(sanitize(""), "");
}

// ── 8. default budget is the igniter's 1000 ───────────────────────────────

#[test]
fn default_max_matches_the_igniter() {
    assert_eq!(DEFAULT_MAX, 1000);
}
