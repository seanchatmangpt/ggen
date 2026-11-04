//! DS-10 lease-gate court — Chicago tests over the real execute loop:
//! real Ed25519 keys (deterministic RFC 8032 seeds), real file ledger,
//! real epoch watermark JSON. Asserts final state: which plans were
//! emitted, and that the on-disk ledger bytes are UNCHANGED after a typed
//! refusal. No mocks.

use ed25519_dalek::{SigningKey, VerifyingKey};
use ggen_sjira::authority::{admission_digest, AuthorityGraph, AuthorityNode};
use ggen_sjira::execute::{run, ExecuteOpts, LeaseGateRefusal};
use ggen_sjira::lease::{
    admit as admit_lease, sign_record, SovereignLease, SignatureRecord,
};
use ggen_sjira::{DependencyEdge, FrontierInput};

use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::path::PathBuf;

// ── fixtures (mirrors execute_test.rs's real-file discipline) ─────────────

fn temp_ledger_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "ggen-sjira-execute-lease-{}-{tag}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp ledger dir");
    dir
}

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

/// Deterministic Ed25519 keypair from a fixed 32-byte seed (no RNG).
fn key(seed_byte: u8) -> SigningKey {
    SigningKey::from_bytes(&[seed_byte; 32])
}

fn keys(seed_bytes: &[u8]) -> BTreeMap<String, VerifyingKey> {
    seed_bytes
        .iter()
        .map(|b| {
            let k = key(*b);
            (format!("signer-{}", b), k.verifying_key())
        })
        .collect()
}

/// A real 2-signature admitted lease bound to `subject` (full lease.rs
/// admit law: canonical bytes signed by 2 distinct signers, real Ed25519
/// verification, real digest delta).
fn admitted_lease(subject: &str) -> ggen_sjira::lease::AdmittedLease {
    let base = SovereignLease {
        lease_id: "lease-ds10".into(),
        subject_iri: subject.into(),
        shapes: vec!["ggen:PackShape".to_string()],
        ontology_digest: "sha256:PRE".into(),
        granted_at: "2026-11-03T00:00:00Z".into(),
        signatures: vec![],
    };
    let mut sigs = vec![
        sign_record(&base, "signer-1", &key(1)),
        sign_record(&base, "signer-2", &key(2)),
    ];
    sigs.sort_by(|a, b| a.signer.cmp(&b.signer));
    let signed = SovereignLease { signatures: sigs, ..base };
    admit_lease(&signed, "sha256:CURRENT", &keys(&[1, 2]))
        .expect("2-sig lease admits")
}

/// Not-admitted variant (same shape, `admitted` flipped) — exercising the
/// "a non-admitted lease is not authority" branch of the gate.
fn not_admitted(mut lease: ggen_sjira::lease::AdmittedLease) -> ggen_sjira::lease::AdmittedLease {
    lease.admitted = false;
    lease
}

fn ledger_bytes(ledger: &std::path::Path) -> Vec<u8> {
    std::fs::read(ledger).unwrap_or_default()
}

const SUBJECT_OA: &str = "sj:WorkOrder/OA";
const SUBJECT_FOREIGN: &str = "sj:WorkOrder/OTHER";

// ── 1. with an admitted lease bound to the order → proceeds ───────────────

#[test]
fn admitted_bound_lease_proceeds() {
    let dir = temp_ledger_dir("bound");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let opts = ExecuteOpts {
        lease: Some(admitted_lease(SUBJECT_OA)),
        ..ExecuteOpts::legacy()
    };
    let out = run(&[input("OA", AUTH_A)], &ledger, &graph, &opts)
        .expect("an admitted lease binding OA must admit the plan emission");

    assert_eq!(out.plans.len(), 1);
    assert_eq!(out.plans[0].identity, "OA");
    assert_eq!(out.plans[0].target_standing, "PARTIAL_ALIVE");
    assert!(out.plans[0].plan_digest.starts_with("sha256:"));
    assert_eq!(out.journal.admitted_count(), 1);

    let on_disk = ggen_sjira::ledger::fetch(&ledger).expect("ledger verifies");
    assert_eq!(on_disk.len(), 1);
    assert_eq!(on_disk[0]["work_order_id"], json!("OA"));
}

// ── 2. foreign lease (subject mismatch) → LeaseNotBound, ledger unchanged ─

#[test]
fn foreign_lease_refuses_lease_not_bound_and_ledger_unchanged() {
    let dir = temp_ledger_dir("foreign");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let before = ledger_bytes(&ledger); // file does not exist yet
    let opts = ExecuteOpts {
        lease: Some(admitted_lease(SUBJECT_FOREIGN)),
        ..ExecuteOpts::legacy()
    };
    let err = run(&[input("OA", AUTH_A)], &ledger, &graph, &opts)
        .expect_err("a lease bound to another subject must refuse");

    assert_eq!(err.hop, "lease");
    assert_eq!(err.reason, "lease_not_bound");
    assert_eq!(err.broken_term, "R_missing_authority");
    assert!(err.detail.contains("OA"), "detail names the order: {}", err.detail);
    assert!(err.detail.contains(SUBJECT_FOREIGN));

    // The gate sits before plan/journal/append: no ledger file may appear.
    let after = ledger_bytes(&ledger);
    assert_eq!(before, after, "a LeaseNotBound refusal must not create or touch the ledger");
    assert!(!ledger.exists(), "no ledger file may be written on refusal");
}

// ── 3. not-admitted lease → LeaseNotBound ─────────────────────────────────

#[test]
fn not_admitted_lease_refuses() {
    let dir = temp_ledger_dir("not-admitted");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let opts = ExecuteOpts {
        lease: Some(not_admitted(admitted_lease(SUBJECT_OA))),
        ..ExecuteOpts::legacy()
    };
    let err = run(&[input("OA", AUTH_A)], &ledger, &graph, &opts)
        .expect_err("a non-admitted lease is not authority");

    assert_eq!(err.reason, "lease_not_bound");
    assert!(err.detail.contains("admitted=false"), "{}", err.detail);
    assert!(!ledger.exists(), "refusal leaves state unmutated");
}

// ── 4. regressed epoch watermark → EpochStale, ledger unchanged ───────────

#[test]
fn stale_epoch_refuses_epoch_stale_and_ledger_unchanged() {
    let dir = temp_ledger_dir("stale-epoch");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    // Order's epoch extension declares an epoch and its plan touches a
    // legacy (pre-watermark) path with no fresh manufacture plan →
    // epoch::check refuses LegacyEdit.
    let fi = input("OA", AUTH_A);

    let watermark = json!({
        "files": [ { "path": "lib/legacy.ex" } ]
    });
    let mut extensions = BTreeMap::new();
    extensions.insert(
        "OA".to_string(),
        json!({
            "epoch": "ep-1",
            "plan_touches": ["lib/legacy.ex"]
        }),
    );
    let opts = ExecuteOpts {
        epoch_watermark: Some(watermark),
        epoch_extensions: extensions,
        ..ExecuteOpts::legacy()
    };
    let err = run(&[fi], &ledger, &graph, &opts)
        .expect_err("pre-watermark plan lines must refuse the epoch gate");

    assert_eq!(err.hop, "lease");
    assert_eq!(err.reason, "epoch_stale");
    assert_eq!(err.broken_term, "mu_on_O");
    assert!(
        err.detail.contains("EPOCH_LEGACY_EDIT"),
        "epoch refusal code surfaced: {}",
        err.detail
    );

    let after = ledger_bytes(&ledger);
    assert!(after.is_empty(), "EpochStale refusal must not write the ledger");
    assert!(!ledger.exists());
}

// ── 5. epoch order with NO watermark → fail closed EpochStale ────────────

#[test]
fn epoch_order_without_watermark_fails_closed() {
    let dir = temp_ledger_dir("no-watermark");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let fi = input("OA", AUTH_A);
    let mut extensions = BTreeMap::new();
    extensions.insert(
        "OA".to_string(),
        json!({ "epoch": "ep-1", "plan_touches": ["lib/legacy.ex"] }),
    );
    let opts = ExecuteOpts {
        epoch_extensions: extensions,
        ..ExecuteOpts::legacy()
    };

    let err = run(&[fi], &ledger, &graph, &opts)
        .expect_err("an epoch-carrying order with no watermark must fail closed");

    assert_eq!(err.reason, "epoch_stale");
    assert!(
        err.detail.contains("EPOCH_WATERMARK_UNAVAILABLE"),
        "{}",
        err.detail
    );
    assert!(!ledger.exists());
}

// ── 6. epoch-free orders are unaffected by a watermark being present ──────

#[test]
fn watermark_present_but_order_epoch_free_proceeds() {
    let dir = temp_ledger_dir("wm-no-epoch");
    let ledger = dir.join("ledger.ndjson");
    let graph = auth_graph(&[AUTH_A]);

    let opts = ExecuteOpts {
        epoch_watermark: Some(json!({ "files": [] })),
        ..ExecuteOpts::legacy()
    };
    let out = run(&[input("OA", AUTH_A)], &ledger, &graph, &opts)
        .expect("an epoch-free order must pass the epoch gate");
    assert_eq!(out.plans.len(), 1);
}

// ── 7. compat court: no lease/epoch → legacy behavior byte-identical ─────

#[test]
fn without_lease_and_watermark_legacy_behavior_identical() {
    let graph = auth_graph(&[AUTH_A]);
    let oa = input("OA", AUTH_A);
    let ob = {
        let mut fi = input("OB", AUTH_A);
        fi.dependencies.push(dep_on("OA", "PARTIAL_ALIVE"));
        fi
    };

    // Run once with explicit legacy opts, once with plain Default — the two
    // ledgers must receive byte-identical event streams and both runs must
    // produce identical plans (same digests).
    let dir_a = temp_ledger_dir("compat-a");
    let dir_b = temp_ledger_dir("compat-b");
    let ledger_a = dir_a.join("ledger.ndjson");
    let ledger_b = dir_b.join("ledger.ndjson");

    let out_a = run(&[oa.clone(), ob.clone()], &ledger_a, &graph, &ExecuteOpts::legacy())
        .expect("legacy opts run");
    let out_b = run(&[oa, ob], &ledger_b, &graph, &ExecuteOpts::default())
        .expect("default opts run");

    assert_eq!(out_a.plans, out_b.plans, "identical plans and digests");
    assert_eq!(
        ledger_bytes(&ledger_a),
        ledger_bytes(&ledger_b),
        "byte-identical ledgers"
    );
    assert_eq!(out_a.plans.len(), 2);
    assert_eq!(out_a.plans[0].identity, "OA");
    assert_eq!(out_a.plans[1].identity, "OB");
}

// ── 8. LeaseGateRefusal renders typed ─────────────────────────────────────

#[test]
fn lease_gate_refusal_is_typed() {
    let bound = LeaseGateRefusal::LeaseNotBound("x".into());
    let as_exec: ggen_sjira::execute::ExecRefusal = bound.into();
    assert_eq!(as_exec.reason, "lease_not_bound");
    assert_eq!(as_exec.hop, "lease");

    let stale = LeaseGateRefusal::EpochStale("y".into());
    let as_exec: ggen_sjira::execute::ExecRefusal = stale.into();
    assert_eq!(as_exec.reason, "epoch_stale");
    assert_eq!(as_exec.hop, "lease");

    // SignatureRecord import is exercised via admitted_lease() above; keep
    // the type in scope check explicit for the court.
    let _rec: Option<SignatureRecord> = None;
    let _subj = SUBJECT_OA;
}
