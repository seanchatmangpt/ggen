//! Chicago integration tests for the receipts module — real admission
//! kernel, final-state assertions, no mocks. Courts: chain build, mid-chain
//! tamper at depth, refusal logging, determinism.

use ggen_sjira::{
    admit_work_order, ChainError, Decision, Refusal, TransitionJournal, GENESIS_PREV_HASH,
};
use serde_json::json;

fn valid_order() -> serde_json::Value {
    json!({
        "identity": "R80-07",
        "title": "receipts lane",
        "description": "cryptographic transition logging",
        "subject": "crates/ggen-sjira/src/receipts.rs",
        "repository": "seanchatmangpt/ggen",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://example.test/auth/R80-07",
        "projections": ["receipt"],
        "required_courts": ["court-vacuity"],
        "required_evidence": ["cargo test"],
        "acceptance": ["verify_chain Ok"],
        "falsifiers": ["f1"]
    })
}

fn admitted_ok() -> Result<ggen_sjira::AdmittedWorkOrder, Refusal> {
    admit_work_order(&valid_order())
}

fn refused_err() -> Result<ggen_sjira::AdmittedWorkOrder, Refusal> {
    admit_work_order(&json!({ "identity": "BAD" }))
}

fn three_link_journal() -> TransitionJournal {
    let good = admitted_ok().expect("order must admit");
    let mut j = TransitionJournal::new();
    j.record(&Ok(good.clone()), None);
    j.record(&refused_err(), None);
    j.record(&Ok(good), None);
    j
}

#[test]
fn chain_builds_genesis_off_zeros_and_links() {
    let good = admitted_ok().expect("order must admit");
    let mut j = TransitionJournal::new();
    let h1 = j.record(&Ok(good.clone()), None);
    let h2 = j.record(&refused_err(), None);
    let h3 = j.record(&Ok(good), None);
    assert_eq!(j.len(), 3);
    assert_eq!(j.verify_chain(), Ok(()));
    assert_eq!(j.transitions()[0].prev_hash, GENESIS_PREV_HASH);
    assert_eq!(j.transitions()[1].prev_hash, h1);
    assert_eq!(j.transitions()[2].prev_hash, h2);
    assert_eq!(j.head(), h3);
    assert_ne!(h1, h2);
    assert_ne!(h2, h3);
    assert_eq!(j.refused_count(), 1);
    assert_eq!(j.admitted_count(), 2);
    assert!(
        matches!(j.transitions()[0].decision,
                 Decision::Admitted { ref authority } if authority == "NONE"),
        "admitted decision carries the stamped NONE authority"
    );
}

#[test]
fn mid_chain_tamper_detected_at_any_depth() {
    let j = three_link_journal();
    assert_eq!(j.verify_chain(), Ok(()));

    // depth-1 body mutation (decision field)
    let mut t1 = j.clone();
    t1.transitions[1].decision = Decision::Refused {
        detail: "Tampered detail".into(),
    };
    match t1.verify_chain() {
        Err(ChainError::Tampered { index: 1, .. }) => {}
        other => panic!("expected Tampered at 1, got {other:?}"),
    }

    // depth-2 body mutation (blast-radius field)
    let mut t2 = j.clone();
    t2.transitions[2].blast_radius_digest = Some("sha256:evil".into());
    match t2.verify_chain() {
        Err(ChainError::Tampered { index: 2, .. }) => {}
        other => panic!("expected Tampered at 2, got {other:?}"),
    }

    // hash-field mutation is Tampered (recomputation fails at that index)
    let mut t3 = j.clone();
    t3.transitions[2].hash = "0".repeat(64);
    match t3.verify_chain() {
        Err(ChainError::Tampered { index: 2, .. }) => {}
        other => panic!("expected Tampered at 2, got {other:?}"),
    }

    // prev_hash mutation is BrokenLink, located at the mutated depth
    let mut b1 = j.clone();
    b1.transitions[1].prev_hash = GENESIS_PREV_HASH.to_string();
    match b1.verify_chain() {
        Err(ChainError::BrokenLink { index: 1, .. }) => {}
        other => panic!("expected BrokenLink at 1, got {other:?}"),
    }
}

#[test]
fn refusal_transitions_logged_with_display_detail() {
    let refused = refused_err();
    let refusal_text = refused.as_ref().unwrap_err().to_string();
    assert!(
        refusal_text.starts_with("Deserialization"),
        "fixture is a deserialization refusal, got: {refusal_text}"
    );

    let mut j = TransitionJournal::new();
    j.record(&refused, None);
    assert_eq!(j.len(), 1);
    assert_eq!(j.refused_count(), 1);
    assert_eq!(j.admitted_count(), 0);
    let t = &j.transitions()[0];
    assert!(
        matches!(t.decision, Decision::Refused { detail: ref d } if d == &refusal_text),
        "the refusal's Display text must be the decision detail"
    );
    // digest-class separation: refusals carry the refusal:sha256: prefix
    assert!(t.work_order_digest.starts_with("refusal:sha256:"));
    assert_eq!(j.verify_chain(), Ok(()));
    assert_eq!(j.head(), t.hash);
}

#[test]
fn determinism_across_instances() {
    let build = || {
        let good = admitted_ok().expect("order must admit");
        let refused = refused_err();
        let mut j = TransitionJournal::new();
        j.record(&Ok(good.clone()), Some("sha256:cc".into()));
        j.record(&refused, None);
        j.record(&Ok(good), None);
        j
    };
    let a = build();
    let b = build();
    assert_eq!(a, b);
    assert_eq!(a.verify_chain(), Ok(()));
    assert_eq!(a.head(), b.head());
}
