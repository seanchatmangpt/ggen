//! Unwitnessed-refusal court (PG-2 Slice 7, test-only — no src changes).
//!
//! Proves the two unwitnessed-refusal paths the igniter fixtures cover:
//! the blast-radius depth-cap truncation refusal (RadiusTruncated,
//! controller.rs:286) and the malformed-work-order admission refusal.
//! Chicago: real collaborators only — the hand-written graph/pin fixtures
//! are the controller's injected seams, and every outcome is journaled
//! through the real receipts::TransitionJournal blake3 chain, asserted
//! with verify_chain() == Ok(()) afterwards.

use ggen_sjira::controller::{
    propose, ControllerDecision, ControllerEdit, Stage, DEFAULT_MAX_DEPTH, DEFAULT_PROTECTED_PATHS,
};
use ggen_sjira::receipts::{Decision, TransitionJournal};
use serde_json::{json, Value};
use std::collections::BTreeMap;

/// Real hand-written symbol graph (the injected seam, not a mock): a
/// linear caller chain long enough to overrun the depth cap.
struct DeepChainGraph {
    /// (caller, callee) pairs forming a linear chain.
    chain: Vec<(String, String)>,
    defs: Vec<(String, String)>,
}

impl ggen_sjira::controller::SymbolGraph for DeepChainGraph {
    fn callers_of(&self, symbol: &str) -> Vec<String> {
        self.chain
            .iter()
            .filter(|(_, callee)| callee == symbol)
            .map(|(caller, _)| caller.clone())
            .collect()
    }

    fn subtypes_of(&self, _symbol: &str) -> Vec<String> {
        Vec::new()
    }

    fn def_site(&self, symbol: &str) -> Option<ggen_sjira::controller::SymbolLoc> {
        self.defs
            .iter()
            .find(|(sym, _)| sym == symbol)
            .map(|(sym, path)| ggen_sjira::controller::SymbolLoc {
                symbol: sym.clone(),
                path: path.clone(),
            })
    }
}

fn pins(pinned: &[(&str, &str)]) -> BTreeMap<String, String> {
    pinned
        .iter()
        .map(|(iri, digest)| (iri.to_string(), digest.to_string()))
        .collect()
}

fn valid_order() -> Value {
    json!({
        "identity": "R80-14",
        "title": "unwitnessed-refusal court",
        "description": "depth-cap and malformed-order refusal witnesses",
        "subject": "src/controller.rs",
        "repository": "seanchatmangpt/ggen",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts-then-receipt",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://graph.test/auth/R80-14",
        "projections": ["receipt", "replay"],
        "required_courts": ["admission-court"],
        "required_evidence": ["exact_head_verification_evidence"],
        "acceptance": ["cargo test -p ggen-sjira controller"],
        "falsifiers": ["a protected-path write is admitted by propose"]
    })
}

fn edits() -> Vec<ControllerEdit> {
    vec![ControllerEdit::new(
        "crates/ggen-sjira/src/controller.rs",
        vec!["controller_propose".to_string()],
    )]
}

/// A callers* chain of DEPTH+1 symbols: controller_propose <- c1 <- ... <-
/// cDEPTH. The closure from controller_propose pops c1 at depth 1 ... and
/// refuses when a caller is discovered at depth == DEFAULT_MAX_DEPTH, so a
/// chain of DEFAULT_MAX_DEPTH+1 links truncates.
fn deep_chain(links: usize) -> DeepChainGraph {
    let mut chain = Vec::new();
    let mut defs = vec![(
        "controller_propose".to_string(),
        "crates/ggen-sjira/src/controller.rs".to_string(),
    )];
    for i in 1..=links {
        let callee = if i == 1 {
            "controller_propose".to_string()
        } else {
            format!("c{}", i - 1)
        };
        chain.push((format!("c{}", i), callee));
        defs.push((format!("c{}", i), format!("src/c{}.rs", i)));
    }
    DeepChainGraph { chain, defs }
}

#[test]
fn depth_cap_truncation_refuses_and_journals() {
    let graph = deep_chain(DEFAULT_MAX_DEPTH + 1);
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa11")]);
    let mut journal = TransitionJournal::new();

    let decision = propose(
        &edits(),
        &valid_order(),
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    match &decision {
        ControllerDecision::Refused {
            stage: Stage::BlastRadius,
            detail,
            radius_digest,
            journal_hash,
            journal_head,
        } => {
            assert!(detail.starts_with("RadiusTruncated"), "got: {detail}");
            assert!(radius_digest.is_some());
            assert_eq!(*journal_head, journal.head());
            assert_eq!(*journal_hash, journal.transitions()[0].hash);
        }
        other => panic!("expected BlastRadius RadiusTruncated refusal, got {other:?}"),
    }

    // The truncation refusal is witnessed, not silent: journaled Refused,
    // chain intact.
    assert_eq!(journal.refused_count(), 1);
    assert_eq!(journal.admitted_count(), 0);
    let t = &journal.transitions()[0];
    assert!(
        matches!(t.decision, Decision::Refused { detail: ref d, .. } if d.starts_with("RadiusTruncated"))
    );
    assert!(t.blast_radius_digest.is_some());
    assert_eq!(journal.verify_chain(), Ok(()));
}

#[test]
fn depth_cap_clean_chain_admits_below_the_cap() {
    // A chain of DEFAULT_MAX_DEPTH links truncates (its deepest caller
    // pops at depth == cap), so the clean boundary is cap - 1 links: the
    // closure completes inside the cap and the proposal admits.
    let graph = deep_chain(DEFAULT_MAX_DEPTH - 1);
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa11")]);
    let mut journal = TransitionJournal::new();
    let decision = propose(
        &edits(),
        &valid_order(),
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );
    match &decision {
        ControllerDecision::Admitted { .. } => {}
        other => panic!("expected admission at the cap boundary, got {other:?}"),
    }
    assert_eq!(journal.verify_chain(), Ok(()));
}

#[test]
fn malformed_work_order_refusal_is_witnessed() {
    // Structurally broken JSON shaped like a work order: a string where a
    // list belongs, plus an unknown field — the deserializer refuses with
    // the field-naming refusal before any validation runs.
    let mut broken = valid_order();
    broken["projections"] = json!("receipt");
    broken["unknown_field"] = json!(1);

    let graph = deep_chain(0);
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa11")]);
    let mut journal = TransitionJournal::new();

    let decision = propose(
        &edits(),
        &broken,
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    match &decision {
        ControllerDecision::Refused {
            stage: Stage::Admission,
            detail,
            journal_hash,
            journal_head,
            ..
        } => {
            assert!(
                detail.starts_with("Deserialization"),
                "expected a field-naming deserialization refusal, got: {detail}"
            );
            assert_eq!(*journal_head, journal.head());
            assert_eq!(*journal_hash, journal.transitions()[0].hash);
        }
        other => panic!("expected Admission refusal, got {other:?}"),
    }

    // Witnessed: the refusal is in the chain, the head advanced, and the
    // chain verifies.
    assert_eq!(journal.refused_count(), 1);
    assert_eq!(journal.admitted_count(), 0);
    assert_eq!(journal.head(), journal.transitions()[0].hash);
    assert_eq!(journal.verify_chain(), Ok(()));

    // Admission-stage refusals bind the blake3 refusal digest derived
    // from the origin authority (the controller's refusal class, distinct
    // from the journal's refusal:sha256: class used by journal.record).
    let t = &journal.transitions()[0];
    assert!(t.work_order_digest.starts_with("blake3:"));
}

#[test]
fn malformed_order_admitted_directly_through_the_kernel_is_refused() {
    // Same malformed order through admit_work_order + journal.record —
    // the journal head advances over the refusal and the chain verifies.
    let mut broken = valid_order();
    broken["standing"] = json!("NOT_A_STANDING");

    let result = ggen_sjira::admit_work_order(&broken);
    assert!(result.is_err());
    let refusal_text = result.as_ref().unwrap_err().to_string();

    let mut journal = TransitionJournal::new();
    journal.record(&result, None);
    assert_eq!(journal.refused_count(), 1);
    let t = &journal.transitions()[0];
    assert!(matches!(t.decision, Decision::Refused { detail: ref d, .. } if d == &refusal_text));
    assert_eq!(journal.head(), t.hash);
    assert_eq!(journal.verify_chain(), Ok(()));
}
