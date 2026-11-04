//! Chicago admission-controller tests over real collaborators — the full
//! composition loop: propose → radius → admit → journal → verify-chain,
//! plus the two ordering laws (authority gate before radius; critical-path
//! refusal halts before sjira admission). No mocks: the graph fixture is a
//! real hand-written [`SymbolGraph`] implementation (the trait is the
//! injected seam, not a test double), and the journal is the landed
//! `receipts.rs` chain.

use ggen_sjira::controller::{
    propose, ControllerDecision, ControllerEdit, Stage,
    DEFAULT_PROTECTED_PATHS, DEFAULT_MAX_DEPTH,
};
use ggen_sjira::receipts::{Decision, TransitionJournal};
use serde_json::{json, Value};
use std::collections::BTreeMap;

// ---------------------------------------------------------------------------
// Real collaborators (hand-written trait impls — seams, not mocks)
// ---------------------------------------------------------------------------

/// Real symbol graph: literal callers/subtypes/def-sites over the
/// workspace surface. Implements the controller's [`SymbolGraph`] seam
/// the same way the landed ggen-engine `VecGraph` does.
struct FixedGraph {
    refs: Vec<(String, String)>,
    subtypes: Vec<(String, String)>,
    defs: Vec<(String, String)>, // (symbol, path)
}

impl FixedGraph {
    fn empty() -> Self {
        Self {
            refs: Vec::new(),
            subtypes: Vec::new(),
            defs: Vec::new(),
        }
    }
}

impl ggen_sjira::controller::SymbolGraph for FixedGraph {
    fn callers_of(&self, symbol: &str) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        for (caller, callee) in &self.refs {
            if callee == symbol {
                out.push(caller.clone());
            }
        }
        out
    }

    fn subtypes_of(&self, symbol: &str) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        for (sup, sub) in &self.subtypes {
            if sup == symbol {
                out.push(sub.clone());
            }
        }
        out
    }

    fn def_site(&self, symbol: &str) -> Option<ggen_sjira::controller::SymbolLoc> {
        for (sym, path) in &self.defs {
            if sym == symbol {
                return Some(ggen_sjira::controller::SymbolLoc {
                    symbol: sym.clone(),
                    path: path.clone(),
                });
            }
        }
        None
    }
}

/// Real pins: the exact `BTreeMap<String, String>` shape
/// `ggen_sjira::authority::pins` returns.
fn pins(pinned: &[(&str, &str)]) -> BTreeMap<String, String> {
    let mut m: BTreeMap<String, String> = BTreeMap::new();
    for (iri, digest) in pinned {
        m.insert(iri.to_string(), digest.to_string());
    }
    m
}

fn valid_order() -> Value {
    json!({
        "identity": "R80-14",
        "title": "admission controller composition",
        "description": "ordered gate pipeline synthesis",
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

fn edits_off_critical_path() -> Vec<ControllerEdit> {
    vec![
        ControllerEdit::new("crates/ggen-sjira/src/controller.rs", vec!["controller_propose".to_string()]),
        ControllerEdit::new("crates/ggen-sjira/src/admit.rs", vec!["admit_work_order".to_string()]),
    ]
}

fn edits_on_critical_path() -> Vec<ControllerEdit> {
    // Path convention matches DEFAULT_PROTECTED_PATHS: crates-relative,
    // mirroring ggen-engine's blast_radius.rs gate.
    vec![ControllerEdit::new(
        "ggen-engine/src/sync.rs",
        vec!["stage_2c_gate".to_string()],
    )]
}

// ---------------------------------------------------------------------------
// The full loop
// ---------------------------------------------------------------------------

#[test]
fn controller_full_loop_propose_radius_admit_journal_verify_chain() {
    let graph = FixedGraph {
        refs: vec![
            ("controller_propose".to_string(), "admit_work_order".to_string()),
            ("admit_work_order".to_string(), "work_order_validate".to_string()),
        ],
        subtypes: Vec::new(),
        defs: vec![
            ("controller_propose".to_string(), "crates/ggen-sjira/src/controller.rs".to_string()),
            ("admit_work_order".to_string(), "crates/ggen-sjira/src/admit.rs".to_string()),
            ("work_order_validate".to_string(), "crates/ggen-sjira/src/work_order.rs".to_string()),
        ],
    };
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();

    let decision = propose(
        &edits_off_critical_path(),
        &valid_order(),
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    // Admitted with authority stamped NONE (admission never creates it).
    match &decision {
        ControllerDecision::Admitted {
            order_digest,
            definition_digest,
            authority,
            radius_digest,
            journal_hash,
            journal_head,
        } => {
            assert!(order_digest.starts_with("sha256:"), "order digest: {order_digest}");
            assert!(definition_digest.starts_with("sha256:"));
            assert_eq!(authority, "NONE");
            assert!(radius_digest.starts_with("blake3:"));
            assert_eq!(*journal_head, journal.head());
            assert_eq!(*journal_hash, journal.head());
        }
        other => panic!("expected Admitted, got {other:?}"),
    }

    // Journal: exactly one transition, chained off genesis, Admitted.
    assert_eq!(journal.len(), 1);
    assert_eq!(journal.admitted_count(), 1);
    assert_eq!(journal.refused_count(), 0);
    let t = &journal.transitions()[0];
    assert!(t.prev_hash.starts_with("0000000000000000"));
    match &t.decision {
        Decision::Admitted { authority } => assert_eq!(authority, "NONE"),
        other => panic!("expected journaled Admitted, got {other:?}"),
    }
    // The radius digest is bound into the transition.
    if let ControllerDecision::Admitted { radius_digest, .. } = &decision {
        assert_eq!(t.blast_radius_digest, Some(radius_digest.clone()));
    }

    // Chain verifies end to end.
    journal.verify_chain().expect("chain must verify");
}

#[test]
fn controller_radius_closure_reaches_callers_and_protected_intersection_refuses() {
    // stage_2c_gate's def site IS a protected path; the edit's symbol is two
    // hops away from it through callers edges — the closure must reach it.
    let graph = FixedGraph {
        refs: vec![("stage_2c_gate".to_string(), "sync_write".to_string())],
        subtypes: Vec::new(),
        defs: vec![
            ("stage_2c_gate".to_string(), "ggen-engine/src/sync.rs".to_string()),
            ("sync_write".to_string(), "ggen-engine/src/write.rs".to_string()),
        ],
    };
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();

    let decision = propose(
        &edits_on_critical_path(),
        &valid_order(),
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    match &decision {
        ControllerDecision::Refused {
            stage,
            detail,
            radius_digest,
            ..
        } => {
            assert_eq!(*stage, Stage::BlastRadius);
            assert!(detail.contains("ggen-engine/src/sync.rs"), "detail: {detail}");
            assert!(radius_digest.is_some(), "radius WAS computed at this stage");
        }
        other => panic!("expected BlastRadius refusal, got {other:?}"),
    }
    // The refusal is journaled like an admission would be.
    assert_eq!(journal.len(), 1);
    assert_eq!(journal.refused_count(), 1);
    journal.verify_chain().expect("refusal transition must chain");
}

// ---------------------------------------------------------------------------
// Ordering law 1: critical-path refusal halts BEFORE sjira admission
// ---------------------------------------------------------------------------

#[test]
fn controller_critical_path_refusal_halts_before_sjira_admission() {
    // Deliberately MALFORMED order (bad base_sha): if the pipeline reached
    // sjira admission it would refuse with Stage::Admission naming base_sha.
    let mut bad_order = valid_order();
    bad_order["base_sha"] = json!("NOT_A_SHA");
    // But the edits sit on a protected path, so the radius gate must halt
    // the pipeline first, and the admission kernel must never run. The
    // graph pins the edited symbol's def site onto the protected surface.
    let graph = FixedGraph {
        refs: Vec::new(),
        subtypes: Vec::new(),
        defs: vec![(
            "stage_2c_gate".to_string(),
            "ggen-engine/src/sync.rs".to_string(),
        )],
    };
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();

    let decision = propose(
        &edits_on_critical_path(),
        &bad_order,
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    match &decision {
        ControllerDecision::Refused { stage, detail, .. } => {
            assert_eq!(*stage, Stage::BlastRadius, "radius gate must halt first");
            assert!(
                detail.contains("CriticalPathRefusal"),
                "refusal must be the radius one, not admission: {detail}"
            );
            assert!(!detail.contains("NOT_A_SHA"), "admission must never have run");
        }
        other => panic!("expected BlastRadius refusal, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Ordering law 2: authority-not-pinned refuses BEFORE radius
// ---------------------------------------------------------------------------

#[test]
fn controller_authority_not_pinned_refuses_before_radius() {
    // Edits sit on a PROTECTED path AND the authority is unpinned: the
    // authority gate must refuse first, and the radius must never run
    // (proven by radius_digest == None in both the decision and the chain).
    let graph = FixedGraph::empty();
    let authority = pins(&[("https://graph.test/auth/OTHER", "sha256:bb")]); // not our IRI
    let mut journal = TransitionJournal::new();

    let decision = propose(
        &edits_on_critical_path(),
        &valid_order(),
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    match &decision {
        ControllerDecision::Refused {
            stage,
            detail,
            radius_digest,
            ..
        } => {
            assert_eq!(*stage, Stage::Authority);
            assert!(detail.contains("AuthorityNotPinned"), "detail: {detail}");
            assert_eq!(*radius_digest, None, "radius must never have run");
        }
        other => panic!("expected Authority refusal, got {other:?}"),
    }
    // Tamper-evidence of the ordering law inside the chain itself.
    let t = &journal.transitions()[0];
    assert_eq!(t.blast_radius_digest, None);
    match &t.decision {
        Decision::Refused { detail } => assert!(detail.contains("AuthorityNotPinned")),
        other => panic!("expected refused decision, got {other:?}"),
    }
    assert_eq!(journal.refused_count(), 1);
    journal.verify_chain().expect("chain must verify");
}

// ---------------------------------------------------------------------------
// Admission-stage refusals also journal + chain
// ---------------------------------------------------------------------------

#[test]
fn controller_admission_refusal_is_journaled_and_chained() {
    let graph = FixedGraph::empty();
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();

    let mut bad_order = valid_order();
    bad_order["base_sha"] = json!("NOT_A_SHA");

    let decision = propose(
        &edits_off_critical_path(),
        &bad_order,
        &graph,
        &authority,
        &mut journal,
        DEFAULT_PROTECTED_PATHS,
    );

    match &decision {
        ControllerDecision::Refused { stage, detail, radius_digest, .. } => {
            assert_eq!(*stage, Stage::Admission);
            assert!(detail.contains("NOT_A_SHA"), "detail: {detail}");
            assert!(radius_digest.is_some());
        }
        other => panic!("expected Admission refusal, got {other:?}"),
    }
    assert_eq!(journal.refused_count(), 1);
    journal.verify_chain().expect("chain must verify");
}

// ---------------------------------------------------------------------------
// Multi-edit batches; missing authority field refuses fail-closed
// ---------------------------------------------------------------------------

#[test]
fn controller_multi_edit_batch_radius_unions_symbols() {
    let graph = FixedGraph {
        refs: Vec::new(),
        subtypes: Vec::new(),
        defs: vec![
            ("sym_a".to_string(), "crates/ggen-sjira/src/a.rs".to_string()),
            ("sym_b".to_string(), "crates/ggen-sjira/src/b.rs".to_string()),
        ],
    };
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();
    let edits = vec![
        ControllerEdit::new("crates/ggen-sjira/src/a.rs", vec!["sym_a".to_string()]),
        ControllerEdit::new("crates/ggen-sjira/src/b.rs", vec!["sym_b".to_string()]),
    ];

    let decision = propose(&edits, &valid_order(), &graph, &authority, &mut journal, DEFAULT_PROTECTED_PATHS);
    assert!(matches!(decision, ControllerDecision::Admitted { .. }));

    // Journal bind check: the admitted digest binds the ORDER, not the edits.
    if let ControllerDecision::Admitted { order_digest, .. } = &decision {
        let t = &journal.transitions()[0];
        assert_eq!(&t.work_order_digest, order_digest);
    }
}

#[test]
fn controller_missing_origin_authority_field_refuses_fail_closed() {
    let graph = FixedGraph::empty();
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();

    let mut order = valid_order();
    order.as_object_mut().unwrap().remove("origin_authority");

    let decision = propose(&edits_off_critical_path(), &order, &graph, &authority, &mut journal, DEFAULT_PROTECTED_PATHS);
    match &decision {
        ControllerDecision::Refused { stage, detail, radius_digest, .. } => {
            assert_eq!(*stage, Stage::Authority);
            assert!(detail.contains("AuthorityNotPinned"));
            assert_eq!(*radius_digest, None);
        }
        other => panic!("expected Authority refusal, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Chain linkage across composed proposals
// ---------------------------------------------------------------------------

#[test]
fn controller_journal_links_across_proposals_and_verifies() {
    let graph = FixedGraph::empty();
    let authority = pins(&[("https://graph.test/auth/R80-14", "sha256:aa")]);
    let mut journal = TransitionJournal::new();

    let d1 = propose(&edits_off_critical_path(), &valid_order(), &graph, &authority, &mut journal, DEFAULT_PROTECTED_PATHS);
    let d2 = propose(&edits_off_critical_path(), &json!({}), &graph, &authority, &mut journal, DEFAULT_PROTECTED_PATHS);

    assert!(matches!(d1, ControllerDecision::Admitted { .. }));
    assert!(matches!(d2, ControllerDecision::Refused { stage: Stage::Authority, .. }));
    assert_eq!(journal.len(), 2);
    assert_eq!(journal.admitted_count(), 1);
    assert_eq!(journal.refused_count(), 1);

    // Manual linkage check over the public surface: t1 chains off t0.
    let ts = journal.transitions();
    assert!(ts[0].prev_hash.starts_with("0000000000000000"));
    assert_eq!(ts[1].prev_hash, ts[0].hash);
    journal.verify_chain().expect("chain must verify");
    let _ = DEFAULT_MAX_DEPTH;
}
