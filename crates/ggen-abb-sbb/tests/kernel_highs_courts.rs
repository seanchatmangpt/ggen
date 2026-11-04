//! Kernel HIGH-defect courts (v26.11.3 lane G3) — pins the corrected laws:
//!
//! - HIGH-1: a [`ggen_abb_sbb::CapTable`] entry is a ceiling, not an equality
//!   (`measured <= declared` passes; `measured > declared` refuses
//!   `CAP_CEILING_MISMATCH`).
//! - HIGH-2: an envelope whose `graph_digest` differs from its embedded
//!   receipt's `graph_digest` refuses `ENVELOPE_GRAPH_MISMATCH`.
//! - HIGH-3: a dangling `sbb.pack` reference refuses whether or not the graph
//!   carries a `packs` list (an absent list no longer disables the check).
//! - L1: `SBB_UNQUALIFIED`, `STALE_QUALIFICATION` and `EXCEEDS_CONTRACT`
//!   refusals carry an imperative remediation sentence.

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_abb_sbb::*;

fn gen() -> Generator {
    Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.11.3".into(),
    }
}

/// Real kernel receipt over a real graph with exactly 5 artifacts, so the
/// ceiling courts measure 5 outputs — not a hand-faked receipt.
fn receipt_with_5_outputs() -> (EaGraph, ManufactureReceipt) {
    let g = synthetic_graph(1, 5);
    let digest = g.digest();
    let ad = admit(
        &g,
        &Request {
            abb: "abb:event-ingest".into(),
            sbb: "sbb:ingest-0000".into(),
            requested_authority: Authority::Construct,
            expected_graph_digest: Some(digest),
            receipt_generation: 0,
            artifact_baseline_generation: 0,
        },
    )
    .expect("5-artifact SBB admits");
    let receipt = manufacture(&ad, &gen()).expect("5-artifact SBB manufactures").receipt;
    assert_eq!(receipt.outputs.len(), 5, "fixture must measure 5 outputs");
    (g, receipt)
}

fn envelope_with_cap(g: &EaGraph, receipt: &ManufactureReceipt, declared: u64) -> Envelope {
    let digest = g.digest();
    Envelope {
        ontology_hash: digest.clone(),
        graph_digest: digest,
        cap_table: CapTable {
            caps: [(
                "cap:event-ingest.max_artifacts".to_string(),
                declared,
            )]
            .into_iter()
            .collect(),
        },
        receipt: receipt.clone(),
    }
}

// --- HIGH-1: ceiling semantics --------------------------------------------------------

/// Corrected law: a declared ceiling with lawful headroom (64 >= measured 5)
/// PASSES. Under the old inverted equality law this refused; the refusal was
/// the defect.
#[test]
fn high1_declared_64_measured_5_passes() {
    let (g, receipt) = receipt_with_5_outputs();
    let env = envelope_with_cap(&g, &receipt, 64);
    let verdict = envelope_court(&env);
    assert!(
        verdict.is_pass(),
        "declared 64 >= measured 5 must PASS, got {verdict:?}"
    );
    assert_eq!(verdict.exit_code(), 0);
}

/// Corrected law: a ceiling below the measured output count (4 < 5) refuses
/// with the typed CAP_CEILING_MISMATCH code, exit 2.
#[test]
fn high1_declared_4_measured_5_refuses_cap_ceiling_mismatch() {
    let (g, receipt) = receipt_with_5_outputs();
    let env = envelope_with_cap(&g, &receipt, 4);
    let verdict = envelope_court(&env);
    let ReplayVerdict::Refused { ref code, ref detail } = verdict else {
        panic!("declared 4 < measured 5 must Refuse, got {verdict:?}");
    };
    assert_eq!(code, "CAP_CEILING_MISMATCH");
    assert!(detail.contains("4") && detail.contains("5"), "detail must carry both sides: {detail}");
    assert!(!verdict.is_pass());
    assert_eq!(verdict.exit_code(), 2);
}

/// Equality at the boundary (declared == measured) still passes: the ceiling
/// admits the exact fill, not only strict headroom.
#[test]
fn high1_declared_equals_measured_still_passes() {
    let (g, receipt) = receipt_with_5_outputs();
    let env = envelope_with_cap(&g, &receipt, 5);
    assert!(envelope_court(&env).is_pass());
}

// --- HIGH-2: envelope-vs-receipt graph digest binding ---------------------------------

/// Falsifies "an envelope can smuggle a receipt manufactured against a
/// different graph": mutating the envelope's graph_digest (leaving the receipt
/// intact) must refuse with ENVELOPE_GRAPH_MISMATCH.
#[test]
fn high2_envelope_receipt_graph_digest_mismatch_refuses() {
    let (g, receipt) = receipt_with_5_outputs();
    let mut env = envelope_with_cap(&g, &receipt, 64);
    env.graph_digest = "sha256:other-graph".into();

    let verdict = envelope_court(&env);
    let ReplayVerdict::Refused { ref code, ref detail } = verdict else {
        panic!("graph-digest mismatch must Refuse, got {verdict:?}");
    };
    assert_eq!(code, "ENVELOPE_GRAPH_MISMATCH");
    assert!(
        detail.contains("other-graph"),
        "detail must name the unbound subject: {detail}"
    );
    assert!(!verdict.is_pass());
    assert_eq!(verdict.exit_code(), 2);

    // The digest moved: the smuggled envelope is a different subject.
    let mut nominal = envelope_with_cap(&g, &receipt, 64);
    nominal.graph_digest = g.digest();
    assert_ne!(envelope_digest(&env), envelope_digest(&nominal));
}

// --- HIGH-3: dangling pack refuses regardless of packs-list presence ------------------

/// Control: a resolvable pack reference admits.
#[test]
fn high3_resolvable_pack_admits() {
    let g = synthetic_graph(1, 1);
    assert!(g.validate().is_ok(), "fixture pack resolves: {:?}", g.validate());
}

/// HIGH-3 core falsifier: with the packs list REMOVED entirely (serde default
/// → empty), an unresolvable sbb.pack must still refuse DanglingReference —
/// the old `!self.packs.is_empty()` guard silently disabled the check here.
#[test]
fn high3_absent_packs_list_still_refuses_unresolvable_pack() {
    let mut g = synthetic_graph(1, 1);
    g.candidate_sbbs[0].pack = "pack:does-not-exist".into();
    g.packs.clear();

    let err = g.validate().expect_err("unresolvable pack must refuse");
    assert!(
        matches!(err, Refusal::DanglingReference { ref to, .. } if to == "pack:does-not-exist"),
        "expected dangling pack refusal, got {err}"
    );
}

/// With the packs list PRESENT but not containing the name, the same refusal
/// fires (pre-existing behavior, pinned so the fix did not regress it).
#[test]
fn high3_present_packs_list_refuses_unresolvable_pack() {
    let mut g = synthetic_graph(1, 1);
    g.candidate_sbbs[0].pack = "pack:does-not-exist".into();

    let err = g.validate().expect_err("unresolvable pack must refuse");
    assert!(
        matches!(err, Refusal::DanglingReference { ref to, .. } if to == "pack:does-not-exist"),
        "expected dangling pack refusal, got {err}"
    );
}

// --- L1: remediation sentences ---------------------------------------------------------

/// Every pinned refusal's Display must carry an imperative remediation
/// sentence, not just the code and identifiers.
#[test]
fn l1_refusals_carry_remediation_sentences() {
    // SBB_UNQUALIFIED: the SBB's only qualification is refuted.
    let mut g = synthetic_graph(1, 1);
    g.qualifications[0].verdict = Verdict::Refuted;
    let req = Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: None,
        receipt_generation: 0,
        artifact_baseline_generation: 0,
    };
    let err = admit(&g, &req).expect_err("refuted qualification must refuse");
    let msg = err.to_string();
    assert!(msg.starts_with("SBB_UNQUALIFIED"), "got {msg}");
    assert!(msg.contains("qualify"), "SBB_UNQUALIFIED remediation missing: {msg}");

    // STALE_QUALIFICATION: the qualification binds a superseded SBB digest.
    let mut g = synthetic_graph(1, 1);
    g.qualifications[0].sbb_digest = "sha256:superseded".into();
    let err = admit(&g, &req).expect_err("stale qualification must refuse");
    let msg = err.to_string();
    assert!(msg.starts_with("STALE_QUALIFICATION"), "got {msg}");
    assert!(
        msg.contains("re-qualify"),
        "STALE_QUALIFICATION remediation missing: {msg}"
    );

    // EXCEEDS_CONTRACT: the realization carries more artifacts than the contract
    // allows. The qualification's contract digest is re-bound to the shrunken
    // contract so the refusal surfaces as EXCEEDS_CONTRACT, not CONTRACT_CHANGED.
    let mut g = synthetic_graph(1, 2);
    g.contracts[0].max_artifacts = 1;
    g.qualifications[0].contract_digest = contract_digest(&g.contracts[0]);
    let err = admit(&g, &req).expect_err("over-ceiling realization must refuse");
    let msg = err.to_string();
    assert!(msg.starts_with("EXCEEDS_CONTRACT"), "got {msg}");
    assert!(
        msg.contains("re-qualify"),
        "EXCEEDS_CONTRACT remediation missing: {msg}"
    );
}
