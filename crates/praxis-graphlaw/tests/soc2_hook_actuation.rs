//! Knowledge-hook-driven actuation of the SOC2 audit engagement process,
//! per an explicit product directive that the Stage 2 Fortune-5 rescale
//! (`packs/soc2-audit-pack`, `crates/cng/tests/fixtures/soc2/`) demonstrate
//! that "knowledge hooks... actuate the entire SOC2" rather than the
//! process being driven only by static PDDL planning.
//!
//! Uses the real, tested `kh:` hook-pack vocabulary (the same
//! `kh:kind "sparql"` + `kh:action .../handler#sparql-construct` pattern
//! exercised throughout `knowledge_hooks_e2e_tier3.rs`/`tier4.rs`), not a
//! toy/simulated mechanism — these hooks run through the real
//! `TripleStore::materialize()` hook-evaluation pipeline.
//!
//! Two actuation shapes, matching the two real SOC2 process actions a
//! knowledge hook can honestly perform without asserting a compliance
//! verdict (the compliance-overclaim fence from `packs/soc2-audit-pack`
//! applies here too: no hook below ever derives or checks a "compliant" or
//! "opinion" predicate):
//!
//! 1. POSITIVE ACTUATION (`test_evidence_sufficiency_hook_actuates_control_tested`):
//!    a hook observes each control's evidence-item count against its
//!    sampling target and, once sufficient, derives that control's
//!    `controlTested` fact -- the operating-effectiveness-testing phase
//!    (SOC2_PHASES[5] in `crates/cng/src/bench/soc2.rs`) advanced by the
//!    hook engine, not by a human flipping a status field.
//! 2. NEGATIVE ACTUATION / GATING (`test_remediation_gate_refuses_when_a_critical_exception_is_unremediated`,
//!    `test_cuec_gate_refuses_when_sequoia_carve_out_evidence_is_missing`):
//!    hooks with `kh:effect "refuse"` block `materialize()` (standing in
//!    for the bundle-assembly phase, SOC2_PHASES[8]) with a typed error
//!    while a high-severity exception lacks remediation evidence, or while
//!    the Sequoia Colocation Partners carve-out's Complementary User Entity
//!    Control lacks its own evidence -- both real AICPA reporting
//!    requirements (an unremediated exception cannot be silently dropped;
//!    a carve-out sub-service organization's user entity still needs CUEC
//!    evidence), enforced here as an actual refusal, not a comment.
//!
//! Predicates below are test-local (`http://example.org/soc2#...`), not
//! the pack's public SKOS/PROV vocabulary -- this test exercises the hook
//! ENGINE using SOC2-flavored data as a realistic illustration; it is not
//! required to be byte-identical to `packs/soc2-audit-pack`'s own
//! ontology, matching this crate's own test-file convention (e.g.
//! `lib_test.rs`'s `http://example.org/...` fixtures) rather than the
//! ggen-pack convention.

mod common;

use common::{assert_contains_triple, assert_not_contains_triple};
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const NS: &str = "http://example.org/soc2#";

fn iri(local: &str) -> String {
    format!("{NS}{local}")
}

/// The evidence-sufficiency hook pack: a control point whose evidence-item
/// count has reached (or exceeded) its sampling target gets `controlTested`
/// derived by the hook engine's SPARQL-CONSTRUCT action -- the real
/// mechanism `knowledge_hooks_e2e_tier3.rs`'s
/// `test_c3_datalog_construct_delta_cascade` already exercises, applied
/// here to SOC2 evidence-sufficiency instead of a VIP-spend threshold.
fn evidence_sufficiency_hook_pack() -> String {
    format!(
        r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix soc2: <{NS}> .

        soc2:evidence_sufficiency_hook a kh:Hook ;
            kh:name "evidence_sufficiency_actuation" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?ctrl WHERE {{ ?ctrl <{NS}evidenceCount> ?ec . ?ctrl <{NS}samplingTarget> ?st . FILTER(?ec >= ?st) }}" ;
            kh:effect "emit-delta" ;
            kh:action soc2:mark_control_tested ;
            kh:priority 1 .

        soc2:mark_control_tested a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT {{ ?ctrl <{NS}controlTested> 'true' }} WHERE {{ ?ctrl <{NS}evidenceCount> ?ec . ?ctrl <{NS}samplingTarget> ?st . FILTER(?ec >= ?st) }}" .
        "#
    )
}

#[test]
fn test_evidence_sufficiency_hook_actuates_control_tested() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(&evidence_sufficiency_hook_pack())
        .unwrap();

    // ctrl_access_provisioning: evidence collection reached its 10-sample
    // target (12 >= 10) -- the hook must fire and derive controlTested.
    // ctrl_dr_failover_test: only 3 of a 10-sample target collected so far
    // (still inside the 12-month observation period) -- must NOT fire.
    store
        .load_triples(
            &format!(
                "<{a}> <{ec}> 12 .\n<{a}> <{st}> 10 .\n\
                 <{b}> <{ec}> 3 .\n<{b}> <{st}> 10 .\n",
                a = iri("ctrl_access_provisioning"),
                b = iri("ctrl_dr_failover_test"),
                ec = iri("evidenceCount"),
                st = iri("samplingTarget"),
            ),
            // N-Triples requires every literal to be quoted (a bare `12` is not
            // valid N-Triples); Turtle allows the bare-integer shorthand these
            // evidence counts use, matching the established convention
            // elsewhere in this test suite (e.g. `knowledge_hooks_e2e_tier4.rs`'s
            // ledger-balance scenario, which loads bare-integer balances via
            // `Syntax::Turtle` for the same reason).
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    assert_contains_triple(&store, "ctrl_access_provisioning", "controlTested", "true");
    assert_not_contains_triple(&store, "ctrl_dr_failover_test", "controlTested", "true");
}

/// The remediation-gate hook pack: any high-severity Exception whose
/// remediationStatus is still "open" refuses materialize() outright --
/// standing in for "bundle assembly cannot complete while a critical
/// exception is unremediated" (a real AICPA reporting requirement, not an
/// invented rule). Modeled as a direct positive existence check on an
/// explicit `remediationStatus` field (open/verified), not as negation over
/// an absent remediation-evidence triple: this engine's rule/hook
/// evaluation has no negation-as-failure support (a documented limitation,
/// `docs/ALGORITHM_COMPLEXITY.md` / PROJ-405), and real audit tooling
/// tracks exception lifecycle status explicitly rather than inferring
/// "still open" from absence of evidence anyway.
fn remediation_gate_hook_pack() -> String {
    format!(
        r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix soc2: <{NS}> .

        soc2:remediation_gate a kh:Hook ;
            kh:name "remediation_gate" ;
            kh:kind "sparql" ;
            kh:query "ASK {{ ?exc <{NS}severity> 'high' ; <{NS}remediationStatus> 'open' }}" ;
            kh:effect "refuse" ;
            kh:reason "bundle assembly refused: a high-severity exception has remediationStatus 'open' (AICPA carve-out/exception reporting requires remediation evidence, not silent drop)" .
        "#
    )
}

#[test]
fn test_remediation_gate_refuses_when_a_critical_exception_is_unremediated() {
    let mut store = TripleStore::new();
    store.load_hook_pack(&remediation_gate_hook_pack()).unwrap();

    // A remediated high-severity exception (verified) plus one still open --
    // the open one must trip the gate.
    store
        .load_triples(
            &format!(
                "<{a}> <{sev}> \"high\" .\n<{a}> <{rs}> \"verified\" .\n\
                 <{b}> <{sev}> \"high\" .\n<{b}> <{rs}> \"open\" .\n",
                a = iri("exc_capacity_blip"),
                b = iri("exc_billing_reconciliation_gap"),
                sev = iri("severity"),
                rs = iri("remediationStatus"),
            ),
            Syntax::NTriples,
        )
        .unwrap();

    let result = store.materialize();
    assert!(
        result.is_err(),
        "an open high-severity exception must refuse materialize(), not silently succeed"
    );
    let err = result.unwrap_err();
    assert!(
        err.contains("remediation_gate") || err.contains("AICPA"),
        "refusal must name the refusing hook/reason (typed, not opaque): {err}"
    );
}

/// The CUEC-gate hook pack: the Sequoia Colocation Partners
/// Complementary User Entity Control (`CTRL-VENDOR-OVERSIGHT-COLOCATION`
/// in `packs/soc2-audit-pack/templates/arclight-case-study.ttl.tmpl`)
/// refuses bundle-assembly progression while its own evidenceStatus is
/// "missing" -- the real AICPA carve-out-method requirement that a user
/// entity relying on a sub-service organization's carved-out controls must
/// still evidence its OWN oversight control.
fn cuec_gate_hook_pack() -> String {
    format!(
        r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix soc2: <{NS}> .

        soc2:cuec_gate a kh:Hook ;
            kh:name "cuec_gate" ;
            kh:kind "sparql" ;
            kh:query "ASK {{ ?ctrl <{NS}isCuec> 'true' ; <{NS}evidenceStatus> 'missing' }}" ;
            kh:effect "refuse" ;
            kh:reason "bundle assembly refused: the Sequoia Colocation Partners carve-out CUEC lacks its own vendor-oversight evidence" .
        "#
    )
}

#[test]
fn test_cuec_gate_refuses_when_sequoia_carve_out_evidence_is_missing() {
    let mut store = TripleStore::new();
    store.load_hook_pack(&cuec_gate_hook_pack()).unwrap();

    store
        .load_triples(
            &format!(
                "<{a}> <{cuec}> \"true\" .\n<{a}> <{es}> \"missing\" .\n",
                a = iri("ctrl_vendor_oversight_colocation"),
                cuec = iri("isCuec"),
                es = iri("evidenceStatus"),
            ),
            Syntax::NTriples,
        )
        .unwrap();

    let result = store.materialize();
    assert!(
        result.is_err(),
        "missing CUEC evidence for the Sequoia carve-out must refuse materialize()"
    );
    let err = result.unwrap_err();
    assert!(
        err.contains("cuec_gate") || err.contains("Sequoia"),
        "refusal must name the refusing hook/reason (typed, not opaque): {err}"
    );
}

/// Positive control for the CUEC gate: once evidence is collected
/// (evidenceStatus "collected"), the same hook pack must NOT refuse --
/// proving the gate is a real conditional, not an unconditional refusal.
#[test]
fn test_cuec_gate_does_not_refuse_once_evidence_is_collected() {
    let mut store = TripleStore::new();
    store.load_hook_pack(&cuec_gate_hook_pack()).unwrap();

    store
        .load_triples(
            &format!(
                "<{a}> <{cuec}> \"true\" .\n<{a}> <{es}> \"collected\" .\n",
                a = iri("ctrl_vendor_oversight_colocation"),
                cuec = iri("isCuec"),
                es = iri("evidenceStatus"),
            ),
            Syntax::NTriples,
        )
        .unwrap();

    let result = store.materialize();
    assert!(
        result.is_ok(),
        "collected CUEC evidence must NOT refuse materialize(): {:?}",
        result.err()
    );
}
