//! Coverage for self-monitoring-pack's three previously-untested lifecycle
//! fixtures, closing retrofit:SabotageFixtureCoverageGap's self-monitoring
//! leg (`pattern-overdue.ttl`, `pattern-ungoverned.ttl`,
//! `pattern-fires-discharged.ttl` had zero .rs test references anywhere;
//! their behavior was documented in fixture headers and verified only by
//! past scratch harnesses).
//!
//! Mirrors `self_monitoring_hook_actuation.rs`: ontology + hook pack + one
//! fixture through the real `kh:` mechanism, no hand-simulated derivation.

mod common;

use common::{assert_contains_triple, assert_not_contains_triple};
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const HOOK_TTL: &str = include_str!("../../../packs/self-monitoring-pack/hook.ttl");
const ONTOLOGY_TTL: &str = include_str!("../../../packs/self-monitoring-pack/ontology.ttl");
const OVERDUE_TTL: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/pattern-overdue.ttl");
const UNGOVERNED_TTL: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/pattern-ungoverned.ttl");
const FIRES_DISCHARGED_TTL: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/pattern-fires-discharged.ttl");
/// The pack's shipped, checked-in open-obligations query surface — run
/// verbatim (include_str!, never re-typed). Deliberately carries no
/// ORDER BY/VALUES (its own header documents the engine limitations), so it
/// runs on this crate's own `TripleStore::query`.
const OPEN_OBLIGATIONS_RQ: &str =
    include_str!("../../../packs/self-monitoring-pack/queries/open_obligations.rq");

const SMON: &str = "http://seanchatmangpt.github.io/packs/self-monitoring#";
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

fn run_fixture(fixture_ttl: &str) -> TripleStore {
    let mut store = TripleStore::new();
    store
        .load_triples(ONTOLOGY_TTL, Syntax::Turtle)
        .expect("ontology.ttl must load as valid Turtle");
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    store
        .load_triples(fixture_ttl, Syntax::Turtle)
        .expect("fixture must load as valid Turtle");
    store
        .materialize()
        .expect("materialize() must succeed (no refusing hooks in this pack)");
    store
}

/// pattern-overdue.ttl: 4 same-topic GroundingQuestions drive the High/ActNow
/// prioritized obligation, a later unrelated turn leaves it undischarged —
/// the overdue hook must fire THROUGH THE HOOK MECHANISM, deriving an
/// OverdueEscalationObligation that escalates the High-severity obligation.
#[test]
fn overdue_fixture_derives_an_overdue_escalation_through_the_hook_mechanism() {
    let store = run_fixture(OVERDUE_TTL);
    assert_contains_triple(
        &store,
        "",
        RDF_TYPE,
        &format!("{SMON}OverdueEscalationObligation"),
    );
    assert_contains_triple(&store, "", &format!("{SMON}escalates"), "");
    // The underlying prioritized obligation carries the exact High/ActNow
    // parameterization the fixture stages (cnt >= 4 lane).
    assert_contains_triple(
        &store,
        "",
        &format!("{SMON}severity"),
        &format!("{SMON}High"),
    );
    assert_contains_triple(
        &store,
        "",
        &format!("{SMON}deadlineHint"),
        &format!("{SMON}ActNow"),
    );
}

/// pattern-ungoverned.ttl: a GroundingQuestion immediately followed by an
/// smon:Other turn falls outside every governed pattern — exactly ONE
/// UngovernedTransition (turn-1 -> turn-2) and ZERO EscalationObligations.
#[test]
fn ungoverned_fixture_flags_exactly_one_ungoverned_transition_and_no_escalation() {
    let store = run_fixture(UNGOVERNED_TTL);
    let rows = store
        .query(&format!(
            "SELECT ?u WHERE {{ ?u <{RDF_TYPE}> <{SMON}UngovernedTransition> . }}"
        ))
        .expect("ungoverned-transition count query must run");
    assert_eq!(rows.len(), 1, "exactly one UngovernedTransition: {rows:?}");
    assert_contains_triple(
        &store,
        "",
        &format!("{SMON}hasUngovernedQuestion"),
        "turn-1",
    );
    assert_contains_triple(
        &store,
        "",
        &format!("{SMON}hasUngovernedResponse"),
        "turn-2",
    );
    assert_not_contains_triple(&store, "", RDF_TYPE, &format!("{SMON}EscalationObligation"));
}

/// Rows returned by the pack's shipped open_obligations.rq against `store`.
fn open_obligation_rows(store: &TripleStore) -> usize {
    store
        .query(OPEN_OBLIGATIONS_RQ)
        .expect("open_obligations.rq must parse and run on this engine")
        .len()
}

/// pattern-fires-discharged.ttl is the hand-built AFTER-discharge example
/// (its own header: not a hook input — it demonstrates the recorded
/// discharge). The pack's shipped open_obligations.rq must return ZERO open
/// obligations against it; the non-zero control is the SAME node with only
/// smon:status flipped back to Open (statuses are separate, later facts per
/// the fixture's own discipline), proving the query discriminates on status
/// with everything else held byte-identical.
///
/// FINDING, recorded not papered over: pattern-fires.ttl's own hook
/// derivation was tried first as the control and yields ZERO rows — the
/// base derive hook constructs only type/prior/repeat/reason (no
/// smon:status/severity/deadlineHint), and the prioritized variants type
/// their nodes smon:PrioritizedEscalationObligation, so no hook-derived
/// node ever satisfies open_obligations.rq's full required property set on
/// `a smon:EscalationObligation`. Which side of that inconsistency is wrong
/// (the query, the base hook's property set, or a missing subclass
/// materialization rule) is pack law, tracked in load-path.ttl, not decided
/// unilaterally here.
#[test]
fn discharged_example_yields_zero_open_obligations_and_status_flip_is_the_control() {
    // The discharged example graph, loaded as-is (no hooks — per its header).
    let mut discharged = TripleStore::new();
    discharged
        .load_triples(ONTOLOGY_TTL, Syntax::Turtle)
        .expect("ontology.ttl must load");
    discharged
        .load_triples(FIRES_DISCHARGED_TTL, Syntax::Turtle)
        .expect("pattern-fires-discharged.ttl must load as valid Turtle");
    assert_eq!(
        open_obligation_rows(&discharged),
        0,
        "a discharged obligation must not appear as Open"
    );

    // Control: identical graph, status flipped to Open -> exactly one row.
    let flipped_ttl =
        FIRES_DISCHARGED_TTL.replace("smon:status smon:Discharged", "smon:status smon:Open");
    assert_ne!(
        flipped_ttl, FIRES_DISCHARGED_TTL,
        "flip must change the fixture"
    );
    let mut open = TripleStore::new();
    open.load_triples(ONTOLOGY_TTL, Syntax::Turtle)
        .expect("ontology.ttl must load");
    open.load_triples(&flipped_ttl, Syntax::Turtle)
        .expect("flipped fixture must load");
    assert_eq!(
        open_obligation_rows(&open),
        1,
        "the same obligation with status Open must be queryable as Open"
    );
}
