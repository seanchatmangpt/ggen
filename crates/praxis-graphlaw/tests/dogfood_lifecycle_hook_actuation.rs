//! Knowledge-hook-driven actuation of dogfood-lifecycle-pack's obligation
//! lifecycle (`packs/dogfood-lifecycle-pack/{hook.ttl,fixtures/*.ttl}`),
//! closing retrofit:SabotageFixtureCoverageGap's dogfood-lifecycle leg: the
//! pack's 8 non-session-good fixtures previously had ZERO .rs test references
//! anywhere (their behavior was verified only by a scratch consumer crate
//! under a past session's scratchpad, per hook.ttl's own header) — this file
//! is the committed, repo-resident replacement for that scratch verification.
//!
//! Mirrors `ma_case_hook_actuation.rs` / `self_monitoring_hook_actuation.rs`:
//! real fixture files loaded via `include_str!`, exercised through the SAME
//! real `kh:` hook-pack mechanism (`praxis_graphlaw::TripleStore::
//! load_hook_pack` + `.materialize()`) — not a hand-simulated derivation.
//!
//! Fixture semantics under test (each fixture's own header documents the
//! expected outcome; this file asserts exactly those outcomes):
//! - session-malformed.ttl        — deliberate Turtle PARSE error: loading must FAIL.
//! - session-error-not-blocked.ttl — dfl:Error (adjacent to Blocked in the same
//!   closed Outcome scheme) must derive NO obligation (fire precision).
//! - session-discharged.ttl       — positive discharge: dischargedBy event-5.
//! - session-adversarial-discharge.ttl — later Ok by a DIFFERENT agent must
//!   NOT discharge (same-agent conjunct isolated).
//! - session-cross-session-collision.ttl — later Ok by the same agent in a
//!   DIFFERENT session must NOT discharge and NOT escalate (same-session
//!   conjunct isolated).
//! - session-escalated.ttl        — 3 later same-agent/same-session events:
//!   escalatedBy event-7 (and ALSO dischargedBy event-5 — the two pointer
//!   properties coexist by design, see hook.ttl's escalate note).
//! - session-not-yet-overdue.ttl  — only 2 later events: NO escalatedBy
//!   (adjacent-boundary fire precision for the join-based >=3 threshold).
//!
//! session-iri-collision.ttl (the 8th fixture) is tested against the pack's
//! CURRENT cardinality law: shapes.ttl was deleted in the SHACL->SPARQL-gates
//! migration and its `sh:maxCount 1` constraints now live in
//! `gates/020_single_valued.rq` — so the test below runs that gate's REAL
//! committed query text (include_str!, never re-typed) over the real fixture,
//! with session-good.ttl as the zero-rows control proving the gate
//! discriminates. (The equivalent engine-level `--shapes` refusal is covered
//! in `crates/ggen-engine/tests/lint_validate_e2e.rs`.)

mod common;

use common::{assert_contains_triple, assert_not_contains_triple};
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const HOOK_TTL: &str = include_str!("../../../packs/dogfood-lifecycle-pack/hook.ttl");

const MALFORMED_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-malformed.ttl");
const ERROR_NOT_BLOCKED_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-error-not-blocked.ttl");
const DISCHARGED_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-discharged.ttl");
const ADVERSARIAL_DISCHARGE_TTL: &str = include_str!(
    "../../../packs/dogfood-lifecycle-pack/fixtures/session-adversarial-discharge.ttl"
);
const CROSS_SESSION_TTL: &str = include_str!(
    "../../../packs/dogfood-lifecycle-pack/fixtures/session-cross-session-collision.ttl"
);
const ESCALATED_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-escalated.ttl");
const NOT_YET_OVERDUE_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-not-yet-overdue.ttl");
const IRI_COLLISION_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-iri-collision.ttl");
const SESSION_GOOD_TTL: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/fixtures/session-good.ttl");
/// The pack's REAL committed single-valued gate (sh:maxCount 1 translated to
/// SPARQL in the SHACL->gates migration) — never re-typed here.
const GATE_020_SINGLE_VALUED: &str =
    include_str!("../../../packs/dogfood-lifecycle-pack/gates/020_single_valued.rq");

const DFL: &str = "http://seanchatmangpt.github.io/packs/dogfood-lifecycle#";

/// Load hook.ttl + one session fixture into a fresh store and materialize.
fn materialized(fixture_ttl: &str) -> TripleStore {
    let mut store = TripleStore::new();
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

/// The deliberately-malformed fixture (unclosed IRI on event-1's prov:used)
/// must be REFUSED at parse time — a real Turtle syntax error, not a silent
/// partial load.
#[test]
fn malformed_fixture_is_refused_at_load() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    assert!(
        store.load_triples(MALFORMED_TTL, Syntax::Turtle).is_err(),
        "session-malformed.ttl's unclosed IRI must be a load error, not accepted"
    );
}

/// Fire precision: a dfl:Error outcome (adjacent member of the same closed
/// Outcome scheme as Blocked) must derive NO obligation — a sloppy
/// "outcome != Ok" pattern would wrongly fire here.
#[test]
fn error_outcome_derives_no_obligation() {
    let store = materialized(ERROR_NOT_BLOCKED_TTL);
    // A whole-dump substring check would false-positive on the hook pack's own
    // kh:query literals (they contain the dfl:Obligation IRI as text), so
    // assert on the actual triple shape: no node is typed dfl:Obligation and
    // no derivedFrom pointer exists.
    assert_not_contains_triple(
        &store,
        "",
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        &format!("{DFL}Obligation"),
    );
    assert_not_contains_triple(&store, "", &format!("{DFL}derivedFrom"), "");
}

/// Positive discharge: the obligation derived from Blocked event-4 must end
/// up dischargedBy event-5 (a later same-agent, same-session dfl:Ok event)
/// with obligationStatus Discharged.
#[test]
fn discharged_fixture_discharges_via_event_5() {
    let store = materialized(DISCHARGED_TTL);
    assert_contains_triple(&store, "", &format!("{DFL}derivedFrom"), "event-4");
    assert_contains_triple(&store, "", &format!("{DFL}dischargedBy"), "event-5");
    assert_contains_triple(
        &store,
        "",
        &format!("{DFL}obligationStatus"),
        &format!("{DFL}Discharged"),
    );
}

/// Gamed input: the later Ok event belongs to a DIFFERENT agent, so the
/// same-agent conjunct of discharge_review_obligation must refuse to fire.
#[test]
fn different_agent_ok_does_not_discharge() {
    let store = materialized(ADVERSARIAL_DISCHARGE_TTL);
    // The obligation itself IS derived (event-4 is Blocked)...
    assert_contains_triple(&store, "", &format!("{DFL}derivedFrom"), "event-4");
    // ...but never discharged.
    assert_not_contains_triple(&store, "", &format!("{DFL}dischargedBy"), "");
    assert_not_contains_triple(
        &store,
        "",
        &format!("{DFL}obligationStatus"),
        &format!("{DFL}Discharged"),
    );
}

/// Gamed input: the later Ok event is by the SAME agent but in a DIFFERENT
/// session — the same-session conjunct must refuse both discharge and
/// escalation (this fixture isolates the conjunct session-adversarial-
/// discharge.ttl does not).
#[test]
fn cross_session_ok_neither_discharges_nor_escalates() {
    let store = materialized(CROSS_SESSION_TTL);
    assert_contains_triple(&store, "", &format!("{DFL}derivedFrom"), "sessionA-event-4");
    assert_not_contains_triple(&store, "", &format!("{DFL}dischargedBy"), "");
    assert_not_contains_triple(&store, "", &format!("{DFL}escalatedBy"), "");
}

/// Positive escalation: three further same-agent/same-session events after
/// the Blocked one — escalatedBy must point at event-7 (the third), and
/// dischargedBy at event-5 (also Ok): the two pointer properties coexist by
/// design.
#[test]
fn escalated_fixture_escalates_via_event_7_and_also_discharges() {
    let store = materialized(ESCALATED_TTL);
    assert_contains_triple(&store, "", &format!("{DFL}escalatedBy"), "event-7");
    assert_contains_triple(&store, "", &format!("{DFL}dischargedBy"), "event-5");
}

/// Adjacent-boundary fire precision: only TWO further events follow the
/// Blocked one, so the join-based >=3 threshold must NOT produce any
/// escalatedBy triple at all.
#[test]
fn not_yet_overdue_fixture_does_not_escalate() {
    let store = materialized(NOT_YET_OVERDUE_TTL);
    assert_contains_triple(&store, "", &format!("{DFL}derivedFrom"), "event-4");
    assert_not_contains_triple(&store, "", &format!("{DFL}escalatedBy"), "");
}

/// Run gate 020's real committed query text over `ttl` on oxigraph — the
/// SAME engine production gate evaluation uses (`GraphLawStore::query`
/// delegates SPARQL to its oxigraph mirror; praxis-graphlaw's own custom
/// evaluator refuses VALUES, see `values_clause_is_refused_not_silent` below
/// and `plan_query_or_refuse`'s VALUES check).
fn gate_020_rows(ttl: &str) -> Vec<String> {
    use oxigraph::io::RdfFormat;
    use oxigraph::sparql::{QueryResults, SparqlEvaluator};
    use oxigraph::store::Store;

    let store = Store::new().expect("oxigraph store");
    store
        .load_from_reader(RdfFormat::Turtle, ttl.as_bytes())
        .expect("fixture must parse (the collision defect is semantic, not syntactic)");
    let prepared = SparqlEvaluator::new()
        .parse_query(GATE_020_SINGLE_VALUED)
        .expect("gate 020's committed query must parse under real SPARQL 1.1");
    let results = prepared.on_store(&store).execute().expect("query runs");
    let mut rows = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            rows.push(format!("{:?}", solution.expect("solution")));
        }
    }
    rows
}

/// The IRI-collision fixture (two distinct tool events merged onto one
/// `<#event-1>` subject, yielding two skos:notation and two dfl:outcome
/// values) must be caught by the pack's real committed single-valued gate:
/// the gate query returns violation rows naming event-1.
#[test]
fn iri_collision_fixture_is_refused_by_the_single_valued_gate() {
    let rows = gate_020_rows(IRI_COLLISION_TTL);
    assert!(
        !rows.is_empty(),
        "the double-notation/double-outcome merge must produce violation rows"
    );
    let dump = rows.join("\n");
    assert!(dump.contains("event-1"), "names the colliding node: {dump}");
}

/// Zero-rows control: session-good.ttl passes the same gate, proving the
/// query discriminates rather than always-flagging.
#[test]
fn session_good_passes_the_single_valued_gate() {
    let rows = gate_020_rows(SESSION_GOOD_TTL);
    assert!(
        rows.is_empty(),
        "conforming fixture must yield zero violation rows: {rows:?}"
    );
}

/// Engine defect found while writing this file, repaired fail-closed: this
/// crate's own custom SPARQL evaluator silently returned ZERO rows for any
/// query containing a VALUES clause (probed live: the same colliding data
/// that yields 2 rows without VALUES yields 0 rows with it) — a
/// silent-false-pass shape for any caller using `TripleStore::query` with
/// VALUES. `plan_query_or_refuse` now refuses VALUES loudly instead.
#[test]
fn values_clause_is_refused_not_silent() {
    let mut store = TripleStore::new();
    store
        .load_triples(IRI_COLLISION_TTL, Syntax::Turtle)
        .expect("fixture parses");
    let err = store
        .query(GATE_020_SINGLE_VALUED)
        .expect_err("a VALUES query must be a loud refusal, never silent zero rows");
    // Gate 020 contains BOTH VALUES and a trailing ORDER BY (also probed
    // broken, refused since g15's full audit); the refusal names whichever
    // it finds first — either is the correct loud-refusal behavior.
    assert!(
        err.contains("unsupported construct")
            && (err.contains("VALUES") || err.contains("ORDER BY")),
        "names an unsupported construct: {err}"
    );
}
