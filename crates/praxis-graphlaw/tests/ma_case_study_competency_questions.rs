//! Real, committed, re-runnable execution of ma-case-study-pack's committed
//! `queries/*.rq` competency questions against its committed fixtures.
//!
//! CLOSES A DOC FABRICATION, not merely a gap (2026-07-22, wave3
//! reverify-unverified-docs pass): `packs/ma-case-study-pack/STANDING.md`'s
//! "Committed, re-runnable CQ evidence (2026-07-19, L5-push pass)" section
//! claims this exact file already existed, committed, with "9 passed, 0
//! failed". It did not: `git log --all` for
//! `**/ma_case_study_competency_questions.rs` returns nothing on any branch
//! -- the file was never committed, at any point, anywhere in this repo's
//! history. Every one of `.specify/pack-l5-promotion.ttl`'s and
//! `.specify/maturity.ttl`'s "per-CQ test coverage UNVERIFIED" notes for
//! this pack undersold the real problem: the doc did not merely fail to
//! verify a real claim, it asserted a false one (a specific file, a
//! specific pass/fail count) that never existed. This file is the real
//! thing STANDING.md described, written for real this pass, not merely a
//! correction of the prose.
//!
//! Covers the same 6 queries STANDING.md's false claim named (CQ1.1, CQ1.2,
//! CQ2.1, CQ3.1, CQ3.2, CQ7.1 -- concepts 1, 2, 3, 7), executed via
//! `praxis_graphlaw::TripleStore::query` (the SAME mechanism
//! `ma_case_hook_actuation.rs`/`ma_case_adversarial_fixtures.rs` already use
//! for this pack) against `fixtures/case.ttl` and, for the two queries whose
//! own headers claim a cross-instance-different-answer property (CQ1.1,
//! CQ7.1), also against `fixtures/case-2.ttl`. Every `.rq` file is loaded
//! via `include_str!` (byte-identical to the committed query, never
//! retyped) and every asserted expected value was independently
//! cross-checked against the real fixture text by this pass (grep line
//! numbers cited in the review that produced this file), not copied
//! unverified from each query's own header comment.

mod common;

use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::sparql::Binding;
use praxis_graphlaw::TripleStore;

const CASE_TTL: &str = include_str!("../../../packs/ma-case-study-pack/fixtures/case.ttl");
const CASE2_TTL: &str = include_str!("../../../packs/ma-case-study-pack/fixtures/case-2.ttl");

const CQ1_1: &str =
    include_str!("../../../packs/ma-case-study-pack/queries/cq1.1-lei-of-entity.rq");
const CQ1_2: &str =
    include_str!("../../../packs/ma-case-study-pack/queries/cq1.2-lei-registered-entities.rq");
const CQ2_1: &str =
    include_str!("../../../packs/ma-case-study-pack/queries/cq2.1-ownership-percentage.rq");
const CQ3_1: &str =
    include_str!("../../../packs/ma-case-study-pack/queries/cq3.1-open-due-diligence-items.rq");
const CQ3_2: &str =
    include_str!("../../../packs/ma-case-study-pack/queries/cq3.2-waived-due-diligence-items.rq");
const CQ7_1: &str =
    include_str!("../../../packs/ma-case-study-pack/queries/cq7.1-vote-outcome-and-quorum.rq");

/// Run `query_ttl` (a committed `.rq` file's real text) against `data_ttl`
/// via the real `TripleStore::query` path and return the raw rows.
fn run(query_ttl: &str, data_ttl: &str) -> Vec<Vec<Binding>> {
    let mut store = TripleStore::new();
    store
        .load_triples(data_ttl, Syntax::Turtle)
        .expect("fixture must load as valid Turtle");
    store
        .query(query_ttl)
        .expect("committed .rq file must execute as valid SPARQL 1.1")
}

/// Fetch one row's raw binding value by SELECT variable name (e.g.
/// "leiCode", no leading '?' -- `praxis_graphlaw::sparql::Binding::var`
/// already strips it, per `crates/praxis-graphlaw/src/sparql/mod.rs`'s
/// `decode` fn). The RAW value is this engine's own N-Triples-ish surface
/// syntax, confirmed live this pass: IRIs are `<...>`-wrapped
/// (`<https://.../entity/...>`) and literals are `"lexical"^^<datatype-IRI>`
/// (e.g. `"71.00"^^<http://www.w3.org/2001/XMLSchema#decimal>`), never a
/// bare string -- use `iri()`/`literal_lexical()` below to strip that
/// framing rather than comparing this raw form directly.
fn val<'a>(row: &'a [Binding], var: &str) -> &'a str {
    row.iter()
        .find(|b| b.var == var)
        .unwrap_or_else(|| panic!("row has no binding for ?{var}: {row:?}"))
        .val
        .as_str()
}

/// Strip a `<...>`-wrapped IRI binding down to the bare IRI string.
fn iri(raw: &str) -> &str {
    raw.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or_else(|| panic!("binding is not an IRI-shaped `<...>` value: {raw}"))
}

/// Strip a `"lexical"^^<datatype>` or plain `"lexical"` literal binding down
/// to its bare lexical value (no quotes, no datatype/language suffix).
fn literal_lexical(raw: &str) -> &str {
    let inner = raw
        .strip_prefix('"')
        .unwrap_or_else(|| panic!("binding is not a quoted literal: {raw}"));
    // Cut at the closing quote that starts the optional ^^<...>/@lang suffix
    // -- values under test here never contain an escaped `"` themselves, so
    // the first remaining `"` is always the closing one.
    match inner.find('"') {
        Some(end) => &inner[..end],
        None => panic!("literal binding has no closing quote: {raw}"),
    }
}

/// CQ1.1 against case.ttl: independently re-verified against the real fixture
/// text (`packs/ma-case-study-pack/fixtures/case.ttl:127-129,137-139`) --
/// exactly 2 LEI-registered entities, each paired with its real LEI code.
#[test]
fn cq1_1_lei_of_entity_against_main_case() {
    let rows = run(CQ1_1, CASE_TTL);
    assert_eq!(rows.len(), 2, "expected exactly 2 rows, got {rows:?}");
    let codes: std::collections::BTreeSet<&str> = rows
        .iter()
        .map(|r| literal_lexical(val(r, "leiCode")))
        .collect();
    assert!(codes.contains("5493001234MERIDIANHG"), "{codes:?}");
    assert!(codes.contains("5493005678CORVANTISX"), "{codes:?}");
}

/// CQ1.1 against case-2.ttl: a DIFFERENT, wholly independent case instance
/// (Solvane/Northgate) answers the SAME query with DIFFERENT real LEI
/// codes, no query change needed -- the Generalization property this pack's
/// two-instance fixture pair exists to demonstrate. Re-verified against
/// `fixtures/case-2.ttl:60-62,70-72`.
#[test]
fn cq1_1_lei_of_entity_against_second_independent_case() {
    let rows = run(CQ1_1, CASE2_TTL);
    assert_eq!(rows.len(), 2, "expected exactly 2 rows, got {rows:?}");
    let codes: std::collections::BTreeSet<&str> = rows
        .iter()
        .map(|r| literal_lexical(val(r, "leiCode")))
        .collect();
    assert!(codes.contains("5493004321SOLVANEAG"), "{codes:?}");
    assert!(codes.contains("5493008765NORTHGATEM"), "{codes:?}");
}

/// CQ1.2 against case.ttl: exactly the 2 LEI-registered entity subjects
/// (acquirer + target), no more, no fewer.
#[test]
fn cq1_2_lei_registered_entities_against_main_case() {
    let rows = run(CQ1_2, CASE_TTL);
    let entities: std::collections::BTreeSet<&str> =
        rows.iter().map(|r| iri(val(r, "entity"))).collect();
    assert_eq!(
        entities,
        std::collections::BTreeSet::from([
            "https://deals.meridian-holdings.example.org/entity/acquirer-meridian-holdings-group",
            "https://deals.meridian-holdings.example.org/entity/target-corvantis-systems",
        ]),
        "got {entities:?}"
    );
}

/// CQ2.1 against case.ttl: exactly 1 `RelationshipRecord`, 100.00% ownership
/// (`fixtures/case.ttl:150-151`).
#[test]
fn cq2_1_ownership_percentage_against_main_case() {
    let rows = run(CQ2_1, CASE_TTL);
    assert_eq!(rows.len(), 1, "expected exactly 1 row, got {rows:?}");
    assert_eq!(literal_lexical(val(&rows[0], "pct")), "100.00");
}

/// CQ3.1 against case.ttl: exactly 1 Open due-diligence item, the specific
/// individual case.ttl asserts (`fixtures/case.ttl:193-196`), matching the
/// query's own header claim -- independently re-verified, not merely
/// trusted.
#[test]
fn cq3_1_open_due_diligence_items_against_main_case() {
    let rows = run(CQ3_1, CASE_TTL);
    assert_eq!(rows.len(), 1, "expected exactly 1 row, got {rows:?}");
    assert_eq!(
        iri(val(&rows[0], "item")),
        "https://deals.meridian-holdings.example.org/ddi/customer-contract-consent-assignments"
    );
}

/// CQ3.2 against case.ttl: exactly 1 Waived item
/// (`fixtures/case.ttl:198-201`).
#[test]
fn cq3_2_waived_due_diligence_items_against_main_case() {
    let rows = run(CQ3_2, CASE_TTL);
    assert_eq!(rows.len(), 1, "expected exactly 1 row, got {rows:?}");
    assert_eq!(
        iri(val(&rows[0], "item")),
        "https://deals.meridian-holdings.example.org/ddi/environmental-phase-2-survey"
    );
}

/// CQ3.2 against case-2.ttl: a real NEGATIVE check, independently
/// re-verified against `fixtures/case-2.ttl:112-118` -- both of its 2
/// DueDiligenceItems are Satisfied/Open, zero are Waived, so the query must
/// return zero rows there (proving the query discriminates, not that it
/// always matches).
#[test]
fn cq3_2_waived_due_diligence_items_against_second_independent_case_is_empty() {
    let rows = run(CQ3_2, CASE2_TTL);
    assert!(rows.is_empty(), "expected 0 rows, got {rows:?}");
}

/// CQ7.1 against case.ttl: `ma:Passed`, 82.50 quorum
/// (`fixtures/case.ttl:247-250`).
#[test]
fn cq7_1_vote_outcome_and_quorum_against_main_case() {
    let rows = run(CQ7_1, CASE_TTL);
    assert_eq!(rows.len(), 1, "expected exactly 1 row, got {rows:?}");
    assert!(
        iri(val(&rows[0], "outcome")).ends_with("Passed"),
        "{:?}",
        rows[0]
    );
    assert_eq!(literal_lexical(val(&rows[0], "quorum")), "82.50");
}

/// CQ7.1 against case-2.ttl: the SAME query, a DIFFERENT independent case
/// instance, a DIFFERENT real answer -- `ma:Failed`, 71.00 quorum
/// (`fixtures/case-2.ttl:164-167`), no query authoring change needed
/// between the two instances.
#[test]
fn cq7_1_vote_outcome_and_quorum_against_second_independent_case() {
    let rows = run(CQ7_1, CASE2_TTL);
    assert_eq!(rows.len(), 1, "expected exactly 1 row, got {rows:?}");
    assert!(
        iri(val(&rows[0], "outcome")).ends_with("Failed"),
        "{:?}",
        rows[0]
    );
    assert_eq!(literal_lexical(val(&rows[0], "quorum")), "71.00");
}
