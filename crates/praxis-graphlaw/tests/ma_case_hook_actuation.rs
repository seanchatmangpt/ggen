//! Knowledge-hook-driven actuation of the Meridian Holdings Group /
//! Corvantis Systems M&A case study
//! (`packs/ma-case-study-pack/fixtures/{case,hook,adversarial-positive,adversarial-negative}.ttl`).
//!
//! Mirrors `crates/multifractal-workflow/tests/bribery_case_fixture.rs`'s
//! structure: real fixture files loaded via `include_str!`, exercised
//! through the SAME real, tested `kh:` hook-pack mechanism
//! (`praxis_graphlaw::TripleStore::load_hook_pack` +
//! `.materialize()`) `crates/praxis-graphlaw/tests/soc2_hook_actuation.rs`
//! already exercises in production-shaped form -- not a hand-simulated
//! trigger.
//!
//! Per docs/releases/v26.7.14/THESIS.md Section 33.12, an acquisition
//! contains several independent external service processes (buyer
//! diligence, seller disclosure, financing, regulatory review, board
//! authority, shareholder action); `fixtures/hook.ttl`'s
//! `derive_ma_regulatory_filing_obligation` hook actuates exactly one of
//! them -- the regulatory-review process -- by deriving an HSR-Act-style
//! antitrust filing obligation once a deal's raw, admitted aggregate
//! consideration (`schema:amount`) crosses the hook's illustrative filing
//! threshold. Three scenarios below prove this real hook engine behavior,
//! not merely that the fixture files parse:
//!
//! 1. `hook_derives_filing_obligation_from_the_main_case_scenario` -- the
//!    hook fires over the full, 6-process `case.ttl` scenario (the deal's
//!    schema:amount of USD 725,000,000 is far above the threshold).
//! 2. `hook_derives_filing_obligation_from_the_adversarial_positive_fixture`
//!    -- the hook fires over the minimal, self-contained G_x+ fixture (USD
//!    150,000,000, also above threshold) -- Section 33.11's positive
//!    semantic-admission-proof component.
//! 3. `hook_does_not_fire_over_the_adversarial_negative_fixture` -- the
//!    hook does NOT fire over the minimal, self-contained G_x- fixture
//!    (USD 42,000,000, below threshold) -- Section 33.11's negative
//!    component. `adversarial-negative.ttl`'s own header additionally
//!    documents two REAL SHACL violations this graph carries (verified via
//!    `ggen graph validate --files ... --shapes shapes.ttl` this session,
//!    not asserted here); this test's scope is the hook engine only, the
//!    same scope `soc2_hook_actuation.rs` and bribery-case's own
//!    `hook_does_not_fire_for_a_domestic_contractor` test hold to.
//!
//! SCOPE, HONESTLY STATED: unlike
//! `crates/multifractal-workflow/tests/bribery_case_fixture.rs`, this test
//! does NOT exercise F02 observation admission
//! (`multifractal_workflow::f02_observation_admission`) -- see
//! `fixtures/case.ttl`'s own header for why standing up an equivalent
//! `AdmissionPolicy` for a 6-process M&A case is out of this task's scope.
//! This test proves the Knowledge Hook mechanism only, the same scope
//! `soc2_hook_actuation.rs` already holds to for its own SOC2 hooks.

mod common;

use common::{assert_contains_triple, assert_not_contains_triple};
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const CASE_TTL: &str = include_str!("../../../packs/ma-case-study-pack/fixtures/case.ttl");
const HOOK_TTL: &str = include_str!("../../../packs/ma-case-study-pack/fixtures/hook.ttl");
const ADVERSARIAL_POSITIVE_TTL: &str =
    include_str!("../../../packs/ma-case-study-pack/fixtures/adversarial-positive.ttl");
const ADVERSARIAL_NEGATIVE_TTL: &str =
    include_str!("../../../packs/ma-case-study-pack/fixtures/adversarial-negative.ttl");

const MA: &str = "http://seanchatmangpt.github.io/packs/ma-case-study#";
const HAS_REGULATORY_FILING_OBLIGATION: &str =
    "http://seanchatmangpt.github.io/packs/ma-case-study#hasRegulatoryFilingObligation";
const HSR_OBLIGATION: &str =
    "http://seanchatmangpt.github.io/packs/ma-case-study#hsr-act-notification-obligation";

const MAIN_CASE_DEAL: &str = "https://deals.meridian-holdings.example.org/deal/MHC-2026-0091";
const ADVERSARIAL_POSITIVE_DEAL: &str =
    "https://deals.meridian-holdings.example.org/deal/MHC-2026-0142";
const ADVERSARIAL_NEGATIVE_DEAL: &str =
    "https://deals.meridian-holdings.example.org/deal/MHC-2026-0203";

/// The hook fires over the full, 6-process main case scenario: the deal's
/// USD 725,000,000 aggregate consideration is far above
/// `fixtures/hook.ttl`'s 100,000,000 filing threshold, so `materialize()`
/// must derive exactly 1 `ma:hasRegulatoryFilingObligation` triple linking
/// the deal to the static `ma:hsr-act-notification-obligation` catalog
/// individual.
#[test]
fn hook_derives_filing_obligation_from_the_main_case_scenario() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    store
        .load_triples(CASE_TTL, Syntax::Turtle)
        .expect("case.ttl must load as valid Turtle into the hook engine's store");

    store
        .materialize()
        .expect("materialize() must succeed (no refusing hooks in this pack)");

    let dump = store.content_to_string();
    let derived_lines: Vec<&str> = dump
        .lines()
        .filter(|l| l.contains(MAIN_CASE_DEAL) && l.contains(HAS_REGULATORY_FILING_OBLIGATION))
        .collect();
    eprintln!("hook-derived ma:hasRegulatoryFilingObligation triples for {MAIN_CASE_DEAL}:");
    for l in &derived_lines {
        eprintln!("  {l}");
    }

    assert_eq!(
        derived_lines.len(),
        1,
        "the hook must derive exactly 1 ma:hasRegulatoryFilingObligation triple for the main case deal, got: {derived_lines:?}"
    );
    assert_contains_triple(
        &store,
        MAIN_CASE_DEAL,
        HAS_REGULATORY_FILING_OBLIGATION,
        HSR_OBLIGATION,
    );
}

/// The hook fires over the minimal, self-contained G_x+ fixture (Section
/// 33.11): USD 150,000,000 is above the 100,000,000 threshold.
#[test]
fn hook_derives_filing_obligation_from_the_adversarial_positive_fixture() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    store
        .load_triples(ADVERSARIAL_POSITIVE_TTL, Syntax::Turtle)
        .expect("adversarial-positive.ttl must load as valid Turtle into the hook engine's store");

    store
        .materialize()
        .expect("materialize() must succeed (no refusing hooks in this pack)");

    assert_contains_triple(
        &store,
        ADVERSARIAL_POSITIVE_DEAL,
        HAS_REGULATORY_FILING_OBLIGATION,
        HSR_OBLIGATION,
    );
}

/// Negative control (Section 33.11's G_x-): the minimal, self-contained
/// adversarial-negative fixture's deal is USD 42,000,000, below the
/// 100,000,000 threshold -- the hook's SPARQL pattern must NOT match, so
/// `materialize()` must derive NO `ma:hasRegulatoryFilingObligation`
/// triple for it. Proves the hook is a genuine conditional pattern match on
/// deal size, not an unconditional actuation -- the same discipline
/// bribery-case's `hook_does_not_fire_for_a_domestic_contractor` test
/// establishes for its own cross-border-jurisdiction condition.
#[test]
fn hook_does_not_fire_over_the_adversarial_negative_fixture() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    store
        .load_triples(ADVERSARIAL_NEGATIVE_TTL, Syntax::Turtle)
        .expect("adversarial-negative.ttl must load as valid Turtle into the hook engine's store");

    store
        .materialize()
        .expect("materialize() must succeed (no refusing hooks in this pack)");

    let dump = store.content_to_string();
    let derived_lines: Vec<&str> = dump
        .lines()
        .filter(|l| {
            l.contains(ADVERSARIAL_NEGATIVE_DEAL) && l.contains(HAS_REGULATORY_FILING_OBLIGATION)
        })
        .collect();

    assert!(
        derived_lines.is_empty(),
        "a deal below the filing threshold must not trigger obligation derivation, got: {derived_lines:?}"
    );
    assert_not_contains_triple(
        &store,
        ADVERSARIAL_NEGATIVE_DEAL,
        HAS_REGULATORY_FILING_OBLIGATION,
        HSR_OBLIGATION,
    );
}

/// Sanity check on the catalog individual itself: `hook.ttl`'s static
/// `ma:hsr-act-notification-obligation` carries the closed
/// `ma:RegulatoryFilingObligationShape` property set
/// (`ma:filingThresholdAmount` / `ma:triggeringStatute` /
/// `ma:statutoryWaitingPeriodDays`) so a future PDDL-projection stage has
/// real data to project, not a bare, propertyless IRI.
#[test]
fn hook_pack_declares_the_hsr_catalog_obligation_with_its_closed_property_set() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");

    assert_contains_triple(
        &store,
        HSR_OBLIGATION,
        &format!("{MA}filingThresholdAmount"),
        "100000000.00",
    );
    assert_contains_triple(
        &store,
        HSR_OBLIGATION,
        &format!("{MA}triggeringStatute"),
        "15 U.S.C.",
    );
    assert_contains_triple(
        &store,
        HSR_OBLIGATION,
        &format!("{MA}statutoryWaitingPeriodDays"),
        "30",
    );
}
