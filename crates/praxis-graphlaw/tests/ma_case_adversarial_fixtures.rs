//! Coverage for ma-case-study-pack's two previously-untested fixtures,
//! closing retrofit:SabotageFixtureCoverageGap's ma-case-study leg
//! (`mega-deal-second-request.ttl` was previously verified only via an
//! ad-hoc scratch binary its own header discloses; `target-org-colliding.ttl`
//! had zero test references anywhere).
//!
//! Mirrors `ma_case_hook_actuation.rs`: real fixture files via
//! `include_str!`, the real `kh:` hook mechanism
//! (`TripleStore::load_hook_pack` + `.materialize()`), no hand-simulated
//! derivation.

mod common;

use common::assert_contains_triple;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const HOOK_TTL: &str = include_str!("../../../packs/ma-case-study-pack/fixtures/hook.ttl");
const MEGA_DEAL_TTL: &str =
    include_str!("../../../packs/ma-case-study-pack/fixtures/mega-deal-second-request.ttl");
const TARGET_COLLIDING_TTL: &str =
    include_str!("../../../packs/ma-case-study-pack/fixtures/target-org-colliding.ttl");
const ACQUIRER_TTL: &str =
    include_str!("../../../packs/ma-case-study-pack/fixtures/acquirer-org.ttl");

const HAS_REGULATORY_FILING_OBLIGATION: &str =
    "http://seanchatmangpt.github.io/packs/ma-case-study#hasRegulatoryFilingObligation";
const HSR_OBLIGATION: &str =
    "http://seanchatmangpt.github.io/packs/ma-case-study#hsr-act-notification-obligation";
const SECOND_REQUEST_OBLIGATION: &str =
    "http://seanchatmangpt.github.io/packs/ma-case-study#second-request-review-obligation";
const MEGA_DEAL: &str = "https://deals.meridian-holdings.example.org/deal/MHC-2026-0311";

/// The USD 1,250,000,000 deal crosses BOTH thresholds (100M HSR filing and
/// 1B second-request review), so materialize() must derive BOTH obligations
/// for it — proving Verdict authority derives from two independent rules
/// over the same admitted graph, exactly the property the fixture was
/// authored to demonstrate (previously verified only by a scratch binary
/// under a past session's scratchpad, per the fixture's own header).
#[test]
fn mega_deal_derives_both_filing_and_second_request_obligations() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    store
        .load_triples(MEGA_DEAL_TTL, Syntax::Turtle)
        .expect("mega-deal fixture must load as valid Turtle");
    store
        .materialize()
        .expect("materialize() must succeed (no refusing hooks in this pack)");

    assert_contains_triple(
        &store,
        MEGA_DEAL,
        HAS_REGULATORY_FILING_OBLIGATION,
        HSR_OBLIGATION,
    );
    assert_contains_triple(
        &store,
        MEGA_DEAL,
        HAS_REGULATORY_FILING_OBLIGATION,
        SECOND_REQUEST_OBLIGATION,
    );
}

/// Collect the subject IRIs of every Azure-Terraform-namespace resource in
/// a fixture (the `aztf:` vocabulary the collision is staged in).
fn azure_subject_iris(ttl: &str) -> std::collections::BTreeSet<String> {
    let mut store = TripleStore::new();
    store
        .load_triples(ttl, Syntax::Turtle)
        .expect("fixture must load as valid Turtle");
    // Plain BGPs per aztf class, no FILTER functions: this engine's custom
    // executor silently returns zero rows for STRSTARTS/STR (probed live —
    // the same silent-unsupported class as its documented ORDER BY and VALUES
    // gaps), so the collision-staged classes are named explicitly.
    let mut out = std::collections::BTreeSet::new();
    for class in ["ContainerGroup", "Container"] {
        let rows = store
            .query(&format!(
                "SELECT ?s WHERE {{ ?s a <https://ggen.io/ontology/azure-terraform#{class}> . }}"
            ))
            .expect("aztf subject query must run");
        out.extend(
            rows.into_iter()
                .flat_map(|row| row.into_iter().map(|b| b.val)),
        );
    }
    out
}

/// `target-org-colliding.ttl`'s adversarial premise is REAL, not narrated:
/// it parses standalone (its own header: the collision is invisible to F02
/// admission), and its Azure resource subject IRIs genuinely overlap
/// acquirer-org.ttl's — the copy-pasted-without-re-keying failure mode the
/// fixture stages.
///
/// SCOPE, HONESTLY STATED: the mechanism that DETECTS this collision at
/// merge time (`f31_org_merge.rs` / `ma_org_merge.rs`, per the fixture's own
/// header) lives in the multifractal-workflow crate, which is not part of
/// this repository — detection coverage belongs where that mechanism lives.
/// This test pins the fixture's integrity (parseable + collision actually
/// present), so the staged defect cannot silently rot into a non-colliding
/// or unparseable file.
#[test]
fn target_org_colliding_fixture_parses_and_genuinely_collides_with_acquirer() {
    let colliding = azure_subject_iris(TARGET_COLLIDING_TTL);
    let acquirer = azure_subject_iris(ACQUIRER_TTL);
    assert!(
        !colliding.is_empty() && !acquirer.is_empty(),
        "both fixtures must declare aztf-typed Azure resources \
         (colliding: {colliding:?}, acquirer: {acquirer:?})"
    );
    let shared: Vec<&String> = colliding.intersection(&acquirer).collect();
    assert!(
        !shared.is_empty(),
        "the colliding fixture must share at least one Azure resource IRI \
         with acquirer-org.ttl (the staged collision), got disjoint sets: \
         colliding={colliding:?} acquirer={acquirer:?}"
    );
}
