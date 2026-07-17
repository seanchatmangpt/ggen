//! Live verification for `packs/self-monitoring-pack/` -- proves the real
//! `kh:` hook-pack mechanism (`TripleStore::load_hook_pack` +
//! `.materialize()` + `.query()`, the SAME mechanism
//! `crates/praxis-graphlaw/tests/soc2_hook_actuation.rs` and
//! `crates/multifractal-workflow/tests/bribery_case_fixture.rs` already
//! exercise) genuinely fires the self-monitoring pack's
//! `smon:derive_escalation_obligation` hook over a positive fixture and
//! genuinely does NOT fire over two negative fixtures, each isolating one
//! conjunct of the rule:
//!
//!   grounding_question(Q) ∧ same_system(Q, Q_prev) ∧
//!     prior_response_was_survey(Q_prev)
//!   → derive(escalate_to_build)
//!
//! CLASSIFICATION-IS-INPUT: every fixture below hand-asserts
//! `smon:turnKind`/`dcterms:subject`/`smon:immediatelyFollows` directly (see
//! `packs/self-monitoring-pack/ontology.ttl`'s header). This test exercises
//! the SPARQL-CONSTRUCT pattern-match engine over already-classified facts;
//! it does not classify anything and is not an NLP test.

use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const HOOK_TTL: &str = include_str!("../../../packs/self-monitoring-pack/hook.ttl");
const ONTOLOGY_TTL: &str = include_str!("../../../packs/self-monitoring-pack/ontology.ttl");
const FIXTURE_FIRES: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/pattern-fires.ttl");
const FIXTURE_DOES_NOT_FIRE: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/pattern-does-not-fire.ttl");
const FIXTURE_DIFFERENT_TOPIC: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/pattern-different-topic.ttl");

const SMON: &str = "http://seanchatmangpt.github.io/packs/self-monitoring#";

/// Query the store for every `smon:EscalationObligation` node materialize()
/// produced, along with its prior/repeat GroundingQuestion links -- read
/// back via a real SPARQL SELECT over the post-materialize() store, not an
/// assumption about what the CONSTRUCT should have produced.
fn escalation_obligations(store: &TripleStore) -> Vec<(String, String, String)> {
    let rows = store
        .query(&format!(
            "SELECT ?esc ?prior ?repeat WHERE {{ \
                ?esc a <{SMON}EscalationObligation> ; \
                     <{SMON}hasPriorGroundingQuestion> ?prior ; \
                     <{SMON}hasGroundingQuestion> ?repeat . \
             }}"
        ))
        .expect("SELECT over the materialized store must succeed");

    rows.iter()
        .map(|row| {
            let get = |name: &str| {
                row.iter()
                    .find(|b| b.var == name)
                    .map(|b| b.val.clone())
                    .unwrap_or_else(|| panic!("row missing ?{name} binding: {row:?}"))
            };
            (get("esc"), get("prior"), get("repeat"))
        })
        .collect()
}

/// Loads the ontology (for smon: class/property declarations the SHACL law
/// pack does not require but real deployments would co-load) plus the hook
/// pack plus one fixture, then materializes -- exactly the
/// bribery_case_fixture.rs pipeline shape.
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

/// POSITIVE control: the hook fires over fixtures/pattern-fires.ttl and
/// derives exactly one smon:EscalationObligation linking turn-1 (prior) to
/// turn-4 (repeat).
#[test]
fn hook_fires_on_repeat_grounding_question_after_survey_only_response() {
    let store = run_fixture(FIXTURE_FIRES);
    let derived = escalation_obligations(&store);

    eprintln!("pattern-fires.ttl: derived EscalationObligation rows = {derived:?}");

    assert_eq!(
        derived.len(),
        1,
        "expected exactly 1 derived smon:EscalationObligation, got: {derived:?}"
    );
    let (_esc, prior, repeat) = &derived[0];
    // `Binding::val` carries the raw serialized term (e.g. `<...#turn-1>`,
    // angle brackets included) -- `.contains()`, not `.ends_with()`, matches
    // crates/multifractal-workflow/tests/bribery_case_fixture.rs's own
    // convention for exactly this reason.
    assert!(
        prior.contains("pattern-fires#turn-1"),
        "smon:hasPriorGroundingQuestion must point at turn-1, got: {prior}"
    );
    assert!(
        repeat.contains("pattern-fires#turn-4"),
        "smon:hasGroundingQuestion must point at turn-4, got: {repeat}"
    );

    // The reason literal is also a real derived triple, not asserted --
    // confirm it exists and names the pattern. `esc_term` is already a
    // directly-embeddable SPARQL term string (either `<iri>` or a bare
    // `_:label` blank-node reference, per `Binding::val`'s serialization) --
    // it must NOT be re-wrapped in angle brackets, or a `_:esc` blank-node
    // reference is misread as a named node with IRI text "_:esc".
    let esc_term = &derived[0].0;
    let reason_rows = store
        .query(&format!(
            "SELECT ?r WHERE {{ {esc_term} <{SMON}reason> ?r }}"
        ))
        .expect("SELECT for smon:reason must succeed");
    assert_eq!(
        reason_rows.len(),
        1,
        "expected exactly one smon:reason literal"
    );
    let reason = &reason_rows[0]
        .iter()
        .find(|b| b.var == "r")
        .expect("row must bind ?r")
        .val;
    eprintln!("derived smon:reason = {reason:?}");
    assert!(
        reason.contains("SurveyResponse") && reason.contains("escalate"),
        "reason literal must name the pattern, got: {reason}"
    );
}

/// NEGATIVE control 1: isolates prior_response_was_survey(Q_prev). The turn
/// immediately after the first GroundingQuestion is a RunResponse, not a
/// SurveyResponse -- must NOT derive an escalation, even though a second
/// GroundingQuestion on the same topic follows later.
#[test]
fn hook_does_not_fire_when_prior_response_was_a_run_not_a_survey() {
    let store = run_fixture(FIXTURE_DOES_NOT_FIRE);
    let derived = escalation_obligations(&store);

    eprintln!("pattern-does-not-fire.ttl: derived EscalationObligation rows = {derived:?}");

    assert!(
        derived.is_empty(),
        "a RunResponse (not SurveyResponse) following the first GroundingQuestion must not trigger escalation, got: {derived:?}"
    );
}

/// NEGATIVE control 2: isolates same_system(Q, Q_prev). Two
/// GroundingQuestions exist, the first is followed by a SurveyResponse, but
/// the second GroundingQuestion carries a DIFFERENT dcterms:subject
/// (grounding_topic) -- must NOT derive an escalation, proving the
/// shared-topic join is load-bearing, not decorative.
#[test]
fn hook_does_not_fire_when_the_second_grounding_question_is_a_different_topic() {
    let store = run_fixture(FIXTURE_DIFFERENT_TOPIC);
    let derived = escalation_obligations(&store);

    eprintln!("pattern-different-topic.ttl: derived EscalationObligation rows = {derived:?}");

    assert!(
        derived.is_empty(),
        "a second GroundingQuestion on a DIFFERENT grounding_topic must not trigger escalation, got: {derived:?}"
    );
}
