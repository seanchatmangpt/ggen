//! Stage 3 of this session's self-monitoring dogfood exercise: load the REAL
//! smon: Turtle facts Stage 2 extracted from THIS session's own transcript
//! (`packs/self-monitoring-pack/fixtures/session-real.ttl`, 1720 turns,
//! 10326 triples, regenerated fresh this session by
//! `packs/self-monitoring-pack/scripts/transcript_to_turtle.py` against the
//! live `~/.claude/projects/-Users-sac-praxis/
//! 1f9798ec-f62d-48bb-80a0-e9817fafdb71.jsonl` transcript) into a REAL
//! `oxigraph::store::Store`, and run Stage 1's real `hook.ttl`
//! `smon:derive_escalation_obligation_action` CONSTRUCT query -- extracted
//! VERBATIM from hook.ttl (never re-typed; `extract_action_construct_query`
//! plus a self-check `assert!` guard against drift) -- via oxigraph's own
//! independent SPARQL 1.1 engine (`oxigraph::sparql::SparqlEvaluator`), which
//! is wholly separate from this crate's own custom `sparql/` planner+executor
//! that `praxis_graphlaw::TripleStore` uses. Every test below also
//! cross-validates against `TripleStore::load_hook_pack` + `.materialize()`
//! (the SAME proven kh: mechanism `self_monitoring_hook_actuation.rs` already
//! exercises on hand-built fixtures), so every finding in this file is
//! confirmed by TWO independent SPARQL engines, not one.
//!
//! CLASSIFICATION-IS-INPUT: see `packs/self-monitoring-pack/ontology.ttl`'s
//! header. Every fact this file loads was produced by
//! `transcript_to_turtle.py`'s disclosed pattern-matcher, not by any part of
//! `hook.ttl` or this test.

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::sparql::{QueryResults, QuerySolution, SparqlEvaluator};
use oxigraph::store::Store;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;
use std::collections::BTreeMap;

const HOOK_TTL: &str = include_str!("../../../packs/self-monitoring-pack/hook.ttl");
const ONTOLOGY_TTL: &str = include_str!("../../../packs/self-monitoring-pack/ontology.ttl");
/// The REAL session facts (default, disclosed keyword-heuristic
/// classification/topic-tagging -- see `transcript_to_turtle.py`'s module
/// docstring). Regenerated this session via:
/// `python3 packs/self-monitoring-pack/scripts/transcript_to_turtle.py
///   --transcript ~/.claude/projects/-Users-sac-praxis/1f9798ec-....jsonl
///   --session-id 1f9798ec-f62d-48bb-80a0-e9817fafdb71
///   --out packs/self-monitoring-pack/fixtures/session-real.ttl`
/// 1720 turns extracted, 10326 triples, turnKind counts {'Other': 1553,
/// 'SurveyResponse': 148, 'BlockerResponse': 7, 'RunResponse': 9,
/// 'GroundingQuestion': 3}.
const SESSION_REAL_TTL: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/session-real.ttl");
/// A DISCLOSED, CLEARLY-LABELED counterfactual (NOT the pack's default
/// behavior) built by `packs/self-monitoring-pack/scripts/
/// broaden_topic_experiment.py` from `session-real.ttl`, for ADVERSARIAL
/// CHECK (3) -- topic-tag sensitivity. See that script's module docstring
/// for the exact two rewrites applied and their justification.
const SESSION_BROAD_TTL: &str =
    include_str!("../../../packs/self-monitoring-pack/fixtures/session-real-broad-topic.ttl");

const SMON: &str = "http://seanchatmangpt.github.io/packs/self-monitoring#";
const SESSION_IRI: &str = "http://seanchatmangpt.github.io/packs/self-monitoring/sessions/1f9798ec-f62d-48bb-80a0-e9817fafdb71#session";

/// The real, human-typed turn (seq 1681 in `session-real.ttl`, timestamp
/// 2026-07-14T03:38:00.178Z, confirmed by direct transcript inspection this
/// session) carrying this session's explicit user frustration: "ok the whole
/// point is I keep telling you I want the end to end of all of that. You
/// keep coming up with all kinds of other things to do. I just want you to
/// finish the end to end." This is the escalation this task asks whether the
/// hook would have derived BEFORE.
const FRUSTRATION_TURN_SEQ: i64 = 1681;

/// Mechanically extracts the literal `kh:query """...."""` CONSTRUCT text
/// belonging to `smon:derive_escalation_obligation_action` out of `hook.ttl`
/// -- never re-typed, so this test cannot silently drift from the actual
/// committed hook text. `construct_query_is_verbatim_hook_ttl_substring`
/// below guards this with a redundant `assert!`.
///
/// # Complexity
/// O(n) in `hook_ttl`'s byte length (two forward substring scans).
fn extract_action_construct_query(hook_ttl: &str) -> String {
    let marker = "derive_escalation_obligation_action";
    let after_marker = match hook_ttl.find(marker) {
        Some(i) => &hook_ttl[i..],
        None => panic!("hook.ttl must declare smon:derive_escalation_obligation_action"),
    };
    let query_marker = "kh:query \"\"\"";
    let start = match after_marker.find(query_marker) {
        Some(i) => i + query_marker.len(),
        None => panic!("hook.ttl action must declare kh:query \"\"\"...\"\"\""),
    };
    let rest = &after_marker[start..];
    let end = match rest.find("\"\"\"") {
        Some(i) => i,
        None => panic!("hook.ttl action's kh:query must be closed with \"\"\""),
    };
    rest[..end].to_string()
}

/// Extracts the trailing `turn-<N>` sequence ordinal from a serialized RDF
/// term string (angle-bracket-wrapped or bare).
///
/// # Complexity
/// O(n) in `term`'s length.
fn turn_seq(term: &str) -> Option<i64> {
    let idx = term.rfind("#turn-")?;
    let rest = &term[idx + "#turn-".len()..];
    let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
    digits.parse().ok()
}

/// One derived `smon:EscalationObligation` node's fields, read back from a
/// real `oxigraph::store::Store` CONSTRUCT result -- grouped by oxigraph's
/// own blank-node identity (standard SPARQL 1.1: a template blank node gets
/// a FRESH node per solution row, even when the syntactic label `_:esc` is
/// reused across rows).
#[derive(Debug, Default, Clone)]
struct EscNode {
    priors: Vec<String>,
    repeats: Vec<String>,
    reasons: Vec<String>,
}

/// Loads `ontology.ttl` + `session_ttl` into a REAL `oxigraph::store::Store`
/// and runs hook.ttl's own literal CONSTRUCT query (extracted verbatim) via
/// oxigraph's real SPARQL 1.1 engine.
///
/// # Complexity
/// O(t) to parse+load t bytes of Turtle, plus O(m) for m CONSTRUCT solution
/// rows (this hook's WHERE clause self-joins on `turnKind =
/// GroundingQuestion`/`SurveyResponse`, a tiny, bounded subset of any real
/// session graph regardless of total triple count).
fn run_via_oxigraph(session_ttl: &str) -> BTreeMap<String, EscNode> {
    let store = Store::new().expect("oxigraph Store::new must succeed");
    store
        .load_from_slice(RdfParser::from_format(RdfFormat::Turtle), ONTOLOGY_TTL)
        .expect("ontology.ttl must load into a real oxigraph::store::Store");
    store
        .load_from_slice(RdfParser::from_format(RdfFormat::Turtle), session_ttl)
        .expect("session Turtle must load into a real oxigraph::store::Store");

    let construct_query = extract_action_construct_query(HOOK_TTL);
    let prepared = SparqlEvaluator::new()
        .parse_query(&construct_query)
        .expect("hook.ttl's CONSTRUCT query must parse under oxigraph's real SPARQL 1.1 parser");
    let results = prepared
        .on_store(&store)
        .execute()
        .expect("hook.ttl's CONSTRUCT query must execute under oxigraph's real SPARQL engine");

    let mut nodes: BTreeMap<String, EscNode> = BTreeMap::new();
    match results {
        QueryResults::Graph(triples) => {
            let hp = format!("<{SMON}hasPriorGroundingQuestion>");
            let hg = format!("<{SMON}hasGroundingQuestion>");
            let rs = format!("<{SMON}reason>");
            for triple in triples {
                let triple = triple.expect("oxigraph CONSTRUCT triple must decode");
                let subj = triple.subject.to_string();
                let pred = triple.predicate.to_string();
                let obj = triple.object.to_string();
                let entry = nodes.entry(subj).or_default();
                if pred == hp {
                    entry.priors.push(obj);
                } else if pred == hg {
                    entry.repeats.push(obj);
                } else if pred == rs {
                    entry.reasons.push(obj);
                }
            }
        }
        _ => panic!("hook.ttl's action query must be a CONSTRUCT query"),
    }
    nodes
}

/// Runs a real ad hoc SELECT against a real `oxigraph::store::Store` loaded
/// with `ontology.ttl` + `session_ttl`, listing every `smon:GroundingQuestion`
/// turn's sequence index and `dcterms:subject` topic -- an independent
/// cross-check reporting tool, not part of hook.ttl's own hook definition.
///
/// # Complexity
/// O(t) load + O(g) for g GroundingQuestion turns in the graph.
fn list_grounding_questions(session_ttl: &str) -> Vec<(i64, String)> {
    let store = Store::new().expect("oxigraph Store::new must succeed");
    store
        .load_from_slice(RdfParser::from_format(RdfFormat::Turtle), ONTOLOGY_TTL)
        .expect("ontology.ttl must load");
    store
        .load_from_slice(RdfParser::from_format(RdfFormat::Turtle), session_ttl)
        .expect("session Turtle must load");

    let q = format!(
        "SELECT ?q ?seq ?topic WHERE {{ \
            ?q a <{SMON}Turn> ; <{SMON}turnKind> <{SMON}GroundingQuestion> ; \
               <{SMON}sequenceIndex> ?seq ; <http://purl.org/dc/terms/subject> ?topic . }} \
         ORDER BY ?seq"
    );
    let prepared = SparqlEvaluator::new()
        .parse_query(&q)
        .expect("diagnostic SELECT must parse");
    let results = prepared
        .on_store(&store)
        .execute()
        .expect("diagnostic SELECT must execute");
    let mut out = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let solution: QuerySolution = solution.expect("solution must evaluate");
            let q_term = solution.get("q").expect("row must bind ?q").to_string();
            let topic = solution
                .get("topic")
                .expect("row must bind ?topic")
                .to_string();
            let seq = turn_seq(&q_term).expect("q must be a turn-N IRI");
            out.push((seq, topic));
        }
    }
    out
}

/// Loads `ontology.ttl` + `hook.ttl` + `session_ttl` into a real
/// `praxis_graphlaw::TripleStore` and runs `.materialize()` -- the SAME
/// `kh:`-hook-pack mechanism `self_monitoring_hook_actuation.rs` already
/// proves fires correctly on hand-built fixtures, now pointed at real
/// session data.
///
/// # Complexity
/// O(t) parse+load + O(materialize) -- bounded by this hook's tiny
/// GroundingQuestion/SurveyResponse self-join regardless of total corpus
/// size (see `run_via_oxigraph`'s doc comment).
fn run_via_triplestore(session_ttl: &str) -> TripleStore {
    let mut store = TripleStore::new();
    store
        .load_triples(ONTOLOGY_TTL, Syntax::Turtle)
        .expect("ontology.ttl must load into TripleStore");
    store
        .load_hook_pack(HOOK_TTL)
        .expect("hook.ttl must load as a valid kh: hook pack");
    store
        .load_triples(session_ttl, Syntax::Turtle)
        .expect("session Turtle must load into TripleStore");
    store
        .materialize()
        .expect("materialize() must succeed (no refusing hooks in this pack)");
    store
}

/// Reads back every derived `smon:EscalationObligation` row from a
/// materialized `TripleStore`, exactly mirroring
/// `self_monitoring_hook_actuation.rs`'s own `escalation_obligations` helper.
///
/// # Complexity
/// O(r) for r result rows.
fn triplestore_escalation_rows(store: &TripleStore) -> Vec<(String, String, String)> {
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

/// One fully isolated adversarial pair for ADVERSARIAL CHECK (1): a
/// synthetic `smon:GroundingQuestion` turn whose immediately-following
/// response is `resp_kind` (RunResponse or BlockerResponse, never
/// SurveyResponse), paired with a LATER same-topic GroundingQuestion so the
/// (q1, q2) join candidate structurally exists -- the only thing missing is
/// a qualifying SurveyResponse for q1. Uses a topic literal reserved
/// exclusively for this synthetic pair (never used by any real turn), so a
/// firing (if any) could only have come from this injected data, never from
/// real session facts.
///
/// # Complexity
/// O(1): fixed-size string template.
fn adversarial_pair_turtle(
    topic: &str, gq_seq: i64, resp_seq: i64, resp_kind: &str, gq2_seq: i64,
) -> String {
    format!(
        r#"
@prefix smon: <{SMON}> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<{SESSION_IRI_BASE}#turn-adv-{topic}-gq1> a smon:Turn ;
    dcterms:isPartOf <{SESSION_IRI}> ;
    dcterms:subject "{topic}" ;
    smon:sequenceIndex {gq_seq} ;
    smon:turnKind smon:GroundingQuestion .

<{SESSION_IRI_BASE}#turn-adv-{topic}-resp> a smon:Turn ;
    dcterms:isPartOf <{SESSION_IRI}> ;
    dcterms:subject "{topic}" ;
    smon:sequenceIndex {resp_seq} ;
    smon:turnKind smon:{resp_kind} ;
    smon:immediatelyFollows <{SESSION_IRI_BASE}#turn-adv-{topic}-gq1> .

<{SESSION_IRI_BASE}#turn-adv-{topic}-gq2> a smon:Turn ;
    dcterms:isPartOf <{SESSION_IRI}> ;
    dcterms:subject "{topic}" ;
    smon:sequenceIndex {gq2_seq} ;
    smon:turnKind smon:GroundingQuestion .
"#,
        SESSION_IRI_BASE = "http://seanchatmangpt.github.io/packs/self-monitoring/sessions/1f9798ec-f62d-48bb-80a0-e9817fafdb71",
    )
}

// ============================================================================
// Self-check: the extracted CONSTRUCT text is genuinely verbatim.
// ============================================================================

#[test]
fn construct_query_is_verbatim_hook_ttl_substring() {
    let construct_query = extract_action_construct_query(HOOK_TTL);
    assert!(
        HOOK_TTL.contains(&construct_query),
        "extracted CONSTRUCT text must be a byte-for-byte substring of hook.ttl"
    );
    assert!(construct_query.starts_with("CONSTRUCT {"));
    assert!(construct_query.trim_end().ends_with('}'));
    eprintln!(
        "extracted CONSTRUCT query ({} bytes), verbatim substring of hook.ttl confirmed",
        construct_query.len()
    );
}

// ============================================================================
// PRIMARY RESULT: real session, default (disclosed) heuristic classification.
// ============================================================================

/// Cross-check (1) of 2: real oxigraph::store::Store, hook.ttl's literal
/// CONSTRUCT query, real session-real.ttl facts.
#[test]
fn real_session_default_heuristic_zero_escalations_oxigraph() {
    let grounding_questions = list_grounding_questions(SESSION_REAL_TTL);
    eprintln!("real session GroundingQuestion turns (seq, topic): {grounding_questions:?}");
    assert_eq!(
        grounding_questions,
        vec![
            (1656, "\"status\"".to_string()),
            (1663, "\"cli-swarm\"".to_string()),
            (1703, "\"bribery-e2e-mfact-receipt-workflow\"".to_string()),
        ],
        "the 3 real GroundingQuestion turns and their default-heuristic topics must match this session's known transcript content"
    );

    let nodes = run_via_oxigraph(SESSION_REAL_TTL);
    eprintln!(
        "oxigraph CONSTRUCT over real session-real.ttl: {} EscalationObligation node(s) derived",
        nodes.len()
    );
    assert!(
        nodes.is_empty(),
        "no two of the 3 real GroundingQuestion turns share a dcterms:subject topic under the \
         default keyword heuristic, so hook.ttl's real CONSTRUCT must derive ZERO \
         EscalationObligation nodes on this session's real, unmodified data; got: {nodes:?}"
    );
}

/// Cross-check (2) of 2: real `praxis_graphlaw::TripleStore` (the proven
/// `kh:`-hook-pack mechanism), same real session-real.ttl facts.
#[test]
fn real_session_default_heuristic_zero_escalations_triplestore() {
    let store = run_via_triplestore(SESSION_REAL_TTL);
    let rows = triplestore_escalation_rows(&store);
    eprintln!(
        "TripleStore materialize() over real session-real.ttl: {} EscalationObligation row(s)",
        rows.len()
    );
    assert!(
        rows.is_empty(),
        "TripleStore's own proven kh: hook mechanism must independently agree with oxigraph: \
         zero EscalationObligation rows on this session's real, unmodified data; got: {rows:?}"
    );
}

// ============================================================================
// ADVERSARIAL CHECK (1): GroundingQuestion -> RunResponse/BlockerResponse
// must never fire, verified as a real injected pair inside the real, dense
// 10326-triple session graph (not only in isolated hand-built fixtures).
// ============================================================================

#[test]
fn adversarial_run_and_blocker_responses_never_fire_inside_real_session_graph() {
    let run_pair = adversarial_pair_turtle(
        "adversarial-run-response-topic",
        1721,
        1722,
        "RunResponse",
        1723,
    );
    let blocker_pair = adversarial_pair_turtle(
        "adversarial-blocker-response-topic",
        1724,
        1725,
        "BlockerResponse",
        1726,
    );
    let augmented = format!("{SESSION_REAL_TTL}\n{run_pair}\n{blocker_pair}");

    let nodes = run_via_oxigraph(&augmented);
    eprintln!(
        "oxigraph CONSTRUCT over real session + adversarial Run/BlockerResponse pairs: {} node(s)",
        nodes.len()
    );
    assert!(
        nodes.is_empty(),
        "a GroundingQuestion immediately followed by a RunResponse or BlockerResponse (never a \
         SurveyResponse) must NEVER produce an EscalationObligation, even when injected into the \
         real, dense 10326-triple session graph alongside 1720 real turns; got: {nodes:?}"
    );

    let store = run_via_triplestore(&augmented);
    let rows = triplestore_escalation_rows(&store);
    eprintln!(
        "TripleStore materialize() over real session + adversarial Run/BlockerResponse pairs: {} row(s)",
        rows.len()
    );
    assert!(
        rows.is_empty(),
        "TripleStore must independently agree: zero rows with the adversarial Run/BlockerResponse \
         pairs injected; got: {rows:?}"
    );
}

// ============================================================================
// ADVERSARIAL CHECK (2) + (3): false-negative investigation + topic-tag
// sensitivity, via the DISCLOSED session-real-broad-topic.ttl counterfactual
// (packs/self-monitoring-pack/scripts/broaden_topic_experiment.py). Proves
// the mechanism, GIVEN correctly classified/tagged input, derives the
// escalation correctly and BEFORE this session's own real explicit-
// frustration turn.
// ============================================================================

#[test]
fn broadened_topic_experiment_fires_correctly_and_before_real_frustration_turn_oxigraph() {
    let nodes = run_via_oxigraph(SESSION_BROAD_TTL);
    eprintln!(
        "oxigraph CONSTRUCT over session-real-broad-topic.ttl: {} EscalationObligation node(s)",
        nodes.len()
    );

    // Standard SPARQL 1.1 semantics: oxigraph mints a FRESH blank node per
    // CONSTRUCT solution row, even though the template reuses the syntactic
    // label `_:esc` -- so 3 qualifying rows must yield 3 DISTINCT nodes here,
    // each cleanly carrying exactly one prior + one repeat + one reason.
    assert_eq!(
        nodes.len(),
        3,
        "the broadened counterfactual makes all 3 real GroundingQuestion turns share one topic, \
         so 3 (q1,q2) pairs qualify: (1656,1663) (1656,1703) (1663,1703); oxigraph's correct \
         per-row blank-node semantics must produce 3 distinct EscalationObligation nodes; got: {nodes:?}"
    );

    let mut pairs: Vec<(i64, i64)> = Vec::new();
    for node in nodes.values() {
        assert_eq!(
            node.priors.len(),
            1,
            "each real row must bind exactly 1 prior: {node:?}"
        );
        assert_eq!(
            node.repeats.len(),
            1,
            "each real row must bind exactly 1 repeat: {node:?}"
        );
        assert_eq!(
            node.reasons.len(),
            1,
            "each real row must bind exactly 1 reason: {node:?}"
        );
        assert!(
            node.reasons[0].contains("SurveyResponse") && node.reasons[0].contains("escalate"),
            "reason literal must name the pattern: {node:?}"
        );
        let prior_seq = turn_seq(&node.priors[0]).expect("prior must be a turn-N IRI");
        let repeat_seq = turn_seq(&node.repeats[0]).expect("repeat must be a turn-N IRI");
        pairs.push((prior_seq, repeat_seq));
    }
    pairs.sort_unstable();

    assert_eq!(
        pairs,
        vec![(1656, 1663), (1656, 1703), (1663, 1703)],
        "derived (prior, repeat) turn-sequence pairs must match the 3 real GroundingQuestion \
         turns' known real sequence indices exactly"
    );

    // The FIRST derivable pair's repeat turn (1663, "can it go from the CLI
    // to arrazo to global swarm?", real timestamp 2026-07-14T03:24:04.487Z)
    // occurs strictly BEFORE FRUSTRATION_TURN_SEQ (1681, real timestamp
    // 2026-07-14T03:38:00.178Z, ~14 real minutes later) -- the hook, given
    // correctly classified/tagged input, would have derived the escalation
    // obligation before the user's explicit frustration turn, not after.
    let earliest_repeat = pairs
        .iter()
        .map(|(_, r)| *r)
        .min()
        .expect("pairs must be non-empty");
    assert!(
        earliest_repeat < FRUSTRATION_TURN_SEQ,
        "the earliest derivable EscalationObligation's repeat turn (seq {earliest_repeat}) must \
         precede the real explicit-frustration turn (seq {FRUSTRATION_TURN_SEQ})"
    );
    eprintln!(
        "earliest derivable EscalationObligation repeat turn = seq {earliest_repeat}, \
         real explicit frustration turn = seq {FRUSTRATION_TURN_SEQ} \
         ({} turns later in the real transcript)",
        FRUSTRATION_TURN_SEQ - earliest_repeat
    );
}

/// Same broadened counterfactual, via `TripleStore`'s own proven `kh:`
/// mechanism -- confirming the FIX for the formerly-disclosed (hook.ttl's
/// prior header, "ENGINE LIMITATION FOUND") single-blank-node-label aliasing
/// bug on genuinely multi-row CONSTRUCT firing:
/// `crates/praxis-graphlaw/src/hooks/construct.rs`'s
/// `instantiate_term_pattern` now mints a per-solution-row-scoped blank
/// node (via `mint_or_reuse_construct_blank_node`'s content-addressed
/// interning) instead of echoing the template's raw label verbatim, so 3
/// genuinely qualifying solution rows now produce 3 distinct
/// `EscalationObligation` nodes here too, matching oxigraph's independent,
/// standards-compliant CONSTRUCT evaluator
/// (`broadened_topic_experiment_fires_correctly_and_before_real_frustration_turn_oxigraph`)
/// pair-for-pair rather than aliasing onto one shared node.
#[test]
fn broadened_topic_experiment_via_triplestore_matches_oxigraph_after_blank_node_fix() {
    let store = run_via_triplestore(SESSION_BROAD_TTL);
    let rows = triplestore_escalation_rows(&store);
    eprintln!(
        "TripleStore materialize() over session-real-broad-topic.ttl: {} EscalationObligation row(s): {rows:?}",
        rows.len()
    );

    // Fixed engine behavior, matching oxigraph's 3 clean pairs: each of the
    // 3 genuinely-qualifying solution rows now gets its own fresh blank
    // node (per-solution CONSTRUCT scoping, SPARQL 1.1 sec 16.2), so
    // `?esc a Obligation ; hasPrior ?prior ; hasRepeat ?repeat` yields 3
    // rows with 3 distinct `esc` identities, not a cross product aliased
    // onto one shared node.
    assert_eq!(
        rows.len(),
        3,
        "TripleStore must now derive exactly 3 EscalationObligation rows (one per genuinely \
         qualifying solution), matching oxigraph's correct output; got: {rows:?}"
    );
    let distinct_esc: std::collections::BTreeSet<&str> =
        rows.iter().map(|(esc, _, _)| esc.as_str()).collect();
    assert_eq!(
        distinct_esc.len(),
        3,
        "each of the 3 rows must carry its OWN distinct blank-node identity (the aliasing bug \
         is fixed); got distinct esc ids: {distinct_esc:?}"
    );

    let mut pairs: Vec<(i64, i64)> = rows
        .iter()
        .map(|(_, prior, repeat)| {
            (
                turn_seq(prior).expect("prior must be a turn-N IRI"),
                turn_seq(repeat).expect("repeat must be a turn-N IRI"),
            )
        })
        .collect();
    pairs.sort_unstable();
    assert_eq!(
        pairs,
        vec![(1656, 1663), (1656, 1703), (1663, 1703)],
        "TripleStore's derived (prior, repeat) pairs must match oxigraph's exactly; got: {rows:?}"
    );
}
