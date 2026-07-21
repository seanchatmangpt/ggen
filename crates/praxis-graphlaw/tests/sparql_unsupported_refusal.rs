//! The unsupported-construct audit of this crate's custom SPARQL executor,
//! committed as permanent law (retrofit/ggen-self-g15).
//!
//! Origin: three constructs were discovered ad hoc to be SILENTLY mishandled
//! (ORDER BY — documented in packs/self-monitoring-pack/queries/
//! open_obligations.rq's header; VALUES — refused in g13; STRSTARTS — found
//! in g14), suggesting a systematic class. A 17-case probe battery against
//! known data confirmed it: beyond those three, LIMIT/OFFSET, FILTER
//! arithmetic, REGEX, EXISTS all silently return ZERO rows; DISTINCT returns
//! DUPLICATE rows; and worst, FILTER NOT EXISTS is ignored outright,
//! returning the UNFILTERED set — fail-open.
//!
//! `plan_query_or_refuse` now refuses every probed-broken construct loudly
//! (`find_unsupported_construct`/`find_unsupported_expression`). This file
//! pins BOTH halves: each broken construct is refused with a message naming
//! it, and each probed-WORKING construct still returns the correct rows (so
//! the refusal net cannot silently over-block).

use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const TTL: &str = r#"
@prefix ex: <urn:ex:> .
ex:a ex:p ex:b . ex:a ex:p ex:c . ex:a ex:n 5 .
ex:b ex:p ex:c . ex:d ex:q ex:e .
"#;

fn store() -> TripleStore {
    let mut s = TripleStore::new();
    s.load_triples(TTL, Syntax::Turtle)
        .expect("probe data loads");
    s
}

/// Every construct probed as broken must be a loud refusal naming itself.
#[test]
fn probed_broken_constructs_are_refused_not_silently_wrong() {
    let s = store();
    let broken: &[(&str, &str)] = &[
        (
            "SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o } ORDER BY ?o",
            "ORDER BY",
        ),
        (
            "SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o } LIMIT 1",
            "LIMIT/OFFSET",
        ),
        ("SELECT DISTINCT ?s WHERE { ?s <urn:ex:p> ?o }", "DISTINCT"),
        (
            "SELECT ?s WHERE { ?s <urn:ex:n> ?n . FILTER(?n >= 2 + 2) }",
            "arithmetic",
        ),
        (
            "SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o . FILTER(STRSTARTS(STR(?o), 'urn:')) }",
            "function other than STR",
        ),
        (
            "SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o . FILTER(REGEX(STR(?o), 'ex')) }",
            "function other than STR",
        ),
        (
            "SELECT ?s WHERE { ?s <urn:ex:p> ?o . FILTER NOT EXISTS { ?s <urn:ex:n> ?n } }",
            "EXISTS",
        ),
        (
            "SELECT ?s WHERE { ?s <urn:ex:p> ?o . FILTER EXISTS { ?s <urn:ex:n> ?n } }",
            "EXISTS",
        ),
        (
            "SELECT ?p WHERE { VALUES ?p { <urn:ex:p> } <urn:ex:a> ?p ?o }",
            "VALUES",
        ),
    ];
    for (query, expected_name) in broken {
        let err = s
            .query(query)
            .expect_err(&format!("must refuse, not answer: {query}"));
        assert!(
            err.contains(expected_name),
            "refusal must name the construct ({expected_name}): {err}"
        );
    }
}

/// Every construct probed as WORKING must still return the correct rows —
/// the refusal net must not over-block.
#[test]
fn probed_working_constructs_still_answer_correctly() {
    let s = store();
    let working: &[(&str, usize)] = &[
        ("SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o }", 2),
        (
            "SELECT ?s ?e WHERE { ?s <urn:ex:p> ?o . OPTIONAL { ?s <urn:ex:q> ?e } }",
            3,
        ),
        (
            "SELECT ?s WHERE { ?s <urn:ex:p> ?o . MINUS { ?s <urn:ex:n> ?n } }",
            1,
        ),
        (
            "SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o . FILTER(?o != <urn:ex:b>) }",
            1,
        ),
        (
            "SELECT ?o WHERE { <urn:ex:a> <urn:ex:p> ?o . FILTER(STR(?o) != 'x') }",
            2,
        ),
        (
            "SELECT ?s WHERE { ?s <urn:ex:p> ?o . OPTIONAL { ?s <urn:ex:q> ?e } FILTER(!BOUND(?e)) }",
            3,
        ),
        (
            "SELECT ?s (COUNT(?o) AS ?c) WHERE { ?s <urn:ex:p> ?o } GROUP BY ?s",
            2,
        ),
        (
            "SELECT ?s WHERE { { SELECT ?s WHERE { ?s <urn:ex:p> ?o } } }",
            3,
        ),
        (
            "SELECT ?s WHERE { { ?s <urn:ex:n> ?n } UNION { ?s <urn:ex:q> ?e } }",
            2,
        ),
    ];
    for (query, expected_rows) in working {
        let rows = s
            .query(query)
            .unwrap_or_else(|e| panic!("must still answer (not over-blocked): {query}: {e}"));
        assert_eq!(
            rows.len(),
            *expected_rows,
            "row count must stay correct for: {query}"
        );
    }
}
