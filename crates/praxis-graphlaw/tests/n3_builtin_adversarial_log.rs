//! Adversarial edge-case testing for logical builtins (type-mismatched
//! equalTo/notEqualTo, bound on genuinely unbound variables).

use praxis_graphlaw::TripleStore;

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples
        .iter()
        .map(praxis_graphlaw::TripleStore::decode_triple)
        .collect()
}

fn materialize(data: &str) -> Vec<String> {
    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    decode_all(&inferred)
}

// ===========================================================================
// log: type-mismatched equalTo/notEqualTo (literal vs IRI vs blank node),
// and bound on a genuinely unbound variable.
// ===========================================================================

#[test]
fn log_equal_to_literal_vs_iri_are_not_equal() {
    // The literal "42" must NOT log:equalTo the numerically-unrelated IRI
    // :fortyTwo -- they are different RDF term kinds entirely; equalTo must
    // not coerce an IRI into a number or otherwise conflate the two.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a \"42\" .\n\
                :s :b :fortyTwo .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Equal"))
            && decoded.iter().any(|d| d.contains("/NotEqual")),
        "a string literal and an IRI must never be log:equalTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_iri_vs_blank_node_are_not_equal() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a :namedThing .\n\
                :s :b [ ] .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Equal"))
            && decoded.iter().any(|d| d.contains("/NotEqual")),
        "a named IRI and a fresh blank node must never be log:equalTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_literal_vs_blank_node_are_not_equal() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a \"something\" .\n\
                :s :b [ ] .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Equal"))
            && decoded.iter().any(|d| d.contains("/NotEqual")),
        "a literal and a fresh blank node must never be log:equalTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_same_iri_is_equal() {
    // Sanity/control case: the SAME IRI referenced twice via two different
    // predicates must be log:equalTo (and never notEqualTo) -- confirms
    // the mismatched-type tests above are actually exercising a type
    // difference, not a general "equalTo never fires" bug.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a :sameThing .\n\
                :s :b :sameThing .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/Equal"))
            && !decoded.iter().any(|d| d.contains("/NotEqual")),
        "the same IRI referenced twice must be log:equalTo and never notEqualTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_numeric_literals_different_lexical_same_value_are_equal() {
    // 042 and 42 differ lexically but are the same number -- log:equalTo's
    // numeric fallback (`numeric_value(s) == numeric_value(o)`) must treat
    // them as equal, unlike a plain string/term comparison.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a 42.0 .\n\
                :s :b 42 .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/Equal")),
        "42.0 and 42 are the same numeric value and must be log:equalTo, got: {:?}",
        decoded
    );
}

// log:bound on a genuinely unbound variable: a variable that never appears
// in any ground fact must fail to satisfy log:bound, so the rule using it
// must never fire.
#[test]
fn log_bound_on_genuinely_unbound_variable_never_fires() {
    // ?neverBound is not connected to any ground fact via the rule body
    // pattern (no triple in the data ties it to anything), so it should
    // remain unbound and log:bound must reject it.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s a :Thing .\n\
                { ?s a :Thing . ?neverBound log:bound ?neverBound } => { ?s a :ShouldNotFire }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/ShouldNotFire")),
        "log:bound on a variable with no binding anywhere in the rule must never fire, got: {:?}",
        decoded
    );
}

#[test]
fn log_bound_on_a_bound_variable_fires() {
    // Control case: the same shape, but ?s IS bound by the preceding
    // pattern -- log:bound must let the rule fire.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s a :Thing .\n\
                { ?s a :Thing . ?s log:bound ?s } => { ?s a :ShouldFire }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/ShouldFire")),
        "log:bound on an already-bound variable must let the rule fire, got: {:?}",
        decoded
    );
}
