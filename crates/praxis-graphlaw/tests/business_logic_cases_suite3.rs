use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// SUITE 3: STATE-MACHINE TRANSITION (Single + 1000 Cases, Legal/Illegal)
// =========================================================================

/// Positive: Legal state transition (Draft -> UnderReview) fires and projects
#[test]
fn test_suite3_state_machine_legal_transition_single() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:transition_hook a kh:Hook ;
            kh:name "legal_transition" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?doc WHERE { ?doc <http://example.org/state> 'Draft' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:transition_action .

        ex:transition_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?doc <http://example.org/state> 'UnderReview' } WHERE { ?doc <http://example.org/state> 'Draft' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Doc1 <http://example.org/state> 'Draft' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert_eq!(receipts.len(), 1);
}

/// Negative: Illegal state transition (Draft -> Approved, skipping UnderReview) is refused
#[test]
fn test_suite3_state_machine_illegal_transition_single() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:guard_hook a kh:Hook ;
            kh:name "illegal_transition_guard" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?doc <http://example.org/state> 'Draft' ; <http://example.org/next_state> 'Approved' }" ;
            kh:effect "refuse" ;
            kh:reason "Gated: Cannot skip UnderReview state" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store.load_triples("ex:Doc2 <http://example.org/state> 'Draft' ; <http://example.org/next_state> 'Approved' .", Syntax::Turtle).unwrap();

    let res = store.materialize();
    assert!(res.is_err());
}

/// Positive: Legal transitions applied to 1000 documents
#[test]
fn test_suite3_state_machine_1000_legal() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:bulk_transition a kh:Hook ;
            kh:name "bulk_legal_transitions" ;
            kh:kind "delta" ;
            kh:var "http://example.org/state" ;
            kh:on "assert" ;
            kh:effect "emit-delta" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();

    // Load 1000 documents
    let mut docs = String::new();
    for i in 0..1000 {
        docs.push_str(&format!(
            "ex:Doc{} <http://example.org/state> 'Draft' .\n",
            i
        ));
    }
    store.load_triples(&docs, Syntax::Turtle).unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert!(!receipts.is_empty());
}

/// Negative: Malformed hook with wrong type for kh:kind (should be string/IRI, not number)
#[test]
fn test_suite3_state_machine_malformed_wrong_type() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:bad_kind a kh:Hook ;
            kh:name "wrong_type_hook" ;
            kh:kind 123 ;
            kh:var "v" ;
            kh:effect "emit-delta" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Wrong type for kh:kind should fail SHACL gating"
    );
}

/// Negative: Circular state machine (state A -> B -> A) is caught during execution
#[test]
fn test_suite3_state_machine_circular_infinite_loop() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:cycle_a a kh:Hook ;
            kh:name "cycle_to_b" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?doc WHERE { ?doc <http://example.org/state> 'A' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_a ;
            kh:priority 1 .

        ex:action_a a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?doc <http://example.org/state> 'B' } WHERE { ?doc <http://example.org/state> 'A' }" .

        ex:cycle_b a kh:Hook ;
            kh:name "cycle_to_a" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?doc WHERE { ?doc <http://example.org/state> 'B' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_b ;
            kh:priority 2 .

        ex:action_b a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?doc <http://example.org/state> 'A' } WHERE { ?doc <http://example.org/state> 'B' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Doc3 <http://example.org/state> 'A' .", Syntax::Turtle)
        .unwrap();

    let inferred = store.materialize().unwrap();
    // Should terminate gracefully with recursion limit or detect cycle
    assert!(
        inferred.len() < 100,
        "Circular transitions should be bounded"
    );
}
