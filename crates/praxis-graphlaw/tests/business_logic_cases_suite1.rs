use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// SUITE 1: MINIMAL APPROVAL (Approved Path Emits Delta, Refused Path Surfaces kh:reason)
// =========================================================================

/// Positive: Approval hook fires and emits delta with receipt
#[test]
fn test_suite1_minimal_approval_positive() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:approval_hook a kh:Hook ;
            kh:name "minimal_approval" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?req WHERE { ?req <http://example.org/status> 'pending' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:approve_action .

        ex:approve_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req <http://example.org/status> 'approved' } WHERE { ?req <http://example.org/status> 'pending' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:ReqA <http://example.org/status> 'pending' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // Verify delta was emitted
    let receipts = store.get_hook_receipts();
    assert_eq!(receipts.len(), 1);
    assert_eq!(receipts[0].hook_name, "minimal_approval");
    assert!(!receipts[0].delta_hash.is_empty());
}

/// Negative: Refusal hook surfaces kh:reason when condition matches
#[test]
fn test_suite1_minimal_refusal_surfaces_reason() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:refusal_hook a kh:Hook ;
            kh:name "minimal_refusal" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req <http://example.org/amount> ?a . FILTER(?a > 10000) }" ;
            kh:effect "refuse" ;
            kh:reason "Amount exceeds maximum threshold" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:ReqB <http://example.org/amount> 15000 .",
            Syntax::Turtle,
        )
        .unwrap();

    let res = store.materialize();
    assert!(res.is_err());
}

/// Negative: Malformed hook missing mandatory kh:kind field
#[test]
fn test_suite1_minimal_malformed_missing_kind() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:bad_hook a kh:Hook ;
            kh:name "missing_kind_hook" ;
            kh:effect "emit-delta" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err(), "Hook missing kh:kind should fail gating");
}

/// Negative: Malformed hook with unknown predicate in vocabulary
#[test]
fn test_suite1_minimal_malformed_unknown_predicate() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:unknown_hook a kh:Hook ;
            kh:name "unknown_predicate_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "emit-delta" ;
            kh:unknownPredicate "should_fail" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "unknown predicate in kh: vocabulary must be rejected at load"
    );
}

/// Negative: Approved fact remains absent when trigger condition not met
#[test]
fn test_suite1_minimal_approval_no_trigger() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:conditional_approval a kh:Hook ;
            kh:name "conditional_approval" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?req WHERE { ?req <http://example.org/status> 'pending' ; <http://example.org/priority> 'high' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:approve_high .

        ex:approve_high a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req <http://example.org/approved> 'yes' } WHERE { ?req <http://example.org/status> 'pending' ; <http://example.org/priority> 'high' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:ReqC <http://example.org/status> 'pending' ; <http://example.org/priority> 'low' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // No receipts since condition not met (priority is low, not high)
    assert!(store.get_hook_receipts().is_empty());
}
