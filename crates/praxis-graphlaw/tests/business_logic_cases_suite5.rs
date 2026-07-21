use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// SUITE 5: SLA ESCALATION (Threshold Matching Asserts Count, Receipts Only Cross-Threshold)
// =========================================================================

/// Positive: SLA threshold crossed triggers escalation
#[test]
fn test_suite5_sla_escalation_threshold_crossed() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:sla_escalation a kh:Hook ;
            kh:name "sla_threshold_escalation" ;
            kh:kind "count" ;
            kh:var "http://example.org/pending_ticket" ;
            kh:op ">=" ;
            kh:k 5 ;
            kh:effect "emit-delta" ;
            kh:action ex:escalate_action .

        ex:escalate_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { <http://example.org/SLA> <http://example.org/alert> 'critical' } WHERE { ?t <http://example.org/pending_ticket> ?any }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();

    // Load 4 tickets (below threshold)
    store
        .load_triples(
            "ex:Ticket <http://example.org/pending_ticket> 1 , 2 , 3 , 4 .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();
    assert!(
        store.get_hook_receipts().is_empty(),
        "Below threshold should not fire"
    );

    // Add 5th ticket (at threshold)
    store
        .load_triples(
            "ex:Ticket <http://example.org/pending_ticket> 5 .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();

    // Receipt should be emitted now
    assert_eq!(store.get_hook_receipts().len(), 1);
}

/// Negative: Below SLA threshold, no receipt emitted
#[test]
fn test_suite5_sla_escalation_below_threshold() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:sla_below a kh:Hook ;
            kh:name "sla_below_threshold" ;
            kh:kind "count" ;
            kh:var "http://example.org/error" ;
            kh:op ">=" ;
            kh:k 10 ;
            kh:effect "emit-delta" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();

    // Load only 5 errors (below 10)
    store
        .load_triples(
            "ex:System <http://example.org/error> 1 , 2 , 3 , 4 , 5 .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();

    // No receipt should be generated
    assert!(store.get_hook_receipts().is_empty());
}

/// Positive: SLA threshold count operator ">=" matches exactly at k
#[test]
fn test_suite5_sla_escalation_exact_threshold() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:sla_exact a kh:Hook ;
            kh:name "sla_exact_match" ;
            kh:kind "count" ;
            kh:var "http://example.org/request" ;
            kh:op "=" ;
            kh:k 10 ;
            kh:effect "emit-delta" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:API <http://example.org/request> 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // Should fire exactly at count 10
    assert_eq!(store.get_hook_receipts().len(), 1);
}

/// Negative: Malformed SLA hook with wrong operator type (string instead of enum)
#[test]
fn test_suite5_sla_escalation_malformed_operator() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:bad_op a kh:Hook ;
            kh:name "bad_operator" ;
            kh:kind "count" ;
            kh:var "http://example.org/item" ;
            kh:op "invalid_op" ;
            kh:k 5 ;
            kh:effect "emit-delta" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err(), "invalid kh:op operator must be rejected");
}

/// Negative: SLA threshold hook missing kh:k field
#[test]
fn test_suite5_sla_escalation_missing_k() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:missing_k a kh:Hook ;
            kh:name "missing_k_threshold" ;
            kh:kind "count" ;
            kh:var "http://example.org/item" ;
            kh:op ">=" ;
            kh:effect "emit-delta" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err(), "Missing kh:k should fail SHACL gating");
}
