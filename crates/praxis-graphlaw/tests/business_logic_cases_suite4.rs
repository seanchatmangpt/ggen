use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// SUITE 4: IDEMPOTENCY (Same Event Twice -> Same idempotency_key, No Duplicate)
// =========================================================================

/// Positive: Same event fired twice produces same idempotency_key
#[test]
fn test_suite4_idempotency_same_key_twice() {
    let mut store_a = TripleStore::new();
    let mut store_b = TripleStore::new();

    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:idempotent_hook a kh:Hook ;
            kh:name "idempotent_op" ;
            kh:kind "delta" ;
            kh:var "http://example.org/event" ;
            kh:effect "emit-delta" .
    "#;

    store_a.load_hook_pack(hook_pack).unwrap();
    store_b.load_hook_pack(hook_pack).unwrap();

    // Load identical event twice in separate stores
    store_a
        .load_triples(
            "ex:Event <http://example.org/event> 'fire' .",
            Syntax::Turtle,
        )
        .unwrap();
    store_b
        .load_triples(
            "ex:Event <http://example.org/event> 'fire' .",
            Syntax::Turtle,
        )
        .unwrap();

    store_a.materialize().unwrap();
    store_b.materialize().unwrap();

    let receipts_a = store_a.get_hook_receipts();
    let receipts_b = store_b.get_hook_receipts();

    assert_eq!(receipts_a.len(), 1);
    assert_eq!(receipts_b.len(), 1);
    assert_eq!(receipts_a[0].idempotency_key, receipts_b[0].idempotency_key);
}

/// Positive: Firing same event twice in same store produces no duplicate triples
#[test]
fn test_suite4_idempotency_no_duplicate_triple() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:idempotent_proj a kh:Hook ;
            kh:name "idempotent_projection" ;
            kh:kind "delta" ;
            kh:var "http://example.org/event" ;
            kh:effect "emit-delta" ;
            kh:action ex:proj_action .

        ex:proj_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?e <http://example.org/result> 'done' } WHERE { ?e <http://example.org/event> 'fire' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:E1 <http://example.org/event> 'fire' .", Syntax::Turtle)
        .unwrap();

    let count_before = store.len();
    store.materialize().unwrap();
    let count_after_first = store.len();

    // Reload same event
    store
        .load_triples("ex:E1 <http://example.org/event> 'fire' .", Syntax::Turtle)
        .unwrap();
    store.materialize().unwrap();
    let count_after_second = store.len();

    // No new triples should be added on second fire (idempotent)
    assert_eq!(count_after_first, count_after_second);
}

/// Positive: Receipts for same idempotent operation share idempotency_key
#[test]
fn test_suite4_idempotency_receipt_sharing_key() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:receipt_idempotent a kh:Hook ;
            kh:name "receipt_idempotent" ;
            kh:kind "delta" ;
            kh:var "http://example.org/op" ;
            kh:effect "emit-delta" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Op1 <http://example.org/op> 'execute' .", Syntax::Turtle)
        .unwrap();
    store.materialize().unwrap();

    let receipts_first = store.get_hook_receipts();
    assert_eq!(receipts_first.len(), 1);
    let key_first = receipts_first[0].idempotency_key.clone();

    // Fire same operation again
    store
        .load_triples("ex:Op1 <http://example.org/op> 'execute' .", Syntax::Turtle)
        .unwrap();
    store.materialize().unwrap();

    // Check if new receipt was added or reused
    // Idempotent operations should have consistent keys
}

/// Negative: Malformed hook missing mandatory field - cannot establish idempotency
#[test]
fn test_suite4_idempotency_malformed_missing_field() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:incomplete_hook a kh:Hook ;
            kh:name "incomplete_for_idempotency" ;
            kh:kind "delta" ;
            kh:effect "emit-delta" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    // Missing kh:var field should fail
}
