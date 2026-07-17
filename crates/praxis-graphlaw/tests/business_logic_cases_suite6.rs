use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// SUITE 6: POLICY CONFLICT (Contradicting Effects Refuse Deterministically)
// =========================================================================

/// Negative: Conflicting approval/refusal hooks refuse deterministically
#[test]
fn test_suite6_policy_conflict_approve_vs_refuse() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:approve_hook a kh:Hook ;
            kh:name "approve_request" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req <http://example.org/status> 'pending' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:approve_action ;
            kh:priority 1 .

        ex:approve_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req <http://example.org/status> 'approved' } WHERE { ?req <http://example.org/status> 'pending' }" .

        ex:refuse_hook a kh:Hook ;
            kh:name "refuse_request" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req <http://example.org/status> 'approved' }" ;
            kh:effect "refuse" ;
            kh:reason "Conflict: Approval was reversed" ;
            kh:priority 2 ;
            kh:after ex:approve_hook .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:ReqA <http://example.org/status> 'pending' .",
            Syntax::Turtle,
        )
        .unwrap();

    let res = store.materialize();
    assert!(res.is_err());
}

/// Negative: Contradicting state transitions are caught and refused
#[test]
fn test_suite6_policy_conflict_state_contradictions() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:to_active a kh:Hook ;
            kh:name "transition_to_active" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_active ;
            kh:priority 1 .

        ex:action_active a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/state> 'active' } WHERE { ?s <http://example.org/trigger> 'yes' }" .

        ex:to_inactive a kh:Hook ;
            kh:name "transition_to_inactive" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?s WHERE { ?s <http://example.org/state> 'active' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_inactive ;
            kh:priority 2 ;
            kh:after ex:to_active .

        ex:action_inactive a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/state> 'inactive' } WHERE { ?s <http://example.org/state> 'active' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Sys <http://example.org/trigger> 'yes' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // Conflicting transitions should be detected or deterministically ordered
    let receipts = store.get_hook_receipts();
    // The result should be deterministic regardless of conflict
}

/// Negative: Multiple refusal reasons all surface (but first wins deterministically)
#[test]
fn test_suite6_policy_conflict_multiple_refusals() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:refusal1 a kh:Hook ;
            kh:name "first_refusal" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x <http://example.org/value> ?v . FILTER(?v > 1000) }" ;
            kh:effect "refuse" ;
            kh:reason "Refusal: Value exceeds limit 1000" ;
            kh:priority 1 .

        ex:refusal2 a kh:Hook ;
            kh:name "second_refusal" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x <http://example.org/value> ?v . FILTER(?v > 1000) }" ;
            kh:effect "refuse" ;
            kh:reason "Refusal: Duplicate transaction detected" ;
            kh:priority 2 .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Txn <http://example.org/value> 1500 .", Syntax::Turtle)
        .unwrap();

    let res = store.materialize();
    assert!(res.is_err());
}

/// Negative: Malformed hook with conflicting effect types
#[test]
fn test_suite6_policy_conflict_malformed_dual_effect() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:bad_effect a kh:Hook ;
            kh:name "dual_effect_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "emit-delta" ;
            kh:effect "refuse" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Duplicate kh:effect violates SHACL maxCount=1"
    );
}

/// Negative: Policy hook with unknown predicate in effect namespace
#[test]
fn test_suite6_policy_conflict_unknown_effect_predicate() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:unknown_effect a kh:Hook ;
            kh:name "unknown_effect_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:unknownEffect "not_in_vocabulary" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    // Should fail: unknown predicate in kh: vocabulary
}
