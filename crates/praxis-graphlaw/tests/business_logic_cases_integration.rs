use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// Integration: Comprehensive Multi-Suite Scenarios
// =========================================================================

/// Integration: Complex approval workflow combining suites 1, 2, 4
#[test]
fn test_integration_complex_approval_workflow() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:validate_hook a kh:Hook ;
            kh:name "validation_phase" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?req WHERE { ?req <http://example.org/phase> 'validation' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:validate_action ;
            kh:priority 1 .

        ex:validate_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req <http://example.org/phase> 'approved' } WHERE { ?req <http://example.org/phase> 'validation' }" .

        ex:audit_hook a kh:Hook ;
            kh:name "audit_phase" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?req WHERE { ?req <http://example.org/phase> 'approved' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:audit_action ;
            kh:priority 2 ;
            kh:after ex:validate_hook .

        ex:audit_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req <http://example.org/phase> 'complete' } WHERE { ?req <http://example.org/phase> 'approved' }" .
    "#;

    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Req1 <http://example.org/phase> 'validation' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // Both phases should fire in order
    let receipts = store.get_hook_receipts();
    assert!(receipts.len() >= 2);
}
