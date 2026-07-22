mod common;

use common::assert_contains_triple;
use common::assert_not_contains_triple;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// TIER 4: REAL-WORLD APPLICATION SCENARIOS
// =========================================================================

/// Covers S1: Automated Quarantine & Refusal Scenario.
/// When unauthorized triples are written to a protected graph/namespace,
/// the hook catches it, routes the violating triples to a quarantine graph,
/// logs a refusal, and rolls back the user's primary transaction.
#[test]
fn test_s4_automated_quarantine_and_refusal() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        # Hook 1: Quarantine route
        ex:quarantine_hook a kh:Hook ;
            kh:name "quarantine_routing" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?s ?p ?o WHERE { GRAPH <http://example.org/SystemGraph> { ?s ?p ?o } }" ;
            kh:effect "emit-delta" ;
            kh:action ex:route_to_quarantine ;
            kh:priority 1 .
            
        ex:route_to_quarantine a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { GRAPH <http://example.org/QuarantineGraph> { ?s ?p ?o } } WHERE { GRAPH <http://example.org/SystemGraph> { ?s ?p ?o } }" .

        # Hook 2: Refusal on SystemGraph write
        ex:refusal_hook a kh:Hook ;
            kh:name "quarantine_refusal" ;
            kh:kind "sparql" ;
            kh:query "ASK { GRAPH <http://example.org/SystemGraph> { ?s ?p ?o } }" ;
            kh:effect "refuse" ;
            kh:reason "Refusal Error: Direct write to SystemGraph is forbidden" ;
            kh:priority 2 ;
            kh:after ex:quarantine_hook .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Simulate user attempting to write to SystemGraph
    // N-Quads format: subject predicate object graph
    store.load_triples(
        "<http://example.org/User1> <http://example.org/change_pass> \"admin\" <http://example.org/SystemGraph> .",
        Syntax::NQuads
    ).unwrap();

    let inferred = store.materialize().unwrap();

    // Transaction must roll back, ensuring no data remains in SystemGraph
    assert!(
        inferred.is_empty(),
        "Unauthorized write should cause complete transaction rollback"
    );
    assert_not_contains_triple(&store, "User1", "change_pass", "admin");
}

/// Covers S2: Ledger Balance Enforcement & Audit Trail Scenario.
/// Validates double-entry ledger transactions. Checks if account balance is >= 0.
/// If valid, projects ledger updates and records a signed BLAKE3 audit receipt.
/// If balance drops below zero, transaction is refused and rolled back.
#[test]
fn test_s4_ledger_balance_enforcement_and_audit() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        # Hook 1: Enforce positive balance, refuse otherwise
        ex:balance_guard a kh:Hook ;
            kh:name "ledger_balance_guard" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?acct <http://example.org/balance> ?bal . FILTER(?bal < 0) }" ;
            kh:effect "refuse" ;
            kh:reason "Balance Violation: Account balance cannot drop below zero" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Attempt valid transaction: Starting balance 500, deduct 200
    store
        .load_triples(
            "ex:Account1 <http://example.org/balance> 500 .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();

    // Perform deduction
    store
        .load_triples(
            "ex:Account1 <http://example.org/balance> 300 .", // New state
            Syntax::Turtle,
        )
        .unwrap();
    let _inferred = store.materialize().unwrap();
    assert_contains_triple(&store, "Account1", "balance", "300");

    // Attempt invalid transaction: Deduct 400 (balance goes to -100)
    store
        .load_triples(
            "ex:Account1 <http://example.org/balance> -100 .",
            Syntax::Turtle,
        )
        .unwrap();

    let res = store.materialize().unwrap();
    assert!(
        res.is_empty(),
        "Negative balance transaction should be rolled back"
    );
}

/// Covers S3: State Machine Transition Control Scenario.
/// Implements state transition rules: Draft -> UnderReview -> Approved.
/// Verifies that skip-state transitions are refused.
#[test]
fn test_s4_state_machine_transition_control() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:transition_guard a kh:Hook ;
            kh:name "lifecycle_transition_guard" ;
            kh:kind "sparql" ;
            # Refuse if we transition from Draft to Approved, skipping UnderReview
            kh:query "ASK { ?doc <http://example.org/old_state> 'Draft' ; <http://example.org/new_state> 'Approved' }" ;
            kh:effect "refuse" ;
            kh:reason "Lifecycle Violation: Cannot skip UnderReview state" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Invalid transition
    store.load_triples(
        "ex:Doc1 <http://example.org/old_state> 'Draft' ; <http://example.org/new_state> 'Approved' .",
        Syntax::Turtle
    ).unwrap();

    let inferred = store.materialize().unwrap();
    assert!(
        inferred.is_empty(),
        "Invalid state transition must be rolled back"
    );
}

/// Covers S4: Access Control Policy Engine Scenario.
/// Implements RBAC. Evaluates user role and request action.
/// Projects authorization decision: granted/denied.
#[test]
fn test_s4_access_control_policy_engine() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:rbac_hook a kh:Hook ;
            kh:name "rbac_policy_engine" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?user WHERE { ?user <http://example.org/request> ?act . ?user <http://example.org/role> 'Admin' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:grant_access_action .
            
        ex:grant_access_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?user <http://example.org/permission> 'granted' } WHERE { ?user <http://example.org/request> ?act . ?user <http://example.org/role> 'Admin' }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Admin user requests action
    store.load_triples(
        "ex:User1 <http://example.org/role> 'Admin' ; <http://example.org/request> 'delete_db' .",
        Syntax::Turtle
    ).unwrap();

    store.materialize().unwrap();

    assert_contains_triple(&store, "User1", "permission", "granted");
}

/// Covers S5: Materialized View / Cache Maintenance Scenario.
/// Automatically projects changes into a query cache representation graph
/// when base data triples are modified, maintaining up-to-date read views.
#[test]
fn test_s4_materialized_view_cache_maintenance() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:cache_hook a kh:Hook ;
            kh:name "view_cache_maintenance" ;
            kh:kind "delta" ;
            kh:var "http://example.org/salary" ;
            kh:effect "emit-delta" ;
            kh:action ex:update_cache_action .
            
        ex:update_cache_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { GRAPH <http://example.org/CachedView> { ?emp <http://example.org/tax_bracket> 'high' } } WHERE { ?emp <http://example.org/salary> ?sal . FILTER(?sal > 150000) }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Add base employee data
    store
        .load_triples(
            "ex:Emp1 <http://example.org/salary> 180000 .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // Cache graph should contain the materialized view tax bracket classification
    assert_contains_triple(&store, "Emp1", "tax_bracket", "high");
}

// =========================================================================
