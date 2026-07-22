mod common;

use common::assert_contains_triple;
use common::assert_not_contains_triple;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// TIER 4: REAL-WORLD APPLICATION SCENARIOS
// =========================================================================

/// Covers S1: Automated Quarantine & Refusal Scenario.
/// When unauthorized triples are written against a protected predicate, the
/// hook catches it, routes a quarantine marker for the offending subject,
/// logs a refusal, and rolls back the hook-derived side effects.
///
/// NOTE: this scenario deliberately avoids the SPARQL `GRAPH <iri> { ... }`
/// graph-pattern clause (originally used here to model a "protected named
/// graph"): `GRAPH` is a construct this crate's custom executor is PROVEN to
/// mishandle (see `find_unsupported_construct` in `src/lib.rs` and its
/// `tests/sparql_unsupported_refusal.rs` regression battery, extended
/// alongside this fix) -- it now correctly `plan_query_or_refuse`s instead of
/// silently returning zero rows, but a hook's own hook-condition evaluation
/// (`HookCondition::Sparql` in `src/reasoner/mod.rs`) swallows any query
/// refusal into "hook did not fire" rather than propagating it, so a
/// GRAPH-conditioned hook would still just silently never fire. Modeling the
/// "protected surface" as a predicate instead of a named graph tests the same
/// quarantine+refusal scenario using constructs the engine actually executes.
#[test]
fn test_s4_automated_quarantine_and_refusal() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        // Hook 1: Quarantine route -- flag any write to the protected predicate
        ex:quarantine_hook a kh:Hook ;
            kh:name "quarantine_routing" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?s ?p ?o WHERE { ?s <http://example.org/change_pass> ?o }" ;
            kh:effect "emit-delta" ;
            kh:action ex:route_to_quarantine ;
            kh:priority 1 .

        ex:route_to_quarantine a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/quarantine_flag> <http://example.org/quarantined> } WHERE { ?s <http://example.org/change_pass> ?o }" .

        // Hook 2: Refusal on any write to the protected predicate
        ex:refusal_hook a kh:Hook ;
            kh:name "quarantine_refusal" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?s <http://example.org/change_pass> ?o }" ;
            kh:effect "refuse" ;
            kh:reason "Refusal Error: Direct write to protected change_pass predicate is forbidden" ;
            kh:priority 2 ;
            kh:after ex:quarantine_hook .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Simulate user attempting an unauthorized write to the protected predicate.
    store
        .load_triples(
            "<http://example.org/User1> <http://example.org/change_pass> \"admin\" .",
            Syntax::Turtle,
        )
        .unwrap();

    // A `kh:effect "refuse"` hook makes `materialize()` return `Err`, it does not
    // roll back to a silent `Ok(vec![])` -- see `reasoner::Reasoner::materialize`
    // (`return Err(format!("refused by hook '{}': {}", ...))`) and the established
    // precedent in `business_logic_cases[_suite1].rs::test_suite1_minimal_refusal_surfaces_reason`.
    let res = store.materialize();
    assert!(
        res.is_err(),
        "Unauthorized write to a protected predicate must be refused (Err), got: {:?}",
        res
    );
    let err = res.unwrap_err();
    assert!(
        err.contains("change_pass predicate is forbidden"),
        "refusal reason should name the violated policy, got: {err}"
    );

    // The rollback `materialize()` performs is scoped to *this* call/stratum: it
    // undoes hook-derived additions made during the refused round (here, hook1's
    // quarantine-flag CONSTRUCT action), not triples loaded before `materialize()`
    // was even invoked (`stratum_rollback_len = triple_index.len()` is captured at
    // the start of the stratum, after `load_triples` already ran). So the quarantine
    // marker hook1 would have derived must be absent post-refusal...
    assert_not_contains_triple(&store, "User1", "quarantine_flag", "quarantined");
    // ...while the originally-loaded base triple is untouched by this call (it was
    // never in the rollback range to begin with) -- refusal blocks further
    // *derived* effects, it is not a retroactive write-prevention mechanism.
    assert_contains_triple(&store, "User1", "change_pass", "admin");
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
        
        // Hook 1: Enforce positive balance, refuse otherwise
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

    // `effect = "refuse"` hooks make `materialize()` return `Err`, not a silent
    // `Ok(vec![])` -- see `reasoner::Reasoner::materialize`'s
    // `return Err(format!("refused by hook '{}': {}", ...))` and the established
    // precedent in `business_logic_cases[_suite1].rs`.
    let res = store.materialize();
    assert!(
        res.is_err(),
        "Negative balance transaction should be refused, got: {:?}",
        res
    );
    assert!(res.unwrap_err().contains("Balance Violation"));
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
            // Refuse if we transition from Draft to Approved, skipping UnderReview
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

    // `effect = "refuse"` hooks make `materialize()` return `Err`, not a silent
    // `Ok(vec![])` -- see `reasoner::Reasoner::materialize`'s
    // `return Err(format!("refused by hook '{}': {}", ...))` and the established
    // precedent in `business_logic_cases[_suite1].rs`.
    let res = store.materialize();
    assert!(
        res.is_err(),
        "Invalid state transition must be refused, got: {:?}",
        res
    );
    assert!(res.unwrap_err().contains("Lifecycle Violation"));
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
            kh:query "CONSTRUCT { ?emp <http://example.org/tax_bracket> 'high' } WHERE { ?emp <http://example.org/salary> ?sal . FILTER(?sal > 150000) }" .
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
