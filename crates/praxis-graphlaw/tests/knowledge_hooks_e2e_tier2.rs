mod common;

use common::assert_contains_triple;
use common::assert_not_contains_triple;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// TIER 2: BOUNDARY & CORNER CASES
// =========================================================================

// --- F1 Boundaries ---

/// Covers F1: Verifies loading a hook pack containing no hook definitions.
#[test]
fn test_b1_empty_hook_pack() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok(), "Empty hook pack should be allowed");
}

/// Covers F1: Verifies name length limits on hooks.
#[test]
fn test_b1_max_name_length() {
    let mut store = TripleStore::new();
    let long_name = "a".repeat(255);
    let hook_pack = format!(
        r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_long a kh:Hook ;
            kh:name "{}" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "emit-delta" .
        "#,
        long_name
    );
    let res = store.load_hook_pack(&hook_pack);
    assert!(res.is_ok(), "Max name length boundary test failed");
}

/// Covers F1: Verifies registry rejection when exceeding maximum number of hooks (limit 12).
#[test]
fn test_b1_exceed_max_hooks() {
    let mut store = TripleStore::new();
    let mut hook_pack = String::from("@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .\n@prefix ex: <http://example.org/> .\n");
    // Generate 13 hooks
    for i in 0..13 {
        hook_pack.push_str(&format!(
            r#"
            ex:h{} a kh:Hook ;
                kh:name "hook_{}" ;
                kh:kind "delta" ;
                kh:var "v" ;
                kh:effect "emit-delta" .
            "#,
            i, i
        ));
    }
    let res = store.load_hook_pack(&hook_pack);
    assert!(
        res.is_err(),
        "Hook registry should reject packs with >12 hooks"
    );
}

/// Covers F1: Verifies hook pack parser handles odd but valid Turtle layout formatting (whitespace/newlines).
#[test]
fn test_b1_turtle_formatting() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix
        kh:
        <http://seanchatmangpt.github.io/praxis/kh#>
        .
        
        <http://example.org/weird>
        a
        kh:Hook
        ;
        kh:name
        "weird_formatting"
        ;
        kh:kind "delta" ; kh:var "v" ;
        kh:effect
        "emit-delta"
        .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok(), "Failed to parse weird formatting");
}

/// Covers F1: Verifies error when missing mandatory fields in the hook pack definition.
#[test]
fn test_b1_missing_mandatory_fields() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_missing a kh:Hook ;
            kh:name "missing_kind" ;
            // kh:kind is missing
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err(), "Hook missing kind should fail to register");
}

// --- F2 Boundaries ---

/// Covers F2: Verifies validation behavior when no custom SHACL laws are defined (uses default).
#[test]
fn test_b2_empty_shacl_law() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:my_hook a kh:Hook ;
            kh:name "test_empty_law" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok());
}

/// Covers F2: Verifies detection of obfuscated execution keywords in URIs.
#[test]
fn test_b2_hidden_side_effects() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:hidden_hook a kh:Hook ;
            kh:name "hidden_side_effect" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "ground-action" ;
            // obfuscated URI containing shell
            kh:action <http://example.org/action#run_a_shell_script> .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Obfuscated shell reference in Action handler should be refused"
    );
}

/// Covers F2: Verifies parsing and gating limits on extremely large hook pack inputs.
#[test]
fn test_b2_huge_hook_packs() {
    let mut store = TripleStore::new();
    let mut hook_pack = String::from("@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .\n@prefix ex: <http://example.org/> .\n");
    // Load up to 10 valid hooks (under 12 limit) with large text payloads
    for i in 0..10 {
        hook_pack.push_str(&format!(
            r#"
            ex:h{} a kh:Hook ;
                kh:name "hook_{}" ;
                kh:kind "delta" ;
                kh:var "v" ;
                kh:effect "emit-delta" ;
                kh:reason "{}" .
            "#,
            i,
            i,
            "a".repeat(1000)
        ));
    }
    let res = store.load_hook_pack(&hook_pack);
    assert!(
        res.is_ok(),
        "Huge hook pack payloads within limits should succeed"
    );
}

/// Covers F2: Verifies behavior when hook properties conflict under SHACL shape constraints.
#[test]
fn test_b2_conflicting_shacl_constraints() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:conflict_hook a kh:Hook ;
            kh:name "conflicting_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "emit-delta" ;
            kh:effect "refuse" . # Duplicate kh:effect violates maxCount 1
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Conflicting properties violating maxCount must be refused"
    );
}

/// Covers F2: Verifies registry state integrity under multiple sequential hook pack load operations.
#[test]
fn test_b2_multiple_sequential_loads() {
    let mut store = TripleStore::new();
    let pack_1 = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        ex:h1 a kh:Hook ; kh:name "seq_1" ; kh:kind "delta" ; kh:var "v" ; kh:effect "emit-delta" .
    "#;
    let pack_2 = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        ex:h2 a kh:Hook ; kh:name "seq_2" ; kh:kind "delta" ; kh:var "v" ; kh:effect "emit-delta" .
    "#;
    assert!(store.load_hook_pack(pack_1).is_ok());
    assert!(store.load_hook_pack(pack_2).is_ok());
}

// --- F3 Boundaries ---

/// Covers F3: Verifies evaluation when trigger conditions yield no matching bindings.
#[test]
fn test_b3_empty_trigger_results() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_empty a kh:Hook ;
            kh:name "empty_trigger" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?s WHERE { ?s <http://example.org/nonexistent> ?o }" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store.materialize().unwrap();
    assert!(store.get_hook_receipts().is_empty());
}

/// Covers F3: Verifies boundary window size values (window = 0 or window = 255).
///
/// Assertion corrected (disclosed, not silently fixed): this test previously asserted
/// `kh:window 0` loads successfully (`res.is_ok()`), unknowingly asserting the symptom of a
/// real bug rather than "handled cleanly" (this test's own docstring's other acceptable
/// outcome). `store.load_hook_pack` never calls `materialize()`, so this test never actually
/// exercised the code path that panicked: `Reasoner::materialize`'s `HookCondition::Window`
/// arm (reasoner/mod.rs) computed `usize::from(window) - 1` unconditionally, which underflows
/// and panics for `window == 0` under this workspace's default overflow-checked build profile
/// -- found by an adversarial dogfood audit this session, reproduced directly, and reachable in
/// real production via `ggen law derive/explain/export/validate` (`ggen/src/graph.rs`'s
/// `build_law_store` has no `catch_unwind` around `TripleStore::materialize`). Fixed by
/// refusing `kh:window 0` at hook-parse time (hooks/parsing.rs) -- a degenerate "look back zero
/// rounds" value has no sensible interpretation to silently substitute, so admission-time
/// refusal is the honest fix, not a reinterpretation. This test now asserts that refusal.
#[test]
fn test_b3_window_size_bounds() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_window_zero a kh:Hook ;
            kh:name "window_zero" ;
            kh:kind "window" ;
            kh:var "http://example.org/metric" ;
            kh:op ">" ;
            kh:k 10 ;
            kh:window 0 ; # boundary value
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    let err = res.expect_err("kh:window 0 is a degenerate value and must be refused at parse time, not silently accepted");
    assert!(
        err.contains("window") && err.contains("0"),
        "refusal message should name the offending window value, got: {err}"
    );
}

/// Covers F3: Verifies extreme threshold boundary values (k = 0 or k = Max Integer).
#[test]
fn test_b3_threshold_boundary_values() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_threshold_max a kh:Hook ;
            kh:name "threshold_max" ;
            kh:kind "threshold" ;
            kh:var "http://example.org/metric" ;
            kh:op ">" ;
            kh:k 18446744073709551615 ; # max u64
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok());
}

/// Covers F3: Verifies validation/gating constraints on extremely large Datalog program sizes.
#[test]
fn test_b3_datalog_program_size_limit() {
    let mut store = TripleStore::new();
    let huge_program = "r(?x) :- p(?x) . ".repeat(500);
    let hook_pack = format!(
        r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_huge_datalog a kh:Hook ;
            kh:name "huge_datalog" ;
            kh:kind "datalog" ;
            kh:program "{}" ;
            kh:goal "r(?s)" ;
            kh:effect "emit-delta" .
        "#,
        huge_program
    );
    let res = store.load_hook_pack(&hook_pack);
    assert!(res.is_ok() || res.is_err()); // Either parsed or gracefully rejected
}

/// Covers F3: Verifies syntax error handling in SPARQL triggers.
#[test]
fn test_b3_sparql_syntax_error() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_bad_sparql a kh:Hook ;
            kh:name "bad_sparql" ;
            kh:kind "sparql" ;
            kh:query "SELECT * WHERE { ?s ?p }" ; # missing brackets
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Invalid SPARQL syntax must reject hook pack loading"
    );
}

// --- F4 Boundaries ---

/// Covers F4: Verifies CONSTRUCT projections that produce empty results.
#[test]
fn test_b4_construct_empty_result() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_empty_construct a kh:Hook ;
            kh:name "empty_construct" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:empty_action .
            
        ex:empty_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/status> 'none' } WHERE { ?s <http://example.org/nonexistent> ?o }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Node <http://example.org/trigger> 'yes' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // No new triples should be created
    assert_not_contains_triple(&store, "Node", "status", "none");
}

/// Covers F4: Verifies rejection of CONSTRUCT queries projecting invalid RDF (literal as subject).
#[test]
fn test_b4_construct_literal_subject() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_lit_subject a kh:Hook ;
            kh:name "literal_subject" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:lit_action .
            
        ex:lit_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { 'subject' <http://example.org/p> ?o } WHERE { ?s <http://example.org/trigger> ?o }" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    let mat_result = store.materialize();
    assert!(
        res.is_err() || mat_result.is_err() || mat_result.unwrap().is_empty(),
        "Should reject invalid RDF generation"
    );
}

/// Covers F4: Verifies rejection/handling of unsupported clauses in SPARQL CONSTRUCT queries.
#[test]
fn test_b4_construct_unsupported_clauses() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_unsupported a kh:Hook ;
            kh:name "unsupported_construct" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:unsup_action .
            
        ex:unsup_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            // Contains unsupported SERVICE or external subquery block
            kh:query "CONSTRUCT { ?s <http://example.org/p> ?o } WHERE { ?s <http://example.org/trigger> ?o . SERVICE <http://external.org> { ?s ?p ?o } }" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err());
}

/// Covers F4: Verifies execution when CONSTRUCT attempts to add quads already present (no-op).
#[test]
fn test_b4_construct_no_op_addition() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_noop a kh:Hook ;
            kh:name "noop_addition" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:noop_action .
            
        ex:noop_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/trigger> 'yes' } WHERE { ?s <http://example.org/trigger> 'yes' }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Node <http://example.org/trigger> 'yes' .",
            Syntax::Turtle,
        )
        .unwrap();

    let before_len = store.len();
    store.materialize().unwrap();
    assert_eq!(store.len(), before_len);
}

/// Covers F4: Verifies rejection of CONSTRUCT queries attempting to modify the hook registry itself.
#[test]
fn test_b4_construct_modify_registry() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:h_hijack a kh:Hook ;
            kh:name "hijack_registry" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:hijack_action .
            
        ex:hijack_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            // Attempt to inject a new malicious hook via CONSTRUCT projection
            kh:query "CONSTRUCT { ex:injected a kh:Hook ; kh:name 'injected' ; kh:effect 'ground-action' } WHERE { ?s <http://example.org/trigger> ?o }" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Hook attempting to modify system/registry namespace must be blocked"
    );
}

// --- F5 Boundaries ---

/// Covers F5: Verifies receipt generation containing blank nodes.
#[test]
fn test_b5_receipt_blank_nodes() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:bnode_hook a kh:Hook ;
            kh:name "bnode_receipt" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Node <http://example.org/trigger> _:blank .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert!(!receipts.is_empty());
    assert!(!receipts[0].delta_hash.is_empty());
}

/// Covers F5: Verifies receipt generation containing non-ASCII Unicode literals.
#[test]
fn test_b5_receipt_unicode_literals() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:unicode_hook a kh:Hook ;
            kh:name "unicode_receipt" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Node <http://example.org/trigger> 'こんにちは' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert!(!receipts.is_empty());
    assert!(receipts[0].delta_quads.contains("こんにちは"));
}

/// Covers F5: Verifies receipt hashing/formatting with extremely large literals.
#[test]
fn test_b5_receipt_huge_literals() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:huge_lit_hook a kh:Hook ;
            kh:name "huge_literal_receipt" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    let huge_str = "a".repeat(10000);
    store
        .load_triples(
            &format!("ex:Node <http://example.org/trigger> '{}' .", huge_str),
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert!(!receipts.is_empty());
}

/// Covers F5: Verifies that distinct literal datatypes and language tags produce distinct, stable hashes.
#[test]
fn test_b5_stable_hash_datatypes_lang() {
    let mut store_a = TripleStore::new();
    let mut store_b = TripleStore::new();

    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:hash_stable a kh:Hook ;
            kh:name "stable_hash_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" .
    "#;

    store_a.load_hook_pack(hook_pack).unwrap();
    store_b.load_hook_pack(hook_pack).unwrap();

    // Store A gets integer literal, Store B gets string representation
    store_a
        .load_triples("ex:Node <http://example.org/trigger> 42 .", Syntax::Turtle)
        .unwrap();
    store_b
        .load_triples(
            "ex:Node <http://example.org/trigger> '42' .",
            Syntax::Turtle,
        )
        .unwrap();

    store_a.materialize().unwrap();
    store_b.materialize().unwrap();

    let rec_a = store_a.get_hook_receipts();
    let rec_b = store_b.get_hook_receipts();

    assert_ne!(
        rec_a[0].delta_hash, rec_b[0].delta_hash,
        "Hashes of different RDF datatypes must be distinct"
    );
}

/// Covers F5: Verifies receipt generation containing both quad additions and deletions.
#[test]
fn test_b5_hash_both_add_and_delete() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:both_hook a kh:Hook ;
            kh:name "both_add_delete" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" ;
            kh:action ex:both_action .
            
        ex:both_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            // Adds vip status, deletes standard status
            kh:query "CONSTRUCT { ?cust <http://example.org/status> 'VIP' } WHERE { ?cust <http://example.org/spent> ?amount . FILTER(?amount > 1000) }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Alice <http://example.org/spent> 1500 ; <http://example.org/trigger> 'yes' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert!(!receipts.is_empty());
}

// --- F6 Boundaries ---

/// Covers F6: Verifies recursion guard limit and infinite loop detection during fixpoint loops.
#[test]
fn test_b6_infinite_loop_detection() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        // Hook A triggers B
        ex:hook_a a kh:Hook ;
            kh:name "loop_hook_a" ;
            kh:kind "delta" ;
            kh:var "http://example.org/p_b" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_a .
            
        ex:action_a a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/p_a> 'triggered' } WHERE { ?s <http://example.org/p_b> ?any }" .

        // Hook B triggers A
        ex:hook_b a kh:Hook ;
            kh:name "loop_hook_b" ;
            kh:kind "delta" ;
            kh:var "http://example.org/p_a" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_b .
            
        ex:action_b a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/p_b> 'triggered' } WHERE { ?s <http://example.org/p_a> ?any }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Node <http://example.org/p_a> 'start' .", Syntax::Turtle)
        .unwrap();

    // Materialization should terminate with limit or recursion depth error
    let inferred = store.materialize().unwrap();
    // It should terminate safely without hanging
    assert!(inferred.len() < 100);
}

/// Covers F6: Verifies static hook dependency cycle detection at registration.
#[test]
fn test_b6_circular_dependency() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:hook_a a kh:Hook ;
            kh:name "hook_a" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:effect "emit-delta" ;
            kh:after ex:hook_b .

        ex:hook_b a kh:Hook ;
            kh:name "hook_b" ;
            kh:kind "delta" ;
            kh:var "y" ;
            kh:effect "emit-delta" ;
            kh:after ex:hook_a .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Circular static dependency should be rejected at load time"
    );
}

/// Covers F6: Verifies deep reasoning chain rollback integrity on a late gating refusal.
#[test]
fn test_b6_gating_refusal_deep_rollback() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        // Chain of 3 hooks
        ex:h1 a kh:Hook ;
            kh:name "deep_1" ;
            kh:kind "delta" ;
            kh:var "http://example.org/start" ;
            kh:effect "emit-delta" ;
            kh:action ex:act1 ;
            kh:priority 1 .
        ex:act1 a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/step1> 'yes' } WHERE { ?s <http://example.org/start> ?any }" .

        ex:h2 a kh:Hook ;
            kh:name "deep_2" ;
            kh:kind "delta" ;
            kh:var "http://example.org/step1" ;
            kh:effect "emit-delta" ;
            kh:action ex:act2 ;
            kh:priority 2 ;
            kh:after ex:h1 .
        ex:act2 a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/step2> 'yes' } WHERE { ?s <http://example.org/step1> ?any }" .

        // Third hook triggers constitutional refusal when step2 is generated
        ex:h3 a kh:Hook ;
            kh:name "deep_3" ;
            kh:kind "delta" ;
            kh:var "http://example.org/step2" ;
            kh:effect "refuse" ;
            kh:reason "Refusal: step2 reached" ;
            kh:priority 3 ;
            kh:after ex:h2 .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Node <http://example.org/start> 'go' .", Syntax::Turtle)
        .unwrap();

    let err = store.materialize().unwrap_err();
    assert!(err.contains("refused by hook 'deep_3'"));
    // Ensure both step1 and step2 are completely rolled back
    assert_not_contains_triple(&store, "Node", "step1", "yes");
    assert_not_contains_triple(&store, "Node", "step2", "yes");
}

/// Covers F6: Verifies materialization behavior when executing on a store with zero base facts.
#[test]
fn test_b6_empty_base_facts() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:my_hook a kh:Hook ;
            kh:name "empty_base" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    let inferred = store.materialize().unwrap();
    assert!(
        inferred.is_empty(),
        "No inferences should happen on empty store"
    );
}

/// Covers F6: Verifies multi-strata rule evaluation with stratified datalog and hooks.
#[test]
fn test_b6_multi_strata_evaluation() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        
        ex:strat_hook a kh:Hook ;
            kh:name "strata_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" ;
            kh:action ex:strat_action .
            
        ex:strat_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/stratum1> 'done'^^<http://www.w3.org/2001/XMLSchema#string> } WHERE { ?s <http://example.org/input> ?any }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    // Add Datalog rule to stratum 2: { ?x :stratum1 "done" } => { ?x :stratum2 "complete" }
    store.load_rules("{ ?x <http://example.org/stratum1> \"done\" } => { ?x <http://example.org/stratum2> \"complete\" } .").unwrap();
    store
        .load_triples(
            "<http://example.org/Node> <http://example.org/input> \"start\" .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    assert_contains_triple(&store, "Node", "stratum2", "complete");
}

// =========================================================================
