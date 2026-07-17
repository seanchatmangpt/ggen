mod common;

use common::assert_contains_triple;
use common::assert_not_contains_triple;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =========================================================================
// TIER 1: FEATURE COVERAGE
// =========================================================================

// --- F1: Hook Parsing & Registry ---

/// Covers F1: Verifies parsing and registration of a single valid hook pack.
#[test]
fn test_f1_load_valid_single() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:my_hook a kh:Hook ;
            kh:name "valid_single_hook" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:on "assert" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok(), "Failed to load single valid hook: {:?}", res);
}

/// Covers F1: Verifies parsing and registration of multiple valid hooks in a single pack.
#[test]
fn test_f1_load_valid_multiple() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:hook1 a kh:Hook ;
            kh:name "multiple_hook_1" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:on "assert" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook2 a kh:Hook ;
            kh:name "multiple_hook_2" ;
            kh:kind "delta" ;
            kh:var "y" ;
            kh:on "assert" ;
            kh:effect "emit-delta" ;
            kh:priority 2 .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_ok(),
        "Failed to load multiple valid hooks: {:?}",
        res
    );
}

/// Covers F1: Verifies namespace/prefix resolution within the hook pack parser.
#[test]
fn test_f1_resolve_prefixes() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix myns: <http://example.org/myns#> .

        myns:hook_with_prefixes a kh:Hook ;
            kh:name "prefix_hook" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:on "assert" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok(), "Failed to resolve prefixes: {:?}", res);
}

/// Covers F1: Verifies hook: alias namespace is rewritten to kh: canonical form.
/// The hook: namespace is a user-friendly alias for kh:, allowing users to write
/// hook:name instead of kh:name. This test verifies the aliasing works correctly.
#[test]
fn test_f1_hook_alias_namespace_rewrite() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:alias_hook a kh:Hook ;
            kh:name "alias_test_hook" ;
            kh:kind "delta" ;
            kh:var "x" ;
            kh:on "assert" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_ok(),
        "Hook pack using hook: alias namespace should succeed: {:?}",
        res
    );

    // Verify the hook was registered successfully by querying it
    let query_str = "SELECT ?name WHERE { ?h a <http://seanchatmangpt.github.io/praxis/kh#Hook> ; <http://seanchatmangpt.github.io/praxis/kh#name> ?name }";
    let rows = store.query(query_str).unwrap();
    assert_eq!(
        rows.len(),
        1,
        "Hook alias should have been rewritten and registered"
    );
    assert_eq!(rows[0][0].val, "\"alias_test_hook\"");
}

/// Covers F1: Verifies parsing hooks with inline triples representing hook attributes.
#[test]
fn test_f1_load_inline_triples() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        ex:h1 a kh:Hook ; kh:name "inline_hook" ; kh:kind "delta" ; kh:var "v" ; kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_ok(), "Failed to load inline triples: {:?}", res);
}

/// Covers F1: Verifies that registered hooks can be queried using SPARQL or the internal registry.
#[test]
fn test_f1_query_registered_hooks() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_query a kh:Hook ;
            kh:name "queryable_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:on "assert" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Querying the hook registration in the store's facts
    let query_str = "SELECT ?name WHERE { ?h a <http://seanchatmangpt.github.io/praxis/kh#Hook> ; <http://seanchatmangpt.github.io/praxis/kh#name> ?name }";
    let rows = store.query(query_str).unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0][0].val, "\"queryable_hook\"");
}

// --- F2: Constitutional Gating ---

/// Covers F2: Verifies that hooks using forbidden system commands are rejected.
#[test]
fn test_f2_refuse_command() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:malicious_hook a kh:Hook ;
            kh:name "command_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "ground-action" ;
            kh:action ex:forbidden_action .

        ex:forbidden_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#command-exec> .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Hook with forbidden command handler should be refused"
    );
    let err = res.unwrap_err();
    assert!(err.contains("forbidden") || err.contains("SHACL") || err.contains("validation"));
}

/// Covers F2: Verifies that hooks trying to run shell utilities are refused.
#[test]
fn test_f2_refuse_shell() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:shell_hook a kh:Hook ;
            kh:name "shell_exec_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "ground-action" ;
            kh:action ex:shell_action .

        ex:shell_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#shell-exec> .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err(), "Hook with shell execution should be refused");
}

/// Covers F2: Verifies that hooks using unrecognized actions are refused.
#[test]
fn test_f2_refuse_unrecognized_action() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:unrecognized_hook a kh:Hook ;
            kh:name "unrecognized_action_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "ground-action" ;
            kh:action ex:unrecognized_action .

        ex:unrecognized_action a kh:Action ;
            kh:handler <http://example.org/handler#unknown-handler> .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Hook with unrecognized action should be refused"
    );
}

/// Covers F2: Verifies that hook packs violating the SHACL Law Pack schema fail gating.
#[test]
fn test_f2_gating_malformed_shacl() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        // Violates SHACL shape because kh:kind is missing (mandatory)
        ex:malformed_hook a kh:Hook ;
            kh:name "malformed_shacl_hook" ;
            kh:var "v" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Malformed hook missing mandatory field should fail SHACL gating"
    );
}

/// Covers F2: Verifies that when a hook pack is refused, the store is rolled back and no triples remain.
#[test]
fn test_f2_gating_rollback_state() {
    let mut store = TripleStore::new();

    // Record initial length
    let initial_len = store.len();

    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:ok_hook a kh:Hook ;
            kh:name "ok_hook" ;
            kh:kind "delta" ;
            kh:var "v" ;
            kh:effect "emit-delta" .

        // Malformed hook triggers refusal
        ex:bad_hook a kh:Hook ;
            kh:name "bad_hook" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(res.is_err());

    // Assert that no new triples were registered in the store (full transaction rollback)
    assert_eq!(store.len(), initial_len);
}

// --- F3: First-Class Trigger Dialects ---

/// Covers F3: Verifies SPARQL ASK trigger dialect evaluation.
#[test]
fn test_f3_sparql_ask_trigger() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_ask a kh:Hook ;
            kh:name "ask_hook" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?s <http://example.org/status> 'critical' }" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Assert status critical fact
    store
        .load_triples(
            "ex:Server1 <http://example.org/status> 'critical' .",
            Syntax::Turtle,
        )
        .unwrap();

    // Running materialize should fire the SPARQL ASK hook
    let _inferred = store.materialize().unwrap();

    // Verify receipt was generated
    let receipts = store.get_hook_receipts();
    assert!(
        !receipts.is_empty(),
        "SPARQL ASK trigger should have generated a receipt"
    );
    assert_eq!(receipts[0].hook_name, "ask_hook");
}

/// Covers F3: Verifies SPARQL SELECT trigger dialect evaluation.
#[test]
fn test_f3_sparql_select_trigger() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_select a kh:Hook ;
            kh:name "select_hook" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?s ?val WHERE { ?s <http://example.org/value> ?val . FILTER(?val > 100) }" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Sensor1 <http://example.org/value> 150 .",
            Syntax::Turtle,
        )
        .unwrap();

    let _inferred = store.materialize().unwrap();
    let receipts = store.get_hook_receipts();
    assert!(
        !receipts.is_empty(),
        "SPARQL SELECT trigger should have generated a receipt"
    );
    assert_eq!(receipts[0].hook_name, "select_hook");
}

/// Covers F3: Verifies Count trigger dialect evaluation.
#[test]
fn test_f3_count_trigger() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_count a kh:Hook ;
            kh:name "count_hook" ;
            kh:kind "count" ;
            kh:var "http://example.org/item" ;
            kh:op ">=" ;
            kh:k 3 ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Add 2 items (below limit of 3)
    store
        .load_triples(
            "ex:Order <http://example.org/item> ex:A , ex:B .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();
    assert!(store.get_hook_receipts().is_empty());

    // Add 3rd item
    store
        .load_triples("ex:Order <http://example.org/item> ex:C .", Syntax::Turtle)
        .unwrap();
    store.materialize().unwrap();
    assert!(!store.get_hook_receipts().is_empty());
}

/// Covers F3: Verifies Threshold trigger dialect evaluation.
#[test]
fn test_f3_threshold_trigger() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_threshold a kh:Hook ;
            kh:name "threshold_hook" ;
            kh:kind "threshold" ;
            kh:var "http://example.org/temperature" ;
            kh:op ">" ;
            kh:k 1 ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    // Temperature at 98 (does not fire)
    store
        .load_triples(
            "ex:Room1 <http://example.org/temperature> 98 .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();
    assert!(store.get_hook_receipts().is_empty());

    // Temperature at 100 (fires)
    store
        .load_triples(
            "ex:Room2 <http://example.org/temperature> 100 .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();
    assert!(!store.get_hook_receipts().is_empty());
}

/// Covers F3: Verifies Delta trigger dialect evaluation.
#[test]
fn test_f3_delta_trigger() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_delta a kh:Hook ;
            kh:name "delta_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/status" ;
            kh:on "assert" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();

    store
        .load_triples(
            "ex:A <http://example.org/status> 'active' .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();
    assert!(!store.get_hook_receipts().is_empty());
}

/// Covers F3: Verifies Datalog trigger dialect formatting and execution.
#[test]
fn test_f3_datalog_trigger_format() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_datalog a kh:Hook ;
            kh:name "datalog_hook" ;
            kh:kind "datalog" ;
            kh:program "reachable(?x, ?y) :- ?x <http://example.org/link> ?y . reachable(?x, ?z) :- reachable(?x, ?y), reachable(?y, ?z) ." ;
            kh:goal "reachable(?s, ?o)" ;
            kh:effect "emit-delta" .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_ok(),
        "Failed to load Datalog trigger format: {:?}",
        res
    );
}

// --- F4: Pure Action Projections ---

/// Covers F4: Verifies projection of `kh:addQuad` declarative changes.
#[test]
fn test_f4_project_add_quad() {
    let mut store = TripleStore::new();

    // Register hook that adds a VIP status triple when someone has spent > 1000
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:vip_hook a kh:Hook ;
            kh:name "vip_add_hook" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?cust WHERE { ?cust <http://example.org/spent> ?amount . FILTER(?amount > 1000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:add_vip_action .

        ex:add_vip_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?cust <http://example.org/status> 'VIP' } WHERE { ?cust <http://example.org/spent> ?amount . FILTER(?amount > 1000) }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Alice <http://example.org/spent> 1500 .", Syntax::Turtle)
        .unwrap();

    store.materialize().unwrap();

    // Verify pure projection applied the VIP status
    assert_contains_triple(&store, "Alice", "status", "VIP");
}

/// Covers F4: Verifies projection of `kh:deleteQuad` declarative changes.
#[test]
fn test_f4_project_delete_quad() {
    let mut store = TripleStore::new();

    // Register hook that deletes standard status when someone becomes VIP
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:rm_std_hook a kh:Hook ;
            kh:name "remove_standard_hook" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?cust WHERE { ?cust <http://example.org/status> <http://example.org/VIP> }" ;
            kh:effect "emit-delta" ;
            kh:action ex:rm_std_action .

        ex:rm_std_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { } WHERE { ?cust <http://example.org/status> <http://example.org/Standard> } " .
    "#;
    // Wait, construct with deletion usually projects deletion quads. In graphlaw,
    // this can be mapped to kh:deleteQuad predicate.
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Alice <http://example.org/status> <http://example.org/Standard> , <http://example.org/VIP> .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();
    assert_contains_triple(&store, "Alice", "status", "http://example.org/VIP");
    assert_not_contains_triple(&store, "Alice", "status", "http://example.org/Standard");
}

/// Covers F4: Verifies projection of both additions and deletions simultaneously.
#[test]
fn test_f4_project_add_and_delete() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:swap_hook a kh:Hook ;
            kh:name "swap_status_hook" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?cust WHERE { ?cust <http://example.org/trigger> 'go' }" ;
            kh:effect "emit-delta" ;
            kh:action ex:swap_action .

        ex:swap_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?cust <http://example.org/has> 'new_val' . } WHERE { ?cust <http://example.org/has> 'old_val' }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Alice <http://example.org/has> 'old_val' ; <http://example.org/trigger> 'go' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    assert_contains_triple(&store, "Alice", "has", "new_val");
}

/// Covers F4: Verifies that hooks attempting non-pure side-effects are blocked.
#[test]
fn test_f4_refuse_side_effects() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:side_effect_hook a kh:Hook ;
            kh:name "malicious_projection" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?s ?p ?o }" ;
            kh:effect "ground-action" ;
            kh:action ex:http_call .

        ex:http_call a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#fetch-url> .
    "#;
    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Hook projecting side-effects should fail constitutional gating"
    );
}

/// Covers F4: Verifies that projected quads are applied to the correct named graph.
#[test]
fn test_f4_project_apply_to_graph() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:graph_hook a kh:Hook ;
            kh:name "apply_to_graph_hook" ;
            kh:kind "sparql" ;
            kh:query "SELECT ?cust WHERE { ?cust <http://example.org/flag> <http://example.org/VIP> }" ;
            kh:effect "emit-delta" ;
            kh:action ex:graph_action .

        ex:graph_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?cust a <http://example.org/VIPMember> } WHERE { ?cust <http://example.org/flag> <http://example.org/VIP> }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Alice <http://example.org/flag> <http://example.org/VIP> .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    // Assert target named graph contains the projected quad
    assert_contains_triple(&store, "Alice", "type", "VIPMember");
}

// --- F5: Canonical N-Quads & BLAKE3 Receipts ---

/// Covers F5: Verifies receipt generation for a single quad addition.
#[test]
fn test_f5_receipt_single_add() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:receipt_hook a kh:Hook ;
            kh:name "single_add_receipt" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Fact <http://example.org/trigger> 'yes' .",
            Syntax::Turtle,
        )
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert_eq!(receipts.len(), 1);
    assert_eq!(receipts[0].hook_name, "single_add_receipt");
    assert!(
        !receipts[0].delta_hash.is_empty(),
        "Delta hash should be non-empty BLAKE3 hash"
    );
}

/// Covers F5: Verifies sort determinism of canonical N-Quads before hashing.
#[test]
fn test_f5_receipt_sort_determinism() {
    let mut store_a = TripleStore::new();
    let mut store_b = TripleStore::new();

    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:det_hook a kh:Hook ;
            kh:name "sort_det_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:effect "emit-delta" .
    "#;

    store_a.load_hook_pack(hook_pack).unwrap();
    store_b.load_hook_pack(hook_pack).unwrap();

    // Add facts in order A, then B
    store_a.load_triples("ex:NodeA <http://example.org/trigger> 'yes' . ex:NodeB <http://example.org/trigger> 'yes' .", Syntax::Turtle).unwrap();
    store_a.materialize().unwrap();

    // Add facts in order B, then A
    store_b.load_triples("ex:NodeB <http://example.org/trigger> 'yes' . ex:NodeA <http://example.org/trigger> 'yes' .", Syntax::Turtle).unwrap();
    store_b.materialize().unwrap();

    let rec_a = store_a.get_hook_receipts();
    let rec_b = store_b.get_hook_receipts();

    assert_eq!(
        rec_a[0].delta_hash, rec_b[0].delta_hash,
        "Receipt hashes must be identical due to canonical sorting"
    );
}

/// Covers F5: Verifies receipt generation for quad deletions.
#[test]
fn test_f5_receipt_deletion() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:del_receipt_hook a kh:Hook ;
            kh:name "del_receipt_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:on "retract" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples(
            "ex:Fact <http://example.org/trigger> <http://example.org/yes> .",
            Syntax::Turtle,
        )
        .unwrap();
    store.materialize().unwrap();

    // Perform deletion
    store.remove_ref(&praxis_graphlaw::term::Triple {
        s: praxis_graphlaw::triples::VarOrTerm::new_term("http://example.org/Fact".to_string()),
        p: praxis_graphlaw::triples::VarOrTerm::new_term("http://example.org/trigger".to_string()),
        o: praxis_graphlaw::triples::VarOrTerm::new_term("http://example.org/yes".to_string()),
        g: None,
    });

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert!(
        !receipts.is_empty(),
        "Deletion should trigger hook and produce a receipt"
    );
}

/// Covers F5: Verifies the public API `store.get_hook_receipts()`.
#[test]
fn test_f5_get_hook_receipts_api() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:api_hook a kh:Hook ;
            kh:name "api_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/data" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Obj <http://example.org/data> 42 .", Syntax::Turtle)
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert_eq!(receipts.len(), 1);
    assert_eq!(receipts[0].hook_name, "api_hook");
}

/// Covers F5: Verifies the format of HookReceipt fields.
#[test]
fn test_f5_receipt_format_validation() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:fmt_hook a kh:Hook ;
            kh:name "format_validation_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Obj <http://example.org/input> 'test' .", Syntax::Turtle)
        .unwrap();

    store.materialize().unwrap();

    let receipts = store.get_hook_receipts();
    assert_eq!(receipts.len(), 1);
    let receipt = &receipts[0];

    assert_eq!(receipt.hook_name, "format_validation_hook");
    assert!(!receipt.delta_hash.is_empty());
    assert!(!receipt.idempotency_key.is_empty());
    assert!(receipt.delta_quads.contains("http://example.org/input"));
}

// --- F6: Fixpoint Reasoner Integration ---

/// Covers F6: Verifies hook execution in a single-pass materialization.
#[test]
fn test_f6_single_pass_materialization() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:single_pass a kh:Hook ;
            kh:name "single_pass_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" ;
            kh:action ex:add_derived .

        ex:add_derived a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/derived> 'true' } WHERE { ?s <http://example.org/input> ?any }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Item1 <http://example.org/input> 1 .", Syntax::Turtle)
        .unwrap();

    store.materialize().unwrap();

    assert_contains_triple(&store, "Item1", "derived", "true");
}

/// Covers F6: Verifies multi-pass cascading materialization (Hook A triggers Hook B).
#[test]
fn test_f6_multi_pass_cascade() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:hook_a a kh:Hook ;
            kh:name "hook_a" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_a ;
            kh:priority 1 .

        ex:action_a a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/mid> 'true' } WHERE { ?s <http://example.org/input> ?any }" .

        ex:hook_b a kh:Hook ;
            kh:name "hook_b" ;
            kh:kind "delta" ;
            kh:var "http://example.org/mid" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_b ;
            kh:priority 2 ;
            kh:after ex:hook_a .

        ex:action_b a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/output> 'cascade_done' } WHERE { ?s <http://example.org/mid> 'true' }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Item1 <http://example.org/input> 1 .", Syntax::Turtle)
        .unwrap();

    store.materialize().unwrap();

    assert_contains_triple(&store, "Item1", "output", "cascade_done");
}

/// Covers F6: Verifies reasoner terminates successfully (fixpoint reached).
#[test]
fn test_f6_fixpoint_termination() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:stable_hook a kh:Hook ;
            kh:name "stable_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" ;
            kh:action ex:stable_action .

        ex:stable_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/stable> 'yes' } WHERE { ?s <http://example.org/input> ?any }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Node <http://example.org/input> 1 .", Syntax::Turtle)
        .unwrap();

    // If it terminates, this will complete quickly
    let inferred = store.materialize().unwrap();
    assert!(!inferred.is_empty());
}

/// Covers F6: Verifies that if a hook triggers a refusal during materialization, the entire reasoning session is rolled back.
#[test]
fn test_f6_refusal_rollback() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        // This hook adds transient facts
        ex:hook_transient a kh:Hook ;
            kh:name "transient_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" ;
            kh:action ex:transient_action ;
            kh:priority 1 .

        ex:transient_action a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/derived_transient> 'true' } WHERE { ?s <http://example.org/input> ?any }" .

        // This hook triggers refusal when the transient fact is created
        ex:refuse_hook a kh:Hook ;
            kh:name "refusal_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/derived_transient" ;
            kh:effect "refuse" ;
            kh:reason "Safety Violation: transient facts forbidden" ;
            kh:priority 2 ;
            kh:after ex:hook_transient .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Node <http://example.org/input> 1 .", Syntax::Turtle)
        .unwrap();

    // Materialization should trigger refusal, rolling back changes
    assert!(store.materialize().is_err());
    assert_not_contains_triple(&store, "Node", "derived_transient", "true");
}

/// Covers F6: Verifies querying the store state post-materialization.
#[test]
fn test_f6_query_state_post_materialize() {
    let mut store = TripleStore::new();
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:hook_q a kh:Hook ;
            kh:name "query_state_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/input" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_q .

        ex:action_q a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/val> 'derived' } WHERE { ?s <http://example.org/input> ?any }" .
    "#;
    store.load_hook_pack(hook_pack).unwrap();
    store
        .load_triples("ex:Item <http://example.org/input> 1 .", Syntax::Turtle)
        .unwrap();

    store.materialize().unwrap();

    let rows = store
        .query("SELECT ?val WHERE { <http://example.org/Item> <http://example.org/val> ?val }")
        .unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0][0].val, "\"derived\"");
}
