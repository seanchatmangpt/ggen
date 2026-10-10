//! Lane W `graphlawstore-repoint` e2e: `GraphLawStore` must keep its public
//! `GraphEngine` API byte-compatible after its internals were repointed from
//! `praxis_graphlaw::TripleStore` to the `graphlaw` kernel (the same
//! `n3_run`/`hooks_apply`/`shacl_check`/`shex_check` helpers that
//! `law_engine.rs` already uses — one kernel binding, string-only seam).
//!
//! Chicago discipline: real Turtle/N3/SHACL files in a real TempDir, real
//! materialization, assertions on observable state (derived facts, DENIED
//! lines, BLAKE3 hashes) — no doubles.

use ggen_engine::graph::{
    DeterministicGraph, EngineQueryResults, GraphEngine, GraphLawStore, ShaclOutcome,
};

/// Real Turtle facts + N3 rule + SHACL shape, written to a real TempDir and
/// loaded from disk (no inline-only fixtures).
fn write_fixtures(
    dir: &std::path::Path,
) -> (std::path::PathBuf, std::path::PathBuf, std::path::PathBuf) {
    let facts = dir.join("facts.ttl");
    std::fs::write(
        &facts,
        "@prefix ex: <http://example.org/> .\nex:rex a ex:Dog .\n",
    )
    .expect("write facts.ttl");
    let rules = dir.join("rules.n3");
    std::fs::write(
        &rules,
        "@prefix ex: <http://example.org/> .\n{?s a ex:Dog} => {?s a ex:Animal} .\n",
    )
    .expect("write rules.n3");
    let shapes = dir.join("shapes.ttl");
    std::fs::write(
        &shapes,
        r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:DogShape a sh:NodeShape ;
            sh:targetClass ex:Dog ;
            sh:property [ sh:path ex:name ; sh:minCount 1 ] .
        "#,
    )
    .expect("write shapes.ttl");
    (facts, rules, shapes)
}


/// Canonical quads have no terminating ` .`; re-insertion needs one.
fn insert_derived(g: &DeterministicGraph, derived: &[String]) {
    let doc = if derived.is_empty() {
        String::new()
    } else {
        format!("{} .\n", derived.join(" .\n"))
    };
    g.insert_turtle(&doc).expect("insert derived facts");
}

/// (a) Materialization over real disk fixtures derives the expected triple,
/// visible through the same public API (query), and `rules_loaded` is exact.
#[test]
fn repoint_materialize_derives_expected_triple_from_disk_fixtures() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let (facts, rules, _shapes) = write_fixtures(tmp.path());
    let facts_text = std::fs::read_to_string(&facts).expect("read facts");

    let store = GraphLawStore::new().expect("store");
    GraphEngine::insert_turtle(&store, &facts_text).expect("insert facts");
    GraphEngine::load_rules(&store, &std::fs::read_to_string(&rules).expect("read rules"))
        .expect("load rules");

    // Hash before materialization = hash of the asserted facts alone.
    let hash_before = GraphEngine::state_hash(&store).expect("hash before");

    let outcome = GraphEngine::materialize(&store).expect("materialize");
    assert_eq!(outcome.rules_loaded, 1, "one N3 rule");
    assert_eq!(
        outcome.derived.len(),
        1,
        "exactly one derived triple: {:?}",
        outcome.derived
    );
    assert!(
        outcome.derived[0].contains("rex") && outcome.derived[0].contains("Animal"),
        "derived triple must be rex a Animal: {:?}",
        outcome.derived
    );
    let ask = "ASK { <http://example.org/rex> a <http://example.org/Animal> }";
    assert_eq!(
        GraphEngine::query(&store, ask).expect("ask after"),
        EngineQueryResults::Boolean(true),
        "derived fact visible to SPARQL through the same public API"
    );

    // Hash after materialization = hash of facts + the derived triple.
    let oxi_after = DeterministicGraph::new().expect("after-mirror");
    oxi_after.insert_turtle(&facts_text).expect("insert facts");
    insert_derived(&oxi_after, &outcome.derived);
    assert_eq!(
        GraphEngine::state_hash(&store).expect("hash after"),
        GraphEngine::state_hash(&oxi_after).expect("hash after-mirror"),
        "hash after materialize must equal the hash of facts + derived"
    );
    let hash_after = GraphEngine::state_hash(&store).expect("hash after 2");
    assert_ne!(hash_before, hash_after, "materialization changed the state");
}

/// (b) Refuse-path: a `kh:effect "refuse"` hook pack now LOADS (the GAP is
/// closed upstream — `Verdict::Refuse`/`HookVerdict` exist in graphlaw), and
/// its firing surfaces at materialize as the same DENIED-class outcome the
/// N3 fuse path produces (`Err`, `DENIED:` line naming the hook and its
/// `kh:reason` — fail-closed for sync). A violated N3 denial rule still
/// surfaces its `DENIED:` line via `check_denials` exactly as before.
#[test]
fn repoint_refuse_pack_loads_and_denial_surfaces_at_materialize() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let (facts, rules, _shapes) = write_fixtures(tmp.path());

    let store = GraphLawStore::new().expect("store");
    GraphEngine::insert_turtle(&store, &std::fs::read_to_string(&facts).expect("read facts"))
        .expect("insert facts");
    GraphEngine::load_rules(&store, &std::fs::read_to_string(&rules).expect("read rules"))
        .expect("load rules");

    // The refuse hook loads — no load-time FM-LAW-016 refusal anymore.
    let refuse_hook = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        <http://example.org/h#deny> a kh:Hook ;
            kh:name "deny" ; kh:kind "sparql" ;
            kh:query "SELECT ?s WHERE { ?s a <http://example.org/Dog> }" ;
            kh:effect "refuse" ;
            kh:reason "dogs must not be materialized" ;
            kh:action <http://example.org/h#a> ; kh:priority 1 .
        <http://example.org/h#a> a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s <http://example.org/flag> 'x' } WHERE { ?s a <http://example.org/Dog> }" .
    "#;
    GraphEngine::load_hook_pack(&store, refuse_hook).expect("refuse-effect pack now loads");

    // The refusal surfaces at materialize as a DENIED-class outcome naming
    // the hook and its kh:reason — fail-closed, never silent.
    let err = GraphEngine::materialize(&store).expect_err("refuse verdict must fail materialize");
    let msg = err.to_string();
    assert!(msg.contains("DENIED"), "DENIED-class outcome: {msg}");
    assert!(msg.contains("deny"), "hook name surfaces: {msg}");
    assert!(
        msg.contains("dogs must not be materialized"),
        "kh:reason text surfaces: {msg}"
    );

    // Denied path through the SAME public API shape as before: violated
    // denial rule -> `DENIED:` line from check_denials.
    GraphEngine::load_rules(
        &store,
        "@prefix ex: <http://example.org/> . {?s a ex:Animal} => false.",
    )
    .expect("denial rule");
    let denials = GraphEngine::check_denials(&store).expect("denials");
    assert_eq!(denials.len(), 1, "one DENIED line: {denials:?}");
    assert!(denials[0].contains("DENIED"), "{denials:?}");
}

/// (b2) Emit-delta regression: an emit-delta hook pack still loads and
/// materializes, its CONSTRUCT delta entering the mirror through the
/// canonical door and visible to SPARQL.
#[test]
fn repoint_emit_delta_pack_still_fires_and_derives() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let (facts, _rules, _shapes) = write_fixtures(tmp.path());

    let store = GraphLawStore::new().expect("store");
    GraphEngine::insert_turtle(&store, &std::fs::read_to_string(&facts).expect("read facts"))
        .expect("insert facts");

    let emit_hook = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        <http://example.org/h#emit> a kh:Hook ;
            kh:name "emit" ; kh:kind "sparql" ;
            kh:query "SELECT ?s WHERE { ?s a <http://example.org/Dog> }" ;
            kh:effect "emit-delta" ;
            kh:action <http://example.org/h#a> ; kh:priority 1 .
        <http://example.org/h#a> a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?s a <http://example.org/Hooked> } WHERE { ?s a <http://example.org/Dog> }" .
    "#;
    GraphEngine::load_hook_pack(&store, emit_hook).expect("emit-delta pack loads");
    GraphEngine::materialize(&store).expect("materialize");
    let ask = "ASK { <http://example.org/rex> a <http://example.org/Hooked> }";
    assert_eq!(
        GraphEngine::query(&store, ask).expect("ask"),
        EngineQueryResults::Boolean(true),
        "emit-delta hook derived fact visible to SPARQL"
    );
}

/// (c) State-hash contract: `state_hash` is computed ggen-side over the
/// mirror's canonical quads — identical fact states hash identically
/// regardless of which law kernel produced the derived facts.
#[test]
fn repoint_state_hash_contract_preserved() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let (facts, rules, _shapes) = write_fixtures(tmp.path());
    let facts_text = std::fs::read_to_string(&facts).expect("read facts");

    let store = GraphLawStore::new().expect("store");
    GraphEngine::insert_turtle(&store, &facts_text).expect("insert facts");
    GraphEngine::load_rules(&store, &std::fs::read_to_string(&rules).expect("read rules"))
        .expect("load rules");

    let oxi = DeterministicGraph::new().expect("mirror");
    oxi.insert_turtle(&facts_text).expect("insert facts");
    assert_eq!(
        GraphEngine::state_hash(&store).expect("hash before"),
        GraphEngine::state_hash(&oxi).expect("oxi hash before"),
        "identical fact states must hash identically before materialization"
    );

    let outcome = GraphEngine::materialize(&store).expect("materialize");
    insert_derived(&oxi, &outcome.derived);
    assert_eq!(
        GraphEngine::state_hash(&store).expect("hash after"),
        GraphEngine::state_hash(&oxi).expect("oxi hash after"),
        "identical fact states must hash identically after materialization"
    );
}

/// SHACL shapes from disk flag the missing `ex:name` on `ex:rex` and
/// conform after the fix — through the same public `validate_shacl`.
#[test]
fn repoint_shacl_from_disk_flags_and_conforms() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let (facts, _rules, shapes) = write_fixtures(tmp.path());

    let store = GraphLawStore::new().expect("store");
    GraphEngine::insert_turtle(&store, &std::fs::read_to_string(&facts).expect("read facts"))
        .expect("insert facts");
    let shapes_text = std::fs::read_to_string(&shapes).expect("read shapes");

    let bad: ShaclOutcome = GraphEngine::validate_shacl(&store, &shapes_text).expect("shacl");
    assert!(!bad.conforms);
    assert!(bad.violations.iter().any(|v| v.contains("rex")));

    GraphEngine::insert_turtle(
        &store,
        "@prefix ex: <http://example.org/> . ex:rex ex:name \"Rex\" .",
    )
    .expect("insert name");
    let good: ShaclOutcome = GraphEngine::validate_shacl(&store, &shapes_text).expect("shacl 2");
    assert!(good.conforms, "{:?}", good.violations);
}
