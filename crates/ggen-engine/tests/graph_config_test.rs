//! Chicago TDD tests for `ggen_engine::graph` and `ggen_engine::config`.
//!
//! Real collaborators only: in-memory oxigraph stores and real files in a
//! `TempDir`. No mocks, no test doubles.

use ggen_engine::config::{GgenConfig, PackRef};
use ggen_engine::graph::{Delta, DeterministicGraph};

const GROUND_TTL_A: &str = r#"
@prefix ex: <http://example.com/> .
ex:a ex:knows ex:b .
ex:b ex:knows ex:c .
ex:c ex:name "gamma" .
"#;

/// Same triples as `GROUND_TTL_A`, listed in a different order.
const GROUND_TTL_A_PERMUTED: &str = r#"
@prefix ex: <http://example.com/> .
ex:c ex:name "gamma" .
ex:a ex:knows ex:b .
ex:b ex:knows ex:c .
"#;

#[test]
fn state_hash_deterministic_under_insertion_order() {
    let g1 = DeterministicGraph::new().expect("store");
    g1.insert_turtle(GROUND_TTL_A).expect("insert a");

    let g2 = DeterministicGraph::new().expect("store");
    g2.insert_turtle(GROUND_TTL_A_PERMUTED)
        .expect("insert permuted");

    assert_eq!(
        g1.state_hash().expect("hash g1"),
        g2.state_hash().expect("hash g2"),
        "isomorphic graphs inserted in different orders must hash equal"
    );
}

#[test]
fn state_hash_deterministic_under_incremental_insertion_order() {
    let g1 = DeterministicGraph::new().expect("store");
    g1.insert_turtle("@prefix ex: <http://example.com/> . ex:a ex:p ex:b .")
        .expect("insert 1");
    g1.insert_turtle("@prefix ex: <http://example.com/> . ex:c ex:p ex:d .")
        .expect("insert 2");

    let g2 = DeterministicGraph::new().expect("store");
    g2.insert_turtle("@prefix ex: <http://example.com/> . ex:c ex:p ex:d .")
        .expect("insert 2 first");
    g2.insert_turtle("@prefix ex: <http://example.com/> . ex:a ex:p ex:b .")
        .expect("insert 1 second");

    assert_eq!(
        g1.state_hash().expect("hash g1"),
        g2.state_hash().expect("hash g2")
    );
}

#[test]
fn blank_node_graphs_hash_equal_under_renaming() {
    let g1 = DeterministicGraph::new().expect("store");
    g1.insert_turtle(
        r#"@prefix ex: <http://example.com/> .
           _:alpha ex:knows _:beta .
           _:beta ex:name "b" ."#,
    )
    .expect("insert g1");

    let g2 = DeterministicGraph::new().expect("store");
    g2.insert_turtle(
        r#"@prefix ex: <http://example.com/> .
           _:x ex:knows _:y .
           _:y ex:name "b" ."#,
    )
    .expect("insert g2");

    assert_eq!(
        g1.state_hash().expect("hash g1"),
        g2.state_hash().expect("hash g2"),
        "blank-node renaming must not change the state hash"
    );
}

#[test]
fn delta_compute_apply_round_trip_restores_target_hash() {
    let baseline = DeterministicGraph::new().expect("store");
    baseline
        .insert_turtle(GROUND_TTL_A)
        .expect("insert baseline");

    let target = DeterministicGraph::new().expect("store");
    target
        .insert_turtle(
            r#"@prefix ex: <http://example.com/> .
               ex:a ex:knows ex:b .
               ex:c ex:name "gamma prime" .
               ex:d ex:knows ex:a ."#,
        )
        .expect("insert target");

    let delta = Delta::compute(&baseline, &target).expect("compute delta");
    assert!(!delta.additions.is_empty());
    assert!(!delta.deletions.is_empty());

    delta.apply(&baseline).expect("apply delta");
    assert_eq!(
        baseline.state_hash().expect("hash baseline"),
        target.state_hash().expect("hash target"),
        "apply(compute(baseline, target)) must reproduce the target state hash"
    );

    // Delta hash is a pure function of its sorted content.
    assert_eq!(delta.hash(), delta.hash());
    let empty = Delta {
        additions: vec![],
        deletions: vec![],
    };
    assert_ne!(delta.hash(), empty.hash());
}

#[test]
fn query_returns_solutions_from_real_store() {
    let g = DeterministicGraph::new().expect("store");
    let inserted = g.insert_turtle(GROUND_TTL_A).expect("insert");
    assert_eq!(inserted, 3);

    let results = g
        .query("SELECT ?s WHERE { ?s <http://example.com/knows> ?o } ORDER BY ?s")
        .expect("query");
    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        let rows: Vec<_> = solutions.collect::<Result<Vec<_>, _>>().expect("rows");
        assert_eq!(rows.len(), 2);
    } else {
        panic!("expected SELECT solutions");
    }
}

#[test]
fn config_parses_valid_ggen_toml_from_real_file() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let path = dir.path().join("ggen.toml");
    std::fs::write(
        &path,
        r#"
[project]
name = "demo"

[ontology]
source = "schema/demo.ttl"

[ontology.prefixes]
ex = "http://example.com/"

[packs]
local_pack = { path = "packs/local" }
remote_pack = { git = "https://github.com/seanchatmangpt/pack", version = "1.0.0" }

[templates]
dir = "templates"
"#,
    )
    .expect("write ggen.toml");

    let cfg = GgenConfig::load(&path).expect("load config");
    assert_eq!(cfg.project.name, "demo");
    assert_eq!(
        cfg.ontology.source,
        std::path::PathBuf::from("schema/demo.ttl")
    );
    assert_eq!(
        cfg.ontology.prefixes.get("ex").map(String::as_str),
        Some("http://example.com/")
    );
    assert_eq!(
        cfg.packs.get("local_pack"),
        Some(&PackRef::Path {
            path: "packs/local".into(),
            extra_ontologies: Vec::new(),
            lock: true
        })
    );
    assert_eq!(
        cfg.packs.get("remote_pack"),
        Some(&PackRef::Git {
            git: "https://github.com/seanchatmangpt/pack".into(),
            version: "1.0.0".into()
        })
    );
    assert_eq!(cfg.templates.dir, std::path::PathBuf::from("templates"));
}

#[test]
fn config_unknown_key_is_rejected() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let path = dir.path().join("ggen.toml");
    std::fs::write(
        &path,
        r#"
[project]
name = "demo"
sneaky_extra = true

[ontology]
source = "schema/demo.ttl"

[templates]
dir = "templates"
"#,
    )
    .expect("write ggen.toml");

    let err = GgenConfig::load(&path).expect_err("unknown key must fail");
    let msg = err.to_string();
    assert!(
        msg.contains("FM-CONFIG-002"),
        "expected FM-CONFIG-002, got: {msg}"
    );
}

#[test]
fn config_missing_file_fails_closed() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let err = GgenConfig::load(&dir.path().join("nope.toml")).expect_err("missing file");
    assert!(err.to_string().contains("FM-CONFIG-001"), "got: {err}");
}
