//! End-to-end tests for the admitted wasm4pm graph facts (PROJ-305
//! follow-through): the real `packs/wasm4pm-facts-pack/ontology.ttl` (55
//! cognition breeds + 60 PI algorithms), the real N3 standing rule
//! (`ontology/rules/breed_standing.n3`), and the real registry-report
//! template — no fixture forks of the admitted files.

use std::path::{Path, PathBuf};

use ggen_engine::graph::{GraphEngine as _, GraphLawStore};
use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn read(rel: &str) -> String {
    let p = repo_root().join(rel);
    std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("reading {}: {e}", p.display()))
}

/// Scaffold a minimal project whose graph is exactly the admitted wasm4pm
/// facts, with the real standing rule and the real registry template.
fn scaffold(root: &Path) {
    const GGEN_TOML: &str = r#"
[project]
name = "wasm4pm-facts"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[law]
rules = ["rules/breed_standing.n3"]
"#;
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(
        root.join("ontology.ttl"),
        read("packs/wasm4pm-facts-pack/ontology.ttl"),
    )
    .expect("write ontology");
    std::fs::create_dir_all(root.join("rules")).expect("mkdir rules");
    std::fs::write(
        root.join("rules/breed_standing.n3"),
        read("ontology/rules/breed_standing.n3"),
    )
    .expect("write rule");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(
        root.join("templates/breed_algorithm_registry.md.tmpl"),
        read("packs/wasm4pm-facts-pack/templates/breed_algorithm_registry.md.tmpl"),
    )
    .expect("write template");
}

/// The standing rule derives law:standing "EvidenceBound" for every cited
/// breed: derived-triple count > 0, and the allen_temporal breed derives the
/// exact expected standing triple.
#[test]
fn standing_rule_derives_evidence_bound_for_cited_breeds() {
    let engine = GraphLawStore::new().expect("engine");
    engine
        .insert_turtle(&read("packs/wasm4pm-facts-pack/ontology.ttl"))
        .expect("insert facts");
    let loaded = engine
        .load_rules(&read("ontology/rules/breed_standing.n3"))
        .expect("load standing rule");
    assert!(loaded > 0, "standing rule file must load at least one rule");

    let outcome = engine.materialize().expect("materialize");
    assert!(
        !outcome.derived.is_empty(),
        "standing rule must derive at least one triple"
    );

    let quads = engine.canonical_quads().expect("canonical quads");
    let expected = "<https://wasm4pm.dev/ns#Breed_allen_temporal> \
                    <http://seanchatmangpt.github.io/praxis/law#standing> \
                    \"EvidenceBound\"";
    assert!(
        quads.iter().any(|q| q.contains(expected)),
        "allen_temporal must derive law:standing \"EvidenceBound\"; derived {} triples",
        outcome.derived.len()
    );
    // Every one of the 55 breeds carries a citation, so every one derives.
    let standing_count = quads
        .iter()
        .filter(|q| q.contains("<http://seanchatmangpt.github.io/praxis/law#standing>"))
        .count();
    assert_eq!(standing_count, 55, "all 55 cited breeds derive standing");
}

/// The registry report renders from the admitted facts with all 55 breeds
/// and 60 algorithms, and breed status shows the law-derived standing.
#[test]
fn registry_report_lists_all_breeds_and_algorithms() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    assert_eq!(
        report.written,
        vec![PathBuf::from(
            "docs/releases/v26.7.6/BREED_ALGORITHM_REGISTRY.md"
        )]
    );
    let out = std::fs::read_to_string(
        dir.path()
            .join("docs/releases/v26.7.6/BREED_ALGORITHM_REGISTRY.md"),
    )
    .expect("read report");
    assert!(out.contains("## Cognition breeds (55)"), "breed count");
    assert!(
        out.contains("## Process-intelligence algorithms (60)"),
        "algorithm count"
    );
    assert!(
        out.contains("| `allen_temporal` | AllenTemporal | EvidenceBound |"),
        "allen_temporal row carries the law-derived standing"
    );
    assert!(
        out.contains("| `a_star` | AStar | discovery | REPLAYABLE |"),
        "a_star algorithm row present"
    );
}

/// Determinism: two independent syncs over the same admitted facts produce
/// byte-identical output and the same graph hash.
#[test]
fn sync_over_breeds_graph_is_deterministic() {
    let run = || {
        let dir = TempDir::new().expect("tempdir");
        scaffold(dir.path());
        let report = sync(dir.path(), SyncOptions::default()).expect("sync");
        let bytes = std::fs::read(
            dir.path()
                .join("docs/releases/v26.7.6/BREED_ALGORITHM_REGISTRY.md"),
        )
        .expect("read report");
        (report.graph_hash_hex, bytes)
    };
    let (hash_a, out_a) = run();
    let (hash_b, out_b) = run();
    assert_eq!(hash_a, hash_b, "graph hash must be run-independent");
    assert_eq!(out_a, out_b, "rendered registry must be byte-identical");
}
