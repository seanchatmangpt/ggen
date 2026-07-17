//! End-to-end tests for the GraphLaw law-state engine inside the sync
//! pipeline: real filesystem, real praxis-graphlaw reasoner, real Tera
//! rendering — no mocks.
//!
//! The load-bearing test is
//! [`when_guard_passes_only_after_n3_materialization`]: a template whose
//! `when:` ASK is only satisfiable by a rule-derived fact refuses under the
//! oxigraph engine (typed `[FM-LAW-*]`) and renders under the default
//! GraphLaw engine — the proof the roxi-fork reasoner is in the loop, not
//! decoration.

use std::path::Path;

use ggen_engine::sync::{sync, EngineKind, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const GGEN_TOML_WITH_RULES: &str = r#"
[project]
name = "lawdemo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[law]
rules = ["rules/animal.n3"]
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog .
ex:rex ex:name "Rex" .
"#;

/// N3 rule: every Dog is an Animal. `ex:rex a ex:Animal` exists ONLY as a
/// derived fact — no engine that skips materialization can satisfy the
/// `when:` guard below.
const RULE_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => {?s a ex:Animal}.";

/// Template gated on the derived fact.
const TEMPLATE_WHEN_DERIVED: &str = "---\nto: out/animals.txt\nwhen: ASK { ?s a <http://example.org/Animal> }\nsparql:\n  animals: SELECT ?s WHERE { ?s a <http://example.org/Animal> } ORDER BY ?s\n---\n{% for row in animals %}{{ row.s }}\n{% endfor %}";

fn scaffold(root: &Path, ggen_toml: &str, template: &str) {
    std::fs::write(root.join("ggen.toml"), ggen_toml).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), template).expect("write template");
    std::fs::create_dir_all(root.join("rules")).expect("mkdir rules");
    std::fs::write(root.join("rules/animal.n3"), RULE_N3).expect("write rule");
}

/// THE proof the GraphLaw reasoner is in the loop: the same fixture refuses
/// under oxigraph (no rule support — typed FM-LAW refusal at the law stage)
/// and renders under GraphLaw (the `when:` ASK passes only because
/// materialization derived `ex:rex a ex:Animal`).
#[test]
fn when_guard_passes_only_after_n3_materialization() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), GGEN_TOML_WITH_RULES, TEMPLATE_WHEN_DERIVED);

    // Oxigraph engine: configured rules are a typed refusal, never silence.
    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            engine: EngineKind::Oxigraph,
        },
    )
    .expect_err("oxigraph engine must refuse [law].rules");
    assert!(err.to_string().contains("FM-LAW-001"), "{err}");
    assert!(
        !dir.path().join("out/animals.txt").exists(),
        "refused run must write nothing"
    );

    // Default (GraphLaw) engine: materialization derives the fact, the
    // guard passes, the file lands with the derived subject in it.
    let report = sync(dir.path(), SyncOptions::default()).expect("graphlaw sync");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/animals.txt")]
    );
    let content = std::fs::read_to_string(dir.path().join("out/animals.txt")).expect("read output");
    assert_eq!(content, "http://example.org/rex\n");
}

/// Without a `[law]` table the two engines are interchangeable: same
/// outputs, same graph hash (A/B determinism).
#[test]
fn engines_agree_when_no_law_configured() {
    const PLAIN_TOML: &str = "\n[project]\nname = \"lawdemo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n";
    const PLAIN_TEMPLATE: &str = "---\nto: out/names.txt\nsparql:\n  names: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in names %}{{ row.name }}\n{% endfor %}";

    let run = |engine: EngineKind| {
        let dir = TempDir::new().expect("tempdir");
        scaffold(dir.path(), PLAIN_TOML, PLAIN_TEMPLATE);
        let report = sync(
            dir.path(),
            SyncOptions {
                dry_run: false,
                engine,
            },
        )
        .expect("sync");
        let content =
            std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
        (report.graph_hash_hex, content)
    };
    let (hash_law, out_law) = run(EngineKind::GraphLaw);
    let (hash_oxi, out_oxi) = run(EngineKind::Oxigraph);
    assert_eq!(out_law, out_oxi, "outputs must be engine-independent");
    assert_eq!(hash_law, hash_oxi, "graph hash must be engine-independent");
}

/// A SHACL shapes file whose constraint the ontology violates is a typed
/// FM-LAW refusal naming the focus node; nothing is written.
#[test]
fn shacl_violation_refuses_sync_naming_focus_node() {
    const TOML_SHAPES: &str = "\n[project]\nname = \"lawdemo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[law]\nshapes = [\"shapes/dog.ttl\"]\n";
    const SHAPES: &str = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
ex:DogShape a sh:NodeShape ;
    sh:targetClass ex:Dog ;
    sh:property [ sh:path ex:license ; sh:minCount 1 ] .
"#;
    const PLAIN_TEMPLATE: &str = "---\nto: out/names.txt\n---\nstatic\n";

    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), TOML_SHAPES, PLAIN_TEMPLATE);
    std::fs::create_dir_all(dir.path().join("shapes")).expect("mkdir shapes");
    std::fs::write(dir.path().join("shapes/dog.ttl"), SHAPES).expect("write shapes");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("unlicensed dog must refuse the SHACL gate");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-013"), "{msg}");
    assert!(
        msg.contains("rex"),
        "refusal must name the focus node: {msg}"
    );
    assert!(
        !dir.path().join("out/names.txt").exists(),
        "gate must precede writes"
    );
}

/// A violated denial rule (`{ body } => false.`) is a typed FM-LAW refusal;
/// nothing is written.
#[test]
fn denial_violation_refuses_sync() {
    const TOML_DENIAL: &str = "\n[project]\nname = \"lawdemo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[law]\nrules = [\"rules/animal.n3\", \"rules/no-dogs.n3\"]\n";
    const DENIAL_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => false.";
    const PLAIN_TEMPLATE: &str = "---\nto: out/names.txt\n---\nstatic\n";

    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), TOML_DENIAL, PLAIN_TEMPLATE);
    std::fs::write(dir.path().join("rules/no-dogs.n3"), DENIAL_N3).expect("write denial");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("asserted dog under a no-dogs denial must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-011"), "{msg}");
    assert!(msg.contains("DENIED"), "{msg}");
    assert!(
        !dir.path().join("out/names.txt").exists(),
        "gate must precede writes"
    );
}

/// Two-run determinism under the law stage: the same fixture synced twice
/// produces the identical graph hash and payload, and the second receipt
/// chains onto the first (verified head, linear history).
#[test]
fn two_runs_same_fixture_same_graph_hash_and_valid_chain() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), GGEN_TOML_WITH_RULES, TEMPLATE_WHEN_DERIVED);

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    let receipt1: SyncReceipt = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("receipt 1"),
    )
    .expect("parse receipt 1");

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    let receipt2: SyncReceipt = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("receipt 2"),
    )
    .expect("parse receipt 2");

    assert_eq!(
        first.graph_hash_hex, second.graph_hash_hex,
        "materialized graph hash must be deterministic across runs"
    );
    assert_eq!(
        receipt1.payload.graph_hash, receipt2.payload.graph_hash,
        "receipt-bound graph hash must be deterministic"
    );
    assert_eq!(
        receipt1.payload.outputs, receipt2.payload.outputs,
        "output hashes must be identical across runs"
    );
    assert_eq!(
        receipt2.record.prev_chain_hash_hex, receipt1.record.chain_hash_hex,
        "second receipt must chain onto the first"
    );
    let recomputed = receipt2.record.recompute_chain_hash().expect("recompute");
    let hex: String = recomputed.iter().map(|b| format!("{b:02x}")).collect();
    assert_eq!(
        hex, receipt2.record.chain_hash_hex,
        "chain head must verify"
    );
}
