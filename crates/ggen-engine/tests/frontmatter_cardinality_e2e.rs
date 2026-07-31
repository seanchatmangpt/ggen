//! Explicit frontmatter projection cardinality — real graph, Tera, filesystem,
//! determinism recheck, and refusal boundaries.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "cardinality"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:a ex:name "alpha" .
ex:z ex:name "zeta" .
ex:aux ex:aux "not-the-driver" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("manifest");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("ontology");
    std::fs::create_dir_all(root.join("templates")).expect("templates");
}

fn template(root: &Path, source: &str) {
    std::fs::write(root.join("templates/cardinality.tmpl"), source).expect("template");
}

fn run(root: &Path) -> ggen_engine::sync::SyncReport {
    sync(root, SyncOptions::default()).expect("sync")
}

const QUERIES: &str = r#"
sparql:
  00_aux: |
    PREFIX ex: <http://example.org/>
    SELECT ?aux WHERE { ?s ex:aux ?aux } ORDER BY ?aux
  entities: |
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name
for_each: entities
"#;

#[test]
fn explicit_driver_aggregates_static_target_in_row_order() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        &format!(
            "---\nto: registry.txt\n{QUERIES}force: true\ndeterminism: true\n---\n{{{{ row.name }}}}\n"
        ),
    );

    let report = run(dir.path());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("registry.txt")).expect("output"),
        "alpha\nzeta\n"
    );
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("registry.txt")]
    );
}

#[test]
fn explicit_driver_fans_out_dynamic_targets() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        &format!(
            "---\nto: \"out/{{{{ row.name }}}}.txt\"\n{QUERIES}determinism: true\n---\n{{{{ row.name }}}}\n"
        ),
    );

    run(dir.path());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/alpha.txt")).expect("alpha"),
        "alpha\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/zeta.txt")).expect("zeta"),
        "zeta\n"
    );
}

#[test]
fn missing_driver_refuses_before_hook_or_write() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        "---\nto: out.txt\nsparql:\n  entities: SELECT ?name WHERE { ?s <http://example.org/name> ?name }\nfor_each: missing\nsh_before: \"echo ran > hook.log\"\n---\nbody\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-019"), "{err}");
    assert!(!dir.path().join("hook.log").exists());
    assert!(!dir.path().join("out.txt").exists());
}

#[test]
fn scalar_driver_refuses() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        "---\nto: out.txt\nsparql:\n  enabled: ASK { ?s <http://example.org/name> ?name }\nfor_each: enabled\n---\nbody\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-019"), "{err}");
}

#[test]
fn static_aggregate_refuses_row_varying_lifecycle_law() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        &format!(
            "---\nto: registry.txt\n{QUERIES}sh_after: \"echo {{{{ row.name }}}} >> hook.log\"\n---\n{{{{ row.name }}}}\n"
        ),
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-020"), "{err}");
    assert!(!dir.path().join("registry.txt").exists());
    assert!(!dir.path().join("hook.log").exists());
}
