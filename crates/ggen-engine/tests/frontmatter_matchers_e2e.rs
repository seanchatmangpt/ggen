//! Real-boundary coverage for typed, Tera-rendered host-content selectors.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "frontmatter-matchers"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alpha ex:name "alpha" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(root.join("targets")).expect("mkdir targets");
}

#[test]
fn minimal_structured_selector_uses_sane_defaults_and_row_rendering() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("targets/alpha.txt"),
        "header\n// SLOT:alpha\nfooter\n",
    )
    .expect("seed target");
    std::fs::write(
        dir.path().join("templates/matcher.tmpl"),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
    ORDER BY ?name
inject: true
before:
  pattern: "// SLOT:{{ row.name }}"
skip_if:
  pattern: "generated {{ row.name }}"
determinism: true
---
generated {{ row.name }}
"#,
    )
    .expect("write template");

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(first.written.len(), 1);
    assert!(first
        .decisions
        .get("targets/alpha.txt")
        .expect("decision")
        .contains("matcher=contains"));

    let target =
        std::fs::read_to_string(dir.path().join("targets/alpha.txt")).expect("read target");
    assert_eq!(target, "header\ngenerated alpha\n// SLOT:alpha\nfooter\n");

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert!(second.written.is_empty());
    assert_eq!(second.skipped.len(), 1);
}

#[test]
fn invalid_regex_refuses_before_sh_before() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("targets/alpha.txt"), "// SLOT:alpha\n").expect("seed target");
    std::fs::write(
        dir.path().join("templates/matcher.tmpl"),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
inject: true
before:
  matcher: regex
  pattern: "("
sh_before: "echo should-not-run >> hooks.log"
---
generated
"#,
    )
    .expect("write template");

    let error = sync(dir.path(), SyncOptions::default()).expect_err("invalid regex must refuse");
    assert!(error.to_string().contains("FM-WRITE-008"), "{error}");
    assert!(!dir.path().join("hooks.log").exists());
}

#[test]
fn unique_cardinality_refuses_before_sh_before() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("targets/alpha.txt"),
        "// SLOT:alpha\nbody\n// SLOT:alpha\n",
    )
    .expect("seed target");
    std::fs::write(
        dir.path().join("templates/matcher.tmpl"),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
inject: true
before:
  matcher: exact
  pattern: "// SLOT:{{ row.name }}"
  occurrence: unique
sh_before: "echo should-not-run >> hooks.log"
---
generated
"#,
    )
    .expect("write template");

    let error = sync(dir.path(), SyncOptions::default()).expect_err("duplicate unique must refuse");
    assert!(error.to_string().contains("FM-WRITE-008"), "{error}");
    assert!(!dir.path().join("hooks.log").exists());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("targets/alpha.txt")).expect("read"),
        "// SLOT:alpha\nbody\n// SLOT:alpha\n"
    );
}
