//! Real-boundary coverage for ontology-driven output-phase frontmatter.
//!
//! These tests cross the filesystem and subprocess boundaries. They prove
//! that Tera specialization applies to structural slots, idempotence,
//! lifecycle hooks, and shape paths—not only to the output path and body.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "frontmatter-maximalism"

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
    std::fs::create_dir_all(root.join("shapes")).expect("mkdir shapes");
}

fn write_template(root: &Path, content: &str) {
    std::fs::write(root.join("templates/maximal.tmpl"), content).expect("write template");
}

#[test]
fn row_context_projects_structural_hooks_and_shape_paths() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("targets/alpha.txt"),
        "header\n// SLOT:alpha\nfooter\n",
    )
    .expect("seed target");
    std::fs::write(dir.path().join("shapes/alpha.ttl"), "# governing shape\n")
        .expect("write shape");

    write_template(
        dir.path(),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
    ORDER BY ?name
inject: true
before: "// SLOT:{{ row.name }}"
skip_if: "generated {{ row.name }}"
sh_before: "echo before-{{ row.name }} >> hooks.log"
sh_after: "echo after-{{ row.name }} >> hooks.log"
shape:
  - "shapes/{{ row.name }}.ttl"
determinism: true
---
generated {{ row.name }}
"#,
    );

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(
        first.written,
        vec![std::path::PathBuf::from("targets/alpha.txt")]
    );

    let target = std::fs::read_to_string(dir.path().join("targets/alpha.txt"))
        .expect("read injected target");
    let generated = target.find("generated alpha").expect("generated text");
    let slot = target.find("// SLOT:alpha").expect("slot marker");
    assert!(
        generated < slot,
        "content must be injected before the rendered slot"
    );

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert!(
        second.written.is_empty(),
        "skip_if must make the second sync a no-op"
    );
    assert_eq!(second.skipped.len(), 1);

    let hooks =
        std::fs::read_to_string(dir.path().join("hooks.log")).expect("read real hook evidence");
    assert_eq!(
        hooks.lines().collect::<Vec<_>>(),
        vec!["before-alpha", "after-alpha", "before-alpha"],
        "sh_before runs before the later skip decision; sh_after runs only after mutation"
    );
}

#[test]
fn rendered_shape_path_refuses_before_any_hook_or_write() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("targets/alpha.txt"),
        "header\n// SLOT:alpha\nfooter\n",
    )
    .expect("seed target");

    write_template(
        dir.path(),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
    ORDER BY ?name
inject: true
before: "// SLOT:{{ row.name }}"
sh_before: "echo should-not-run >> hooks.log"
shape:
  - "shapes/{{ row.name }}.ttl"
---
generated {{ row.name }}
"#,
    );

    let error =
        sync(dir.path(), SyncOptions::default()).expect_err("missing rendered shape must refuse");
    assert!(error.to_string().contains("FM-TPL-014"), "{error}");
    assert!(!dir.path().join("hooks.log").exists());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("targets/alpha.txt"))
            .expect("target remains readable"),
        "header\n// SLOT:alpha\nfooter\n"
    );
}
