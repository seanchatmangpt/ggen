//! Chicago-TDD end-to-end test for LEAD 1 of the 2026-08-03 unverified-leads
//! audit: "`shape:` is hashed but never SHACL-validated while a working
//! engine sits unused."
//!
//! Confirmed real before this test existed: `admit_shape_files` in
//! `crate::sync` read every `shape:` file, hashed its bytes into the sync
//! receipt's input closure (a "governing" input, tracked for drift across
//! syncs), but never called `GraphEngine::validate_shacl` against it --
//! despite `praxis-graphlaw`'s real SHACL engine being fully reachable
//! (proven by `ggen graph validate`/`ggen law validate` and by
//! `graph.rs`'s own `graphlaw_validate_shacl_flags_focus_node` unit test).
//! A `shape:` declaration was therefore existence-checked only: a project
//! could declare `ex:DogShape` requiring `ex:name`, feed it data violating
//! that exact constraint, and `ggen sync run` would still succeed and write
//! a receipt as if the shape had been enforced.
//!
//! Real filesystem, real sync pipeline, real SHACL engine -- no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// `ex:rex a ex:Dog .` with NO `ex:name` -- violates the shape below, which
/// requires `sh:minCount 1` on `ex:name`. Modeled directly on
/// `graph.rs`'s own `graphlaw_validate_shacl_flags_focus_node` unit test, so
/// this is a known-good SHACL pattern already proven against this exact
/// engine, not a newly-invented one.
const ONTOLOGY_VIOLATING: &str = r"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog .
";

const SHAPE_DOG: &str = r"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
ex:DogShape a sh:NodeShape ;
    sh:targetClass ex:Dog ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .
";

fn scaffold(root: &Path, ontology: &str) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ontology).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(root.join("shapes")).expect("mkdir shapes");
    std::fs::write(root.join("shapes/dog.ttl"), SHAPE_DOG).expect("write shape");
}

fn write_template(root: &Path, name: &str, content: &str) {
    std::fs::write(root.join("templates").join(name), content).expect("write template");
}

#[test]
fn sync_refuses_when_declared_shape_is_violated() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), ONTOLOGY_VIOLATING);
    write_template(
        dir.path(),
        "s.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes/dog.ttl\n---\nbody\n",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("sync must refuse: ex:rex has no ex:name, violating ex:DogShape");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-025"), "{msg}");
    assert!(
        msg.contains("rex"),
        "violation must name the focus node: {msg}"
    );
    assert!(
        !dir.path().join("out.txt").exists(),
        "no output should be written when the declared shape is violated"
    );
    assert!(
        !dir.path().join(".ggen-v2/receipt.json").exists(),
        "no receipt should be written when the declared shape is violated"
    );
}

#[test]
fn sync_passes_when_declared_shape_conforms() {
    let dir = TempDir::new().expect("tempdir");
    // Same shape, but data that satisfies it (`ex:name` present).
    scaffold(
        dir.path(),
        "@prefix ex: <http://example.org/> .\nex:rex a ex:Dog ; ex:name \"Rex\" .\n",
    );
    write_template(
        dir.path(),
        "s.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes/dog.ttl\n---\nbody\n",
    );

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed: ex:rex ex:name \"Rex\" conforms to ex:DogShape");
    assert!(dir.path().join("out.txt").exists());
}
