//! Subprocess-level (real `ggen` binary, `chicago_tdd_tools::cli_proof::CliHarness`
//! -- no mocks) companion to `shape_shacl_enforcement_e2e.rs`.
//!
//! That file already proves both directions of the `shape:` SHACL gate at
//! the library level (`ggen_engine::sync::sync(...)` called in-process):
//! `sync_refuses_when_declared_shape_is_violated` and
//! `sync_passes_when_declared_shape_conforms`. Neither crosses the real CLI
//! boundary -- they never spawn the `ggen` binary, so they cannot prove the
//! error actually reaches a user's terminal (real stderr text, real
//! non-zero exit code) or that a real `ggen sync run` invocation actually
//! writes the file on the conforming path. This file closes that gap,
//! following `hygen_parity_e2e.rs`'s pattern (`CliHarness::cargo_bin("ggen")`
//! spawning the real compiled binary).
//!
//! Same known-good SHACL shape/ontology pair as
//! `shape_shacl_enforcement_e2e.rs` (itself modeled on `graph.rs`'s own
//! `graphlaw_validate_shacl_flags_focus_node` unit test) -- not a
//! newly-invented fixture.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use chicago_tdd_tools::cli_proof::CliHarness;
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
/// requires `sh:minCount 1` on `ex:name`.
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

fn run_sync(root: &Path) -> chicago_tdd_tools::cli_proof::CliOutput {
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(root)
        .run()
        .expect("spawn ggen sync run")
}

#[test]
fn sync_run_cli_refuses_when_declared_shape_is_violated() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), ONTOLOGY_VIOLATING);
    write_template(
        dir.path(),
        "s.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes/dog.ttl\n---\nbody\n",
    );

    let output = run_sync(dir.path());
    let _ = output
        .assert_failure()
        .assert_stderr_contains("FM-TPL-025")
        .assert_stderr_contains("rex");
    assert!(
        !dir.path().join("out.txt").exists(),
        "no output should be written when the declared shape is violated: stdout={} stderr={}",
        output.stdout,
        output.stderr
    );
    assert!(
        !dir.path().join(".ggen-v2/receipt.json").exists(),
        "no receipt should be written when the declared shape is violated"
    );
}

#[test]
fn sync_run_cli_succeeds_when_declared_shape_conforms() {
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

    let output = run_sync(dir.path());
    let _ = output.assert_success();
    assert!(
        dir.path().join("out.txt").exists(),
        "out.txt should be written when the declared shape conforms: stdout={} stderr={}",
        output.stdout,
        output.stderr
    );
}
