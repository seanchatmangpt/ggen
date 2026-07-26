//! Chicago-TDD regression for declarative generation output-root authority.
//!
//! A non-default `[generation].output_dir` must scope every rule output. The
//! test deliberately names an output `ggen.toml`: ignoring the configured root
//! would overwrite the source manifest and change the schema on the next sync.
//! This reproduces the TPOT consumer failure that exposed the engine defect.

use std::path::PathBuf;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

#[test]
fn output_dir_scopes_rule_outputs_and_preserves_the_source_manifest() {
    let dir = TempDir::new().expect("tempdir");
    let manifest = r#"[project]
name = "output-root-regression"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "generated"

[[generation.rules]]
name = "generated-manifest"
query = { inline = "SELECT ?name WHERE { ?subject <http://example.org/name> ?name } ORDER BY ?name" }
template = { inline = "generated={{ results[0].name }}\n" }
output_file = "ggen.toml"
mode = "Overwrite"
"#;
    std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");
    std::fs::write(
        dir.path().join("ontology.ttl"),
        "@prefix ex: <http://example.org/> . ex:item ex:name \"alive\" .\n",
    )
    .expect("write ontology");

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(
        first.written,
        vec![PathBuf::from("generated/ggen.toml")],
        "the configured output root must be part of the receipted target"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("generated/ggen.toml")).expect("generated output"),
        "generated=alive\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("ggen.toml")).expect("source manifest"),
        manifest,
        "generation must never overwrite its own source manifest"
    );

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert!(
        second.written.is_empty(),
        "identical second sync should not rewrite: {:?}",
        second.written
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("ggen.toml")).expect("source manifest"),
        manifest,
        "source authority must survive replay"
    );
}
