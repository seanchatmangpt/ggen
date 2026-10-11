//! Chicago-TDD tests for `ProjectIndex::from_root_with_overlay`'s ggen.toml
//! schema dispatch: the shared `ggen_config::classify_ggen_toml` classifier
//! gates every arm (declarative-rules parsed, frontmatter -> explicit empty
//! index, ambiguous/unsupported -> typed `IndexError`, malformed -> parse
//! error). Real filesystem (`tempfile::TempDir`), no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::HashMap;
use std::path::Path;

use ggen_lsp::project_index::{IndexError, ProjectIndex};

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    std::fs::create_dir_all(path.parent().expect("parent")).expect("mkdir");
    std::fs::write(path, content).expect("write");
}

fn empty_overlay() -> HashMap<std::path::PathBuf, String> {
    HashMap::new()
}

#[test]
fn declarative_rules_manifest_is_indexed_with_rule_entries() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[[generation.rules]]\nname = \"names\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { inline = \"x\" }\noutput_file = \"out.txt\"\n",
    );

    let index = ProjectIndex::from_root_with_overlay(dir.path(), &empty_overlay())
        .expect("declarative-rules manifest must index");
    assert_eq!(index.root, dir.path().to_path_buf());
    assert_eq!(index.rule_entries.len(), 1, "one generation rule expected");
}

#[test]
fn frontmatter_manifest_yields_explicit_empty_index_not_error() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );

    let index = ProjectIndex::from_root_with_overlay(dir.path(), &empty_overlay())
        .expect("frontmatter manifest is a valid project, not an error");
    assert!(
        index.rule_entries.is_empty(),
        "frontmatter schema yields an empty index, got {:?}",
        index.rule_entries
    );
}

#[test]
fn ambiguous_manifest_is_typed_refusal() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[ai]\nprovider = \"openai\"\n",
    );

    match ProjectIndex::from_root_with_overlay(dir.path(), &empty_overlay()) {
        Err(IndexError::AmbiguousSchema { matched, .. }) => {
            assert!(!matched.is_empty(), "ambiguous refusal names its markers");
        }
        other => panic!("expected AmbiguousSchema, got {other:?}"),
    }
}

#[test]
fn unsupported_manifest_is_typed_refusal() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[some_other_tool]\nkey = \"value\"\n",
    );

    match ProjectIndex::from_root_with_overlay(dir.path(), &empty_overlay()) {
        Err(IndexError::UnsupportedSchema { .. }) => {}
        other => panic!("expected UnsupportedSchema, got {other:?}"),
    }
}

#[test]
fn malformed_manifest_is_parse_refusal() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    write(dir.path(), "ggen.toml", "not [ valid toml");

    match ProjectIndex::from_root_with_overlay(dir.path(), &empty_overlay()) {
        Err(IndexError::ManifestParse { .. }) => {}
        other => panic!("expected ManifestParse, got {other:?}"),
    }
}

#[test]
fn missing_manifest_is_not_found_refusal() {
    let dir = tempfile::TempDir::new().expect("tempdir");

    match ProjectIndex::from_root_with_overlay(dir.path(), &empty_overlay()) {
        Err(IndexError::ManifestNotFound { .. }) => {}
        other => panic!("expected ManifestNotFound, got {other:?}"),
    }
}
