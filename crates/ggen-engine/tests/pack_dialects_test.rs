#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Chicago tests for pack.toml `[[capabilities.dialects]]` parsing and
//! validation (`FM-PACK-021`). Real packs on real tempdir filesystems,
//! resolved through the public `crate::pack::resolve` path; assertions on
//! the returned state, never on interactions.

use std::fs;
use std::path::{Path, PathBuf};

use ggen_engine::config::GgenConfig;
use ggen_engine::pack::{self, DialectGlob};

/// Write a minimal valid pack (pack.toml + ontology.ttl + one template)
/// under `root/<name>/`, with the caller's `[capabilities]` TOML table
/// spliced in. The pack lives under `<root>/consumer/<name>/`; the returned
/// path is the consumer project dir (the resolve config root).
fn write_pack(root: &Path, name: &str, capabilities_toml: &str) -> PathBuf {
    let pack_root = root.join("consumer").join(name);
    fs::create_dir_all(pack_root.join("templates")).expect("create pack tree");
    fs::write(
        pack_root.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.1.0\"\ndescription = \"test pack\"\n\
             {capabilities_toml}\n"
        ),
    )
    .expect("write pack.toml");
    fs::write(
        pack_root.join("ontology.ttl"),
        "@prefix ex: <http://e/> . ex:a ex:b ex:c .",
    )
    .expect("write ontology.ttl");
    fs::write(pack_root.join("templates").join("main.tera"), "{{ name }}").expect("write template");
    root.join("consumer")
}

fn config_with(root: &Path, name: &str) -> GgenConfig {
    let project = root.join("consumer");
    fs::create_dir_all(project.join("templates")).expect("mkdir consumer templates");
    fs::write(project.join("ontology.ttl"), "").expect("write consumer ontology");
    fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"dialect-consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{name} = {{ path = \"{name}\" }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    GgenConfig::load(&project.join("ggen.toml")).expect("load config")
}

fn err_contains(root: &Path, name: &str, capabilities_toml: &str, needle: &str) -> String {
    let cfg_root = write_pack(root, name, capabilities_toml);
    let config = config_with(&cfg_root, name);
    match pack::resolve(&config, &cfg_root) {
        Ok(packs) => panic!("expected refusal naming `{needle}`, got resolved packs: {packs:?}"),
        Err(err) => {
            let msg = format!("{err}");
            assert!(
                msg.contains(needle),
                "expected message containing `{needle}`, got: {msg}"
            );
            msg
        }
    }
}

#[test]
fn absent_dialects_key_synthesizes_default_turtle_glob() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let cfg_root = write_pack(tmp.path(), "default_pack", "");
    let config = config_with(&cfg_root, "default_pack");

    let packs = pack::resolve(&config, &cfg_root).expect("resolve succeeds");
    assert_eq!(packs.len(), 1);
    assert_eq!(
        packs[0].dialects,
        vec![DialectGlob {
            glob: "*.ttl".to_string(),
            dialect: "turtle".to_string()
        }],
        "absent `dialects` key must synthesize the backward-compatible *.ttl -> turtle default"
    );
}

#[test]
fn explicit_globs_parse_with_declared_dialects() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let cfg_root = write_pack(
        tmp.path(),
        "explicit_pack",
        "[capabilities]\ntypes = [\"test\"]\n\
         [[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"turtle\"\n\
         [[capabilities.dialects]]\nglob = \"graphs/*.trig\"\ndialect = \"trig\"\n",
    );
    let config = config_with(&cfg_root, "explicit_pack");

    let packs = pack::resolve(&config, &cfg_root).expect("resolve succeeds");
    assert_eq!(packs.len(), 1);
    assert_eq!(
        packs[0].dialects,
        vec![
            DialectGlob {
                glob: "*.ttl".to_string(),
                dialect: "turtle".to_string()
            },
            DialectGlob {
                glob: "graphs/*.trig".to_string(),
                dialect: "trig".to_string()
            },
        ],
        "declared globs must round-trip in declaration order"
    );
}

#[test]
fn every_closed_vocabulary_dialect_is_accepted() {
    for dialect in [
        "turtle", "trig", "ntriples", "nquads", "n3", "rdf-xml", "json-ld",
    ] {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let cfg_root = write_pack(
            tmp.path(),
            "vocab_pack",
            &format!(
                "[capabilities]\n[[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"{dialect}\"\n"
            ),
        );
        let config = config_with(&cfg_root, "vocab_pack");
        let packs = pack::resolve(&config, &cfg_root)
            .unwrap_or_else(|e| panic!("dialect `{dialect}` must be admitted: {e}"));
        assert_eq!(packs[0].dialects[0].dialect, dialect);
    }
}

#[test]
fn unknown_dialect_refused_fm_pack_021() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let msg = err_contains(
        tmp.path(),
        "bad_dialect",
        "[capabilities]\n[[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"turtlelite\"\n",
        "FM-PACK-021",
    );
    assert!(
        msg.contains("turtlelite"),
        "refusal must name the offending dialect: {msg}"
    );
}

#[test]
fn uppercase_dialect_refused_case_sensitively() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    err_contains(
        tmp.path(),
        "cased_dialect",
        "[capabilities]\n[[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"Turtle\"\n",
        "FM-PACK-021",
    );
}

#[test]
fn empty_dialects_list_refused_fm_pack_021() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    err_contains(
        tmp.path(),
        "empty_dialects",
        "[capabilities]\ndialects = []\n",
        "FM-PACK-021",
    );
}

#[test]
fn unknown_dialects_entry_field_refused_by_closed_schema() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    err_contains(
        tmp.path(),
        "extra_field",
        "[capabilities]\n[[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"turtle\"\nmedia_type = \"text/turtle\"\n",
        "FM-PACK-003",
    );
}
