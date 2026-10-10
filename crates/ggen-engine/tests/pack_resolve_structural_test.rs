//! Structural refusal court for `pack::resolve` (`crates/ggen-engine/src/pack.rs`).
//!
//! Every distinct refusal class the parser-diff test's `engine_err_class`
//! enumerates gets a dedicated minimal fixture and an exact-class match, so
//! each class is pinned directly rather than hit incidentally:
//!
//! - "invalid pack.toml"   ([FM-PACK-003] — syntactically broken TOML)
//! - "ontology.ttl missing" ([FM-PACK-004] — file absent; EMPTY file resolves)
//! - "zero templates"       ([FM-PACK-005] — empty templates/ dir; ABSENT
//!   dir also lands here, pinned below)
//! - missing-dir            ([FM-PACK-001] — declared pack path does not exist)
//! - dependency-closure     ([FM-PACK-014] — declared dependency not in the
//!   consumer's `[packs]` table)
//!
//! Plus one positive court: a valid pack with a populated `[capabilities]`
//! table round-trips `semantic_types`/`provides`/`requires` through resolve.
//!
//! Chicago TDD: real fixture pack dirs on disk, real `pack::resolve`, no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use ggen_engine::config::GgenConfig;
use tempfile::TempDir;

/// The same classification the parser-diff test uses; asserted against
/// verbatim here so the two courts cannot drift apart silently.
fn engine_err_class(err: &str) -> &'static str {
    if err.contains("invalid pack.toml") {
        "invalid-pack-toml"
    } else if err.contains("ontology.ttl missing") {
        "missing-ontology"
    } else if err.contains("zero templates") {
        "missing-templates"
    } else if err.contains("directory") && err.contains("does not exist") {
        "missing-dir"
    } else if err.contains("FM-PACK-014") || err.contains("dependency") {
        "dependency-closure"
    } else {
        "other"
    }
}

/// Scaffold a minimal consumer project whose single `[packs.<key>]` entry
/// points at `pack_dir` (absolute path), then run the real `pack::resolve`.
fn engine_resolve(pack_key: &str, pack_dir: &Path) -> Result<Vec<ggen_engine::pack::Pack>, String> {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("project");
    std::fs::create_dir_all(project.join("templates")).expect("project templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("project ontology");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"structural-court\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [templates]\ndir = \"templates\"\n\n\
             [packs.{pack_key}]\npath = \"{}\"\n",
            pack_dir.display()
        ),
    )
    .expect("ggen.toml");

    let config = GgenConfig::load(&project.join("ggen.toml")).map_err(|e| e.to_string())?;
    ggen_engine::pack::resolve(&config, &project).map_err(|e| e.to_string())
}

/// A full valid pack dir (pack.toml + ontology.ttl + one template) in a
/// TempDir, with optional extra text appended to pack.toml and optional
/// capability-table text.
struct FixturePack {
    _dir: TempDir,
    pack_dir: PathBuf,
}

fn fixture_pack_with(
    pack_toml_body: &str, ontology: Option<&str>, templates: &[&str],
) -> FixturePack {
    let dir = TempDir::new().expect("tempdir");
    let pack_dir = dir.path().join("fixture-pack");
    std::fs::create_dir_all(pack_dir.join("templates")).expect("pack templates");
    let raw = format!(
        "[pack]\nname = \"fixture-pack\"\nversion = \"1.0.0\"\n\
         description = \"structural court fixture\"\n\n\
         {pack_toml_body}\n"
    );
    std::fs::write(pack_dir.join("pack.toml"), &raw).expect("pack.toml");
    if let Some(ttl) = ontology {
        std::fs::write(pack_dir.join("ontology.ttl"), ttl).expect("ontology.ttl");
    }
    for tmpl in templates {
        let path = pack_dir.join("templates").join(tmpl);
        std::fs::create_dir_all(path.parent().expect("parent")).expect("template parents");
        std::fs::write(&path, "---\nto: out.txt\n---\n").expect("template");
    }
    FixturePack {
        _dir: dir,
        pack_dir,
    }
}

fn valid_fixture() -> FixturePack {
    fixture_pack_with(
        "",
        Some("@prefix ex: <http://e.com#> .\nex:Widget a ex:DomainClass .\n"),
        &["one.tmpl"],
    )
}

// ---------------------------------------------------------------------------
// Class 1: invalid pack.toml (FM-PACK-003)
// ---------------------------------------------------------------------------

#[test]
fn syntactically_broken_pack_toml_refuses_with_invalid_pack_toml_class() {
    let dir = TempDir::new().expect("tempdir");
    let pack_dir = dir.path().join("fixture-pack");
    std::fs::create_dir_all(pack_dir.join("templates")).expect("templates");
    std::fs::write(pack_dir.join("pack.toml"), "not = [valid toml").expect("pack.toml");
    std::fs::write(pack_dir.join("ontology.ttl"), "").expect("ontology");
    std::fs::write(pack_dir.join("templates").join("t.tmpl"), "x").expect("tmpl");

    let err = engine_resolve("fixture-pack", &pack_dir).expect_err("broken TOML must refuse");
    assert_eq!(engine_err_class(&err), "invalid-pack-toml", "{err}");
    assert!(err.contains("FM-PACK-003"), "{err}");
}

// ---------------------------------------------------------------------------
// Class 2: ontology.ttl missing (FM-PACK-004) vs ontology present but empty
// ---------------------------------------------------------------------------

#[test]
fn missing_ontology_ttl_refuses_with_missing_ontology_class() {
    let fx = fixture_pack_with("", None, &["one.tmpl"]);
    assert!(!fx.pack_dir.join("ontology.ttl").exists());

    let err =
        engine_resolve("fixture-pack", &fx.pack_dir).expect_err("missing ontology must refuse");
    assert_eq!(engine_err_class(&err), "missing-ontology", "{err}");
    assert!(err.contains("FM-PACK-004"), "{err}");
}

/// Pin the boundary: an ontology that EXISTS but is empty is admitted —
/// the class fires on absence, not emptiness.
#[test]
fn present_but_empty_ontology_ttl_resolves() {
    let fx = fixture_pack_with("", Some(""), &["one.tmpl"]);
    let packs = engine_resolve("fixture-pack", &fx.pack_dir)
        .expect("an empty-but-present ontology.ttl must resolve");
    assert_eq!(packs.len(), 1);
}

// ---------------------------------------------------------------------------
// Class 3: zero templates (FM-PACK-005)
// ---------------------------------------------------------------------------

#[test]
fn templates_dir_present_but_empty_refuses_with_zero_templates_class() {
    let fx = fixture_pack_with("", Some("x"), &[]);
    assert!(fx.pack_dir.join("templates").is_dir());

    let err = engine_resolve("fixture-pack", &fx.pack_dir).expect_err("zero templates must refuse");
    assert_eq!(engine_err_class(&err), "missing-templates", "{err}");
    assert!(err.contains("FM-PACK-005"), "{err}");
}

/// Surprise pinned: a pack with NO templates/ directory at all also lands in
/// the zero-templates class (the missing-dir class is reserved for the
/// declared pack root, not subdirectories).
#[test]
fn templates_dir_absent_entirely_also_lands_in_zero_templates_class() {
    let fx = fixture_pack_with("", Some("x"), &[]);
    std::fs::remove_dir(fx.pack_dir.join("templates")).expect("remove templates dir");
    assert!(!fx.pack_dir.join("templates").exists());

    let err =
        engine_resolve("fixture-pack", &fx.pack_dir).expect_err("no templates dir must refuse");
    assert_eq!(engine_err_class(&err), "missing-templates", "{err}");
}

// ---------------------------------------------------------------------------
// Class 4: declared pack directory does not exist (FM-PACK-001)
// ---------------------------------------------------------------------------

#[test]
fn declared_pack_path_not_existing_refuses_with_missing_dir_class() {
    let dir = TempDir::new().expect("tempdir");
    let ghost = dir.path().join("no-such-pack");
    assert!(!ghost.exists());

    let err = engine_resolve("ghost-pack", &ghost).expect_err("missing pack dir must refuse");
    assert_eq!(engine_err_class(&err), "missing-dir", "{err}");
    assert!(err.contains("FM-PACK-001"), "{err}");
}

// ---------------------------------------------------------------------------
// Class 5: dependency closure (FM-PACK-014)
// ---------------------------------------------------------------------------

#[test]
fn undeclared_dependency_refuses_with_dependency_closure_class() {
    let fx = fixture_pack_with(
        "[dependencies]\nmissing-dep = \"1.0.0\"\n",
        Some("x"),
        &["one.tmpl"],
    );

    let err = engine_resolve("fixture-pack", &fx.pack_dir)
        .expect_err("a dependency absent from the consumer [packs] table must refuse");
    assert_eq!(engine_err_class(&err), "dependency-closure", "{err}");
    assert!(err.contains("FM-PACK-014"), "{err}");
}

// ---------------------------------------------------------------------------
// Positive: valid pack resolves with capabilities round-tripped
// ---------------------------------------------------------------------------

#[test]
fn valid_pack_resolves_with_capability_fields_round_tripped() {
    let fx = fixture_pack_with(
        "[capabilities]\n\
         types = [\"ex:Widget\", \"ex:Gadget\"]\n\
         provides = [\"urn:ggen:cap:render\"]\n\
         requires = [\"urn:ggen:cap:render\"]\n",
        Some("@prefix ex: <http://e.com#> .\n"),
        &["one.tmpl", "sub/two.tmpl"],
    );

    let packs = engine_resolve("fixture-pack", &fx.pack_dir).expect("valid pack must resolve");
    assert_eq!(packs.len(), 1);
    let pack = &packs[0];

    assert_eq!(pack.name, "fixture-pack");
    assert_eq!(pack.version, "1.0.0");
    assert_eq!(pack.description, "structural court fixture");

    let expect_types: std::collections::BTreeSet<String> = ["ex:Widget", "ex:Gadget"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(pack.semantic_types, expect_types, "types round-trip");

    let expect_provides: std::collections::BTreeSet<String> = ["urn:ggen:cap:render"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(pack.provides, expect_provides, "provides round-trip");

    let expect_requires: std::collections::BTreeSet<String> = ["urn:ggen:cap:render"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(pack.requires, expect_requires, "requires round-trip");

    // Both flat and nested templates discovered, sorted.
    assert_eq!(pack.template_paths.len(), 2);
    let names: Vec<String> = pack
        .template_paths
        .iter()
        .map(|p| {
            p.file_name()
                .expect("file name")
                .to_string_lossy()
                .into_owned()
        })
        .collect();
    assert!(names.contains(&"one.tmpl".to_string()) && names.contains(&"two.tmpl".to_string()));
}
