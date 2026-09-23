//! Chicago-TDD witnesses for dependency-scoped pack resolution.
//!
//! Real filesystem, real pack.toml parsing, real dependency admission, real
//! sync, and real portable receipt output. No mocks.
//!
//! Falsifiers:
//! - Direct scope containing an unrelated pack.
//! - Two-level scope failing to include the second dependency hop.
//! - Missing, version-incompatible, or cyclic dependencies being admitted.
//! - A portable receipt labeling an unrelated top-level pack as a dependency.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use ggen_engine::{
    config::GgenConfig,
    pack::{dependency_scope, resolve, ScopeDepth},
    portable_receipt::PORTABLE_RECEIPT_REL_PATH,
    sync::{sync, SyncOptions},
};
use tempfile::TempDir;

struct Fixture {
    _dir: TempDir,
    project: PathBuf,
}

fn safe_file_name(name: &str) -> String {
    name.replace('-', "_")
}

fn write_pack(root: &Path, name: &str, version: &str, dependencies: &[(&str, &str)]) {
    let pack = root.join("packs").join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("pack templates");

    let mut manifest = format!(
        "[pack]\nname = \"{name}\"\nversion = \"{version}\"\n\
         description = \"dependency scope fixture\"\n"
    );
    if !dependencies.is_empty() {
        manifest.push_str("\n[dependencies]\n");
        for (dependency, requirement) in dependencies {
            let _ = writeln!(manifest, "{dependency} = \"{requirement}\"");
        }
    }
    std::fs::write(pack.join("pack.toml"), manifest).expect("pack.toml");

    let subject = safe_file_name(name);
    std::fs::write(
        pack.join("ontology.ttl"),
        format!(
            "@prefix ex: <http://example.com/dependency-scope#> .\n\
             ex:{subject} a ex:PackFixture .\n"
        ),
    )
    .expect("ontology");

    std::fs::write(
        pack.join("templates").join(format!("{subject}.rs.tmpl")),
        format!(
            "---\nto: src/{subject}.rs\n---\n\
// generated from {name}\n"
        ),
    )
    .expect("template");
}

fn write_project(root: &Path, pack_names: &[&str]) -> PathBuf {
    let project = root.join("project");
    std::fs::create_dir_all(project.join("templates")).expect("project templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("project ontology");

    let mut manifest = String::from(
        "[project]\nname = \"dependency-scope-fixture\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n",
    );
    for name in pack_names {
        let _ = write!(manifest, "\n[packs.{name}]\npath = \"../packs/{name}\"");
        let _ = writeln!(manifest);
    }
    std::fs::write(project.join("ggen.toml"), manifest).expect("ggen.toml");
    project
}

fn standard_fixture() -> Fixture {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "a-root", "1.0.0", &[("b-direct", "^1.0.0")]);
    write_pack(
        dir.path(),
        "b-direct",
        "1.2.0",
        &[("c-transitive", "3.0.0")],
    );
    write_pack(dir.path(), "c-transitive", "3.0.0", &[]);
    write_pack(dir.path(), "z-unrelated", "9.0.0", &[]);
    let project = write_project(
        dir.path(),
        &["a-root", "b-direct", "c-transitive", "z-unrelated"],
    );
    Fixture { _dir: dir, project }
}

fn resolved(fx: &Fixture) -> Vec<ggen_engine::pack::Pack> {
    let config = GgenConfig::load(&fx.project.join("ggen.toml")).expect("load config");
    resolve(&config, &fx.project).expect("resolve packs")
}

fn names<'a>(packs: &[&'a ggen_engine::pack::Pack]) -> Vec<&'a str> {
    packs.iter().map(|pack| pack.name.as_str()).collect()
}

#[test]
fn dependency_scopes_preserve_local_direct_two_level_and_global_boundaries() {
    let fx = standard_fixture();
    let packs = resolved(&fx);

    assert_eq!(
        names(&dependency_scope(&packs, "a-root", ScopeDepth::Local).expect("local")),
        vec!["a-root"]
    );
    assert_eq!(
        names(&dependency_scope(&packs, "a-root", ScopeDepth::Direct).expect("direct")),
        vec!["a-root", "b-direct"]
    );
    assert_eq!(
        names(&dependency_scope(&packs, "a-root", ScopeDepth::TwoLevel).expect("two level"),),
        vec!["a-root", "b-direct", "c-transitive"]
    );
    assert_eq!(
        names(&dependency_scope(&packs, "a-root", ScopeDepth::Transitive).expect("transitive"),),
        vec!["a-root", "b-direct", "c-transitive"]
    );
    assert_eq!(
        names(&dependency_scope(&packs, "a-root", ScopeDepth::Global).expect("global")),
        vec!["a-root", "b-direct", "c-transitive", "z-unrelated"]
    );

    let err = dependency_scope(&packs, "not-resolved", ScopeDepth::Direct)
        .expect_err("unknown subject must refuse");
    assert!(err.to_string().contains("FM-PACK-017"), "{err}");
}

#[test]
fn missing_declared_dependency_refuses_without_auto_installing() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "a-root", "1.0.0", &[("b-missing", "1.0.0")]);
    let project = write_project(dir.path(), &["a-root"]);
    let config = GgenConfig::load(&project.join("ggen.toml")).expect("config");

    let err = resolve(&config, &project).expect_err("missing dependency must refuse");
    assert!(err.to_string().contains("FM-PACK-014"), "{err}");
    assert!(err.to_string().contains("b-missing"), "{err}");
}

#[test]
fn incompatible_dependency_version_refuses() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "a-root", "1.0.0", &[("b-direct", "2.0.0")]);
    write_pack(dir.path(), "b-direct", "1.0.0", &[]);
    let project = write_project(dir.path(), &["a-root", "b-direct"]);
    let config = GgenConfig::load(&project.join("ggen.toml")).expect("config");

    let err = resolve(&config, &project).expect_err("version mismatch must refuse");
    assert!(err.to_string().contains("FM-PACK-015"), "{err}");
    assert!(err.to_string().contains("2.0.0"), "{err}");
    assert!(err.to_string().contains("1.0.0"), "{err}");
}

#[test]
fn cyclic_dependency_graph_refuses() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "a-root", "1.0.0", &[("b-direct", "1.0.0")]);
    write_pack(dir.path(), "b-direct", "1.0.0", &[("a-root", "1.0.0")]);
    let project = write_project(dir.path(), &["a-root", "b-direct"]);
    let config = GgenConfig::load(&project.join("ggen.toml")).expect("config");

    let err = resolve(&config, &project).expect_err("cycle must refuse");
    assert!(err.to_string().contains("FM-PACK-016"), "{err}");
}

#[test]
fn sync_receipt_binds_only_the_subjects_declared_dependency_closure() {
    let fx = standard_fixture();
    sync(
        &fx.project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real sync");

    let envelope: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(fx.project.join(PORTABLE_RECEIPT_REL_PATH))
            .expect("portable receipt"),
    )
    .expect("receipt json");

    assert_eq!(envelope["subject"]["pack"].as_str(), Some("a-root"));
    let dependencies = envelope["dependencies"]
        .as_array()
        .expect("dependencies array");
    assert_eq!(dependencies.len(), 2, "{dependencies:?}");
    assert_eq!(dependencies[0]["name"].as_str(), Some("b-direct"));
    assert_eq!(dependencies[0]["version"].as_str(), Some("1.2.0"));
    assert_eq!(dependencies[1]["name"].as_str(), Some("c-transitive"));
    assert_eq!(dependencies[1]["version"].as_str(), Some("3.0.0"));

    for dependency in dependencies {
        let digest = dependency["digest"].as_str().expect("digest");
        assert!(digest.starts_with("sha256:"), "{digest}");
        assert_eq!(digest.len(), "sha256:".len() + 64, "{digest}");
        assert_eq!(
            dependency["scope"]
                .as_array()
                .expect("scope")
                .iter()
                .map(|value| value.as_str().expect("scope str"))
                .collect::<Vec<_>>(),
            vec!["SEMANTICS", "LAW", "PROJECTION"]
        );
    }
    assert!(
        dependencies
            .iter()
            .all(|dependency| dependency["name"].as_str() != Some("z-unrelated")),
        "unrelated top-level pack must not be mislabeled as a dependency"
    );
}
