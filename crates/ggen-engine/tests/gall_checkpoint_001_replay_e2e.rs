//! GALL-001 Chicago court: real filesystem, pack resolution, sync, replay,
//! and mutation/refusal falsifiers.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use ggen_engine::{
    portable_receipt::PORTABLE_RECEIPT_REL_PATH,
    replay::verify_project_replay,
    sync::{sync, SyncOptions},
};
use tempfile::TempDir;

struct Fixture {
    _dir: TempDir,
    project: PathBuf,
    root_pack: PathBuf,
    dependency_pack: PathBuf,
}

fn write_pack(root: &Path, name: &str, version: &str, dependency: Option<(&str, &str)>) -> PathBuf {
    let pack = root.join("packs").join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("pack templates");
    let mut manifest = format!(
        "[pack]\nname = \"{name}\"\nversion = \"{version}\"\ndescription = \"GALL-001 replay fixture\"\n"
    );
    if let Some((dep, req)) = dependency {
        manifest.push_str(&format!("\n[dependencies]\n{dep} = \"{req}\"\n"));
    }
    std::fs::write(pack.join("pack.toml"), manifest).expect("pack manifest");
    std::fs::write(
        pack.join("ontology.ttl"),
        format!("@prefix ex: <http://example.com/gall001#> .\nex:{name} a ex:Pack .\n"),
    )
    .expect("ontology");
    std::fs::write(
        pack.join("templates").join(format!("{name}.rs.tmpl")),
        format!("---\nto: src/{name}.rs\n---\n// generated from {name}\n"),
    )
    .expect("template");
    pack
}

fn fixture() -> Fixture {
    let dir = TempDir::new().expect("tempdir");
    let root_pack = write_pack(
        dir.path(),
        "root_pack",
        "1.0.0",
        Some(("dependency_pack", "1.0.0")),
    );
    let dependency_pack = write_pack(dir.path(), "dependency_pack", "1.0.0", None);
    let project = dir.path().join("project");
    std::fs::create_dir_all(project.join("templates")).expect("project templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("project ontology");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"gall-001\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[packs.root_pack]\npath = \"../packs/root_pack\"\n\n[packs.dependency_pack]\npath = \"../packs/dependency_pack\"\n",
    )
    .expect("ggen.toml");

    Fixture {
        _dir: dir,
        project,
        root_pack,
        dependency_pack,
    }
}

fn run_sync(project: &Path) {
    sync(
        project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("initial sync");
}

fn receipt(project: &Path) -> serde_json::Value {
    serde_json::from_slice(
        &std::fs::read(project.join(PORTABLE_RECEIPT_REL_PATH)).expect("portable receipt"),
    )
    .expect("receipt json")
}

#[test]
fn clean_replay_matches_exact_subject_and_consequence_set() {
    let fx = fixture();
    run_sync(&fx.project);
    let before = receipt(&fx.project);
    assert_eq!(before["replay"]["status"], "UNKNOWN");

    let report = verify_project_replay(&fx.project, SyncOptions::default()).expect("replay");
    assert_eq!(report.status, "PASS");
    assert!(report.cleared_consequences >= 1);

    let after = receipt(&fx.project);
    assert_eq!(after["replay"]["status"], "PASS");
    assert_eq!(after["subject"], before["subject"]);
    assert_eq!(after["dependencies"], before["dependencies"]);
    assert_eq!(after["graph"], before["graph"]);
    assert_eq!(after["consequences"], before["consequences"]);
}

#[test]
fn mutated_pack_refuses_old_replay_identity() {
    let fx = fixture();
    run_sync(&fx.project);
    std::fs::write(
        fx.root_pack.join("ontology.ttl"),
        "@prefix ex: <http://example.com/gall001#> .\nex:mutated a ex:Pack .\n",
    )
    .expect("mutate pack");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("old replay identity must refuse");
    assert!(err.to_string().contains("FM-CHAIN-017"), "{err}");
}

#[test]
fn mutated_dependency_refuses_old_replay_identity() {
    let fx = fixture();
    run_sync(&fx.project);
    std::fs::write(
        fx.dependency_pack.join("ontology.ttl"),
        "@prefix ex: <http://example.com/gall001#> .\nex:dependency_mutated a ex:Pack .\n",
    )
    .expect("mutate dependency");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("old dependency identity must refuse");
    assert!(err.to_string().contains("FM-CHAIN-017"), "{err}");
}

#[test]
fn mutated_generated_output_is_not_the_replay_oracle() {
    let fx = fixture();
    run_sync(&fx.project);
    let original = receipt(&fx.project);
    let target = original["consequences"][0]["target"]
        .as_str()
        .expect("target");
    std::fs::write(fx.project.join(target), "tampered generated output\n").expect("tamper");

    verify_project_replay(&fx.project, SyncOptions::default())
        .expect("clean replay ignores tampered output as oracle");
    let after = receipt(&fx.project);
    assert_eq!(after["replay"]["status"], "PASS");
    assert_eq!(after["consequences"], original["consequences"]);
}

#[test]
fn dry_run_cannot_manufacture_replay_pass() {
    let fx = fixture();
    run_sync(&fx.project);
    let err = verify_project_replay(
        &fx.project,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect_err("dry-run replay must refuse");
    assert!(err.to_string().contains("FM-CHAIN-016"), "{err}");
}
