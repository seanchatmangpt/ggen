//! GALL-001 Chicago court: real filesystem, pack resolution, sync, replay,
//! and mutation/refusal falsifiers.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use ggen_engine::{
    portable_receipt::{PORTABLE_RECEIPT_REL_PATH, WORK_ORDER_REL_PATH},
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
    std::fs::write(
        project.join(WORK_ORDER_REL_PATH),
        "@prefix schema: <https://schema.org/> .\n<urn:gall:001> a schema:Action ; schema:name \"GALL-001\" .\n",
    )
    .expect("semantic work order");

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
    assert_eq!(after["composition"], before["composition"]);
    assert_eq!(after["graph"], before["graph"]);
    assert_eq!(after["work_order"], before["work_order"]);
    assert_eq!(after["consequences"], before["consequences"]);
    assert_eq!(after["toolchain"], before["toolchain"]);
    assert_eq!(after["environment"], before["environment"]);
    for field in [
        "subject",
        "dependencies",
        "composition",
        "graph",
        "work_order",
        "consequences",
        "toolchain",
        "environment",
    ] {
        assert_eq!(after["replay"]["identity"][field]["equal"], true);
        assert!(
            after["replay"]["identity"][field]["sha256"]
                .as_str()
                .expect("identity digest")
                .starts_with("sha256:")
        );
    }
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


#[test]
fn mutated_project_graph_refuses_old_replay_identity() {
    let fx = fixture();
    run_sync(&fx.project);
    std::fs::write(
        fx.project.join("ontology.ttl"),
        "@prefix ex: <http://example.com/gall001#> .\nex:changed a ex:Graph .\n",
    )
    .expect("mutate project graph");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("old graph identity must refuse");
    assert!(err.to_string().contains("FM-CHAIN-017"), "{err}");
    assert_eq!(receipt(&fx.project)["replay"]["status"], "UNKNOWN");
}

#[test]
fn tampered_environment_identity_refuses_before_clean_reconstruction() {
    let fx = fixture();
    run_sync(&fx.project);
    let mut source = receipt(&fx.project);
    source["environment"]["variables_sha256"] =
        serde_json::Value::String("sha256:stale-environment".to_string());
    std::fs::write(
        fx.project.join(PORTABLE_RECEIPT_REL_PATH),
        serde_json::to_vec(&source).expect("serialize tampered source"),
    )
    .expect("write tampered source");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("stale environment identity must refuse");
    assert!(err.to_string().contains("FM-CHAIN-017"), "{err}");
    assert!(fx.project.join("src/root_pack.rs").exists());
}

#[test]
fn unrelated_top_level_pack_cannot_hide_behind_selected_subject() {
    let fx = fixture();
    let aux = write_pack(fx._dir.path(), "zz_aux", "2.0.0", None);
    let config_path = fx.project.join("ggen.toml");
    let mut config = std::fs::read_to_string(&config_path).expect("read config");
    config.push_str("\n[packs.zz_aux]\npath = \"../packs/zz_aux\"\n");
    std::fs::write(&config_path, config).expect("append unrelated pack");

    run_sync(&fx.project);
    let before = receipt(&fx.project);
    assert_ne!(
        before["subject"]["pack"].as_str(),
        Some("zz_aux"),
        "fixture must keep zz_aux outside compatibility subject selection"
    );
    assert_eq!(
        before["composition"]["resolved_packs"]
            .as_array()
            .expect("composition")
            .len(),
        3
    );

    std::fs::write(
        aux.join("ontology.ttl"),
        "@prefix ex: <http://example.com/gall001#> .\nex:mutated_aux a ex:Pack .\n",
    )
    .expect("mutate unrelated top-level pack");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("unselected top-level pack identity must remain replay-bearing");
    assert!(err.to_string().contains("FM-CHAIN-017"), "{err}");
    assert_eq!(receipt(&fx.project)["replay"]["status"], "UNKNOWN");
}


#[test]
fn tampered_toolchain_identity_refuses_before_clean_reconstruction() {
    let fx = fixture();
    run_sync(&fx.project);
    let mut source = receipt(&fx.project);
    source["toolchain"]["rustc"] =
        serde_json::Value::String("stale-rustc-identity".to_string());
    std::fs::write(
        fx.project.join(PORTABLE_RECEIPT_REL_PATH),
        serde_json::to_vec(&source).expect("serialize tampered source"),
    )
    .expect("write tampered source");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("stale toolchain identity must refuse");
    assert!(err.to_string().contains("FM-CHAIN-017"), "{err}");
    assert!(fx.project.join("src/root_pack.rs").exists());
}

#[test]
fn gate_refused_source_cannot_report_replay_pass() {
    let fx = fixture();
    std::fs::write(
        fx.project.join("law-refuse.rq"),
        "SELECT ?s WHERE { ?s ?p ?o }",
    )
    .expect("write refusing law gate");
    let config_path = fx.project.join("ggen.toml");
    let mut config = std::fs::read_to_string(&config_path).expect("read config");
    config.push_str("\n[law]\ngates = [\"law-refuse.rq\"]\n");
    std::fs::write(&config_path, config).expect("append refusing law gate");

    sync(&fx.project, SyncOptions::default()).expect_err("law gate must refuse");
    let refused = receipt(&fx.project);
    assert_ne!(refused["standing"], "ALIVE");
    assert_eq!(refused["replay"]["status"], "UNKNOWN");

    let err = verify_project_replay(&fx.project, SyncOptions::default())
        .expect_err("refused source cannot enter replay court");
    assert!(err.to_string().contains("FM-CHAIN-016"), "{err}");
    assert_eq!(receipt(&fx.project)["replay"]["status"], "UNKNOWN");
}
