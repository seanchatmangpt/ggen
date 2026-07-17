//! Chicago-TDD end-to-end tests for `ggen doctor run`: real filesystem,
//! real syncs, real subprocess (`assert_cmd` spawns the NEW ggen binary).
//! No mocks.

use std::path::{Path, PathBuf};

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

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn scaffold(root: &Path, names: &[&str]) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    write_ontology(root, names);
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn write_ontology(root: &Path, names: &[&str]) {
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology");
}

fn examples_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("examples")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir");
    for entry in std::fs::read_dir(src).expect("read_dir") {
        let entry = entry.expect("entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy");
        }
    }
}

/// Copy demo-pack and demo-project into a fresh TempDir; return (dir, project_root).
fn scaffold_with_pack() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &examples_dir().join("demo-pack"),
        &dir.path().join("demo-pack"),
    );
    copy_tree(
        &examples_dir().join("demo-project"),
        &dir.path().join("demo-project"),
    );
    let project = dir.path().join("demo-project");
    (dir, project)
}

fn run_doctor(project: &Path) -> assert_cmd::assert::Assert {
    assert_cmd::Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(project)
        .args(["doctor", "run"])
        .assert()
}

fn stdout_json(assert: &assert_cmd::assert::Assert) -> serde_json::Value {
    let out = assert.get_output();
    serde_json::from_slice(&out.stdout).expect("doctor stdout is JSON")
}

fn stderr_str(assert: &assert_cmd::assert::Assert) -> String {
    String::from_utf8_lossy(&assert.get_output().stderr).into_owned()
}

/// (a) A clean, synced project: `doctor run` succeeds, `healthy: true`, and
/// all three checks individually report `"status": "pass"`.
#[test]
fn clean_synced_project_is_healthy() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice", "bob"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let assert = run_doctor(dir.path()).success();
    let json = stdout_json(&assert);
    assert_eq!(json["healthy"], true, "{json}");
    assert_eq!(json["checks"]["lockfile_drift"]["status"], "pass", "{json}");
    assert_eq!(
        json["checks"]["orphaned_artifacts"]["status"], "pass",
        "{json}"
    );
    assert_eq!(
        json["checks"]["receipt_staleness"]["status"], "pass",
        "{json}"
    );
}

/// (b) A fresh, never-synced project (`ggen.toml` only, no packs, no
/// receipt): `doctor run` succeeds with `healthy: true` — the "nothing to
/// have drifted from yet" rule for a project that has never been generated.
#[test]
fn never_synced_project_is_healthy_by_definition() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    assert!(!dir.path().join(".ggen-v2/receipt.json").exists());
    assert!(!dir.path().join("ggen.lock").exists());

    let assert = run_doctor(dir.path()).success();
    let json = stdout_json(&assert);
    assert_eq!(json["healthy"], true, "{json}");
    assert_eq!(json["checks"]["lockfile_drift"]["status"], "pass", "{json}");
    assert_eq!(
        json["checks"]["orphaned_artifacts"]["detail"],
        "no receipt found; nothing has been generated yet",
        "{json}"
    );
    assert_eq!(
        json["checks"]["receipt_staleness"]["detail"],
        "no receipt found; nothing has been generated yet",
        "{json}"
    );
}

/// (c) After a pack sync, corrupting the locked pack's on-disk content makes
/// `doctor run` fail closed, naming `lockfile_drift` — while it still
/// computes (and would independently report) the other two checks.
#[test]
fn corrupted_pack_content_fails_lockfile_drift() {
    let (_dir, project) = scaffold_with_pack();
    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let pack_ontology = project
        .parent()
        .expect("root")
        .join("demo-pack/ontology.ttl");
    let mut ttl = std::fs::read_to_string(&pack_ontology).expect("pack ttl");
    ttl.push_str(
        "\n<http://example.com/ontology#evil> <http://example.com/ontology#hasName> \"tampered\" .\n",
    );
    std::fs::write(&pack_ontology, ttl).expect("corrupt pack");

    let assert = run_doctor(&project).failure();
    let stderr = stderr_str(&assert);
    assert!(stderr.contains("lockfile_drift"), "{stderr}");
    assert!(stderr.contains("FM-PACK-008"), "{stderr}");
}

/// (d) Deleting one receipt-recorded output file after a sync makes
/// `doctor run` fail closed, naming `receipt_staleness` and the missing
/// path — while `lockfile_drift` and `orphaned_artifacts` are each computed
/// independently and still individually pass (proves no short-circuiting).
#[test]
fn deleted_output_fails_receipt_staleness_independently_of_other_checks() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice", "bob"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let missing = dir.path().join("out/names.txt");
    assert!(missing.is_file());
    std::fs::remove_file(&missing).expect("delete output");

    let assert = run_doctor(dir.path()).failure();
    let stderr = stderr_str(&assert);
    assert!(stderr.contains("receipt_staleness"), "{stderr}");
    assert!(stderr.contains("out/names.txt"), "{stderr}");

    // lockfile_drift and orphaned_artifacts are each still individually
    // fine — nothing here should mention them as failing.
    assert!(!stderr.contains("lockfile_drift:"), "{stderr}");
    assert!(!stderr.contains("orphaned_artifacts:"), "{stderr}");
}

/// (e) Deleting the template file that produced a receipt-recorded output
/// (while leaving `templates/` itself intact and readable) makes
/// `doctor run` fail closed, naming `orphaned_artifacts` and the orphaned
/// output's path — while `lockfile_drift` and `receipt_staleness` are each
/// computed independently and still individually pass (the output file
/// itself is untouched on disk, only its producing template is gone).
#[test]
fn orphaned_artifact_after_template_deletion_fails_independently() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice", "bob"]);
    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let output = dir.path().join("out/names.txt");
    assert!(output.is_file());

    // Delete only the template FILE, not the `templates/` directory — the
    // directory must remain present and readable so the orphan-detection
    // logic fires, not a `templates dir ... unreadable` config error.
    let template = dir.path().join("templates/one.tmpl");
    assert!(template.is_file());
    std::fs::remove_file(&template).expect("delete template");
    assert!(dir.path().join("templates").is_dir());

    let assert = run_doctor(dir.path()).failure();
    let stderr = stderr_str(&assert);
    assert!(stderr.contains("orphaned_artifacts"), "{stderr}");
    assert!(stderr.contains("out/names.txt"), "{stderr}");

    // lockfile_drift and receipt_staleness are each still individually
    // fine — nothing here should mention them as failing.
    assert!(!stderr.contains("lockfile_drift:"), "{stderr}");
    assert!(!stderr.contains("receipt_staleness:"), "{stderr}");
}
