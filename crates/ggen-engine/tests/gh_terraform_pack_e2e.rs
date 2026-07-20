//! Chicago-TDD end-to-end proof for the expanded `packs/gh-terraform-pack`:
//! real filesystem (`TempDir`), real graph engine, real Tera, real `bash -n`.
//! Proves the pack generates the full Terraform GitHub-management surface
//! (16 .tf files + README), the 12-script gh CLI library, the per-resource /
//! per-data-source fan-out docs, and the Japanese docs — and that a second
//! sync is byte-identical (no drift).
//!
//! This test never runs `terraform apply` or any mutating `gh` command —
//! generation and `bash -n` syntax checks only.

use std::path::{Path, PathBuf};
use std::process::Command;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
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

fn scaffold() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("gh-terraform-pack"),
        &dir.path().join("gh-terraform-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\ngh-terraform-pack = { path = \"../gh-terraform-pack\" }\n\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    (dir, project)
}

fn read(project: &Path, rel: &str) -> String {
    std::fs::read_to_string(project.join(rel)).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

fn count_md_files(dir: &Path) -> usize {
    std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "md"))
        .count()
}

const TF_FILES: [&str; 16] = [
    "versions.tf",
    "variables.tf",
    "repository.tf",
    "branch-protection.tf",
    "rulesets.tf",
    "labels.tf",
    "milestones.tf",
    "issues.tf",
    "teams.tf",
    "collaborators.tf",
    "environments.tf",
    "secrets.tf",
    "webhooks.tf",
    "files.tf",
    "outputs.tf",
    "README.md",
];

const SCRIPTS: [&str; 12] = [
    "pr-create.sh",
    "pr-checks.sh",
    "pr-view-mergeable.sh",
    "pr-merge.sh",
    "pr-close-stale.sh",
    "run-view.sh",
    "run-rerun.sh",
    "issue-create.sh",
    "issue-close.sh",
    "release-view.sh",
    "repo-state.sh",
    "drift-report.sh",
];

#[test]
fn gh_terraform_pack_generates_full_surface_and_is_idempotent() {
    let (_dir, project) = scaffold();

    let first = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert!(!first.written.is_empty(), "pack must write files");

    // (1) All 16 terraform-directory files exist.
    for f in TF_FILES {
        assert!(
            project.join("infra/terraform/github").join(f).is_file(),
            "missing infra/terraform/github/{f}"
        );
    }

    // (1a) versions.tf pins integrations/github 5.41.0.
    let versions = read(&project, "infra/terraform/github/versions.tf");
    assert!(
        versions.contains(r#"source  = "integrations/github""#),
        "{versions}"
    );
    assert!(versions.contains(r#"version = "5.41.0""#), "{versions}");

    // (1b) labels.tf carries the 10 desired-state labels.
    let labels = read(&project, "infra/terraform/github/labels.tf");
    for label in [
        "異常",
        "改善",
        "第五水準条件",
        "証拠待ち",
        "隔離",
        "公開停止",
        "受領済み",
        "Terraform漂流",
        "gh作動待ち",
        "l5-condition",
    ] {
        assert!(
            labels.contains(label),
            "labels.tf missing {label}: {labels}"
        );
    }

    // (1c) branch-protection.tf carries the real required status contexts.
    let bp = read(&project, "infra/terraform/github/branch-protection.tf");
    for ctx in ["Check", "Test", "Build", "Doctest"] {
        assert!(
            bp.contains(&format!("\"{ctx}\"")),
            "branch-protection.tf missing context {ctx}: {bp}"
        );
    }

    // (1d) secrets.tf references the variable, never a literal secret value.
    let secrets = read(&project, "infra/terraform/github/secrets.tf");
    assert!(secrets.contains("var.ggen_signing_key_value"), "{secrets}");
    // The only acceptable mention of the key is through the var reference —
    // strip those and demand no residual assignment of a literal.
    let stripped = secrets.replace("var.ggen_signing_key_value", "");
    assert!(
        !stripped.contains("plaintext_value = \"") && !stripped.contains("encrypted_value = \""),
        "secrets.tf must not embed a literal secret value: {secrets}"
    );

    // (2) Fan-out doc counts.
    let resources = count_md_files(&project.join("docs/gh-terraform/resources"));
    assert!(resources >= 87, "resources fan-out: {resources} < 87");
    let data_sources = count_md_files(&project.join("docs/gh-terraform/data-sources"));
    assert!(
        data_sources >= 75,
        "data-sources fan-out: {data_sources} < 75"
    );

    // (3) Deprecation honesty in the fan-out docs.
    let custom_role = read(
        &project,
        "docs/gh-terraform/resources/github_organization_custom_role.md",
    );
    assert!(
        custom_role.contains("github_organization_repository_role"),
        "custom_role doc must name its successor: {custom_role}"
    );
    let project_card = read(
        &project,
        "docs/gh-terraform/resources/github_project_card.md",
    );
    assert!(
        project_card.contains("dead")
            || project_card.contains("廃止")
            || project_card.contains("Dead"),
        "github_project_card.md must mark the dead API: {project_card}"
    );

    // (4) All 12 scripts exist, have the bash shebang, and pass `bash -n`.
    for script in SCRIPTS {
        let path = project.join("scripts/gh").join(script);
        assert!(path.is_file(), "missing scripts/gh/{script}");
        let body = read(&project, &format!("scripts/gh/{script}"));
        assert!(
            body.starts_with("#!/usr/bin/env bash"),
            "{script} missing shebang: {}",
            &body[..body.len().min(80)]
        );
        let out = Command::new("bash")
            .arg("-n")
            .arg(&path)
            .output()
            .expect("spawn bash -n");
        assert!(
            out.status.success(),
            "bash -n {script} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    // (5) Safety mechanisms in the mutating / diffing scripts.
    let close_stale = read(&project, "scripts/gh/pr-close-stale.sh");
    assert!(
        close_stale.contains("--confirm"),
        "pr-close-stale.sh must gate on --confirm: {close_stale}"
    );
    let drift = read(&project, "scripts/gh/drift-report.sh");
    assert!(
        drift.contains("detailed-exitcode"),
        "drift-report.sh must use -detailed-exitcode: {drift}"
    );

    // (6a) ggen.lock records the pack.
    let lock = read(&project, "ggen.lock");
    assert!(lock.contains("[packs.gh-terraform-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (7) L5 standing doc keeps its honest 実地未確認 marker.
    let l5 = read(&project, "docs/gh-terraform/L5-STATUS.md");
    assert!(
        l5.contains("実地未確認"),
        "L5-STATUS.md must preserve the honest unverified marker: {l5}"
    );

    // (6) Second sync is a no-op: nothing written, lock byte-identical.
    let second = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        second.written.is_empty(),
        "second sync wrote: {:?}",
        second.written
    );
    let lock2 = read(&project, "ggen.lock");
    assert_eq!(
        lock2, lock,
        "ggen.lock must be byte-identical across identical runs"
    );
}
