//! Tier-2 REAL-API acceptance test for `packs/gh-terraform-pack` per TCPS
//! spec 第二十四・二十五章 (現地現物: go and see the real GitHub API state).
//!
//! Chicago TDD with the GENERATED artifacts as the real collaborators: the
//! test syncs the pack, then drives the generated `scripts/gh/accept-*.sh`
//! standard work — it never re-implements the gh/terraform operations inline
//! (法則七: 人の記憶を標準にしない). The only direct process spawns are the
//! two prerequisite probes in `gate()` (the irreducible bootstrap that must
//! precede any generated artifact existing) and the generated scripts
//! themselves.
//!
//! Repo deletion is deliberately NOT tested (user decision, 2026-07-20). The
//! sandbox repo `<owner>/tcps-accept-test-sandbox` is created once if absent,
//! reused across runs, and LEFT carrying the full TCPS desired state — it IS
//! the living L5 exemplar repository.
//!
//! Ignored by default (Tier 2). Run explicitly:
//!
//! ```bash
//! just tf-acceptance
//! # or
//! cargo test -p ggen-engine --test gh_terraform_acceptance_e2e -- --ignored --nocapture
//! ```
//!
//! Prerequisites (panics loudly if missing): `terraform` on PATH; `gh`
//! authenticated. Env: `TCPS_ACCEPTANCE_PREFIX` overrides the sandbox name
//! prefix (default `tcps-accept-test`, sandbox = `<prefix>-sandbox`).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};
use std::process::Command;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

// ── scaffold (same pattern as gh_terraform_pack_e2e.rs) ─────────────────────

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

// ── process helpers ─────────────────────────────────────────────────────────

/// Run a command, capture output, return (exit_code, stdout, stderr).
fn run(cmd: &mut Command) -> (i32, String, String) {
    let out = cmd
        .output()
        .unwrap_or_else(|e| panic!("failed to spawn {cmd:?}: {e}"));
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// Run one of the GENERATED standard-work scripts; return (exit, stdout, stderr).
fn run_script(project: &Path, script: &str, args: &[&str]) -> (i32, String, String) {
    let path = project.join("scripts/gh").join(script);
    assert!(path.is_file(), "generated script missing: {script}");
    run(Command::new("bash").arg(&path).args(args))
}

/// Run a generated script and require exit 0.
fn script_ok(project: &Path, script: &str, args: &[&str]) -> String {
    let (code, stdout, stderr) = run_script(project, script, args);
    assert_eq!(
        code, 0,
        "{script} {args:?} failed (exit {code})\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    stdout
}

/// Gate: the irreducible bootstrap boundary — the only direct spawns in this
/// test. Everything past this point goes through generated standard work.
/// (Modeled on crates/ggen-cli/tests/llm_e2e_test.rs env gating.)
fn gate() -> String {
    if Command::new("terraform").arg("version").output().is_err() {
        panic!(
            "\n=== PREREQUISITE MISSING: terraform ===\n\
             This Tier-2 acceptance test requires the `terraform` binary on PATH.\n\
             Install: https://developer.hashicorp.com/terraform/install\n\
             Then re-run: just tf-acceptance\n"
        );
    }
    let (code, owner, stderr) = run(Command::new("gh").args(["api", "user", "--jq", ".login"]));
    if code != 0 || owner.trim().is_empty() {
        panic!(
            "\n=== PREREQUISITE MISSING: gh authentication ===\n\
             This Tier-2 acceptance test requires an authenticated GitHub CLI.\n\
             Run: gh auth login\n\
             Then re-run: just tf-acceptance\n\
             gh stderr: {stderr}\n"
        );
    }
    owner.trim().to_owned()
}

// ── the acceptance test ─────────────────────────────────────────────────────

#[test]
#[ignore] // Tier 2: real GitHub API + terraform. Run via `just tf-acceptance`.
fn gh_terraform_pack_acceptance_real_api() {
    // 1. Gate (bootstrap boundary) + sandbox identity.
    let owner = gate();
    let repo_name = std::env::var("TCPS_ACCEPTANCE_PREFIX")
        .unwrap_or_else(|_| "tcps-accept-test".to_owned())
        + "-sandbox";
    let full_name = format!("{owner}/{repo_name}");

    // 2. Sync the pack — from here on, generated standard work only.
    let (_dir, project) = scaffold();
    let result = sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    assert!(!result.written.is_empty(), "pack must write files");
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    let content_hash = lock
        .lines()
        .find_map(|l| l.trim().strip_prefix("content_hash = "))
        .unwrap_or("\"<missing>\"")
        .trim_matches('"')
        .to_owned();
    let tf_dir = project.join("infra/terraform/github");
    let tf_dir_s = tf_dir.to_string_lossy().into_owned();

    // 3. accept-setup.sh — idempotent sandbox setup (create-if-absent, never delete).
    let setup_out = script_ok(&project, "accept-setup.sh", &["--repo", &full_name]);
    println!("[setup] {}", setup_out.trim());

    // 4. accept-import.sh (runs terraform init itself if needed) — import-convergence of everything already remote.
    script_ok(
        &project,
        "accept-import.sh",
        &["--dir", &tf_dir_s, "--owner", &owner, "--repo", &repo_name],
    );

    // 5. accept-apply.sh — FULL desired state onto the sandbox (L5 exemplar).
    script_ok(
        &project,
        "accept-apply.sh",
        &["--dir", &tf_dir_s, "--owner", &owner, "--repo", &repo_name],
    );

    // 6. accept-verify.sh — 現地現物 (labels, merge policy, protection contexts).
    let verify1 = script_ok(&project, "accept-verify.sh", &["--repo", &full_name]);
    assert!(
        verify1.contains("\"verify\":\"pass\""),
        "verify after apply: {verify1}"
    );

    // 7. Drift cycle: inject → drift-report detects (exit 1, andon YELLOW,
    //    drift detected via plan exit 2) → repair apply → drift-report clean.
    script_ok(&project, "accept-drift-inject.sh", &["--repo", &full_name]);
    let (drift_code, drift_out, drift_err) = run_script(
        &project,
        "drift-report.sh",
        &[
            "--dir", &tf_dir_s, "--owner", &owner, "--repo", &repo_name,
            "--secret-value", "sandbox-dummy-value",
        ],
    );
    assert_ne!(
        drift_code, 0,
        "drift-report must be non-zero on drift:\n{drift_out}\n{drift_err}"
    );
    assert!(
        drift_out.contains("\"drift\":\"detected\""),
        "drift-report output: {drift_out}"
    );

    script_ok(
        &project,
        "accept-apply.sh",
        &["--dir", &tf_dir_s, "--owner", &owner, "--repo", &repo_name],
    );
    let clean_out = script_ok(
        &project,
        "drift-report.sh",
        &[
            "--dir", &tf_dir_s, "--owner", &owner, "--repo", &repo_name,
            "--secret-value", "sandbox-dummy-value",
        ],
    );
    assert!(
        clean_out.contains("\"drift\":\"none\""),
        "drift-report after repair: {clean_out}"
    );

    // 8. Final 現地現物 + receipt. The sandbox keeps the full desired state.
    let verify2 = script_ok(&project, "accept-verify.sh", &["--repo", &full_name]);
    assert!(
        verify2.contains("\"verify\":\"pass\""),
        "final verify: {verify2}"
    );
    // 9. accept-receipt.sh — append a hash-chained receipt line to the
    //    durable committed ledger at <workspace>/.tcps/acceptance-receipts.jsonl.
    let ledger_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../.tcps");
    std::fs::create_dir_all(&ledger_dir).expect("mkdir .tcps");
    let ledger = ledger_dir.join("acceptance-receipts.jsonl");
    let lines_before = std::fs::read_to_string(&ledger)
        .map(|s| s.lines().count())
        .unwrap_or(0);
    let receipt_json = format!(
        "{{\"receipt\":\"gh-terraform-acceptance\",\
         \"pack_content_hash\":\"{content_hash}\",\
         \"repo\":\"{full_name}\",\
         \"verify\":{verify}}}",
        verify = verify2.trim(),
    );
    let appended = script_ok(
        &project,
        "accept-receipt.sh",
        &["--file", &ledger.display().to_string(), "--json", &receipt_json],
    );
    let after = std::fs::read_to_string(&ledger).expect("read .tcps ledger");
    let lines_after: Vec<&str> = after.lines().collect();
    assert_eq!(
        lines_after.len(),
        lines_before + 1,
        "ledger must grow by exactly one line"
    );
    let new_line = lines_after.last().expect("new ledger line");
    assert!(
        new_line.contains("\"prev_hash\":"),
        "appended receipt must carry prev_hash: {new_line}"
    );
    assert_eq!(
        appended.trim(),
        *new_line,
        "accept-receipt.sh must print the appended line"
    );

    println!(
        "{{\"receipt\":\"gh-terraform-acceptance\",\
         \"pack_content_hash\":\"{content_hash}\",\
         \"repo\":\"{full_name}\",\
         \"exemplar_left_applied\":true,\
         \"standard_work\":[\"accept-setup.sh\",\"accept-import.sh\",\
         \"accept-apply.sh\",\"accept-verify.sh\",\"accept-drift-inject.sh\",\
         \"drift-report.sh\"],\
         \"verify\":{verify}}}",
        verify = verify2.trim(),
    );
}
