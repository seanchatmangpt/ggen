//! Tier-2 REAL-API acceptance test for `packs/gh-terraform-pack` per TCPS
//! spec 第二十四・二十五章 (現地現物: go and see the real GitHub API state).
//!
//! Chicago TDD, real collaborators only: real `gh` CLI, real `terraform`
//! binary, real GitHub API, a real throwaway repository. No mocks.
//!
//! Ignored by default (Tier 2). Run explicitly:
//!
//! ```bash
//! just tf-acceptance
//! # or
//! cargo test -p ggen-engine --test gh_terraform_acceptance_e2e -- --ignored --nocapture
//! ```
//!
//! Prerequisites (the test panics loudly if any is missing):
//! - `terraform` on PATH
//! - `gh` on PATH and authenticated (`gh auth login`), ideally with the
//!   `delete_repo` scope so the throwaway repo can be cleaned up.
//!
//! Env: `TCPS_ACCEPTANCE_PREFIX` overrides the throwaway repo name prefix
//! (default `tcps-accept-test`).

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

fn run_ok(cmd: &mut Command, what: &str) -> String {
    let (code, stdout, stderr) = run(cmd);
    assert_eq!(
        code, 0,
        "{what} failed (exit {code})\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    stdout
}

/// Gate: hard panic with instructions if a prerequisite is missing
/// (modeled on crates/ggen-cli/tests/llm_e2e_test.rs env gating).
fn gate() -> (String, String) {
    if Command::new("terraform").arg("version").output().is_err() {
        panic!(
            "\n=== PREREQUISITE MISSING: terraform ===\n\
             This Tier-2 acceptance test requires the `terraform` binary on PATH.\n\
             Install: https://developer.hashicorp.com/terraform/install\n\
             Then re-run: just tf-acceptance\n"
        );
    }
    let (code, token, stderr) = run(Command::new("gh").args(["auth", "token"]));
    if code != 0 || token.trim().is_empty() {
        panic!(
            "\n=== PREREQUISITE MISSING: gh authentication ===\n\
             This Tier-2 acceptance test requires an authenticated GitHub CLI.\n\
             Run: gh auth login   (ideally with the delete_repo scope:\n\
                  gh auth refresh -h github.com -s delete_repo)\n\
             Then re-run: just tf-acceptance\n\
             gh stderr: {stderr}\n"
        );
    }
    let owner = run_ok(
        Command::new("gh").args(["api", "user", "--jq", ".login"]),
        "gh api user",
    );
    (token.trim().to_owned(), owner.trim().to_owned())
}

// ── throwaway repo + cleanup guard ──────────────────────────────────────────

struct ThrowawayRepo {
    full_name: String, // owner/name
}

impl Drop for ThrowawayRepo {
    fn drop(&mut self) {
        let (code, _stdout, stderr) =
            run(Command::new("gh").args(["repo", "delete", &self.full_name, "--yes"]));
        if code != 0 {
            // Never panic in Drop — but be LOUD about the leaked repo.
            eprintln!(
                "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\
                 !!! LEAKED THROWAWAY REPO: {name}\n\
                 !!! `gh repo delete {name} --yes` failed (exit {code}).\n\
                 !!! Likely missing the delete_repo scope. Fix with:\n\
                 !!!   gh auth refresh -h github.com -s delete_repo\n\
                 !!!   gh repo delete {name} --yes\n\
                 !!! stderr: {stderr}\n\
                 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
                name = self.full_name,
            );
        } else {
            eprintln!("[cleanup] deleted throwaway repo {}", self.full_name);
        }
    }
}

// ── the acceptance test ─────────────────────────────────────────────────────

/// The label resource addresses in the generated main.tf we target-apply.
/// Targeted apply avoids the github_repository resource conflicting with the
/// pre-created throwaway repo.
const LABEL_TARGETS: &[&str] = &[
    "github_issue_label.ijou",
    "github_issue_label.kaizen",
    "github_issue_label.l5_jouken",
    "github_issue_label.shouko_machi",
    "github_issue_label.kakuri",
    "github_issue_label.koukai_teishi",
    "github_issue_label.juryou_zumi",
    "github_issue_label.terraform_hyouryuu",
    "github_issue_label.gh_sadou_machi",
    "github_issue_label.l5_condition",
];

fn tf(dir: &Path, token: &str, args: &[&str]) -> (i32, String, String) {
    let mut cmd = Command::new("terraform");
    cmd.current_dir(dir).env("GITHUB_TOKEN", token).args(args);
    run(&mut cmd)
}

fn with_targets_and_vars(base: &[&str], owner: &str, repo: &str) -> Vec<String> {
    let mut v: Vec<String> = base.iter().map(|s| (*s).to_owned()).collect();
    for t in LABEL_TARGETS {
        v.push(format!("-target={t}"));
    }
    v.push(format!("-var=github_owner={owner}"));
    v.push(format!("-var=repository={repo}"));
    v
}

#[test]
#[ignore] // Tier 2: real GitHub API + terraform. Run via `just tf-acceptance`.
fn gh_terraform_pack_acceptance_real_api() {
    // 1. Gate + credential bootstrap (via gh; user-authorized).
    let (token, owner) = gate();
    let prefix =
        std::env::var("TCPS_ACCEPTANCE_PREFIX").unwrap_or_else(|_| "tcps-accept-test".to_owned());
    let repo_name = format!("{prefix}-{}", std::process::id());
    let full_name = format!("{owner}/{repo_name}");

    // 2. Create throwaway repo (private); guard deletes it on Drop.
    run_ok(
        Command::new("gh").args(["repo", "create", &full_name, "--private"]),
        "gh repo create",
    );
    let _guard = ThrowawayRepo {
        full_name: full_name.clone(),
    };

    // 3. Scaffold consumer + sync the pack (real ggen pipeline).
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

    // 4. terraform init + TARGETED apply of the label resources only.
    let (init_code, init_out, init_err) = tf(&tf_dir, &token, &["init", "-input=false"]);
    assert_eq!(
        init_code, 0,
        "terraform init failed:\n{init_out}\n{init_err}"
    );

    let apply_args = with_targets_and_vars(
        &["apply", "-auto-approve", "-input=false"],
        &owner,
        &repo_name,
    );
    let apply_refs: Vec<&str> = apply_args.iter().map(String::as_str).collect();
    let (apply_code, apply_out, apply_err) = tf(&tf_dir, &token, &apply_refs);
    assert_eq!(
        apply_code, 0,
        "targeted terraform apply failed:\n{apply_out}\n{apply_err}"
    );

    // 5. 現地現物: verify via the real GitHub API that 異常 exists.
    let labels_json = run_ok(
        Command::new("gh").args([
            "api",
            &format!("repos/{full_name}/labels"),
            "--jq",
            ".[].name",
        ]),
        "gh api labels",
    );
    assert!(
        labels_json.lines().any(|l| l.trim() == "異常"),
        "label 異常 not found on {full_name}; labels:\n{labels_json}"
    );

    // 6. Drift injection: mutate 異常's color out-of-band, expect plan exit 2.
    run_ok(
        Command::new("gh").args([
            "api",
            "-X",
            "PATCH",
            &format!("repos/{full_name}/labels/異常"),
            "-f",
            "color=ffffff",
        ]),
        "gh api PATCH label (drift injection)",
    );
    let plan_args = with_targets_and_vars(
        &["plan", "-detailed-exitcode", "-input=false"],
        &owner,
        &repo_name,
    );
    let plan_refs: Vec<&str> = plan_args.iter().map(String::as_str).collect();
    let (drift_plan_code, drift_out, drift_err) = tf(&tf_dir, &token, &plan_refs);
    assert_eq!(
        drift_plan_code, 2,
        "expected drift (plan exit 2), got {drift_plan_code}:\n{drift_out}\n{drift_err}"
    );

    // Re-apply repairs the drift; plan then exits 0.
    let (reapply_code, reapply_out, reapply_err) = tf(&tf_dir, &token, &apply_refs);
    assert_eq!(
        reapply_code, 0,
        "repair apply failed:\n{reapply_out}\n{reapply_err}"
    );
    let (clean_plan_code, clean_out, clean_err) = tf(&tf_dir, &token, &plan_refs);
    assert_eq!(
        clean_plan_code, 0,
        "expected clean plan (exit 0) after repair, got {clean_plan_code}:\n{clean_out}\n{clean_err}"
    );

    // 7. Targeted destroy, then Drop deletes the repo.
    let destroy_args = with_targets_and_vars(
        &["destroy", "-auto-approve", "-input=false"],
        &owner,
        &repo_name,
    );
    let destroy_refs: Vec<&str> = destroy_args.iter().map(String::as_str).collect();
    let (destroy_code, destroy_out, destroy_err) = tf(&tf_dir, &token, &destroy_refs);
    assert_eq!(
        destroy_code, 0,
        "targeted terraform destroy failed:\n{destroy_out}\n{destroy_err}"
    );

    // 8. Receipt JSON (printed; --nocapture makes it visible).
    println!(
        "{{\"receipt\":\"gh-terraform-acceptance\",\
         \"pack_content_hash\":\"{content_hash}\",\
         \"repo\":\"{full_name}\",\
         \"steps\":{{\"init\":{init_code},\"apply\":{apply_code},\
         \"drift_plan\":{drift_plan_code},\"repair_apply\":{reapply_code},\
         \"clean_plan\":{clean_plan_code},\"destroy\":{destroy_code}}}}}"
    );

    // Drop the guard now so we can verify deletion happened (現地現物).
    drop(_guard);
    let (get_code, _o, _e) = run(Command::new("gh").args(["api", &format!("repos/{full_name}")]));
    assert_ne!(
        get_code, 0,
        "throwaway repo {full_name} still exists after cleanup"
    );
}
