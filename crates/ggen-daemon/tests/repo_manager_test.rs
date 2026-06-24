// Chicago TDD: real git processes, real filesystem — no mocks.
//
// Strategy for network-free testing:
//   • `clone_repo` (called by ensure() when .git is absent) always builds a
//     GitHub URL, so we cannot inject a local URL through the public API.
//     Instead we pre-create a real git working-tree with `git init` + `git
//     remote add origin <bare>`, which causes `ensure()` to take the *pull*
//     branch (`.git` exists) and never attempt a network clone.
//   • `commit_and_push` receives a `&Path` directly, so we point it at a
//     worktree cloned from a local bare repo — no network required.

use std::{
    path::{Path, PathBuf},
    process::Command,
};

use tempfile::TempDir;

use ggen_daemon::repo_manager::RepoManager;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a bare git repo at `dest` with an initial commit so it can be
/// cloned from and pushed to.
fn init_bare_repo(dest: &Path) {
    std::fs::create_dir_all(dest).expect("create bare dir");

    let ok = Command::new("git")
        .args(["init", "--bare"])
        .current_dir(dest)
        .status()
        .expect("git init --bare")
        .success();
    assert!(ok, "git init --bare failed");

    // Seed the bare repo with an initial commit via a throw-away clone.
    let seed_dir = dest.parent().unwrap().join("_seed");
    let ok = Command::new("git")
        .args([
            "clone",
            &dest.to_string_lossy(),
            &seed_dir.to_string_lossy(),
        ])
        .status()
        .expect("git clone seed")
        .success();
    assert!(ok, "seed clone failed");

    // Configure identity for the seed worktree so `git commit` works.
    for (k, v) in [("user.email", "test@ggen"), ("user.name", "ggen-test")] {
        Command::new("git")
            .args(["config", k, v])
            .current_dir(&seed_dir)
            .status()
            .expect("git config");
    }

    // Commit an initial file so the bare repo has a HEAD branch.
    std::fs::write(seed_dir.join("README.md"), "seed").expect("write seed");
    Command::new("git")
        .args(["add", "-A"])
        .current_dir(&seed_dir)
        .status()
        .expect("git add");
    Command::new("git")
        .args(["commit", "-m", "init"])
        .current_dir(&seed_dir)
        .status()
        .expect("git commit");
    Command::new("git")
        .args(["push", "origin", "HEAD"])
        .current_dir(&seed_dir)
        .status()
        .expect("git push seed");

    // Clean up the throw-away seed worktree.
    std::fs::remove_dir_all(&seed_dir).ok();
}

/// Clone `bare_url` into `dest` and set local git identity.
fn clone_local(bare_url: &str, dest: &Path) {
    let ok = Command::new("git")
        .args(["clone", bare_url, &dest.to_string_lossy()])
        .status()
        .expect("git clone")
        .success();
    assert!(ok, "git clone into worktree failed");

    for (k, v) in [("user.email", "test@ggen"), ("user.name", "ggen-test")] {
        Command::new("git")
            .args(["config", k, v])
            .current_dir(dest)
            .status()
            .expect("git config");
    }
}

/// Build a local file:/// URL for a directory.
fn file_url(path: &Path) -> String {
    format!("file://{}", path.display())
}

// ---------------------------------------------------------------------------
// 1. RepoManager::new stores fields correctly (sync, no async needed)
// ---------------------------------------------------------------------------
#[test]
fn new_stores_fields_correctly() {
    let work_dir = PathBuf::from("/tmp/ggen-test-workdir");
    let manager = RepoManager::new(work_dir.clone(), "acme-org");

    assert_eq!(manager.work_dir, work_dir);
    assert_eq!(manager.github_owner, "acme-org");
}

// ---------------------------------------------------------------------------
// 2. ensure() on an already-cloned repo performs a pull (no network clone)
// ---------------------------------------------------------------------------
#[tokio::test]
async fn ensure_pulls_when_dot_git_exists() {
    let tmp = TempDir::new().expect("tempdir");
    let bare = tmp.path().join("remote.git");
    let repo_name = "myrepo";

    init_bare_repo(&bare);

    // Create a pre-cloned worktree so .git exists.
    let worktree = tmp.path().join("work").join(repo_name);
    clone_local(&file_url(&bare), &worktree);

    // RepoManager's work_dir = the "work" parent; it will join repo_name.
    let work_parent = tmp.path().join("work");
    let manager = RepoManager::new(work_parent, "unused-owner");

    // ensure() must detect .git, run `git pull --ff-only`, and return the path.
    let result = manager.ensure(repo_name).await;
    assert!(
        result.is_ok(),
        "ensure() failed unexpectedly: {:?}",
        result.err()
    );

    let returned_path = result.unwrap();
    assert_eq!(returned_path, worktree, "returned path does not match worktree");
    assert!(
        returned_path.join(".git").exists(),
        ".git directory must still be present after ensure()"
    );
}

// ---------------------------------------------------------------------------
// 3. commit_and_push on a clean repo returns Ok(false) — nothing to commit
// ---------------------------------------------------------------------------
#[tokio::test]
async fn commit_and_push_clean_repo_returns_false() {
    let tmp = TempDir::new().expect("tempdir");
    let bare = tmp.path().join("remote.git");
    let worktree = tmp.path().join("clone");

    init_bare_repo(&bare);
    clone_local(&file_url(&bare), &worktree);

    let manager = RepoManager::new(tmp.path().to_path_buf(), "unused-owner");

    let result = manager
        .commit_and_push(&worktree, "should not commit anything")
        .await;

    assert!(
        result.is_ok(),
        "commit_and_push on clean repo returned Err: {:?}",
        result.err()
    );
    assert_eq!(
        result.unwrap(),
        false,
        "expected Ok(false) when there is nothing to commit"
    );
}

// ---------------------------------------------------------------------------
// 4. commit_and_push after writing a file returns Ok(true)
// ---------------------------------------------------------------------------
#[tokio::test]
async fn commit_and_push_with_new_file_returns_true() {
    let tmp = TempDir::new().expect("tempdir");
    let bare = tmp.path().join("remote.git");
    let worktree = tmp.path().join("clone");

    init_bare_repo(&bare);
    clone_local(&file_url(&bare), &worktree);

    // Write a new file so there are uncommitted changes.
    std::fs::write(worktree.join("generated.rs"), "fn main() {}").expect("write file");

    let manager = RepoManager::new(tmp.path().to_path_buf(), "unused-owner");

    let result = manager
        .commit_and_push(&worktree, "add generated.rs")
        .await;

    assert!(
        result.is_ok(),
        "commit_and_push returned Err: {:?}",
        result.err()
    );
    assert_eq!(
        result.unwrap(),
        true,
        "expected Ok(true) after committing a new file"
    );

    // Verify the commit actually landed in the bare repo by checking the log.
    let log = Command::new("git")
        .args(["log", "--oneline", "-1"])
        .current_dir(&bare)
        .output()
        .expect("git log in bare");
    let log_str = String::from_utf8_lossy(&log.stdout);
    assert!(
        log_str.contains("add generated.rs"),
        "commit message not found in bare repo log: {log_str}"
    );
}

// ---------------------------------------------------------------------------
// 5. commit_and_push after modifying an existing tracked file returns Ok(true)
// ---------------------------------------------------------------------------
#[tokio::test]
async fn commit_and_push_with_modified_file_returns_true() {
    let tmp = TempDir::new().expect("tempdir");
    let bare = tmp.path().join("remote.git");
    let worktree = tmp.path().join("clone");

    init_bare_repo(&bare);
    clone_local(&file_url(&bare), &worktree);

    // Modify the tracked README.md that was seeded by init_bare_repo.
    std::fs::write(worktree.join("README.md"), "updated content").expect("modify file");

    let manager = RepoManager::new(tmp.path().to_path_buf(), "unused-owner");

    let result = manager
        .commit_and_push(&worktree, "update readme")
        .await;

    assert!(
        result.is_ok(),
        "commit_and_push returned Err: {:?}",
        result.err()
    );
    assert_eq!(result.unwrap(), true, "expected Ok(true) for modified file");
}
