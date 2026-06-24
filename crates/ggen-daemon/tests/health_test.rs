use ggen_daemon::{check_repo, RepoHealthStatus};
use std::process::Command;
use tempfile::TempDir;

fn init_git_repo(dir: &TempDir) {
    let p = dir.path();
    Command::new("git").args(["init"]).current_dir(p).output().unwrap();
    Command::new("git")
        .args(["config", "user.email", "ci@ggen.test"])
        .current_dir(p)
        .output()
        .unwrap();
    Command::new("git")
        .args(["config", "user.name", "CI Bot"])
        .current_dir(p)
        .output()
        .unwrap();
    std::fs::write(p.join("README.md"), "# test").unwrap();
    Command::new("git").args(["add", "."]).current_dir(p).output().unwrap();
    Command::new("git")
        .args(["commit", "-m", "init"])
        .current_dir(p)
        .output()
        .unwrap();
}

#[tokio::test]
async fn clean_repo_is_healthy() {
    let dir = TempDir::new().unwrap();
    init_git_repo(&dir);

    let health = check_repo(dir.path(), "test-repo").await;

    assert_eq!(health.status, RepoHealthStatus::Healthy);
    assert_eq!(health.repo_name, "test-repo");
    assert_eq!(health.uncommitted_files, 0);
    assert!(
        health.branch.is_some(),
        "branch name must be populated for a healthy repo"
    );
}

#[tokio::test]
async fn directory_without_dot_git_is_not_a_repo() {
    let dir = TempDir::new().unwrap();
    // No `git init` — plain directory
    let health = check_repo(dir.path(), "no-git-here").await;
    assert_eq!(health.status, RepoHealthStatus::NoGitDir);
    assert_eq!(health.repo_name, "no-git-here");
}

#[tokio::test]
async fn repo_with_untracked_file_is_dirty() {
    let dir = TempDir::new().unwrap();
    init_git_repo(&dir);

    std::fs::write(dir.path().join("untracked.txt"), "new content").unwrap();

    let health = check_repo(dir.path(), "dirty-repo").await;
    assert_eq!(health.status, RepoHealthStatus::DirtyWorkTree);
    assert!(
        health.uncommitted_files > 0,
        "must count the untracked file"
    );
}

#[tokio::test]
async fn repo_with_staged_change_is_dirty() {
    let dir = TempDir::new().unwrap();
    init_git_repo(&dir);

    std::fs::write(dir.path().join("staged.txt"), "staged change").unwrap();
    Command::new("git")
        .args(["add", "staged.txt"])
        .current_dir(dir.path())
        .output()
        .unwrap();

    let health = check_repo(dir.path(), "staged-repo").await;
    assert_eq!(
        health.status,
        RepoHealthStatus::DirtyWorkTree,
        "staged changes must count as dirty"
    );
    assert!(health.uncommitted_files > 0);
}

#[tokio::test]
async fn repo_name_is_preserved_verbatim() {
    let dir = TempDir::new().unwrap();
    let health = check_repo(dir.path(), "my-unique-name-42").await;
    assert_eq!(health.repo_name, "my-unique-name-42");
}
