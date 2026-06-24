use std::path::{Path, PathBuf};
use serde::{Deserialize, Serialize};
use tokio::process::Command;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RepoHealthStatus {
    Healthy,
    NoGitDir,
    DetachedHead,
    DirtyWorkTree,
    Uncloneable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepoHealth {
    pub repo_name: String,
    pub status: RepoHealthStatus,
    pub branch: Option<String>,
    pub uncommitted_files: usize,
}

/// Check whether a locally-cloned repo is in a healthy state for dispatch.
///
/// This function is `async` because it spawns git subprocesses via
/// `tokio::process::Command`, avoiding blocking the async executor.
pub async fn check_repo(local: &Path, repo_name: &str) -> RepoHealth {
    let local_owned: PathBuf = local.to_owned();
    let repo_name_owned = repo_name.to_owned();

    if !local_owned.join(".git").exists() {
        return RepoHealth {
            repo_name: repo_name_owned,
            status: RepoHealthStatus::NoGitDir,
            branch: None,
            uncommitted_files: 0,
        };
    }

    let branch = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .current_dir(&local_owned)
        .output()
        .await
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_owned());

    let is_detached = branch.as_deref() == Some("HEAD");

    let dirty_count = Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(&local_owned)
        .output()
        .await
        .ok()
        .map(|o| o.stdout.split(|b: &u8| *b == b'\n').filter(|l| !l.is_empty()).count())
        .unwrap_or(0);

    let status = if is_detached {
        RepoHealthStatus::DetachedHead
    } else if dirty_count > 0 {
        RepoHealthStatus::DirtyWorkTree
    } else {
        RepoHealthStatus::Healthy
    };

    RepoHealth {
        repo_name: repo_name_owned,
        status,
        branch,
        uncommitted_files: dirty_count,
    }
}
