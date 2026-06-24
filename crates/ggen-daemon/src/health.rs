use std::path::Path;
use serde::{Deserialize, Serialize};

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
pub fn check_repo(local: &Path, repo_name: &str) -> RepoHealth {
    if !local.join(".git").exists() {
        return RepoHealth {
            repo_name: repo_name.to_owned(),
            status: RepoHealthStatus::NoGitDir,
            branch: None,
            uncommitted_files: 0,
        };
    }

    let branch = std::process::Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .current_dir(local)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_owned());

    let is_detached = branch.as_deref() == Some("HEAD");

    let dirty_count = std::process::Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(local)
        .output()
        .ok()
        .map(|o| o.stdout.split(|b| *b == b'\n').filter(|l| !l.is_empty()).count())
        .unwrap_or(0);

    let status = if is_detached {
        RepoHealthStatus::DetachedHead
    } else if dirty_count > 0 {
        RepoHealthStatus::DirtyWorkTree
    } else {
        RepoHealthStatus::Healthy
    };

    RepoHealth {
        repo_name: repo_name.to_owned(),
        status,
        branch,
        uncommitted_files: dirty_count,
    }
}
