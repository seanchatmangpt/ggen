use std::path::{Path, PathBuf};
use tokio::process::Command;
use tracing::{info, warn};
use crate::error::{DaemonError, Result};

pub struct RepoManager {
    pub work_dir: PathBuf,
    pub github_owner: String,
}

impl RepoManager {
    pub fn new(work_dir: PathBuf, github_owner: impl Into<String>) -> Self {
        Self { work_dir, github_owner: github_owner.into() }
    }

    /// Returns the local path for a repo, cloning if not present, pulling if it is.
    pub async fn ensure(&self, repo_name: &str) -> Result<PathBuf> {
        let local = self.work_dir.join(repo_name);
        if local.join(".git").exists() {
            self.pull(&local).await?;
        } else {
            self.clone_repo(repo_name, &local).await?;
        }
        Ok(local)
    }

    async fn clone_repo(&self, repo_name: &str, dest: &Path) -> Result<()> {
        let url = format!("https://github.com/{}/{}.git", self.github_owner, repo_name);
        info!("cloning {} → {}", url, dest.display());
        if let Some(p) = dest.parent() {
            tokio::fs::create_dir_all(p).await.map_err(DaemonError::Io)?;
        }
        let out = Command::new("git")
            .args(["clone", "--depth=1", &url, &dest.to_string_lossy()])
            .output()
            .await
            .map_err(DaemonError::Io)?;
        if !out.status.success() {
            let stderr = String::from_utf8_lossy(&out.stderr);
            let msg = stderr[..stderr.len().min(200)].to_owned();
            warn!("clone failed for {}: {}", repo_name, msg);
            return Err(DaemonError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("git clone failed for {repo_name}: {msg}"),
            )));
        }
        Ok(())
    }

    async fn pull(&self, local: &Path) -> Result<()> {
        info!("pulling {}", local.display());
        let out = Command::new("git")
            .args(["pull", "--ff-only", "--depth=1"])
            .current_dir(local)
            .output()
            .await
            .map_err(DaemonError::Io)?;
        if !out.status.success() {
            let stderr = String::from_utf8_lossy(&out.stderr);
            let msg = stderr[..stderr.len().min(200)].to_owned();
            warn!("pull failed at {}: {}", local.display(), msg);
            return Err(DaemonError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("git pull failed at {}: {msg}", local.display()),
            )));
        }
        Ok(())
    }

    /// Stage all changes, commit with a message, and push.
    pub async fn commit_and_push(&self, local: &Path, message: &str) -> Result<bool> {
        let status = Command::new("git")
            .args(["status", "--porcelain"])
            .current_dir(local)
            .output()
            .await
            .map_err(DaemonError::Io)?;
        if status.stdout.is_empty() {
            return Ok(false); // nothing to commit
        }

        Command::new("git").args(["add", "-A"]).current_dir(local)
            .status().await.map_err(DaemonError::Io)?;

        let commit_out = Command::new("git")
            .args(["commit", "-m", message])
            .current_dir(local)
            .output()
            .await
            .map_err(DaemonError::Io)?;

        if !commit_out.status.success() {
            let err = String::from_utf8_lossy(&commit_out.stderr);
            warn!("commit failed: {}", &err[..err.len().min(200)]);
            return Ok(false);
        }

        let push_out = Command::new("git")
            .args(["push"])
            .current_dir(local)
            .output()
            .await
            .map_err(DaemonError::Io)?;

        if !push_out.status.success() {
            let err = String::from_utf8_lossy(&push_out.stderr);
            let msg = err[..err.len().min(200)].to_owned();
            warn!("push failed: {}", msg);
            return Err(DaemonError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("git push failed: {msg}"),
            )));
        }

        Ok(true)
    }
}
