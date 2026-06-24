use std::{
    fs::OpenOptions,
    io::Write,
    path::{Path, PathBuf},
    sync::Mutex,
};
use serde::{Deserialize, Serialize};
use crate::error::{DaemonError, Result};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelEvent {
    #[serde(rename = "ocel:id")]
    pub id: String,
    #[serde(rename = "ocel:activity")]
    pub activity: String,
    #[serde(rename = "ocel:timestamp")]
    pub timestamp: String,
    #[serde(rename = "ocel:type")]
    pub object_type: String,
    #[serde(rename = "ocel:attributes")]
    pub attributes: serde_json::Value,
}

pub struct OcelLog {
    path: PathBuf,
    lock: Mutex<()>,
}

impl OcelLog {
    pub fn new(path: PathBuf) -> Result<Self> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        Ok(Self { path, lock: Mutex::new(()) })
    }

    /// Default path: .ggen/ocel/daemon-events.ocel.jsonl
    pub fn default(root: &Path) -> Result<Self> {
        Self::new(root.join(".ggen/ocel/daemon-events.ocel.jsonl"))
    }

    pub fn append(&self, event: &OcelEvent) -> Result<()> {
        let _guard = self.lock.lock().map_err(|_| DaemonError::Io(
            std::io::Error::new(std::io::ErrorKind::Other, "mutex poisoned")
        ))?;
        let line = serde_json::to_string(event)
            .map_err(DaemonError::Json)?;
        let mut f = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)?;
        writeln!(f, "{}", line)?;
        Ok(())
    }

    pub fn emit_dispatch_start(&self, repo: &str, manifest: &str) -> Result<()> {
        self.append(&OcelEvent {
            id: uuid::Uuid::new_v4().to_string(),
            activity: "dispatch:start".into(),
            timestamp: epoch_now(),
            object_type: "BundleDispatch".into(),
            attributes: serde_json::json!({
                "repo": repo,
                "manifest": manifest,
            }),
        })
    }

    pub fn emit_needs_remediation(
        &self,
        repo: &str,
        manifest: &str,
        andon_code: &str,
        steps: &[String],
    ) -> Result<()> {
        self.append(&OcelEvent {
            id: uuid::Uuid::new_v4().to_string(),
            activity: "dispatch:needs-remediation".into(),
            timestamp: epoch_now(),
            object_type: "BundleDispatch".into(),
            attributes: serde_json::json!({
                "repo": repo,
                "manifest": manifest,
                "andon_code": andon_code,
                "recovery_steps": steps,
            }),
        })
    }

    pub fn emit_dispatch_complete(&self, repo: &str, manifest: &str, success: bool, exit_code: i32) -> Result<()> {
        self.append(&OcelEvent {
            id: uuid::Uuid::new_v4().to_string(),
            activity: if success { "dispatch:complete" } else { "dispatch:failed" }.into(),
            timestamp: epoch_now(),
            object_type: "BundleDispatch".into(),
            attributes: serde_json::json!({
                "repo": repo,
                "manifest": manifest,
                "success": success,
                "exit_code": exit_code,
            }),
        })
    }
}

fn epoch_now() -> String {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| format!("{}.{:03}", d.as_secs(), d.subsec_millis()))
        .unwrap_or_else(|_| "0.000".into())
}
