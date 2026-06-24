use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};
use tokio::sync::RwLock;
use crate::error::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobRun {
    pub id: i64,
    pub dispatch_iri: String,
    pub spec_manifest: String,
    pub started_at: String,
    pub finished_at: Option<String>,
    pub exit_code: Option<i64>,
    pub stdout_tail: Option<String>,
    pub stderr_tail: Option<String>,
}

/// In-memory job run store. Optionally persists to a JSON file.
#[derive(Clone)]
pub struct DaemonState {
    pub cron_ttl_path: String,
    runs: Arc<RwLock<Vec<JobRun>>>,
    persist_path: Option<PathBuf>,
}

impl DaemonState {
    pub async fn new(persist_path: Option<PathBuf>, cron_ttl_path: String) -> Result<Self> {
        let runs = if let Some(p) = &persist_path {
            if p.exists() {
                let raw = std::fs::read_to_string(p)?;
                serde_json::from_str::<Vec<JobRun>>(&raw).unwrap_or_default()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        Ok(Self {
            cron_ttl_path,
            runs: Arc::new(RwLock::new(runs)),
            persist_path,
        })
    }

    pub async fn record_start(&self, dispatch_iri: &str, spec_manifest: &str) -> Result<i64> {
        let mut runs = self.runs.write().await;
        let id = runs.len() as i64 + 1;
        runs.push(JobRun {
            id,
            dispatch_iri: dispatch_iri.to_owned(),
            spec_manifest: spec_manifest.to_owned(),
            started_at: unix_now(),
            finished_at: None,
            exit_code: None,
            stdout_tail: None,
            stderr_tail: None,
        });
        self.flush(&runs).await;
        Ok(id)
    }

    pub async fn record_finish(
        &self, run_id: i64, exit_code: i32, stdout_tail: &str, stderr_tail: &str,
    ) -> Result<()> {
        let mut runs = self.runs.write().await;
        if let Some(r) = runs.iter_mut().find(|r| r.id == run_id) {
            r.finished_at = Some(unix_now());
            r.exit_code = Some(exit_code as i64);
            r.stdout_tail = Some(stdout_tail.to_owned());
            r.stderr_tail = Some(stderr_tail.to_owned());
        }
        self.flush(&runs).await;
        Ok(())
    }

    pub async fn recent_runs(&self, limit: i64) -> Result<Vec<JobRun>> {
        let runs = self.runs.read().await;
        let out: Vec<JobRun> = runs.iter()
            .rev()
            .take(limit as usize)
            .cloned()
            .collect();
        Ok(out)
    }

    pub async fn all_runs(&self) -> Vec<JobRun> {
        self.runs.read().await.clone()
    }

    async fn flush(&self, runs: &[JobRun]) {
        if let Some(p) = &self.persist_path {
            if let Ok(json) = serde_json::to_string_pretty(runs) {
                let _ = std::fs::write(p, json);
            }
        }
    }
}

fn unix_now() -> String {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| format!("{}", d.as_secs()))
        .unwrap_or_else(|_| "0".to_owned())
}
