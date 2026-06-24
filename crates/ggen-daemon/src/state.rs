use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};
use tokio::sync::RwLock;
use tracing::warn;
use crate::error::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobRun {
    pub id: i64,
    pub dispatch_iri: String,
    pub spec_manifest: String,
    /// RFC-3339 UTC timestamp when the run was recorded.
    pub started_at: String,
    /// RFC-3339 UTC timestamp when the run finished, if complete.
    pub finished_at: Option<String>,
    pub exit_code: Option<i64>,
    pub stdout_tail: Option<String>,
    pub stderr_tail: Option<String>,
}

/// In-memory job run store with optional JSON persistence.
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
                let raw = tokio::fs::read_to_string(p).await?;
                match serde_json::from_str::<Vec<JobRun>>(&raw) {
                    Ok(runs) => runs,
                    Err(e) => {
                        warn!("ignoring corrupt state file {}: {}", p.display(), e);
                        Vec::new()
                    }
                }
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
            started_at: rfc3339_now(),
            finished_at: None,
            exit_code: None,
            stdout_tail: None,
            stderr_tail: None,
        });
        self.flush(&runs).await;
        Ok(id)
    }

    pub async fn record_finish(
        &self,
        run_id: i64,
        exit_code: i32,
        stdout_tail: &str,
        stderr_tail: &str,
    ) -> Result<()> {
        let mut runs = self.runs.write().await;
        if let Some(r) = runs.iter_mut().find(|r| r.id == run_id) {
            r.finished_at = Some(rfc3339_now());
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
            match serde_json::to_string_pretty(runs) {
                Ok(json) => {
                    if let Err(e) = tokio::fs::write(p, json).await {
                        warn!("failed to persist daemon state to {}: {}", p.display(), e);
                    }
                }
                Err(e) => {
                    warn!("failed to serialize daemon state: {}", e);
                }
            }
        }
    }
}

/// Current wall-clock time as an RFC-3339 UTC string.
fn rfc3339_now() -> String {
    Utc::now().to_rfc3339()
}
