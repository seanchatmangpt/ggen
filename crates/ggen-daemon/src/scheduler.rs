use std::{path::PathBuf, sync::Arc, time::Duration};
use tracing::{info, error};
use crate::{
    dispatch::dispatch_bundle,
    error::Result,
    ontology::{load_jobs, JobDef},
    state::DaemonState,
};

pub struct DaemonScheduler {
    pub jobs: Vec<JobDef>,
    handles: Vec<tokio::task::JoinHandle<()>>,
}

impl DaemonScheduler {
    /// Load jobs from the cron ontology TTL and register them as tokio tasks.
    /// Each job is run at the interval derived from its cron expression.
    /// For simplicity, the cron expression "*/N * * * *" is parsed as N minutes;
    /// other expressions default to a 60-second poll interval.
    pub async fn from_ontology(
        ttl_path: &std::path::Path,
        state: Arc<DaemonState>,
        working_dir: PathBuf,
    ) -> Result<Self> {
        let jobs = load_jobs(ttl_path)?;
        info!("loaded {} jobs from ontology", jobs.len());

        let mut handles = Vec::new();

        for job_def in &jobs {
            let state = Arc::clone(&state);
            let wd = working_dir.clone();
            let dispatch_iri = job_def.dispatch_iri.clone();
            let spec_manifest = job_def.spec_manifest.clone();
            let interval_secs = parse_cron_interval_secs(&job_def.cron_expr);
            let cron_expr = job_def.cron_expr.clone();

            info!(
                "registered job: {} @ '{}' (every {}s)",
                spec_manifest, cron_expr, interval_secs
            );

            let handle = tokio::spawn(async move {
                let mut ticker = tokio::time::interval(Duration::from_secs(interval_secs));
                // Skip the first immediate tick
                ticker.tick().await;
                loop {
                    ticker.tick().await;
                    match dispatch_bundle(&dispatch_iri, &spec_manifest, &wd, &state).await {
                        Ok(r) => info!(
                            "bundle done: {} exit={}",
                            spec_manifest, r.exit_code
                        ),
                        Err(e) => error!(
                            "bundle error: {} — {}",
                            spec_manifest, e
                        ),
                    }
                }
            });
            handles.push(handle);
        }

        Ok(Self { jobs: jobs.clone(), handles })
    }

    /// Wait for a shutdown signal; aborts all job tasks on drop.
    pub async fn run_until_signal(self) {
        tokio::signal::ctrl_c().await.ok();
        info!("shutdown signal received — aborting {} job tasks", self.handles.len());
        for h in self.handles {
            h.abort();
        }
    }
}

/// Parse a cron expression into a poll interval in seconds.
/// Handles the common "*/N * * * *" (every N minutes) pattern.
/// All other expressions default to 60 seconds.
fn parse_cron_interval_secs(expr: &str) -> u64 {
    let parts: Vec<&str> = expr.trim().split_whitespace().collect();
    // "*/N * * * *" — every N minutes
    if parts.len() == 5 {
        if let Some(min_part) = parts.first() {
            if let Some(n_str) = min_part.strip_prefix("*/") {
                if let Ok(n) = n_str.parse::<u64>() {
                    return n * 60;
                }
            }
            // "@every Ns" — not a standard cron but handle it gracefully
        }
    }
    // Default: 60 seconds
    60
}
