use std::{path::PathBuf, sync::Arc};
use tokio::sync::Semaphore;
use tracing::{info, warn};
use serde::{Deserialize, Serialize};

use ggen_core::codegen::executor::{SyncExecutor, SyncOptions};

use crate::{
    catalog::RepoCatalogEntry,
    dispatch::DispatchResult,
    health::{check_repo, RepoHealthStatus},
    repo_manager::RepoManager,
    retry::retry_with_backoff,
    state::DaemonState,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchResult {
    pub spec_manifest: String,
    pub total_repos: usize,
    pub succeeded: usize,
    pub failed: usize,
    pub skipped_unhealthy: usize,
    pub results: Vec<DispatchResult>,
}

/// Dispatch a spec manifest to all repos in the catalog, bounded by concurrency.
/// Uses ggen-core SyncExecutor for generation so the same pipeline is used
/// everywhere; spec files live in the ggen repo, output lands in the cloned
/// target repo via SyncOptions::output_dir.
pub async fn dispatch_to_all(
    dispatch_iri: &str,
    spec_manifest: &str,
    repos: &[RepoCatalogEntry],
    manager: &RepoManager,
    state: &Arc<DaemonState>,
    concurrency: usize,
    ggen_root: PathBuf,
) -> BatchResult {
    let sem = Arc::new(Semaphore::new(concurrency));
    let mut handles = Vec::with_capacity(repos.len());

    for repo in repos {
        let permit = Arc::clone(&sem);
        let state = Arc::clone(state);
        let repo_entry = repo.clone();
        let manager_wd = manager.work_dir.clone();
        let owner = manager.github_owner.clone();
        let manifest = spec_manifest.to_owned();
        let iri = dispatch_iri.to_owned();
        let ggen_root = ggen_root.clone();

        let h = tokio::spawn(async move {
            let _permit = permit.acquire_owned().await.ok()?;
            let mgr = RepoManager::new(manager_wd, owner);

            // Clone/update target repo
            let local = match retry_with_backoff(&repo_entry.name, 3, || mgr.ensure(&repo_entry.name)).await {
                Ok(p) => p,
                Err(e) => {
                    warn!("skipping {}: clone failed: {}", repo_entry.name, e);
                    return None;
                }
            };

            // Health check
            let health = check_repo(&local, &repo_entry.name);
            if health.status != RepoHealthStatus::Healthy
                && health.status != RepoHealthStatus::DirtyWorkTree
            {
                warn!("skipping {}: {:?}", repo_entry.name, health.status);
                return Some(Err(format!("unhealthy: {:?}", health.status)));
            }

            info!("generating {} → {}", manifest, repo_entry.name);

            // Record start in state
            let run_id = match state.record_start(&iri, &manifest).await {
                Ok(id) => id,
                Err(e) => {
                    warn!("state record_start failed for {}: {}", repo_entry.name, e);
                    -1
                }
            };

            // Run SyncExecutor in a blocking thread — spec file lives in ggen_root,
            // output goes to the cloned target repo directory.
            let manifest_path = ggen_root.join(&manifest);
            let output_dir = local.clone();
            let sync_result = tokio::task::spawn_blocking(move || {
                let opts = SyncOptions {
                    manifest_path,
                    output_dir: Some(output_dir),
                    use_cache: false,
                    ..Default::default()
                };
                SyncExecutor::new(opts).execute()
            })
            .await
            .unwrap_or_else(|e| Err(ggen_core::utils::error::Error::new(&e.to_string())));

            let (exit_code, stdout_tail, stderr_tail) = match &sync_result {
                Ok(r) => (0i32, format!("wrote {} files", r.files_synced), String::new()),
                Err(e) => (-1i32, String::new(), e.to_string()),
            };

            if run_id >= 0 {
                let _ = state.record_finish(run_id, exit_code, &stdout_tail, &stderr_tail).await;
            }

            if sync_result.is_err() {
                return Some(Err(stderr_tail));
            }

            let files_synced = sync_result.as_ref().map(|r| r.files_synced).unwrap_or(0);

            // Nothing written — no commit needed
            if files_synced == 0 {
                info!("{}: no files written", repo_entry.name);
                return Some(Ok(DispatchResult {
                    dispatch_iri: iri,
                    spec_manifest: manifest,
                    exit_code: 0,
                    stdout_tail,
                    stderr_tail,
                    success: true,
                }));
            }

            let commit_msg = format!(
                "chore: ggen {} [auto]",
                manifest.split('/').nth_back(1).unwrap_or(&manifest)
            );
            let pushed = mgr.commit_and_push(&local, &commit_msg).await
                .unwrap_or(false);

            info!("{}: {} (pushed={})", repo_entry.name, stdout_tail, pushed);
            Some(Ok(DispatchResult {
                dispatch_iri: iri,
                spec_manifest: manifest,
                exit_code: 0,
                stdout_tail,
                stderr_tail,
                success: true,
            }))
        });
        handles.push((repo.name.clone(), h));
    }

    let mut results = Vec::new();
    let mut succeeded = 0usize;
    let mut failed = 0usize;
    let mut skipped_unhealthy = 0usize;

    for (name, h) in handles {
        match h.await {
            Ok(Some(Ok(r))) => { succeeded += 1; results.push(r); }
            Ok(Some(Err(e))) if e.starts_with("unhealthy") => {
                skipped_unhealthy += 1;
                warn!("{}: {}", name, e);
            }
            Ok(Some(Err(e))) => {
                failed += 1;
                warn!("{}: dispatch error: {}", name, e);
            }
            Ok(None) | Err(_) => { failed += 1; }
        }
    }

    BatchResult {
        spec_manifest: spec_manifest.to_owned(),
        total_repos: repos.len(),
        succeeded,
        failed,
        skipped_unhealthy,
        results,
    }
}
