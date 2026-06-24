use std::sync::Arc;
use tokio::sync::Semaphore;
use tracing::{info, warn};
use serde::{Deserialize, Serialize};

use crate::{
    catalog::RepoCatalogEntry,
    dispatch::{dispatch_bundle, DispatchResult},
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
pub async fn dispatch_to_all(
    dispatch_iri: &str,
    spec_manifest: &str,
    repos: &[RepoCatalogEntry],
    manager: &RepoManager,
    state: &Arc<DaemonState>,
    concurrency: usize,
) -> BatchResult {
    let sem = Arc::new(Semaphore::new(concurrency));
    let mut handles = Vec::with_capacity(repos.len());

    for repo in repos {
        let permit = Arc::clone(&sem);
        let state = Arc::clone(state);
        let repo_name = repo.name.clone();
        let manager_wd = manager.work_dir.clone();
        let owner = manager.github_owner.clone();
        let manifest = spec_manifest.to_owned();
        let iri = dispatch_iri.to_owned();

        let h = tokio::spawn(async move {
            let _permit = permit.acquire_owned().await.ok()?;
            let mgr = RepoManager::new(manager_wd, owner);

            // Clone/update
            let local = match retry_with_backoff(&repo_name, 3, || mgr.ensure(&repo_name)).await {
                Ok(p) => p,
                Err(e) => {
                    warn!("skipping {}: clone failed: {}", repo_name, e);
                    return None;
                }
            };

            // Health check
            let health = check_repo(&local, &repo_name);
            if health.status != RepoHealthStatus::Healthy && health.status != RepoHealthStatus::DirtyWorkTree {
                warn!("skipping {}: {:?}", repo_name, health.status);
                return Some(Err(format!("unhealthy: {:?}", health.status)));
            }

            info!("dispatching {} → {}", manifest, repo_name);
            match dispatch_bundle(&iri, &manifest, &local, &state).await {
                Ok(r) => {
                    // Commit and push if ggen sync wrote files
                    let msg = format!("chore: ggen sync {} [auto]", manifest.split('/').last().unwrap_or(&manifest));
                    let _ = mgr.commit_and_push(&local, &msg).await;
                    Some(Ok(r))
                }
                Err(e) => Some(Err(e.to_string())),
            }
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
