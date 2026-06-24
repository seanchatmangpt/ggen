use std::{path::{Path, PathBuf}, sync::Arc};
use tokio::sync::Semaphore;
use tracing::{info, warn};
use serde::{Deserialize, Serialize};

use ggen_core::codegen::executor::{SyncExecutor, SyncOptions};

use crate::{
    catalog::RepoCatalogEntry,
    dispatch::DispatchResult,
    health::{check_repo, RepoHealthStatus},
    manifest_cache::{hash_file, ManifestCache},
    repo_manager::RepoManager,
    retry::retry_with_backoff,
    state::DaemonState,
    validator::validate_generated,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchResult {
    pub spec_manifest: String,
    pub total_repos: usize,
    pub succeeded: usize,
    pub failed: usize,
    pub skipped_unhealthy: usize,
    /// Repos where the executor was skipped because the manifest hash was
    /// unchanged since the last successful dispatch (a subset of `succeeded`).
    pub cached: usize,
    /// Repos where post-generation validation failed; the push was suppressed.
    pub validation_failed: usize,
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
            let health = check_repo(&local, &repo_entry.name).await;
            if health.status != RepoHealthStatus::Healthy
                && health.status != RepoHealthStatus::DirtyWorkTree
            {
                warn!("skipping {}: {:?}", repo_entry.name, health.status);
                return Some(Err(format!("unhealthy: {:?}", health.status)));
            }

            // Diff-based skip: hash the manifest and compare with the last-applied
            // cache stored in the target repo.  A hash match means the target repo
            // already reflects this manifest version — no executor run needed.
            let manifest_path = ggen_root.join(&manifest);
            let current_hash = hash_file(&manifest_path);
            if let Some(ref hash) = current_hash {
                if let Some(cached_entry) = ManifestCache::read(&local) {
                    if &cached_entry.manifest_hash == hash {
                        info!("{}: manifest unchanged, skipping executor", repo_entry.name);
                        return Some(Ok(DispatchResult {
                            dispatch_iri: iri,
                            spec_manifest: manifest,
                            exit_code: 0,
                            stdout_tail: "cache hit: manifest unchanged".to_owned(),
                            stderr_tail: String::new(),
                            success: true,
                            cached: true,
                            validation_passed: true,
                            validation_ms: 0,
                        }));
                    }
                }
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
            let output_dir = local.clone();
            let manifest_path_exec = manifest_path.clone();
            let sync_result = tokio::task::spawn_blocking(move || {
                let opts = SyncOptions {
                    manifest_path: manifest_path_exec,
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

            // Nothing written — no commit needed; write cache so the next dispatch
            // with the same manifest can skip the executor.
            if files_synced == 0 {
                info!("{}: no files written", repo_entry.name);
                if let Some(hash) = current_hash.as_deref() {
                    write_manifest_cache(&local, hash, &repo_entry.name).await;
                }
                return Some(Ok(DispatchResult {
                    dispatch_iri: iri,
                    spec_manifest: manifest,
                    exit_code: 0,
                    stdout_tail,
                    stderr_tail,
                    success: true,
                    cached: false,
                    validation_passed: true,
                    validation_ms: 0,
                }));
            }

            // Artifact validation: run language-aware checks on the generated files
            // before committing.  A validation failure suppresses the push.
            let validation = validate_generated(
                &local,
                repo_entry.primary_language.as_deref(),
            )
            .await;
            if !validation.passed {
                let err_msg = validation
                    .failure_summary
                    .unwrap_or_else(|| "validation failed".to_owned());
                warn!("{}: validation blocked push: {}", repo_entry.name, err_msg);
                if run_id >= 0 {
                    let _ = state.record_finish(run_id, -1, &stdout_tail, &err_msg).await;
                }
                return Some(Err(format!("validation: {}", err_msg)));
            }

            let commit_msg = format!(
                "chore: ggen {} [auto]",
                bundle_label(&manifest)
            );
            match mgr.commit_and_push(&local, &commit_msg).await {
                Ok(pushed) => {
                    info!("{}: {} (pushed={})", repo_entry.name, stdout_tail, pushed);
                    // Write cache only after a successful push so the next dispatch
                    // skips the executor for this manifest+repo pair.
                    if let Some(hash) = current_hash.as_deref() {
                        write_manifest_cache(&local, hash, &repo_entry.name).await;
                    }
                }
                Err(e) => {
                    let err_msg = format!("commit_and_push failed: {}", e);
                    warn!("{}: {}", repo_entry.name, err_msg);
                    if run_id >= 0 {
                        let _ = state.record_finish(run_id, -1, &stdout_tail, &err_msg).await;
                    }
                    return Some(Err(err_msg));
                }
            }
            Some(Ok(DispatchResult {
                dispatch_iri: iri,
                spec_manifest: manifest,
                exit_code: 0,
                stdout_tail,
                stderr_tail,
                success: true,
                cached: false,
                validation_passed: true,
                validation_ms: validation.elapsed_ms,
            }))
        });
        handles.push((repo.name.clone(), h));
    }

    let mut results = Vec::new();
    let mut succeeded = 0usize;
    let mut failed = 0usize;
    let mut skipped_unhealthy = 0usize;
    let mut cached = 0usize;
    let mut validation_failed = 0usize;

    for (name, h) in handles {
        match h.await {
            Ok(Some(Ok(r))) => {
                succeeded += 1;
                if r.cached {
                    cached += 1;
                }
                results.push(r);
            }
            Ok(Some(Err(e))) if e.starts_with("unhealthy") => {
                skipped_unhealthy += 1;
                warn!("{}: {}", name, e);
            }
            Ok(Some(Err(e))) if e.starts_with("validation:") => {
                failed += 1;
                validation_failed += 1;
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
        cached,
        validation_failed,
        results,
    }
}

/// Write `ManifestCache` to `<local>/.ggen/last-applied.json`, recording the
/// current HEAD SHA for audit purposes.  Failures are logged and swallowed —
/// a missing cache entry only means the next dispatch re-runs the executor;
/// it is never a correctness issue.
async fn write_manifest_cache(local: &Path, manifest_hash: &str, repo_name: &str) {
    let commit_sha = tokio::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(local)
        .output()
        .await
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_owned())
        .unwrap_or_default();

    let entry = ManifestCache {
        manifest_hash: manifest_hash.to_owned(),
        commit_sha,
        applied_at: chrono::Utc::now().to_rfc3339(),
    };
    if let Err(e) = entry.write(local) {
        warn!("{}: manifest cache write failed: {}", repo_name, e);
    }
}

/// Returns the parent directory name of a manifest path, used as a short commit label.
///
/// `.specify/specs/code-quality/ggen.toml` → `"code-quality"`
/// `.specify/specs/ci-standard/rust/ggen.toml` → `"rust"`
fn bundle_label(manifest: &str) -> &str {
    std::path::Path::new(manifest)
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or(manifest)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundle_label_extracts_parent_dir_name() {
        assert_eq!(bundle_label(".specify/specs/code-quality/ggen.toml"), "code-quality");
        assert_eq!(bundle_label(".specify/specs/community-health/ggen.toml"), "community-health");
        assert_eq!(bundle_label(".specify/specs/ci-standard/rust/ggen.toml"), "rust");
        assert_eq!(bundle_label(".specify/specs/security-policy/python/ggen.toml"), "python");
    }

    #[test]
    fn bundle_label_falls_back_on_bare_name() {
        // No parent dir → fall back to the manifest string itself
        assert_eq!(bundle_label("ggen.toml"), "ggen.toml");
        assert_eq!(bundle_label("manifest"), "manifest");
    }
}
