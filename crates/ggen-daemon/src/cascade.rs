use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::Arc;
use tracing::info;

use crate::{
    catalog::RepoCatalogEntry,
    parallel_dispatch::{dispatch_to_all, BatchResult},
    repo_manager::RepoManager,
    state::DaemonState,
};

/// A single wave in a cascade: one spec bundle applied to all repos.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CascadeWave {
    /// The spec bundle (manifest path) applied in this wave.
    pub bundle: String,
    /// The result of dispatching this bundle to all repos, once the wave completes.
    pub result: Option<BatchResult>,
    /// Zero-based index of this wave in the overall cascade.
    pub wave_index: usize,
}

/// Orchestrates sequential dispatch waves.
///
/// Each wave applies one spec bundle to every repo in the catalog before the
/// next bundle is started, producing a commit cascade across the fleet.
pub struct CascadeRunner {
    bundles: Vec<String>,
    repos: Vec<RepoCatalogEntry>,
    manager: RepoManager,
    state: Arc<DaemonState>,
    concurrency: usize,
    ggen_root: PathBuf,
}

impl CascadeRunner {
    /// Create a new `CascadeRunner`.
    ///
    /// # Parameters
    /// - `bundles`     – ordered list of spec-manifest paths (relative to `ggen_root`)
    /// - `repos`       – catalog of target repos to receive each bundle
    /// - `manager`     – repo manager used for clone / pull / commit / push
    /// - `state`       – shared daemon state for recording job runs
    /// - `concurrency` – maximum number of repos processed concurrently per wave
    /// - `ggen_root`   – root of the ggen workspace (spec files are resolved here)
    pub fn new(
        bundles: Vec<String>,
        repos: Vec<RepoCatalogEntry>,
        manager: RepoManager,
        state: Arc<DaemonState>,
        concurrency: usize,
        ggen_root: PathBuf,
    ) -> Self {
        Self {
            bundles,
            repos,
            manager,
            state,
            concurrency,
            ggen_root,
        }
    }

    /// Run all waves in sequence.
    ///
    /// For each bundle in `self.bundles`, every repo in `self.repos` receives
    /// the bundle before the next bundle is started.  Returns a `Vec<CascadeWave>`
    /// with one entry per bundle; each entry records the `BatchResult` for that wave.
    pub async fn run(&self, dispatch_iri: &str) -> Vec<CascadeWave> {
        let mut waves = Vec::with_capacity(self.bundles.len());

        for (wave_index, bundle) in self.bundles.iter().enumerate() {
            info!(
                wave_index,
                bundle = %bundle,
                repos = self.repos.len(),
                "cascade wave starting"
            );

            let batch = dispatch_to_all(
                dispatch_iri,
                bundle,
                &self.repos,
                &self.manager,
                &self.state,
                self.concurrency,
                self.ggen_root.clone(),
            )
            .await;

            info!(
                wave_index,
                bundle = %bundle,
                succeeded = batch.succeeded,
                failed = batch.failed,
                "cascade wave complete"
            );

            waves.push(CascadeWave {
                bundle: bundle.clone(),
                result: Some(batch),
                wave_index,
            });
        }

        waves
    }

    /// Count the total number of repos that succeeded across all completed waves.
    pub fn total_commits(waves: &[CascadeWave]) -> usize {
        waves
            .iter()
            .filter_map(|w| w.result.as_ref())
            .map(|r| r.succeeded)
            .sum()
    }
}
