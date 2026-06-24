use std::{collections::HashSet, path::PathBuf, sync::Arc, time::Instant};
use chrono::Utc;
use tracing::{info, warn, instrument};
use serde::{Deserialize, Serialize};

use crate::{
    catalog::{filter_by_language, load_catalog, RepoCatalogEntry},
    error::Result,
    ontology::{load_jobs, JobDef},
    parallel_dispatch::{dispatch_to_all, BatchResult},
    repo_manager::RepoManager,
    state::{CampaignCheckpoint, DaemonState},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct CampaignResult {
    pub day: Option<u8>,
    pub batches: Vec<BatchResult>,
    pub total_commits: usize,
    pub total_errors: usize,
}

pub struct CampaignRunner {
    pub catalog_path: PathBuf,
    pub cron_ttl_path: PathBuf,
    pub work_dir: PathBuf,
    pub ggen_root: PathBuf,
    pub state: Arc<DaemonState>,
    pub concurrency: usize,
    /// Date-stable campaign ID: same value on restart within the same calendar day,
    /// allowing `run_full` to skip already-checkpointed days after a crash.
    pub campaign_id: String,
}

impl CampaignRunner {
    pub fn new(
        catalog_path: PathBuf,
        cron_ttl_path: PathBuf,
        work_dir: PathBuf,
        state: Arc<DaemonState>,
    ) -> Self {
        let ggen_root = std::env::current_dir().unwrap_or_default();
        let campaign_id = format!("campaign-{}", Utc::now().date_naive());
        Self { catalog_path, cron_ttl_path, work_dir, ggen_root, state, concurrency: 8, campaign_id }
    }

    /// Run all bundles for a specific day (1-7) and write a checkpoint on success.
    #[instrument(
        name = "campaign.run_day",
        skip(self),
        fields(
            day = day,
            campaign_id = %self.campaign_id,
            bundle_count = tracing::field::Empty,
            total_commits = tracing::field::Empty,
            total_errors = tracing::field::Empty,
            elapsed_ms = tracing::field::Empty,
        )
    )]
    pub async fn run_day(&self, day: u8) -> Result<CampaignResult> {
        let t = Instant::now();
        let catalog = load_catalog(&self.catalog_path)?;
        let jobs = load_jobs(&self.cron_ttl_path)?;

        // Filter jobs that belong to this day (by matching day prefix in dispatch IRI)
        let day_jobs: Vec<&JobDef> = jobs.iter()
            .filter(|j| j.dispatch_iri.contains(&format!("Day{}", day)))
            .collect();

        info!("day {}: {} bundles to dispatch", day, day_jobs.len());
        tracing::Span::current().record("bundle_count", day_jobs.len());

        let manager = RepoManager::new(self.work_dir.clone(), "seanchatmangpt");
        let mut batches = Vec::new();

        for job in &day_jobs {
            // Filter repos by language if specified
            let repos: Vec<RepoCatalogEntry> = if let Some(lang) = &job.language {
                filter_by_language(&catalog, lang).into_iter().cloned().collect()
            } else {
                catalog.clone()
            };

            info!("bundle {}: {} repos ({:?} language filter)", job.spec_manifest, repos.len(), job.language);

            let batch = dispatch_to_all(
                &job.dispatch_iri,
                &job.spec_manifest,
                &repos,
                &manager,
                &self.state,
                self.concurrency,
                self.ggen_root.clone(),
            ).await;

            info!("batch done: {}/{} succeeded", batch.succeeded, batch.total_repos);
            batches.push(batch);
        }

        let total_commits: usize = batches.iter().map(|b| b.succeeded).sum();
        let total_errors: usize = batches.iter().map(|b| b.failed).sum();
        let elapsed_ms = t.elapsed().as_millis() as u64;
        tracing::Span::current().record("total_commits", total_commits);
        tracing::Span::current().record("total_errors", total_errors);
        tracing::Span::current().record("elapsed_ms", elapsed_ms);

        // Persist checkpoint so a crash + restart can skip this day.
        let checkpoint = CampaignCheckpoint {
            campaign_id: self.campaign_id.clone(),
            day,
            bundles_dispatched: day_jobs.len(),
            repos_succeeded: total_commits,
            repos_failed: total_errors,
        };
        if let Err(e) = self.state.record_checkpoint(&checkpoint).await {
            warn!("checkpoint write failed for day {}: {}", day, e);
        }

        Ok(CampaignResult { day: Some(day), batches, total_commits, total_errors })
    }

    /// Run the full 7-day campaign, skipping days that are already checkpointed.
    ///
    /// On crash + restart, `campaign_id` stays the same (date-stable), so
    /// `completed_days` returns the already-done days and they are skipped.
    pub async fn run_full(&self) -> Result<Vec<CampaignResult>> {
        let done: HashSet<u8> = self.state.completed_days(&self.campaign_id).await
            .into_iter()
            .collect();

        let mut results = Vec::new();
        for day in 1..=7u8 {
            if done.contains(&day) {
                info!("day {}: already checkpointed, skipping", day);
                continue;
            }
            info!("=== Starting Day {} ===", day);
            let r = self.run_day(day).await?;
            info!("Day {} complete: {} commits, {} errors", day, r.total_commits, r.total_errors);
            results.push(r);
        }

        self.state.mark_campaign_complete(&self.campaign_id).await;
        Ok(results)
    }
}
