use serde::{Deserialize, Serialize};
use std::{collections::HashMap, sync::Arc};
use crate::{error::Result, state::DaemonState};

#[derive(Debug, Serialize, Deserialize)]
pub struct ManifestMetric {
    pub spec_manifest: String,
    pub runs: i64,
    pub succeeded: i64,
    pub failed: i64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CampaignDashboard {
    pub total_commits_attempted: i64,
    pub total_commits_succeeded: i64,
    pub total_commits_failed: i64,
    pub overall_success_rate: f64,
    pub by_manifest: Vec<ManifestMetric>,
}

pub struct MetricsStore {
    state: Arc<DaemonState>,
}

impl MetricsStore {
    pub fn new(state: Arc<DaemonState>) -> Self {
        Self { state }
    }

    pub async fn dashboard(&self) -> Result<CampaignDashboard> {
        let runs = self.state.all_runs().await;
        let finished: Vec<_> = runs.iter().filter(|r| r.finished_at.is_some()).collect();

        let attempted = finished.len() as i64;
        let succeeded = finished.iter().filter(|r| r.exit_code == Some(0)).count() as i64;
        let failed = attempted - succeeded;
        let rate = if attempted > 0 { succeeded as f64 / attempted as f64 } else { 0.0 };

        let mut by_manifest: HashMap<&str, ManifestMetric> = HashMap::new();
        for r in &finished {
            let e = by_manifest.entry(r.spec_manifest.as_str()).or_insert_with(|| ManifestMetric {
                spec_manifest: r.spec_manifest.clone(),
                runs: 0,
                succeeded: 0,
                failed: 0,
            });
            e.runs += 1;
            if r.exit_code == Some(0) { e.succeeded += 1; } else { e.failed += 1; }
        }

        let mut by_manifest_vec: Vec<ManifestMetric> = by_manifest.into_values().collect();
        by_manifest_vec.sort_by(|a, b| b.runs.cmp(&a.runs));

        Ok(CampaignDashboard {
            total_commits_attempted: attempted,
            total_commits_succeeded: succeeded,
            total_commits_failed: failed,
            overall_success_rate: rate,
            by_manifest: by_manifest_vec,
        })
    }
}
