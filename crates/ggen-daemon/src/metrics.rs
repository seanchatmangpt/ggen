use chrono::DateTime;
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
    /// P50 latency (milliseconds) across all completed job runs.
    pub latency_p50_ms: i64,
    /// P95 latency (milliseconds) across all completed job runs.
    pub latency_p95_ms: i64,
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

        // Compute latency percentiles from RFC-3339 timestamp pairs.
        let mut durations_ms: Vec<i64> = finished
            .iter()
            .filter_map(|r| {
                let start = DateTime::parse_from_rfc3339(&r.started_at).ok()?;
                let end = DateTime::parse_from_rfc3339(r.finished_at.as_deref()?).ok()?;
                Some((end - start).num_milliseconds())
            })
            .filter(|&d| d >= 0)
            .collect();

        let (p50, p95) = latency_percentiles(&mut durations_ms);

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
            latency_p50_ms: p50,
            latency_p95_ms: p95,
            by_manifest: by_manifest_vec,
        })
    }
}

/// Return (p50, p95) latency in milliseconds from a slice of durations.
/// The slice is sorted in-place for efficiency; pass an empty slice to get (0, 0).
fn latency_percentiles(durations_ms: &mut Vec<i64>) -> (i64, i64) {
    if durations_ms.is_empty() {
        return (0, 0);
    }
    durations_ms.sort_unstable();
    let len = durations_ms.len();
    let p50_idx = (len * 50 / 100).saturating_sub(1).min(len - 1);
    let p95_idx = (len * 95 / 100).saturating_sub(1).min(len - 1);
    (durations_ms[p50_idx], durations_ms[p95_idx])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn latency_percentiles_empty_returns_zeros() {
        let (p50, p95) = latency_percentiles(&mut vec![]);
        assert_eq!(p50, 0);
        assert_eq!(p95, 0);
    }

    #[test]
    fn latency_percentiles_single_element() {
        let (p50, p95) = latency_percentiles(&mut vec![100]);
        assert_eq!(p50, 100);
        assert_eq!(p95, 100);
    }

    #[test]
    fn latency_percentiles_ten_elements_sorted() {
        let mut durations = vec![10, 20, 30, 40, 50, 60, 70, 80, 90, 100];
        let (p50, p95) = latency_percentiles(&mut durations);
        // p50 = index 4 (50% of 10 = 5, saturating_sub 1 = 4) → 50
        assert_eq!(p50, 50);
        // p95 = index 8 (95% of 10 = 9, saturating_sub 1 = 8) → 90
        assert_eq!(p95, 90);
    }

    #[test]
    fn latency_percentiles_unsorted_input_is_sorted() {
        let mut durations = vec![100, 10, 50, 90, 20, 70, 40, 80, 30, 60];
        let (p50, p95) = latency_percentiles(&mut durations);
        // Same result as sorted
        assert_eq!(p50, 50);
        assert_eq!(p95, 90);
    }
}
