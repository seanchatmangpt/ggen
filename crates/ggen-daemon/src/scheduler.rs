use std::{path::{Path, PathBuf}, sync::Arc, time::Duration};
use tracing::{error, info, warn};
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
    /// Load jobs from the cron ontology TTL and register tokio tasks for each.
    ///
    /// Jobs with unrecognized cron expressions are logged as errors and skipped —
    /// they are NOT silently demoted to 60-second polling.
    pub async fn from_ontology(
        ttl_path: &std::path::Path,
        state: Arc<DaemonState>,
        working_dir: PathBuf,
    ) -> Result<Self> {
        let jobs = load_jobs(ttl_path)?;
        info!("loaded {} jobs from ontology", jobs.len());

        let mut handles = Vec::new();

        for job_def in &jobs {
            let cron_expr = job_def.cron_expr.clone();

            // Validate the expression up front; skip jobs we cannot schedule.
            match duration_until_next_fire(&cron_expr) {
                Some(first_sleep) => {
                    info!(
                        "registered job '{}' @ '{}' (first fire in {:?})",
                        job_def.spec_manifest, cron_expr, first_sleep
                    );
                }
                None => {
                    error!(
                        "unrecognized cron expression '{}' for job '{}' — job will NOT be scheduled",
                        cron_expr, job_def.spec_manifest
                    );
                    continue;
                }
            }

            let state = Arc::clone(&state);
            let wd = working_dir.clone();
            let dispatch_iri = job_def.dispatch_iri.clone();
            let spec_manifest = job_def.spec_manifest.clone();

            let handle = tokio::spawn(async move {
                loop {
                    let sleep_for = match duration_until_next_fire(&cron_expr) {
                        Some(d) => d,
                        None => {
                            // Should not happen for a validated expression, but defend anyway.
                            error!(
                                "cannot recompute next fire for '{}'; retrying in 1 hour",
                                cron_expr
                            );
                            Duration::from_secs(3600)
                        }
                    };
                    info!("job '{}' sleeping {:?} until next fire", spec_manifest, sleep_for);
                    tokio::time::sleep(sleep_for).await;
                    match dispatch_bundle(&dispatch_iri, &spec_manifest, &wd, &state).await {
                        Ok(r) => info!("bundle done: {} exit={}", spec_manifest, r.exit_code),
                        Err(e) => error!("bundle error: {} — {}", spec_manifest, e),
                    }
                }
            });
            handles.push(handle);
        }

        Ok(Self { jobs: jobs.clone(), handles })
    }

    /// Block until SIGINT/SIGTERM, then abort all job tasks.
    pub async fn run_until_signal(self) {
        tokio::signal::ctrl_c().await.ok();
        info!("shutdown signal received — aborting {} job tasks", self.handles.len());
        for h in self.handles {
            h.abort();
        }
    }
}

// ---------------------------------------------------------------------------
// Watch mode (Lever 6)
// ---------------------------------------------------------------------------

/// Start a file-system watcher on `watch_dir` and dispatch the affected
/// manifest whenever a `.toml` or `.ttl` spec file changes.
///
/// Uses `notify-debouncer-full` with a `debounce_secs`-second window (default 5)
/// to avoid duplicate events on rapid saves.  Runs until the tokio runtime is
/// shut down.
pub async fn watch_and_dispatch(
    watch_dir: PathBuf,
    state: Arc<DaemonState>,
    working_dir: PathBuf,
    debounce_secs: u64,
) -> Result<()> {
    use notify::RecursiveMode;
    use notify_debouncer_full::{new_debouncer, DebounceEventResult};
    use std::sync::mpsc;

    let (std_tx, std_rx) = mpsc::channel::<PathBuf>();
    let debounce_dur = Duration::from_secs(debounce_secs.max(1));
    let watch_dir_clone = watch_dir.clone();

    // Spawn a dedicated blocking thread for the notify watcher.
    // The thread runs indefinitely, keeping the Debouncer alive.
    std::thread::spawn(move || {
        let tx = std_tx;
        let mut debouncer = match new_debouncer(
            debounce_dur,
            None,
            move |result: DebounceEventResult| match result {
                Ok(events) => {
                    for event in events {
                        for path in event.event.paths {
                            if is_spec_file(&path) {
                                let _ = tx.send(path);
                            }
                        }
                    }
                }
                Err(errors) => {
                    for e in errors {
                        warn!("watch error: {:?}", e);
                    }
                }
            },
        ) {
            Ok(d) => d,
            Err(e) => {
                error!("watcher init failed: {}", e);
                return;
            }
        };

        if let Err(e) = debouncer.watch(&watch_dir_clone, RecursiveMode::Recursive) {
            error!("watch start failed for {}: {}", watch_dir_clone.display(), e);
            return;
        }

        info!(
            "watch mode: monitoring {} ({}s debounce)",
            watch_dir_clone.display(),
            debounce_secs.max(1)
        );

        // Park forever — the Debouncer must stay alive to receive events.
        loop {
            std::thread::sleep(Duration::from_secs(3600));
        }
    });

    // Bridge: forward path changes from the std::sync::mpsc channel to a tokio
    // unbounded channel so the async loop below can await on them.
    let (async_tx, mut async_rx) = tokio::sync::mpsc::unbounded_channel::<PathBuf>();
    tokio::task::spawn_blocking(move || {
        for path in std_rx {
            if async_tx.send(path).is_err() {
                break;
            }
        }
    });

    // Async dispatch loop: for each changed spec file, trigger a bundle dispatch.
    while let Some(path) = async_rx.recv().await {
        info!("spec changed: {} — triggering dispatch", path.display());
        if let Some(manifest) = path.to_str() {
            match dispatch_bundle("watch:spec-changed", manifest, &working_dir, &state).await {
                Ok(r) => info!("watch dispatch complete: exit={}", r.exit_code),
                Err(e) => warn!("watch dispatch failed: {}", e),
            }
        }
    }

    Ok(())
}

/// Returns true for files that should trigger a re-dispatch when changed.
pub(crate) fn is_spec_file(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|e| e.to_str()),
        Some("toml") | Some("ttl")
    )
}

/// Compute how long to sleep until the next cron fire, relative to now (UTC).
///
/// Supported patterns (standard 5-field cron, UTC):
/// - `*/N * * * *`  — every N minutes, aligned to clock-minute boundaries (N ∈ 1..=59)
/// - `M H * * *`    — daily at HH:MM UTC (M ∈ 0..=59, H ∈ 0..=23)
///
/// Returns `None` for unsupported or invalid expressions.  The caller is expected
/// to log an error and skip (or fall back) when `None` is returned.
pub(crate) fn duration_until_next_fire(expr: &str) -> Option<Duration> {
    let parts: Vec<&str> = expr.trim().split_whitespace().collect();
    if parts.len() != 5 {
        return None;
    }

    let now_secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .ok()?
        .as_secs();

    // "*/N * * * *" — every N minutes, clock-boundary aligned
    if let Some(n_str) = parts[0].strip_prefix("*/") {
        if parts[1] != "*" || parts[2] != "*" || parts[3] != "*" || parts[4] != "*" {
            return None;
        }
        let n: u64 = n_str.parse().ok()?;
        if n == 0 || n > 59 {
            return None;
        }
        let current_min = (now_secs % 3600) / 60;
        let current_sec = now_secs % 60;
        // Integer division gives the slot; the next boundary is one slot ahead.
        let next_boundary = (current_min / n + 1) * n;
        let mins_to_wait = next_boundary - current_min; // always >= 1
        let secs_until = mins_to_wait * 60 - current_sec;
        return Some(Duration::from_secs(secs_until));
    }

    // "M H * * *" — daily at H:M UTC
    if parts[2] == "*" && parts[3] == "*" && parts[4] == "*" {
        let minute: u64 = parts[0].parse().ok()?;
        let hour: u64 = parts[1].parse().ok()?;
        if hour >= 24 || minute >= 60 {
            return None;
        }
        let target_sod = hour * 3600 + minute * 60; // target second-of-day
        let sod = now_secs % 86400;                  // current second-of-day
        let secs_until = if sod < target_sod {
            target_sod - sod
        } else {
            // Already past today's window; fire at the same time tomorrow.
            86400 - sod + target_sod
        };
        return Some(Duration::from_secs(secs_until));
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_five_minutes_sleeps_at_most_five_minutes() {
        let d = duration_until_next_fire("*/5 * * * *").expect("*/5 must parse");
        let secs = d.as_secs();
        assert!(secs > 0 && secs <= 300, "expected 0 < sleep <= 300s, got {}", secs);
    }

    #[test]
    fn every_one_minute_sleeps_at_most_one_minute() {
        let d = duration_until_next_fire("*/1 * * * *").expect("*/1 must parse");
        let secs = d.as_secs();
        assert!(secs > 0 && secs <= 60, "expected 0 < sleep <= 60s, got {}", secs);
    }

    #[test]
    fn daily_9am_schedule_sleeps_within_one_day() {
        let d = duration_until_next_fire("0 9 * * *").expect("0 9 * * * must parse");
        let secs = d.as_secs();
        assert!(secs > 0 && secs <= 86400, "expected 0 < sleep <= 86400s, got {}", secs);
    }

    #[test]
    fn midnight_schedule_sleeps_within_one_day() {
        let d = duration_until_next_fire("0 0 * * *").expect("0 0 * * * must parse");
        let secs = d.as_secs();
        assert!(secs > 0 && secs <= 86400);
    }

    #[test]
    fn end_of_day_schedule_sleeps_within_one_day() {
        let d = duration_until_next_fire("59 23 * * *").expect("23:59 must parse");
        let secs = d.as_secs();
        assert!(secs > 0 && secs <= 86400);
    }

    #[test]
    fn all_seven_campaign_expressions_are_supported() {
        // Every expression used in cron-schedule.ttl must parse without fallback.
        let exprs = [
            "0 9 * * *", "0 10 * * *", "0 11 * * *", "0 12 * * *",
            "0 14 * * *", "0 15 * * *", "0 16 * * *",
        ];
        for expr in exprs {
            assert!(
                duration_until_next_fire(expr).is_some(),
                "TTL expression '{}' must parse",
                expr
            );
        }
    }

    // ── Rejection tests ──────────────────────────────────────────────────────

    #[test]
    fn rejects_dom_field_restriction() {
        assert!(duration_until_next_fire("0 9 1 * *").is_none(), "DOM 1 unsupported");
    }

    #[test]
    fn rejects_month_field_restriction() {
        assert!(duration_until_next_fire("0 9 * 6 *").is_none(), "month 6 unsupported");
    }

    #[test]
    fn rejects_dow_field_restriction() {
        assert!(duration_until_next_fire("0 9 * * 1").is_none(), "DOW 1 unsupported");
    }

    #[test]
    fn rejects_hour_24() {
        assert!(duration_until_next_fire("0 24 * * *").is_none());
    }

    #[test]
    fn rejects_hour_out_of_range() {
        assert!(duration_until_next_fire("0 25 * * *").is_none());
    }

    #[test]
    fn rejects_minute_60() {
        assert!(duration_until_next_fire("60 9 * * *").is_none());
    }

    #[test]
    fn rejects_zero_interval() {
        assert!(duration_until_next_fire("*/0 * * * *").is_none());
    }

    #[test]
    fn rejects_interval_60() {
        // */60 is not valid 5-field cron (minute field is 0-59)
        assert!(duration_until_next_fire("*/60 * * * *").is_none());
    }

    #[test]
    fn rejects_empty_string() {
        assert!(duration_until_next_fire("").is_none());
    }

    #[test]
    fn rejects_wrong_field_count() {
        assert!(duration_until_next_fire("* * * *").is_none());
        assert!(duration_until_next_fire("* * * * * *").is_none());
        assert!(duration_until_next_fire("not a cron expression").is_none());
    }

    #[test]
    fn rejects_interval_with_mixed_wildcards() {
        // "*/5 0 * * *" is not a supported pattern (minute-interval + fixed hour)
        assert!(duration_until_next_fire("*/5 0 * * *").is_none());
    }
}
