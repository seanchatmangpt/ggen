//! Chicago TDD integration tests for the SQLite-backed DaemonState (Lever 2).
//!
//! All tests use real SQLite databases (in-memory via `DaemonState::new(None, ...)`
//! or file-backed via TempDir).  No mocks or stubs.

use std::{fs, sync::Arc};
use tempfile::TempDir;

use ggen_daemon::{CampaignCheckpoint, CampaignRunner, DaemonState};

// ---------------------------------------------------------------------------
// job_runs table
// ---------------------------------------------------------------------------

#[tokio::test]
async fn record_start_inserts_row_with_autoincrement_id() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();

    let id1 = state.record_start("urn:a", "a.toml").await.unwrap();
    let id2 = state.record_start("urn:b", "b.toml").await.unwrap();

    assert!(id1 > 0, "id must be positive, got {}", id1);
    assert!(id2 > id1, "ids must be strictly increasing: {} !> {}", id2, id1);
}

#[tokio::test]
async fn record_finish_updates_exit_code_and_timestamps() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    let id = state.record_start("urn:finish", "spec.toml").await.unwrap();

    state.record_finish(id, 42, "wrote 3 files", "some warning").await.unwrap();

    let runs = state.recent_runs(10).await.unwrap();
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert_eq!(run.exit_code, Some(42));
    assert_eq!(run.stdout_tail.as_deref(), Some("wrote 3 files"));
    assert_eq!(run.stderr_tail.as_deref(), Some("some warning"));
    assert!(run.finished_at.is_some(), "finished_at must be set");
    assert!(
        run.finished_at.as_deref().unwrap().contains('T'),
        "finished_at must be RFC-3339"
    );
}

#[tokio::test]
async fn recent_runs_returns_most_recent_first() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();

    for i in 0..5u32 {
        let id = state.record_start("urn:t", &format!("spec{}.toml", i)).await.unwrap();
        state.record_finish(id, 0, "ok", "").await.unwrap();
    }

    let recent = state.recent_runs(3).await.unwrap();
    assert_eq!(recent.len(), 3, "limit must be respected");
    assert_eq!(recent[0].spec_manifest, "spec4.toml", "most recent must be first");
    assert_eq!(recent[2].spec_manifest, "spec2.toml");
}

// ---------------------------------------------------------------------------
// campaign_checkpoints table
// ---------------------------------------------------------------------------

#[tokio::test]
async fn record_checkpoint_is_idempotent() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();

    let c = CampaignCheckpoint {
        campaign_id: "campaign-2026-06-24".to_owned(),
        day: 3,
        bundles_dispatched: 4,
        repos_succeeded: 10,
        repos_failed: 2,
    };

    // Write twice — must not error or create duplicates.
    state.record_checkpoint(&c).await.unwrap();
    state.record_checkpoint(&c).await.unwrap();

    let days = state.completed_days("campaign-2026-06-24").await;
    assert_eq!(days, vec![3], "exactly one entry despite two writes");
}

#[tokio::test]
async fn completed_days_returns_correct_set() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    let cid = "campaign-test-set";

    for day in [1u8, 3, 5] {
        state.record_checkpoint(&CampaignCheckpoint {
            campaign_id: cid.to_owned(),
            day,
            bundles_dispatched: 1,
            repos_succeeded: 5,
            repos_failed: 0,
        }).await.unwrap();
    }

    let mut days = state.completed_days(cid).await;
    days.sort();
    assert_eq!(days, vec![1, 3, 5]);
}

#[tokio::test]
async fn new_campaign_has_no_completed_days() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    let days = state.completed_days("brand-new-campaign-id").await;
    assert!(days.is_empty(), "fresh campaign must have no completed days");
}

// ---------------------------------------------------------------------------
// Campaign resume: run_full skips checkpointed days
// ---------------------------------------------------------------------------

#[tokio::test]
async fn run_full_skips_already_checkpointed_days() {
    // Arrange: minimal TTL files with one Day1 job and an empty catalog.
    let dir = TempDir::new().unwrap();

    let cron_ttl = dir.path().join("cron.ttl");
    fs::write(
        &cron_ttl,
        r#"@prefix cron: <https://ggen.dev/ontology/cron#> .

<https://ggen.dev/batch/Week1> cron:dispatchBundle <https://ggen.dev/dispatch/Day1/test> .

<https://ggen.dev/dispatch/Day1/test>
    cron:cronExpression "0 6 * * 1" ;
    cron:specManifest ".specify/specs/test/ggen.toml" .
"#,
    ).unwrap();

    let catalog_ttl = dir.path().join("catalog.ttl");
    fs::write(&catalog_ttl, "@prefix doap: <http://usefulinc.com/ns/doap#> .\n").unwrap();

    let repos_dir = dir.path().join("repos");
    fs::create_dir_all(&repos_dir).unwrap();

    // Create state and pre-write a Day 1 checkpoint.
    let state = Arc::new(DaemonState::new(None, cron_ttl.to_string_lossy().into_owned()).await.unwrap());

    let campaign_id = format!("campaign-{}", chrono::Utc::now().date_naive());
    state.record_checkpoint(&CampaignCheckpoint {
        campaign_id: campaign_id.clone(),
        day: 1,
        bundles_dispatched: 1,
        repos_succeeded: 0,
        repos_failed: 0,
    }).await.unwrap();

    // Build a runner whose campaign_id matches the checkpoint written above.
    let mut runner = CampaignRunner::new(
        catalog_ttl,
        cron_ttl,
        repos_dir,
        Arc::clone(&state),
    );
    runner.campaign_id = campaign_id;

    // Act: run the full campaign.
    let results = runner.run_full().await.unwrap();

    // Assert: Day 1 must not appear in results (it was skipped).
    let day_numbers: Vec<u8> = results.iter().filter_map(|r| r.day).collect();
    assert!(
        !day_numbers.contains(&1),
        "Day 1 must be skipped — got day_numbers: {:?}",
        day_numbers
    );

    // No job_runs must have been inserted (the catalog is empty, so dispatch_to_all
    // returns immediately with total_repos=0 for days 2-7, no record_start calls).
    let runs = state.all_runs().await;
    assert!(
        runs.is_empty(),
        "no job_runs must be recorded when catalog is empty and day 1 is checkpointed"
    );
}
