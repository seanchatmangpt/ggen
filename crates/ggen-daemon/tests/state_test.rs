use ggen_daemon::DaemonState;
use tempfile::TempDir;

#[tokio::test]
async fn record_start_assigns_sequential_positive_ids() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    let id1 = state.record_start("urn:a", "a.toml").await.unwrap();
    let id2 = state.record_start("urn:b", "b.toml").await.unwrap();
    assert!(id1 > 0, "id must be positive");
    assert!(id2 > id1, "ids must be strictly increasing");
}

#[tokio::test]
async fn record_finish_captures_exit_code_and_output_tails() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    let id = state.record_start("urn:test", "spec.toml").await.unwrap();
    state
        .record_finish(id, 42, "wrote 3 files", "some warning")
        .await
        .unwrap();

    let runs = state.recent_runs(10).await.unwrap();
    assert_eq!(runs.len(), 1);
    assert_eq!(runs[0].exit_code, Some(42));
    assert_eq!(runs[0].stdout_tail.as_deref(), Some("wrote 3 files"));
    assert_eq!(runs[0].stderr_tail.as_deref(), Some("some warning"));
    assert!(runs[0].finished_at.is_some(), "finished_at must be set");
}

#[tokio::test]
async fn timestamps_are_rfc3339() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    let id = state.record_start("urn:test", "spec.toml").await.unwrap();
    state.record_finish(id, 0, "ok", "").await.unwrap();

    let runs = state.all_runs().await;
    assert_eq!(runs.len(), 1);
    let run = &runs[0];

    // RFC-3339 requires the 'T' separator between date and time.
    assert!(
        run.started_at.contains('T'),
        "started_at must be RFC-3339, got: {}",
        run.started_at
    );
    let finished = run.finished_at.as_deref().expect("finished_at must be set");
    assert!(
        finished.contains('T'),
        "finished_at must be RFC-3339, got: {}",
        finished
    );
}

#[tokio::test]
async fn state_persists_to_disk_and_survives_reload() {
    let dir = TempDir::new().unwrap();
    let persist = dir.path().join("daemon-state.json");

    {
        let state = DaemonState::new(Some(persist.clone()), "test.ttl".to_owned())
            .await
            .unwrap();
        let id = state.record_start("urn:persist", "bundle.toml").await.unwrap();
        state.record_finish(id, 0, "done", "").await.unwrap();
        // DaemonState drops here; all writes must have flushed.
    }

    assert!(persist.exists(), "state file must be written to disk");

    // Reload and verify the run survived the round-trip.
    let state2 = DaemonState::new(Some(persist), "test.ttl".to_owned())
        .await
        .unwrap();
    let runs = state2.all_runs().await;
    assert_eq!(runs.len(), 1, "exactly one run must survive round-trip");
    assert_eq!(runs[0].spec_manifest, "bundle.toml");
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(
        runs[0].started_at.contains('T'),
        "reloaded timestamp must still be RFC-3339"
    );
}

#[tokio::test]
async fn recent_runs_returns_most_recent_first_and_honours_limit() {
    let state = DaemonState::new(None, "test.ttl".to_owned()).await.unwrap();
    for i in 0..5u32 {
        let id = state
            .record_start("urn:test", &format!("spec{}.toml", i))
            .await
            .unwrap();
        state.record_finish(id, 0, "ok", "").await.unwrap();
    }

    let recent = state.recent_runs(3).await.unwrap();
    assert_eq!(recent.len(), 3, "limit must be respected");
    assert_eq!(recent[0].spec_manifest, "spec4.toml", "most recent must be first");
    assert_eq!(recent[2].spec_manifest, "spec2.toml");
}

#[tokio::test]
async fn corrupt_state_file_is_replaced_with_empty_store() {
    let dir = TempDir::new().unwrap();
    let persist = dir.path().join("state.json");
    std::fs::write(&persist, "this is not json!!!").unwrap();

    // Must not panic or return an error; instead silently resets.
    let state = DaemonState::new(Some(persist.clone()), "test.ttl".to_owned())
        .await
        .unwrap();
    assert!(
        state.all_runs().await.is_empty(),
        "corrupt file must yield an empty store"
    );

    // New writes must succeed after the reset.
    let id = state.record_start("urn:after", "after.toml").await.unwrap();
    state.record_finish(id, 0, "ok", "").await.unwrap();
    let runs = state.all_runs().await;
    assert_eq!(runs.len(), 1);
}
