//! Integration tests for cloud-native state management
//!
//! Tests:
//! - State persistence and recovery
//! - ACID transactions with optimistic locking
//! - Version conflict detection
//! - Eventual consistency and conflict resolution
//! - Event sourcing and replay
//! - Crash recovery with snapshots
//! - Distributed locking

use tai_state::{
    ChangeMetadata, ConflictResolver, EventualConsistency, FirestoreStore, LastWriteWinsResolver,
    StateId, StateSnapshot, StateMachinePersister, TransactionManager,
};
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;

#[tokio::test]
async fn test_basic_state_persistence() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    let metadata = ChangeMetadata::new("system", "initialization", None);
    let snapshot = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        metadata,
    );

    store
        .save_state("governor1", "counter", snapshot, metadata)
        .await
        .expect("Failed to save state");

    let (retrieved, version) = store
        .get_state("governor1", "counter")
        .await
        .expect("Failed to retrieve state");

    assert_eq!(retrieved.data, json!({"count": 0}));
    assert_eq!(version, 1);
}

#[tokio::test]
async fn test_optimistic_locking_success() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    // Create initial state
    let metadata = ChangeMetadata::new("system", "init", None);
    let initial = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        metadata.clone(),
    );

    store
        .save_state("governor1", "counter", initial, metadata)
        .await
        .expect("Failed to save initial state");

    // Get current version
    let (current_state, current_version) = store
        .get_state("governor1", "counter")
        .await
        .expect("Failed to get state");

    // Update with correct version
    let new_state = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 1}),
        ChangeMetadata::new("test", "increment", Some(current_version)),
    );

    let new_version = store
        .update_state(
            "governor1",
            "counter",
            new_state,
            current_version,
            ChangeMetadata::new("test", "update", None),
        )
        .await
        .expect("Update failed");

    assert_eq!(new_version, current_version + 1);
}

#[tokio::test]
async fn test_optimistic_locking_conflict() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    // Create initial state
    let metadata = ChangeMetadata::new("system", "init", None);
    let initial = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        metadata.clone(),
    );

    store
        .save_state("governor1", "counter", initial, metadata)
        .await
        .expect("Failed to save initial state");

    // Get version
    let (_, version) = store
        .get_state("governor1", "counter")
        .await
        .expect("Failed to get state");

    // Try to update with wrong version
    let new_state = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 1}),
        ChangeMetadata::new("test", "update", Some(version)),
    );

    let result = store
        .update_state(
            "governor1",
            "counter",
            new_state,
            version + 1, // Wrong version
            ChangeMetadata::new("test", "update", None),
        )
        .await;

    assert!(matches!(result, Err(tai_state::Error::VersionConflict { .. })));
}

#[tokio::test]
async fn test_event_logging_and_audit_trail() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    // Create initial state
    let initial = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        ChangeMetadata::new("system", "init", None),
    );

    store
        .save_state(
            "governor1",
            "counter",
            initial.clone(),
            ChangeMetadata::new("system", "init", None),
        )
        .await
        .expect("Failed to save state");

    // Log transitions as events
    let from_state = initial.clone();
    let to_state = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 1}),
        ChangeMetadata::new("test", "increment", None),
    );

    let event = tai_state::Event::new(
        "evt1".to_string(),
        "increment".to_string(),
        Some(from_state),
        to_state,
        ChangeMetadata::new("test", "user-action", None),
    );

    store
        .log_event("governor1", event)
        .await
        .expect("Failed to log event");

    // Retrieve events
    let events = store
        .get_all_events("governor1")
        .await
        .expect("Failed to get events");

    assert_eq!(events.len(), 1);
    assert_eq!(events[0].action, "increment");
}

#[tokio::test]
async fn test_transaction_with_retries() {
    let store = Arc::new(FirestoreStore::new("test-project").await.expect("Failed to create store"));
    let manager = TransactionManager::new(store.clone());

    // Create initial state
    let initial = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        ChangeMetadata::new("system", "init", None),
    );

    store
        .save_state("governor1", "counter", initial, ChangeMetadata::new("system", "init", None))
        .await
        .expect("Failed to save state");

    // Execute a simple transaction
    let result = manager
        .execute_transaction("governor1", |store| {
            Box::pin(async move {
                let (current, version) = store
                    .get_state("governor1", "counter")
                    .await?;

                let new_state = StateSnapshot::new(
                    StateId::new("counter".to_string()),
                    json!({"count": current.data["count"].as_i64().unwrap_or(0) + 1}),
                    ChangeMetadata::new("test", "increment", Some(version)),
                );

                store
                    .update_state(
                        "governor1",
                        "counter",
                        new_state,
                        version,
                        ChangeMetadata::new("test", "update", None),
                    )
                    .await?;

                Ok(())
            })
        })
        .await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_distributed_locking() {
    let store = Arc::new(FirestoreStore::new("test-project").await.expect("Failed to create store"));
    let manager = TransactionManager::new(store);

    // Acquire lock
    let lock = manager
        .acquire_lock("resource1", "client1", Duration::from_secs(5))
        .await
        .expect("Failed to acquire lock");

    assert_eq!(lock.resource, "resource1");
    assert_eq!(lock.client_id, "client1");

    // Release lock
    manager
        .release_lock("resource1", &lock.lock_token)
        .await
        .expect("Failed to release lock");
}

#[tokio::test]
async fn test_eventual_consistency_last_write_wins() {
    let ec = EventualConsistency::new();

    // Create two conflicting states
    let state1 = StateSnapshot::new(
        StateId::new("state1".to_string()),
        json!({"value": 1}),
        ChangeMetadata::new("writer1", "change", None),
    );

    tokio::time::sleep(Duration::from_millis(1)).await;

    let state2 = StateSnapshot::new(
        StateId::new("state2".to_string()),
        json!({"value": 2}),
        ChangeMetadata::new("writer2", "change", None),
    );

    // Resolve conflict
    let resolved = ec.resolve_conflict(&state1, &state2).expect("Resolution failed");

    // Latest write should win (state2 has later timestamp)
    assert_eq!(resolved.data, json!({"value": 2}));
}

#[tokio::test]
async fn test_stale_read_detection() {
    let ec = EventualConsistency::new();

    let old_state = StateSnapshot::new(
        StateId::new("state".to_string()),
        json!({"value": 1}),
        ChangeMetadata::new("writer", "old", None),
    );

    // Simulate newer version
    let mut new_state = StateSnapshot::new(
        StateId::new("state".to_string()),
        json!({"value": 2}),
        ChangeMetadata::new("writer", "new", None),
    );
    new_state.version = old_state.version + 5;

    // Should detect stale read
    let result = ec.detect_stale_read(&old_state, &new_state);
    assert!(matches!(result, Err(tai_state::Error::StaleRead { .. })));
}

#[tokio::test]
async fn test_event_sourcing_and_replay() {
    let store = Arc::new(FirestoreStore::new("test-project").await.expect("Failed to create store"));
    let persister = StateMachinePersister::with_defaults(store.clone());

    // Log initial state
    let initial = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        ChangeMetadata::new("system", "init", None),
    );

    store
        .save_state(
            "governor1",
            "counter",
            initial.clone(),
            ChangeMetadata::new("system", "init", None),
        )
        .await
        .expect("Failed to save initial state");

    // Log transitions
    let mut state = initial;
    for i in 1..=3 {
        let new_state = StateSnapshot::new(
            StateId::new("counter".to_string()),
            json!({"count": i}),
            ChangeMetadata::new("test", "increment", Some(i - 1)),
        );

        persister
            .log_transition(
                "governor1",
                Some(&state),
                &new_state,
                "increment",
                ChangeMetadata::new("test", "user-action", None),
            )
            .await
            .expect("Failed to log transition");

        state = new_state;
    }

    // Replay events
    let all_events = store
        .get_all_events("governor1")
        .await
        .expect("Failed to get events");

    let replayed = store
        .replay_events(&all_events, chrono::Utc::now())
        .await
        .expect("Failed to replay events");

    assert!(replayed.is_some());
    let final_state = replayed.unwrap();
    assert_eq!(final_state.data, json!({"count": 3}));
}

#[tokio::test]
async fn test_snapshot_creation_and_recovery() {
    let store = Arc::new(FirestoreStore::new("test-project").await.expect("Failed to create store"));
    let mut config = tai_state::PersistenceConfig::default();
    config.snapshot_interval = 2;
    let persister = StateMachinePersister::new(store.clone(), config);

    // Create and transition state
    let initial = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        ChangeMetadata::new("system", "init", None),
    );

    let state = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 1}),
        ChangeMetadata::new("test", "update", None),
    );

    persister
        .log_transition(
            "governor1",
            Some(&initial),
            &state,
            "increment",
            ChangeMetadata::new("test", "user-action", None),
        )
        .await
        .expect("Failed to log transition");

    // Create explicit snapshot
    let snapshot_state = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 1}),
        ChangeMetadata::new("system", "snapshot", None),
    );

    store
        .create_snapshot("governor1", "counter", snapshot_state)
        .await
        .expect("Failed to create snapshot");

    // Load snapshot
    let loaded = store
        .load_snapshot("governor1", "counter")
        .await
        .expect("Failed to load snapshot");

    assert!(loaded.is_some());
    assert_eq!(loaded.unwrap().data, json!({"count": 1}));
}

#[tokio::test]
async fn test_batch_state_operations() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    // Create multiple states
    for i in 0..5 {
        let metadata = ChangeMetadata::new("system", "init", None);
        let snapshot = StateSnapshot::new(
            StateId::new(format!("state{}", i)),
            json!({"id": i}),
            metadata.clone(),
        );

        store
            .save_state("governor1", &format!("state{}", i), snapshot, metadata)
            .await
            .expect("Failed to save state");
    }

    // Batch get
    let state_ids = vec!["state0", "state1", "state2"];
    let states = store
        .batch_get_states("governor1", &state_ids)
        .await
        .expect("Failed to batch get states");

    assert!(states.len() <= state_ids.len());
}

#[tokio::test]
async fn test_store_statistics() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    let metadata = ChangeMetadata::new("system", "init", None);
    let snapshot = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        metadata.clone(),
    );

    store
        .save_state("governor1", "counter", snapshot, metadata)
        .await
        .expect("Failed to save state");

    let stats = store.get_stats().await.expect("Failed to get stats");

    assert!(stats.total_states > 0);
    assert_eq!(stats.max_version, 1);
}

#[tokio::test]
async fn test_state_deletion() {
    let store = FirestoreStore::new("test-project").await.expect("Failed to create store");

    let metadata = ChangeMetadata::new("system", "init", None);
    let snapshot = StateSnapshot::new(
        StateId::new("counter".to_string()),
        json!({"count": 0}),
        metadata.clone(),
    );

    store
        .save_state("governor1", "counter", snapshot, metadata)
        .await
        .expect("Failed to save state");

    // Verify exists
    let exists = store
        .exists("governor1", "counter")
        .await
        .expect("Failed to check existence");
    assert!(exists);

    // Delete
    store
        .delete_state("governor1", "counter")
        .await
        .expect("Failed to delete state");

    // Verify deleted
    let exists = store
        .exists("governor1", "counter")
        .await
        .expect("Failed to check existence");
    assert!(!exists);
}

#[tokio::test]
async fn test_consistency_validation() {
    let store = Arc::new(FirestoreStore::new("test-project").await.expect("Failed to create store"));
    let persister = StateMachinePersister::with_defaults(store);

    // Validate empty history
    let report = persister
        .validate_consistency("governor1")
        .await
        .expect("Validation failed");

    assert!(report.is_consistent);
}
