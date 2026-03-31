//! Chicago TDD tests for the MemoryStore.
//!
//! Uses a real `MemoryStore` -- no mocks, no test doubles.

use ggen_a2a_registry::{AgentEntry, AgentStore, HealthStatus, MemoryStore};
use std::sync::Arc;

/// Helper to build a test `AgentEntry` with sensible defaults.
fn test_entry(id: &str) -> AgentEntry {
    AgentEntry {
        id: id.to_string(),
        name: format!("Agent {}", id),
        agent_type: "test".to_string(),
        endpoint_url: format!("http://localhost:0/{}", id),
        capabilities: vec!["test-cap".to_string()],
        health: HealthStatus::Unknown,
        registered_at: chrono::Utc::now(),
        last_heartbeat: chrono::Utc::now(),
    }
}

#[tokio::test]
async fn register_and_get() {
    let store = MemoryStore::new();
    let entry = test_entry("a1");

    store.register(entry.clone()).await.unwrap();
    let fetched = store.get("a1").await.unwrap();

    assert!(fetched.is_some());
    let fetched = fetched.unwrap();
    assert_eq!(fetched.id, "a1");
    assert_eq!(fetched.name, "Agent a1");
    assert_eq!(fetched.agent_type, "test");
    assert_eq!(fetched.health, HealthStatus::Unknown);
}

#[tokio::test]
async fn get_nonexistent_returns_none() {
    let store = MemoryStore::new();
    let result = store.get("nope").await.unwrap();
    assert!(result.is_none());
}

#[tokio::test]
async fn duplicate_registration_fails() {
    let store = MemoryStore::new();
    let entry = test_entry("dup");

    store.register(entry.clone()).await.unwrap();
    let err = store.register(entry).await.unwrap_err();
    let msg = format!("{err}");
    assert!(msg.contains("already registered"), "unexpected error: {msg}");
}

#[tokio::test]
async fn list_returns_all_registered() {
    let store = MemoryStore::new();

    for i in 0..5 {
        let entry = test_entry(&format!("agent-{}", i));
        store.register(entry).await.unwrap();
    }

    let all = store.list().await.unwrap();
    assert_eq!(all.len(), 5);
}

#[tokio::test]
async fn remove_existing() {
    let store = MemoryStore::new();
    store.register(test_entry("rm")).await.unwrap();

    assert!(store.get("rm").await.unwrap().is_some());
    store.remove("rm").await.unwrap();
    assert!(store.get("rm").await.unwrap().is_none());
}

#[tokio::test]
async fn remove_nonexistent_fails() {
    let store = MemoryStore::new();
    let err = store.remove("ghost").await.unwrap_err();
    let msg = format!("{err}");
    assert!(msg.contains("not found"), "unexpected error: {msg}");
}

#[tokio::test]
async fn update_health_changes_status() {
    let store = MemoryStore::new();
    store.register(test_entry("h1")).await.unwrap();

    store.update_health("h1", HealthStatus::Healthy).await.unwrap();

    let entry = store.get("h1").await.unwrap().unwrap();
    assert_eq!(entry.health, HealthStatus::Healthy);
}

#[tokio::test]
async fn update_health_nonexistent_fails() {
    let store = MemoryStore::new();
    let err = store.update_health("ghost", HealthStatus::Healthy).await.unwrap_err();
    let msg = format!("{err}");
    assert!(msg.contains("not found"), "unexpected error: {msg}");
}

#[tokio::test]
async fn update_health_updates_last_heartbeat() {
    let store = MemoryStore::new();
    store.register(test_entry("hb")).await.unwrap();

    // Capture the initial timestamp.
    let before = chrono::Utc::now();
    store.update_health("hb", HealthStatus::Degraded).await.unwrap();

    let entry = store.get("hb").await.unwrap().unwrap();
    assert!(entry.last_heartbeat >= before);
}

#[tokio::test]
async fn find_by_capability() {
    let store = MemoryStore::new();

    let mut entry_a = test_entry("cap-a");
    entry_a.capabilities = vec!["sparql".to_string(), "code-gen".to_string()];
    store.register(entry_a).await.unwrap();

    let mut entry_b = test_entry("cap-b");
    entry_b.capabilities = vec!["validate".to_string()];
    store.register(entry_b).await.unwrap();

    let results = store.find_by_capability("sparql").await.unwrap();
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "cap-a");

    let results = store.find_by_capability("validate").await.unwrap();
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "cap-b");

    let results = store.find_by_capability("nonexistent").await.unwrap();
    assert!(results.is_empty());
}

#[tokio::test]
async fn arc_store_is_clone_and_shared() {
    let store: Arc<dyn AgentStore> = Arc::new(MemoryStore::new());
    let store2 = Arc::clone(&store);

    store.register(test_entry("shared")).await.unwrap();

    // The second Arc reference should see the same data.
    let fetched = store2.get("shared").await.unwrap();
    assert!(fetched.is_some());
}
