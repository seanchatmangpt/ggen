//! Chicago TDD tests for the AgentRegistry.
//!
//! Uses real `MemoryStore` -- no mocks.

use ggen_a2a_registry::{
    AgentEntry, AgentQuery, AgentRegistry, HealthConfig, HealthStatus, MemoryStore,
};
use std::sync::Arc;
use std::time::Duration;

/// Helper to build a test `AgentEntry`.
fn test_entry(id: &str, agent_type: &str, capabilities: Vec<&str>) -> AgentEntry {
    AgentEntry {
        id: id.to_string(),
        name: format!("Agent {}", id),
        agent_type: agent_type.to_string(),
        endpoint_url: format!("http://localhost:0/{}", id),
        capabilities: capabilities.into_iter().map(String::from).collect(),
        health: HealthStatus::Unknown,
        registered_at: chrono::Utc::now(),
        last_heartbeat: chrono::Utc::now(),
    }
}

#[tokio::test]
async fn register_returns_registration() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    let entry = test_entry("reg-1", "gen", vec!["sparql"]);
    let reg = registry.register(entry).await.unwrap();

    assert_eq!(reg.agent_id, "reg-1");
}

#[tokio::test]
async fn duplicate_register_fails() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    let entry = test_entry("dup", "gen", vec!["sparql"]);
    registry.register(entry.clone()).await.unwrap();
    let err = registry.register(entry).await.unwrap_err();
    let msg = format!("{err}");
    assert!(
        msg.contains("already registered"),
        "unexpected error: {msg}"
    );
}

#[tokio::test]
async fn list_returns_registered_agents() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    registry
        .register(test_entry("l1", "a", vec!["x"]))
        .await
        .unwrap();
    registry
        .register(test_entry("l2", "b", vec!["y"]))
        .await
        .unwrap();

    let agents = registry.list().await.unwrap();
    assert_eq!(agents.len(), 2);
}

#[tokio::test]
async fn discover_by_agent_type() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    registry
        .register(test_entry("gen-1", "code-gen", vec!["sparql"]))
        .await
        .unwrap();
    registry
        .register(test_entry("val-1", "validator", vec!["validate"]))
        .await
        .unwrap();

    let results = registry
        .discover(AgentQuery::new().with_agent_type("code-gen"))
        .await
        .unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "gen-1");
}

#[tokio::test]
async fn discover_by_capability() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    registry
        .register(test_entry("sparql-1", "gen", vec!["sparql", "code-gen"]))
        .await
        .unwrap();
    registry
        .register(test_entry("validate-1", "val", vec!["validate"]))
        .await
        .unwrap();

    let results = registry
        .discover(AgentQuery::new().with_capability("sparql"))
        .await
        .unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "sparql-1");
}

#[tokio::test]
async fn discover_by_health() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    let mut healthy = test_entry("h1", "gen", vec!["sparql"]);
    healthy.health = HealthStatus::Healthy;
    registry.register(healthy).await.unwrap();

    let mut unhealthy = test_entry("u1", "gen", vec!["sparql"]);
    unhealthy.health = HealthStatus::Unhealthy;
    registry.register(unhealthy).await.unwrap();

    let results = registry
        .discover(AgentQuery::new().with_health(HealthStatus::Healthy))
        .await
        .unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "h1");
}

#[tokio::test]
async fn discover_no_match_returns_error() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    registry
        .register(test_entry("x", "gen", vec!["sparql"]))
        .await
        .unwrap();

    let err = registry
        .discover(AgentQuery::new().with_agent_type("nonexistent"))
        .await
        .unwrap_err();
    let msg = format!("{err}");
    assert!(msg.contains("no agents matched"), "unexpected error: {msg}");
}

#[tokio::test]
async fn discover_with_limit() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    for i in 0..5 {
        registry
            .register(test_entry(&format!("lim-{}", i), "gen", vec!["sparql"]))
            .await
            .unwrap();
    }

    let results = registry
        .discover(AgentQuery::new().with_agent_type("gen").with_limit(3))
        .await
        .unwrap();

    assert_eq!(results.len(), 3);
}

#[tokio::test]
async fn deregister_removes_agent() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    registry
        .register(test_entry("del-1", "gen", vec!["x"]))
        .await
        .unwrap();
    assert_eq!(registry.list().await.unwrap().len(), 1);

    registry.deregister("del-1").await.unwrap();
    assert_eq!(registry.list().await.unwrap().len(), 0);
}

#[tokio::test]
async fn deregister_nonexistent_fails() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    let err = registry.deregister("ghost").await.unwrap_err();
    let msg = format!("{err}");
    assert!(msg.contains("not found"), "unexpected error: {msg}");
}

#[tokio::test]
async fn shutdown_succeeds() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    registry
        .register(test_entry("shut-1", "gen", vec!["x"]))
        .await
        .unwrap();
    registry.shutdown().await.unwrap();
}

#[tokio::test]
async fn health_check_nonexistent_fails() {
    let store = Arc::new(MemoryStore::new());
    let registry = AgentRegistry::new(store);

    let err = registry.health_check("ghost").await.unwrap_err();
    let msg = format!("{err}");
    assert!(msg.contains("not found"), "unexpected error: {msg}");
}

#[tokio::test]
async fn health_monitor_lifecycle() {
    let store = Arc::new(MemoryStore::new());
    let config = HealthConfig {
        check_interval: Duration::from_secs(1),
        ping_timeout: Duration::from_millis(100),
        offline_threshold: 2,
    };
    let registry = AgentRegistry::with_health_config(store, config);

    assert!(!registry.is_health_monitor_running());
    registry.start_health_monitor().await;
    assert!(registry.is_health_monitor_running());
    registry.stop_health_monitor().await;
    assert!(!registry.is_health_monitor_running());
}

#[tokio::test]
async fn registry_with_custom_health_config() {
    let store = Arc::new(MemoryStore::new());
    let config = HealthConfig {
        check_interval: Duration::from_secs(30),
        ping_timeout: Duration::from_secs(5),
        offline_threshold: 5,
    };
    let registry = AgentRegistry::with_health_config(store, config);

    registry
        .register(test_entry("cfg-1", "gen", vec!["x"]))
        .await
        .unwrap();
    let agents = registry.list().await.unwrap();
    assert_eq!(agents.len(), 1);
}
