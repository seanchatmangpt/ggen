//! Chicago TDD tests for health status transitions and the HealthMonitor.
//!
//! Uses real stores and real HTTP endpoints (via `tokio::net::TcpListener`).

use ggen_a2a_registry::{
    AgentEntry, AgentStore, HealthConfig, HealthMonitor, HealthStatus, MemoryStore,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::net::TcpListener;

/// Helper to build a test `AgentEntry`.
fn test_entry(id: &str, url: &str) -> AgentEntry {
    AgentEntry {
        id: id.to_string(),
        name: format!("Agent {}", id),
        agent_type: "test".to_string(),
        endpoint_url: url.to_string(),
        capabilities: vec!["test".to_string()],
        health: HealthStatus::Unknown,
        registered_at: chrono::Utc::now(),
        last_heartbeat: chrono::Utc::now(),
    }
}

/// Spin up a minimal HTTP server that responds with the given status code.
/// Returns the URL and a JoinHandle for the server task.
async fn spawn_http_server(status: u16) -> (String, tokio::task::JoinHandle<()>) {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let url = format!("http://127.0.0.1:{}", addr.port());

    let handle = tokio::spawn(async move {
        use tokio::io::AsyncReadExt;
        use tokio::io::AsyncWriteExt;

        loop {
            let Ok((mut stream, _)) = listener.accept().await else {
                break;
            };
            // Read the request (discard it).
            let mut buf = [0u8; 4096];
            let _ = stream.read(&mut buf).await;
            // Write a valid HTTP response.
            let response = format!(
                "HTTP/1.1 {status} OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{{}}"
            );
            let _ = stream.write_all(response.as_bytes()).await;
            let _ = stream.shutdown().await;
        }
    });

    (url, handle)
}

#[tokio::test]
async fn health_status_display_roundtrip() {
    assert_eq!(HealthStatus::Unknown.to_string(), "unknown");
    assert_eq!(HealthStatus::Healthy.to_string(), "healthy");
    assert_eq!(HealthStatus::Degraded.to_string(), "degraded");
    assert_eq!(HealthStatus::Unhealthy.to_string(), "unhealthy");
    assert_eq!(HealthStatus::Offline.to_string(), "offline");
}

#[tokio::test]
async fn ping_agent_200_returns_healthy() {
    let (url, handle) = spawn_http_server(200).await;
    let store = Arc::new(MemoryStore::new());
    let entry = test_entry("p1", &url);
    store.register(entry).await.unwrap();

    let stored = store.get("p1").await.unwrap().unwrap();
    let result = ggen_a2a_registry::ping_agent(&stored, Duration::from_secs(2))
        .await
        .unwrap();

    assert_eq!(result, HealthStatus::Healthy);
    handle.abort();
}

#[tokio::test]
async fn ping_agent_400_returns_degraded() {
    let (url, handle) = spawn_http_server(400).await;
    let store = Arc::new(MemoryStore::new());
    let entry = test_entry("p2", &url);
    store.register(entry).await.unwrap();

    let stored = store.get("p2").await.unwrap().unwrap();
    let result = ggen_a2a_registry::ping_agent(&stored, Duration::from_secs(2))
        .await
        .unwrap();

    assert_eq!(result, HealthStatus::Degraded);
    handle.abort();
}

#[tokio::test]
async fn ping_agent_500_returns_unhealthy() {
    let (url, handle) = spawn_http_server(500).await;
    let store = Arc::new(MemoryStore::new());
    let entry = test_entry("p3", &url);
    store.register(entry).await.unwrap();

    let stored = store.get("p3").await.unwrap().unwrap();
    let result = ggen_a2a_registry::ping_agent(&stored, Duration::from_secs(2))
        .await
        .unwrap();

    assert_eq!(result, HealthStatus::Unhealthy);
    handle.abort();
}

#[tokio::test]
async fn ping_agent_timeout_returns_error() {
    // Bind a port but never accept connections -> timeout.
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let url = format!("http://127.0.0.1:{}", addr.port());

    // Drop the listener so the port is "open" but nobody accepts.
    drop(listener);

    let store = Arc::new(MemoryStore::new());
    let entry = test_entry("pt", &url);
    store.register(entry).await.unwrap();

    let stored = store.get("pt").await.unwrap().unwrap();
    let result = ggen_a2a_registry::ping_agent(&stored, Duration::from_millis(50)).await;

    // Either a timeout or connection refused error -- both acceptable.
    assert!(result.is_err(), "expected error for unreachable endpoint");
}

#[tokio::test]
async fn health_config_defaults() {
    let config = HealthConfig::default();
    assert_eq!(config.check_interval, Duration::from_secs(60));
    assert_eq!(config.ping_timeout, Duration::from_secs(10));
    assert_eq!(config.offline_threshold, 3);
}

#[tokio::test]
async fn health_monitor_starts_and_stops() {
    let store: Arc<dyn AgentStore> = Arc::new(MemoryStore::new());
    let config = HealthConfig {
        check_interval: Duration::from_secs(1),
        ping_timeout: Duration::from_millis(100),
        offline_threshold: 2,
    };
    let monitor = HealthMonitor::new(Arc::clone(&store), config);

    assert!(!monitor.is_running());
    monitor.start().await;
    assert!(monitor.is_running());
    monitor.stop().await;
    // Give the task a moment to actually stop.
    tokio::time::sleep(Duration::from_millis(100)).await;
    assert!(!monitor.is_running());
}

#[tokio::test]
async fn health_monitor_double_start_is_idempotent() {
    let store: Arc<dyn AgentStore> = Arc::new(MemoryStore::new());
    let config = HealthConfig {
        check_interval: Duration::from_secs(1),
        ping_timeout: Duration::from_millis(100),
        offline_threshold: 2,
    };
    let monitor = HealthMonitor::new(Arc::clone(&store), config);

    monitor.start().await;
    monitor.start().await; // Should be a no-op
    assert!(monitor.is_running());
    monitor.stop().await;
}

#[tokio::test(flavor = "multi_thread")]
async fn health_monitor_updates_store_after_check() {
    // Spin up a 200 server so health checks succeed.
    let (url, server_handle) = spawn_http_server(200).await;

    let store: Arc<dyn AgentStore> = Arc::new(MemoryStore::new());
    let entry = test_entry("hm-1", &url);
    store.register(entry).await.unwrap();

    // Use a very short interval so the first check runs quickly.
    let config = HealthConfig {
        check_interval: Duration::from_millis(300),
        ping_timeout: Duration::from_secs(5),
        offline_threshold: 2,
    };
    let monitor = HealthMonitor::new(Arc::clone(&store), config);
    monitor.start().await;

    // Wait long enough for the first tick (300ms) + HTTP round-trip + margin.
    tokio::time::sleep(Duration::from_secs(5)).await;

    let stored = store.get("hm-1").await.unwrap().unwrap();
    assert_eq!(stored.health, HealthStatus::Healthy);

    monitor.stop().await;
    server_handle.abort();
}

#[tokio::test(flavor = "multi_thread")]
async fn health_monitor_promotes_to_offline_after_threshold() {
    // Bind a port but never serve -- all checks will time out.
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let url = format!("http://127.0.0.1:{}", addr.port());
    drop(listener);

    let store: Arc<dyn AgentStore> = Arc::new(MemoryStore::new());
    let entry = test_entry("off-1", &url);
    store.register(entry).await.unwrap();

    let config = HealthConfig {
        check_interval: Duration::from_millis(200),
        ping_timeout: Duration::from_millis(50),
        offline_threshold: 2,
    };
    let monitor = HealthMonitor::new(Arc::clone(&store), config);
    monitor.start().await;

    // Wait for enough check cycles to exceed the offline threshold.
    // 200ms interval * 3 checks (threshold=2) + margin for HTTP timeouts.
    tokio::time::sleep(Duration::from_secs(5)).await;

    let stored = store.get("off-1").await.unwrap().unwrap();
    assert_eq!(stored.health, HealthStatus::Offline);

    monitor.stop().await;
}
