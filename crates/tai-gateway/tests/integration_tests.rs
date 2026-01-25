//! Integration tests for TAI Gateway
//!
//! Chicago TDD pattern: State-based testing with real objects, AAA pattern (Arrange/Act/Assert)

use tai_gateway::{
    auth::InMemoryTokenStore, error::GatewayError, health::CircuitBreakerConfig, health::HealthStatus,
    ratelimit::ClientId, routing::RouteConfig, routing::RouteRegistry, routing::Upstream,
    routing::UpstreamPool, GatewayConfig, Router,
};
use std::collections::HashMap;

/// Test fixture for gateway setup
struct GatewayTestFixture {
    registry: RouteRegistry,
    pool: UpstreamPool,
    router: Router,
}

impl GatewayTestFixture {
    async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let registry = RouteRegistry::new();
        let pool = UpstreamPool::new();
        let router = Router::new(registry.clone(), pool.clone());

        Ok(Self {
            registry,
            pool,
            router,
        })
    }
}

// ARRANGE-ACT-ASSERT PATTERN TESTS

#[tokio::test]
async fn test_routing_request_to_upstream() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Set up route and upstream
    let fixture = GatewayTestFixture::new().await?;

    let route = RouteConfig::new(
        "api-route",
        "/api/v1/users/*",
        vec!["GET".to_string()],
        vec!["upstream-1".to_string()],
    );
    fixture.registry.register(route).await?;

    let upstream = Upstream::new("upstream-1", "http://localhost:8080");
    fixture.pool.register(upstream).await?;

    let instance = fixture.pool.get("upstream-1").await?;
    instance.set_health_status(HealthStatus::Healthy).await;

    // ACT: Route a request
    let result = fixture.router.route("GET", "/api/v1/users/123").await;

    // ASSERT: Verify routing result
    assert!(result.is_ok(), "Route should succeed");
    let context = result?;
    assert_eq!(context.route.id, "api-route");
    assert_eq!(context.selected_upstream, "upstream-1");
    assert_eq!(context.attempt, 1);

    Ok(())
}

#[tokio::test]
async fn test_routing_fails_when_no_healthy_upstream() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Set up route with unhealthy upstream
    let fixture = GatewayTestFixture::new().await?;

    let route = RouteConfig::new(
        "api-route",
        "/api/v1/orders/*",
        vec!["POST".to_string()],
        vec!["upstream-1".to_string()],
    );
    fixture.registry.register(route).await?;

    let upstream = Upstream::new("upstream-1", "http://localhost:8080");
    fixture.pool.register(upstream).await?;

    let instance = fixture.pool.get("upstream-1").await?;
    instance.set_health_status(HealthStatus::Unhealthy).await;

    // ACT: Attempt to route request
    let result = fixture.router.route("POST", "/api/v1/orders/456").await;

    // ASSERT: Verify routing fails appropriately
    assert!(
        result.is_err(),
        "Route should fail when no healthy upstreams available"
    );

    Ok(())
}

#[tokio::test]
async fn test_weighted_load_distribution() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Set up weighted upstream distribution
    let fixture = GatewayTestFixture::new().await?;

    let mut weights = HashMap::new();
    weights.insert("upstream-1".to_string(), 70);
    weights.insert("upstream-2".to_string(), 30);

    let route = RouteConfig::new(
        "weighted-route",
        "/api/v1/data/*",
        vec!["GET".to_string()],
        vec!["upstream-1".to_string(), "upstream-2".to_string()],
    )
    .with_weights(weights);

    fixture.registry.register(route).await?;

    let upstream1 = Upstream::new("upstream-1", "http://localhost:8080");
    let upstream2 = Upstream::new("upstream-2", "http://localhost:8081");
    fixture.pool.register(upstream1).await?;
    fixture.pool.register(upstream2).await?;

    let instance1 = fixture.pool.get("upstream-1").await?;
    let instance2 = fixture.pool.get("upstream-2").await?;
    instance1.set_health_status(HealthStatus::Healthy).await;
    instance2.set_health_status(HealthStatus::Healthy).await;

    // ACT: Collect distribution across multiple requests
    let mut upstream_1_count = 0;
    let mut upstream_2_count = 0;

    for _ in 0..100 {
        match fixture.router.route("GET", "/api/v1/data/test").await {
            Ok(context) => {
                if context.selected_upstream == "upstream-1" {
                    upstream_1_count += 1;
                } else {
                    upstream_2_count += 1;
                }
            }
            Err(_) => {}
        }
    }

    // ASSERT: Verify approximate weight distribution
    let ratio = upstream_1_count as f64 / (upstream_1_count + upstream_2_count) as f64;
    assert!(
        ratio > 0.55 && ratio < 0.85,
        "Weight distribution should be approximately 70:30, got {}:{}",
        upstream_1_count,
        upstream_2_count
    );

    Ok(())
}

#[tokio::test]
async fn test_pattern_matching_exact_path() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Set up exact path route
    let fixture = GatewayTestFixture::new().await?;

    let route = RouteConfig::new(
        "exact-route",
        "/health",
        vec!["GET".to_string()],
        vec!["upstream-1".to_string()],
    );
    fixture.registry.register(route).await?;

    let upstream = Upstream::new("upstream-1", "http://localhost:8080");
    fixture.pool.register(upstream).await?;

    let instance = fixture.pool.get("upstream-1").await?;
    instance.set_health_status(HealthStatus::Healthy).await;

    // ACT: Route exact path
    let result = fixture.router.route("GET", "/health").await;

    // ASSERT: Verify exact match
    assert!(result.is_ok());
    assert_eq!(result?.route.id, "exact-route");

    Ok(())
}

#[tokio::test]
async fn test_pattern_matching_wildcard() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Set up wildcard route
    let fixture = GatewayTestFixture::new().await?;

    let route = RouteConfig::new(
        "wildcard-route",
        "/api/v*/users/*",
        vec!["GET".to_string()],
        vec!["upstream-1".to_string()],
    );
    fixture.registry.register(route).await?;

    let upstream = Upstream::new("upstream-1", "http://localhost:8080");
    fixture.pool.register(upstream).await?;

    let instance = fixture.pool.get("upstream-1").await?;
    instance.set_health_status(HealthStatus::Healthy).await;

    // ACT: Test various paths
    let paths = vec!["/api/v1/users/123", "/api/v2/users/456", "/api/v3/users/789"];

    for path in paths {
        // ASSERT: Verify all paths match
        let result = fixture.router.route("GET", path).await;
        assert!(result.is_ok(), "Path {} should match wildcard pattern", path);
    }

    Ok(())
}

#[tokio::test]
async fn test_http_method_filtering() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Set up route with specific methods
    let fixture = GatewayTestFixture::new().await?;

    let route = RouteConfig::new(
        "method-route",
        "/api/v1/resource/*",
        vec!["GET".to_string(), "POST".to_string()],
        vec!["upstream-1".to_string()],
    );
    fixture.registry.register(route).await?;

    let upstream = Upstream::new("upstream-1", "http://localhost:8080");
    fixture.pool.register(upstream).await?;

    let instance = fixture.pool.get("upstream-1").await?;
    instance.set_health_status(HealthStatus::Healthy).await;

    // ACT: Test allowed method
    let get_result = fixture.router.route("GET", "/api/v1/resource/1").await;

    // ASSERT: Verify allowed method succeeds
    assert!(get_result.is_ok());

    // ACT: Test disallowed method
    let delete_result = fixture.router.route("DELETE", "/api/v1/resource/1").await;

    // ASSERT: Verify disallowed method fails
    assert!(delete_result.is_err());

    Ok(())
}

#[test]
fn test_gateway_config_serialization() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Create gateway configuration
    let config = GatewayConfig {
        name: "Test Gateway".to_string(),
        listen_addr: "0.0.0.0".to_string(),
        listen_port: 9090,
        enable_http2: true,
        enable_grpc: true,
        default_timeout_ms: 15000,
        max_concurrent_connections: Some(1000),
    };

    // ACT: Serialize to JSON
    let json = serde_json::to_string(&config)?;

    // ASSERT: Verify serialization
    assert!(json.contains("Test Gateway"));
    assert!(json.contains("9090"));

    // ACT: Deserialize back
    let deserialized: GatewayConfig = serde_json::from_str(&json)?;

    // ASSERT: Verify round-trip
    assert_eq!(deserialized.name, config.name);
    assert_eq!(deserialized.listen_port, config.listen_port);

    Ok(())
}

#[tokio::test]
async fn test_rate_limiter_client_identification() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Create clients with different identifications
    let ip_client = ClientId::from_ip("192.168.1.100");
    let api_key_client = ClientId::from_api_key("sk-secret-key");
    let user_client = ClientId::from_user_id("user-12345");

    // ACT: Verify string representations
    assert_eq!(ip_client.to_string(), "ip:192.168.1.100");
    assert_eq!(api_key_client.to_string(), "key:sk-secret-key");
    assert_eq!(user_client.to_string(), "user:user-12345");

    // ASSERT: Verify equality
    let ip_client_2 = ClientId::from_ip("192.168.1.100");
    assert_eq!(ip_client, ip_client_2);

    Ok(())
}

#[tokio::test]
async fn test_upstream_statistics_collection() -> Result<(), Box<dyn std::error::Error>> {
    // ARRANGE: Create upstream and record metrics
    let pool = UpstreamPool::new();
    let upstream = Upstream::new("test-upstream", "http://localhost:8080");
    pool.register(upstream).await?;

    let instance = pool.get("test-upstream").await?;

    // ACT: Record requests
    instance.record_request();
    instance.record_request();
    instance.record_request();
    instance.record_error();
    instance.record_error();

    // ASSERT: Verify statistics
    let stats = instance.stats();
    assert_eq!(stats.request_count, 3);
    assert_eq!(stats.error_count, 2);

    Ok(())
}

#[tokio::test]
async fn test_authentication_flow() -> Result<(), Box<dyn std::error::Error>> {
    use chrono::Utc;
    use tai_gateway::AuthToken;
    use std::collections::HashMap;

    // ARRANGE: Set up token store and create token
    let store = InMemoryTokenStore::new();
    let now = Utc::now().timestamp();

    let token = AuthToken {
        id: uuid::Uuid::new_v4().to_string(),
        token_type: "Bearer".to_string(),
        value: "test-token-xyz".to_string(),
        subject: "user-123".to_string(),
        scopes: vec!["read".to_string(), "write".to_string()],
        issued_at: now,
        expires_at: now + 3600,
        claims: HashMap::new(),
    };

    // ACT: Store token
    store.store(token.clone()).await?;

    // ASSERT: Verify token can be retrieved
    let retrieved = store.get("test-token-xyz").await?;
    assert_eq!(retrieved.subject, "user-123");
    assert!(retrieved.has_scope("read"));
    assert!(retrieved.has_scope("write"));

    // ACT: Validate token
    let context = store.validate("test-token-xyz").await?;

    // ASSERT: Verify context
    assert_eq!(context.subject, "user-123");
    assert!(context.requires_scopes(&["read", "write"]));

    Ok(())
}

#[test]
fn test_gateway_config_defaults() {
    // ARRANGE & ACT: Create default config
    let config = GatewayConfig::default();

    // ASSERT: Verify defaults
    assert_eq!(config.name, "TAI Gateway");
    assert_eq!(config.listen_port, 8080);
    assert_eq!(config.enable_http2, true);
    assert_eq!(config.default_timeout_ms, 30000);
}
