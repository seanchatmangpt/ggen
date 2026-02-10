//! Integration tests for entitlement service.
//!
//! Chicago TDD: AAA pattern, state-based verification.

use chrono::Utc;
use ggen_marketplace_tps::{
    auth::{OAuth2Config, OAuth2Manager},
    entitlements::{Entitlement, EntitlementService, EntitlementState, PlanTier},
};
use mockito::Server;

#[tokio::test]
async fn test_get_entitlement_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let entitlement_json = r#"{
        "id": "ent-123",
        "account_id": "acc-123",
        "product_id": "prod-123",
        "plan": "professional",
        "state": "ACTIVE",
        "create_time": "2026-01-01T00:00:00Z",
        "update_time": "2026-01-01T00:00:00Z",
        "usage_quota": 10000,
        "usage_current": 5000
    }"#;

    let _mock = server
        .mock("GET", "/marketplace/entitlements/ent-123")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(entitlement_json)
        .create_async()
        .await;

    // Act
    let result = service.get_entitlement("ent-123").await;

    // Assert
    assert!(result.is_ok());
    let entitlement = result.unwrap();
    assert_eq!(entitlement.id, "ent-123");
    assert_eq!(entitlement.plan, PlanTier::Professional);
    assert_eq!(entitlement.state, EntitlementState::Active);
    assert!(entitlement.is_valid());
}

#[tokio::test]
async fn test_get_entitlement_not_found() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("GET", "/marketplace/entitlements/nonexistent")
        .with_status(404)
        .create_async()
        .await;

    // Act
    let result = service.get_entitlement("nonexistent").await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_list_entitlements() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let entitlements_json = r#"[
        {
            "id": "ent-1",
            "account_id": "acc-123",
            "product_id": "prod-1",
            "plan": "basic",
            "state": "ACTIVE",
            "create_time": "2026-01-01T00:00:00Z",
            "update_time": "2026-01-01T00:00:00Z",
            "usage_quota": 1000,
            "usage_current": 100
        },
        {
            "id": "ent-2",
            "account_id": "acc-123",
            "product_id": "prod-2",
            "plan": "professional",
            "state": "ACTIVE",
            "create_time": "2026-01-01T00:00:00Z",
            "update_time": "2026-01-01T00:00:00Z",
            "usage_quota": 10000,
            "usage_current": 5000
        }
    ]"#;

    let _mock = server
        .mock("GET", "/marketplace/accounts/acc-123/entitlements")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(entitlements_json)
        .create_async()
        .await;

    // Act
    let result = service.list_entitlements("acc-123").await;

    // Assert
    assert!(result.is_ok());
    let entitlements = result.unwrap();
    assert_eq!(entitlements.len(), 2);
    assert_eq!(entitlements[0].plan, PlanTier::Basic);
    assert_eq!(entitlements[1].plan, PlanTier::Professional);
}

#[tokio::test]
async fn test_check_access_with_valid_entitlement() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let entitlements_json = r#"[
        {
            "id": "ent-1",
            "account_id": "acc-123",
            "product_id": "prod-1",
            "plan": "professional",
            "state": "ACTIVE",
            "create_time": "2026-01-01T00:00:00Z",
            "update_time": "2026-01-01T00:00:00Z",
            "usage_quota": 10000,
            "usage_current": 100
        }
    ]"#;

    let _mock = server
        .mock("GET", "/marketplace/accounts/acc-123/entitlements")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(entitlements_json)
        .create_async()
        .await;

    // Act
    let result = service.check_access("acc-123", "prod-1").await;

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap());
}

#[tokio::test]
async fn test_check_access_without_valid_entitlement() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("GET", "/marketplace/accounts/acc-123/entitlements")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body("[]")
        .create_async()
        .await;

    // Act
    let result = service.check_access("acc-123", "prod-1").await;

    // Assert
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[tokio::test]
async fn test_report_usage_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("POST", "/marketplace/entitlements/ent-123/usage")
        .with_status(200)
        .create_async()
        .await;

    // Act
    let result = service.report_usage("ent-123", 100).await;

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_report_usage_quota_exceeded() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("POST", "/marketplace/entitlements/ent-123/usage")
        .with_status(429)
        .create_async()
        .await;

    // Act
    let result = service.report_usage("ent-123", 10000).await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_entitlement_quota_warning() {
    // Arrange
    let high_usage = Entitlement {
        id: "ent-1".to_string(),
        account_id: "acc-1".to_string(),
        product_id: "prod-1".to_string(),
        plan: PlanTier::Basic,
        state: EntitlementState::Active,
        create_time: Utc::now(),
        update_time: Utc::now(),
        usage_quota: 1000,
        usage_current: 850,
    };

    let low_usage = Entitlement {
        id: "ent-2".to_string(),
        account_id: "acc-1".to_string(),
        product_id: "prod-1".to_string(),
        plan: PlanTier::Basic,
        state: EntitlementState::Active,
        create_time: Utc::now(),
        update_time: Utc::now(),
        usage_quota: 1000,
        usage_current: 100,
    };

    // Act & Assert
    assert!(high_usage.is_quota_warning());
    assert!(!low_usage.is_quota_warning());
}

async fn create_test_service(server: &mut Server) -> EntitlementService {
    let config = OAuth2Config {
        client_id: "test".to_string(),
        client_secret: "secret".to_string(),
        auth_url: format!("{}/oauth/authorize", server.url()),
        token_url: format!("{}/oauth/token", server.url()),
        redirect_uri: "http://localhost:8080/callback".to_string(),
        scopes: vec!["read".to_string()],
    };

    // Mock token endpoint
    let _mock = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "test_token",
            "token_type": "Bearer",
            "expires_in": 3600,
            "refresh_token": "refresh_token",
            "scope": "read"
        }"#)
        .create_async()
        .await;

    let auth_manager = OAuth2Manager::new(config);
    let api_url = format!("{}/marketplace", server.url());
    EntitlementService::new(auth_manager, api_url)
}
