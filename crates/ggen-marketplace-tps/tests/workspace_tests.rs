//! Integration tests for workspace service.
//!
//! Chicago TDD: AAA pattern, state-based verification.

use chrono::Utc;
use ggen_marketplace_tps::{
    auth::{OAuth2Config, OAuth2Manager},
    workspace::{AddonConfig, AddonStatus, ResourceLimits, Workspace, WorkspaceService},
};
use mockito::Server;

#[tokio::test]
async fn test_get_workspace_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let workspace_json = r#"{
        "id": "ws-123",
        "name": "Test Workspace",
        "owner_id": "owner-123",
        "limits": {
            "max_users": 50,
            "max_storage_bytes": 10737418240,
            "max_api_calls_daily": 100000,
            "max_concurrent_ops": 10
        },
        "addons": [],
        "created_at": "2026-01-01T00:00:00Z",
        "updated_at": "2026-01-01T00:00:00Z"
    }"#;

    let _mock = server
        .mock("GET", "/workspace/workspaces/ws-123")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(workspace_json)
        .create_async()
        .await;

    // Act
    let result = service.get_workspace("ws-123").await;

    // Assert
    assert!(result.is_ok());
    let workspace = result.unwrap();
    assert_eq!(workspace.id, "ws-123");
    assert_eq!(workspace.name, "Test Workspace");
    assert_eq!(workspace.limits.max_users, 50);
}

#[tokio::test]
async fn test_get_workspace_not_found() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("GET", "/workspace/workspaces/nonexistent")
        .with_status(404)
        .create_async()
        .await;

    // Act
    let result = service.get_workspace("nonexistent").await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_create_workspace_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let response_json = r#"{
        "id": "ws-new",
        "name": "New Workspace",
        "owner_id": "owner-123",
        "limits": {
            "max_users": 10,
            "max_storage_bytes": 1073741824,
            "max_api_calls_daily": 10000,
            "max_concurrent_ops": 5
        },
        "addons": [],
        "created_at": "2026-02-10T00:00:00Z",
        "updated_at": "2026-02-10T00:00:00Z"
    }"#;

    let _mock = server
        .mock("POST", "/workspace/workspaces")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(response_json)
        .create_async()
        .await;

    // Act
    let result = service
        .create_workspace("New Workspace".to_string(), "owner-123".to_string(), None)
        .await;

    // Assert
    assert!(result.is_ok());
    let workspace = result.unwrap();
    assert_eq!(workspace.name, "New Workspace");
    assert_eq!(workspace.owner_id, "owner-123");
}

#[tokio::test]
async fn test_install_addon_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let response_json = r#"{
        "addon_id": "addon-123",
        "workspace_id": "ws-123",
        "status": "active",
        "config": {"feature_enabled": true},
        "installed_at": "2026-02-10T00:00:00Z",
        "updated_at": "2026-02-10T00:00:00Z"
    }"#;

    let _mock = server
        .mock("POST", "/workspace/workspaces/ws-123/addons")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(response_json)
        .create_async()
        .await;

    // Act
    let result = service
        .install_addon(
            "ws-123",
            "addon-123",
            serde_json::json!({"feature_enabled": true}),
        )
        .await;

    // Assert
    assert!(result.is_ok());
    let addon = result.unwrap();
    assert_eq!(addon.addon_id, "addon-123");
    assert_eq!(addon.status, AddonStatus::Active);
}

#[tokio::test]
async fn test_install_addon_conflict() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("POST", "/workspace/workspaces/ws-123/addons")
        .with_status(409)
        .create_async()
        .await;

    // Act
    let result = service
        .install_addon("ws-123", "addon-123", serde_json::json!({}))
        .await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_uninstall_addon_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let _mock = server
        .mock("DELETE", "/workspace/workspaces/ws-123/addons/addon-123")
        .with_status(200)
        .create_async()
        .await;

    // Act
    let result = service.uninstall_addon("ws-123", "addon-123").await;

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_update_addon_config_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let service = create_test_service(&mut server).await;

    let response_json = r#"{
        "addon_id": "addon-123",
        "workspace_id": "ws-123",
        "status": "active",
        "config": {"new_feature": true},
        "installed_at": "2026-02-10T00:00:00Z",
        "updated_at": "2026-02-10T01:00:00Z"
    }"#;

    let _mock = server
        .mock("PATCH", "/workspace/workspaces/ws-123/addons/addon-123")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(response_json)
        .create_async()
        .await;

    // Act
    let result = service
        .update_addon_config(
            "ws-123",
            "addon-123",
            serde_json::json!({"new_feature": true}),
        )
        .await;

    // Assert
    assert!(result.is_ok());
    let addon = result.unwrap();
    assert_eq!(addon.addon_id, "addon-123");
}

#[tokio::test]
async fn test_workspace_has_addon() {
    // Arrange
    let workspace = Workspace {
        id: "ws-1".to_string(),
        name: "Test".to_string(),
        owner_id: "owner-1".to_string(),
        limits: ResourceLimits::default(),
        addons: vec![AddonConfig {
            addon_id: "addon-1".to_string(),
            workspace_id: "ws-1".to_string(),
            status: AddonStatus::Active,
            config: serde_json::json!({}),
            installed_at: Utc::now(),
            updated_at: Utc::now(),
        }],
        created_at: Utc::now(),
        updated_at: Utc::now(),
    };

    // Act & Assert
    assert!(workspace.has_addon("addon-1"));
    assert!(!workspace.has_addon("addon-2"));
}

#[tokio::test]
async fn test_workspace_get_addon() {
    // Arrange
    let now = Utc::now();
    let workspace = Workspace {
        id: "ws-1".to_string(),
        name: "Test".to_string(),
        owner_id: "owner-1".to_string(),
        limits: ResourceLimits::default(),
        addons: vec![
            AddonConfig {
                addon_id: "addon-1".to_string(),
                workspace_id: "ws-1".to_string(),
                status: AddonStatus::Active,
                config: serde_json::json!({"key": "value"}),
                installed_at: now,
                updated_at: now,
            },
            AddonConfig {
                addon_id: "addon-2".to_string(),
                workspace_id: "ws-1".to_string(),
                status: AddonStatus::Disabled,
                config: serde_json::json!({}),
                installed_at: now,
                updated_at: now,
            },
        ],
        created_at: now,
        updated_at: now,
    };

    // Act
    let addon1 = workspace.get_addon("addon-1");
    let addon2 = workspace.get_addon("addon-2");
    let addon3 = workspace.get_addon("addon-3");

    // Assert
    assert!(addon1.is_some());
    assert_eq!(addon1.unwrap().addon_id, "addon-1");
    assert!(addon2.is_none()); // Disabled, so not returned
    assert!(addon3.is_none());
}

async fn create_test_service(server: &mut Server) -> WorkspaceService {
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
    let api_url = format!("{}/workspace", server.url());
    WorkspaceService::new(auth_manager, api_url)
}
