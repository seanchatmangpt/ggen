//! Integration tests for MarketplaceClient with mock HTTP server.
//!
//! Chicago TDD: AAA pattern, state-based verification, real collaborators.

use ggen_marketplace_tps::{
    auth::OAuth2Config,
    MarketplaceClient, MarketplaceConfig,
};
use mockito::Server;

#[tokio::test]
async fn test_marketplace_client_initialization() {
    // Arrange
    let server = Server::new_async().await;
    let config = create_test_config(&server);

    // Act
    let client = MarketplaceClient::new(config);

    // Assert
    assert!(client.start_auth("test_state").is_ok());
}

#[tokio::test]
async fn test_marketplace_client_auth_flow() {
    // Arrange
    let mut server = Server::new_async().await;
    let config = create_test_config(&server);
    let client = MarketplaceClient::new(config);

    // Mock token endpoint
    let _mock = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "test_access_token",
            "token_type": "Bearer",
            "expires_in": 3600,
            "refresh_token": "test_refresh_token",
            "scope": "read write"
        }"#)
        .create_async()
        .await;

    // Act
    let auth_url = client.start_auth("test_state").unwrap();
    let result = client.complete_auth("test_code").await;

    // Assert
    assert!(auth_url.contains("state=test_state"));
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_marketplace_client_not_authenticated_initially() {
    // Arrange
    let server = Server::new_async().await;
    let config = create_test_config(&server);
    let client = MarketplaceClient::new(config);

    // Act
    let is_authed = client.is_authenticated().await.unwrap();

    // Assert
    assert!(!is_authed);
}

#[tokio::test]
async fn test_marketplace_client_service_access() {
    // Arrange
    let server = Server::new_async().await;
    let config = create_test_config(&server);
    let client = MarketplaceClient::new(config);

    // Act
    let auth = client.auth();
    let _entitlements = client.entitlements();
    let _workspace = client.workspace();
    let _clm = client.clm();

    // Assert - verify services are accessible
    assert!(auth.get_authorization_url("test").is_ok());
}

fn create_test_config(server: &Server) -> MarketplaceConfig {
    MarketplaceConfig {
        oauth2: OAuth2Config {
            client_id: "test_client".to_string(),
            client_secret: "test_secret".to_string(),
            auth_url: format!("{}/oauth/authorize", server.url()),
            token_url: format!("{}/oauth/token", server.url()),
            redirect_uri: "http://localhost:8080/callback".to_string(),
            scopes: vec!["read".to_string(), "write".to_string()],
        },
        marketplace_api_url: format!("{}/marketplace", server.url()),
        workspace_api_url: format!("{}/workspace", server.url()),
        clm_api_url: format!("{}/clm", server.url()),
    }
}
