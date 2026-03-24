//! Integration tests for OAuth2 authentication.
//!
//! Chicago TDD: AAA pattern, state-based verification.

use chrono::{Duration, Utc};
use ggen_marketplace_tps::auth::{OAuth2Config, OAuth2Manager, OAuth2Token};
use mockito::Server;

#[tokio::test]
async fn test_oauth2_manager_authorization_url() {
    // Arrange
    let server = Server::new_async().await;
    let config = create_test_oauth2_config(&server);
    let manager = OAuth2Manager::new(config);

    // Act
    let url = manager.get_authorization_url("test_state").unwrap();

    // Assert
    assert!(url.contains("client_id=test_client"));
    assert!(url.contains("state=test_state"));
    assert!(url.contains("response_type=code"));
    assert!(url.contains("scope=read+write"));
    assert!(url.contains("redirect_uri=http%3A%2F%2Flocalhost%3A8080%2Fcallback"));
}

#[tokio::test]
async fn test_oauth2_token_exchange_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let config = create_test_oauth2_config(&server);
    let manager = OAuth2Manager::new(config);

    let _mock = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "new_access_token",
            "token_type": "Bearer",
            "expires_in": 7200,
            "refresh_token": "new_refresh_token",
            "scope": "read write"
        }"#)
        .create_async()
        .await;

    // Act
    let result = manager.exchange_code("authorization_code_123").await;

    // Assert
    assert!(result.is_ok());
    let token = result.unwrap();
    assert_eq!(token.access_token, "new_access_token");
    assert_eq!(token.token_type, "Bearer");
    assert!(token.is_valid());
}

#[tokio::test]
async fn test_oauth2_token_exchange_failure() {
    // Arrange
    let mut server = Server::new_async().await;
    let config = create_test_oauth2_config(&server);
    let manager = OAuth2Manager::new(config);

    let _mock = server
        .mock("POST", "/oauth/token")
        .with_status(401)
        .with_body("Unauthorized")
        .create_async()
        .await;

    // Act
    let result = manager.exchange_code("invalid_code").await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_oauth2_token_refresh_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let config = create_test_oauth2_config(&server);
    let manager = OAuth2Manager::new(config);

    // Initial token exchange
    let _mock1 = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "initial_token",
            "token_type": "Bearer",
            "expires_in": 3600,
            "refresh_token": "refresh_token_123",
            "scope": "read write"
        }"#)
        .create_async()
        .await;

    manager.exchange_code("code").await.unwrap();

    // Token refresh
    let _mock2 = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "refreshed_token",
            "token_type": "Bearer",
            "expires_in": 3600,
            "refresh_token": "new_refresh_token",
            "scope": "read write"
        }"#)
        .create_async()
        .await;

    // Act
    let result = manager.refresh_token().await;

    // Assert
    assert!(result.is_ok());
    let token = result.unwrap();
    assert_eq!(token.access_token, "refreshed_token");
}

#[tokio::test]
async fn test_oauth2_token_expiration() {
    // Arrange
    let expired_token = OAuth2Token {
        access_token: "expired".to_string(),
        token_type: "Bearer".to_string(),
        expires_at: Utc::now() - Duration::seconds(3600),
        refresh_token: None,
        scopes: vec!["read".to_string()],
    };

    let valid_token = OAuth2Token {
        access_token: "valid".to_string(),
        token_type: "Bearer".to_string(),
        expires_at: Utc::now() + Duration::seconds(3600),
        refresh_token: None,
        scopes: vec!["read".to_string()],
    };

    // Act & Assert
    assert!(expired_token.is_expired());
    assert!(!expired_token.is_valid());
    assert!(!valid_token.is_expired());
    assert!(valid_token.is_valid());
}

#[tokio::test]
async fn test_oauth2_get_valid_token_with_expired_token() {
    // Arrange
    let mut server = Server::new_async().await;
    let config = create_test_oauth2_config(&server);
    let manager = OAuth2Manager::new(config);

    // Mock token exchange (initial)
    let _mock1 = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "initial_token",
            "token_type": "Bearer",
            "expires_in": -1,
            "refresh_token": "refresh_token",
            "scope": "read write"
        }"#)
        .create_async()
        .await;

    manager.exchange_code("code").await.unwrap();

    // Mock refresh
    let _mock2 = server
        .mock("POST", "/oauth/token")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{
            "access_token": "refreshed_token",
            "token_type": "Bearer",
            "expires_in": 3600,
            "refresh_token": "new_refresh_token",
            "scope": "read write"
        }"#)
        .create_async()
        .await;

    // Act
    let result = manager.get_valid_token().await;

    // Assert
    assert!(result.is_ok());
    let token = result.unwrap();
    assert_eq!(token.access_token, "refreshed_token");
}

fn create_test_oauth2_config(server: &Server) -> OAuth2Config {
    OAuth2Config {
        client_id: "test_client".to_string(),
        client_secret: "test_secret".to_string(),
        auth_url: format!("{}/oauth/authorize", server.url()),
        token_url: format!("{}/oauth/token", server.url()),
        redirect_uri: "http://localhost:8080/callback".to_string(),
        scopes: vec!["read".to_string(), "write".to_string()],
    }
}
