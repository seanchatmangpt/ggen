//! Integration tests for CLM proxy.
//!
//! Chicago TDD: AAA pattern, state-based verification.

use ggen_marketplace_tps::{
    auth::{OAuth2Config, OAuth2Manager},
    clm_proxy::{ClmProxy, InteractionRequest, InteractionStatus, InteractionType, Priority},
};
use mockito::Server;

#[tokio::test]
async fn test_submit_interaction_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let request = InteractionRequest::new(
        "user-123".to_string(),
        "acc-123".to_string(),
        InteractionType::Support,
        Priority::High,
        "Need help".to_string(),
        "I have an issue".to_string(),
        serde_json::json!({"context": "test"}),
    );

    let _mock = server
        .mock("POST", "/clm/interactions")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{"id": "int-123"}"#)
        .create_async()
        .await;

    // Act
    let result = proxy.submit_interaction(request).await;

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "int-123");
}

#[tokio::test]
async fn test_submit_interaction_failure() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let request = InteractionRequest::new(
        "user-123".to_string(),
        "acc-123".to_string(),
        InteractionType::Bug,
        Priority::Critical,
        "Critical bug".to_string(),
        "System crash".to_string(),
        serde_json::json!({}),
    );

    let _mock = server
        .mock("POST", "/clm/interactions")
        .with_status(500)
        .create_async()
        .await;

    // Act
    let result = proxy.submit_interaction(request).await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_get_interaction_status_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let _mock = server
        .mock("GET", "/clm/interactions/int-123")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{"id": "int-123", "status": "open"}"#)
        .create_async()
        .await;

    // Act
    let result = proxy.get_interaction_status("int-123").await;

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), InteractionStatus::Open);
}

#[tokio::test]
async fn test_get_interaction_status_not_found() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let _mock = server
        .mock("GET", "/clm/interactions/nonexistent")
        .with_status(404)
        .create_async()
        .await;

    // Act
    let result = proxy.get_interaction_status("nonexistent").await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_list_responses_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let responses_json = r#"[
        {
            "interaction_id": "int-123",
            "response_id": "resp-1",
            "agent_id": "agent-1",
            "message": "We are looking into this",
            "status": "inprogress",
            "responded_at": "2026-02-10T00:00:00Z"
        },
        {
            "interaction_id": "int-123",
            "response_id": "resp-2",
            "agent_id": "agent-1",
            "message": "Issue resolved",
            "status": "resolved",
            "responded_at": "2026-02-10T01:00:00Z"
        }
    ]"#;

    let _mock = server
        .mock("GET", "/clm/interactions/int-123/responses")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(responses_json)
        .create_async()
        .await;

    // Act
    let result = proxy.list_responses("int-123").await;

    // Assert
    assert!(result.is_ok());
    let responses = result.unwrap();
    assert_eq!(responses.len(), 2);
    assert_eq!(responses[0].status, InteractionStatus::InProgress);
    assert_eq!(responses[1].status, InteractionStatus::Resolved);
}

#[tokio::test]
async fn test_submit_response_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let _mock = server
        .mock("POST", "/clm/interactions/int-123/responses")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{"response_id": "resp-456"}"#)
        .create_async()
        .await;

    // Act
    let result = proxy
        .submit_response("int-123", "Thank you for the help".to_string())
        .await;

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "resp-456");
}

#[tokio::test]
async fn test_close_interaction_success() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let _mock = server
        .mock("POST", "/clm/interactions/int-123/close")
        .with_status(200)
        .create_async()
        .await;

    // Act
    let result = proxy.close_interaction("int-123").await;

    // Assert
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_close_interaction_failure() {
    // Arrange
    let mut server = Server::new_async().await;
    let proxy = create_test_proxy(&mut server).await;

    let _mock = server
        .mock("POST", "/clm/interactions/int-123/close")
        .with_status(400)
        .create_async()
        .await;

    // Act
    let result = proxy.close_interaction("int-123").await;

    // Assert
    assert!(result.is_err());
}

#[tokio::test]
async fn test_interaction_request_creation() {
    // Arrange & Act
    let request = InteractionRequest::new(
        "user-1".to_string(),
        "acc-1".to_string(),
        InteractionType::Feature,
        Priority::Normal,
        "Feature request".to_string(),
        "Would like to see feature X".to_string(),
        serde_json::json!({"feature": "X"}),
    );

    // Assert
    assert!(!request.id.is_empty());
    assert_eq!(request.user_id, "user-1");
    assert_eq!(request.account_id, "acc-1");
    assert_eq!(request.interaction_type, InteractionType::Feature);
    assert_eq!(request.priority, Priority::Normal);
}

#[tokio::test]
async fn test_priority_ordering() {
    // Arrange
    let priorities = vec![
        Priority::Critical,
        Priority::Low,
        Priority::High,
        Priority::Normal,
    ];

    // Act
    let mut sorted = priorities.clone();
    sorted.sort();

    // Assert
    assert_eq!(sorted[0], Priority::Low);
    assert_eq!(sorted[1], Priority::Normal);
    assert_eq!(sorted[2], Priority::High);
    assert_eq!(sorted[3], Priority::Critical);
}

async fn create_test_proxy(server: &mut Server) -> ClmProxy {
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
    let clm_url = format!("{}/clm", server.url());
    ClmProxy::new(auth_manager, clm_url)
}
