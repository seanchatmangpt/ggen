use ggen_transport::mcp::{EchoHandler, McpError, McpHandler, McpRequest, McpTransport};
use ggen_transport::{OriginValidator, SessionManager};
use std::sync::Arc;

#[tokio::test]
async fn test_mcp_protocol_compliance_request_response() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = McpTransport::new(session_manager, origin_validator);

    transport
        .register_handler("test.method".to_string(), Arc::new(EchoHandler))
        .await;

    let request = McpRequest {
        id: "req-001".to_string(),
        method: "test.method".to_string(),
        params: serde_json::json!({"key": "value"}),
        session_id: None,
        origin: None,
    };

    let response = transport.handle_request(request).await;

    assert!(response.error.is_none());
    assert_eq!(response.id, "req-001");
    assert_eq!(
        response.result.unwrap(),
        serde_json::json!({"key": "value"})
    );
}

#[tokio::test]
async fn test_mcp_method_not_found_error() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = McpTransport::new(session_manager, origin_validator);

    let request = McpRequest {
        id: "req-002".to_string(),
        method: "nonexistent.method".to_string(),
        params: serde_json::json!({}),
        session_id: None,
        origin: None,
    };

    let response = transport.handle_request(request).await;

    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, -32601);
    assert!(error.message.contains("Method not found"));
}

#[tokio::test]
async fn test_mcp_origin_validation_allowed() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::new(vec![
        "https://trusted.com".to_string(),
        "http://localhost:8080".to_string(),
    ]);
    let transport = McpTransport::new(session_manager, origin_validator);

    transport
        .register_handler("test".to_string(), Arc::new(EchoHandler))
        .await;

    let request = McpRequest {
        id: "req-003".to_string(),
        method: "test".to_string(),
        params: serde_json::json!({}),
        session_id: None,
        origin: Some("https://trusted.com".to_string()),
    };

    let response = transport.handle_request(request).await;
    assert!(response.error.is_none());
}

#[tokio::test]
async fn test_mcp_origin_validation_blocked() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::new(vec!["https://trusted.com".to_string()]);
    let transport = McpTransport::new(session_manager, origin_validator);

    let request = McpRequest {
        id: "req-004".to_string(),
        method: "test".to_string(),
        params: serde_json::json!({}),
        session_id: None,
        origin: Some("https://malicious.com".to_string()),
    };

    let response = transport.handle_request(request).await;

    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, -32001);
    assert!(error.message.contains("Origin not allowed"));
}

#[tokio::test]
async fn test_mcp_session_management() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = McpTransport::new(session_manager.clone(), origin_validator);

    let session_id = transport.create_session().await;

    transport
        .register_handler("test".to_string(), Arc::new(EchoHandler))
        .await;

    let request = McpRequest {
        id: "req-005".to_string(),
        method: "test".to_string(),
        params: serde_json::json!({"data": "test"}),
        session_id: Some(session_id.clone()),
        origin: None,
    };

    let response = transport.handle_request(request).await;

    assert!(response.error.is_none());
    assert_eq!(response.session_id.as_ref().unwrap().as_str(), session_id.as_str());
}

#[tokio::test]
async fn test_mcp_session_expired() {
    let session_manager = SessionManager::new(1);
    let origin_validator = OriginValidator::allow_all();
    let transport = McpTransport::new(session_manager.clone(), origin_validator);

    let session_id = transport.create_session().await;

    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    let request = McpRequest {
        id: "req-006".to_string(),
        method: "test".to_string(),
        params: serde_json::json!({}),
        session_id: Some(session_id),
        origin: None,
    };

    let response = transport.handle_request(request).await;

    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, -32002);
    assert!(error.message.contains("Session expired"));
}

#[tokio::test]
async fn test_mcp_error_serialization() {
    let error = McpError::new(-32700, "Parse error".to_string())
        .with_data(serde_json::json!({"line": 10, "column": 5}));

    let serialized = serde_json::to_string(&error).unwrap();
    let deserialized: McpError = serde_json::from_str(&serialized).unwrap();

    assert_eq!(deserialized.code, -32700);
    assert_eq!(deserialized.message, "Parse error");
    assert!(deserialized.data.is_some());
}

#[tokio::test]
async fn test_mcp_multiple_handlers() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = McpTransport::new(session_manager, origin_validator);

    transport
        .register_handler("method1".to_string(), Arc::new(EchoHandler))
        .await;
    transport
        .register_handler("method2".to_string(), Arc::new(EchoHandler))
        .await;

    let request1 = McpRequest {
        id: "1".to_string(),
        method: "method1".to_string(),
        params: serde_json::json!({"a": 1}),
        session_id: None,
        origin: None,
    };

    let request2 = McpRequest {
        id: "2".to_string(),
        method: "method2".to_string(),
        params: serde_json::json!({"b": 2}),
        session_id: None,
        origin: None,
    };

    let response1 = transport.handle_request(request1).await;
    let response2 = transport.handle_request(request2).await;

    assert!(response1.error.is_none());
    assert!(response2.error.is_none());
    assert_eq!(response1.result.unwrap(), serde_json::json!({"a": 1}));
    assert_eq!(response2.result.unwrap(), serde_json::json!({"b": 2}));
}
