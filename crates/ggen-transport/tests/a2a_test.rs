use ggen_transport::a2a::{
    A2aMessage, A2aMessageHandler, A2aMessageType, A2aStreamMessage, A2aStreamType, A2aTransport,
    EchoA2aHandler,
};
use ggen_transport::{OriginValidator, SessionId, SessionManager};
use std::sync::Arc;

#[tokio::test]
async fn test_a2a_protocol_compliance_message_exchange() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    transport
        .register_handler("Request".to_string(), Arc::new(EchoA2aHandler))
        .await;

    let message = A2aMessage {
        id: "msg-001".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({"action": "process"}),
        session_id: None,
        origin: None,
        correlation_id: None,
    };

    let response = transport.handle_message(message).await;

    assert!(response.error.is_none());
    assert_eq!(response.correlation_id, "msg-001");
    assert_eq!(
        response.payload,
        serde_json::json!({"action": "process"})
    );
}

#[tokio::test]
async fn test_a2a_message_types() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    transport
        .register_handler("Request".to_string(), Arc::new(EchoA2aHandler))
        .await;
    transport
        .register_handler("Command".to_string(), Arc::new(EchoA2aHandler))
        .await;
    transport
        .register_handler("Query".to_string(), Arc::new(EchoA2aHandler))
        .await;

    let message_types = vec![
        A2aMessageType::Request,
        A2aMessageType::Command,
        A2aMessageType::Query,
    ];

    for msg_type in message_types {
        let message = A2aMessage {
            id: "test".to_string(),
            from_agent: "agent2".to_string(),
            to_agent: "agent1".to_string(),
            message_type: msg_type,
            payload: serde_json::json!({}),
            session_id: None,
            origin: None,
            correlation_id: None,
        };

        let response = transport.handle_message(message).await;
        assert!(response.error.is_none());
    }
}

#[tokio::test]
async fn test_a2a_routing_to_correct_agent() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    let message = A2aMessage {
        id: "msg-002".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent3".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({}),
        session_id: None,
        origin: None,
        correlation_id: None,
    };

    let response = transport.handle_message(message).await;

    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, "ROUTING_ERROR");
    assert!(error.message.contains("not addressed to this agent"));
}

#[tokio::test]
async fn test_a2a_origin_validation() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::new(vec!["https://allowed.com".to_string()]);
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    transport
        .register_handler("Request".to_string(), Arc::new(EchoA2aHandler))
        .await;

    let message_allowed = A2aMessage {
        id: "msg-003".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({}),
        session_id: None,
        origin: Some("https://allowed.com".to_string()),
        correlation_id: None,
    };

    let message_blocked = A2aMessage {
        id: "msg-004".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({}),
        session_id: None,
        origin: Some("https://blocked.com".to_string()),
        correlation_id: None,
    };

    let response_allowed = transport.handle_message(message_allowed).await;
    let response_blocked = transport.handle_message(message_blocked).await;

    assert!(response_allowed.error.is_none());
    assert!(response_blocked.error.is_some());
    assert_eq!(response_blocked.error.unwrap().code, "ORIGIN_ERROR");
}

#[tokio::test]
async fn test_a2a_session_management() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager.clone(), origin_validator);

    let session = session_manager.create_session().await;

    transport
        .register_handler("Request".to_string(), Arc::new(EchoA2aHandler))
        .await;

    let message = A2aMessage {
        id: "msg-005".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({}),
        session_id: Some(session.id.clone()),
        origin: None,
        correlation_id: None,
    };

    let response = transport.handle_message(message).await;

    assert!(response.error.is_none());
    assert_eq!(response.session_id.as_ref().unwrap().as_str(), session.id.as_str());
}

#[tokio::test]
async fn test_a2a_agent_registration_and_routing() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    transport
        .register_agent_route("agent2".to_string(), "http://agent2:8080".to_string())
        .await;
    transport
        .register_agent_route("agent3".to_string(), "http://agent3:8080".to_string())
        .await;

    let message_to_agent2 = A2aMessage {
        id: "msg-006".to_string(),
        from_agent: "agent1".to_string(),
        to_agent: "agent2".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({}),
        session_id: None,
        origin: None,
        correlation_id: None,
    };

    let result = transport.send_message(message_to_agent2).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_a2a_correlation_id() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    transport
        .register_handler("Request".to_string(), Arc::new(EchoA2aHandler))
        .await;

    let correlation_id = "correlation-123";
    let message = A2aMessage {
        id: "msg-007".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        message_type: A2aMessageType::Request,
        payload: serde_json::json!({}),
        session_id: None,
        origin: None,
        correlation_id: Some(correlation_id.to_string()),
    };

    let response = transport.handle_message(message).await;
    assert_eq!(response.correlation_id, "msg-007");
}

#[tokio::test]
async fn test_a2a_stream_types() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager.clone(), origin_validator);

    transport
        .register_handler("Bidirectional".to_string(), Arc::new(EchoA2aHandler))
        .await;

    let session = session_manager.create_session().await;

    let stream_message = A2aStreamMessage {
        id: "stream-001".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        session_id: session.id,
        stream_type: A2aStreamType::Bidirectional,
        metadata: std::collections::HashMap::new(),
    };

    let result = transport.handle_stream(stream_message).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_a2a_handler_not_found() {
    let session_manager = SessionManager::new(3600);
    let origin_validator = OriginValidator::allow_all();
    let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

    let message = A2aMessage {
        id: "msg-008".to_string(),
        from_agent: "agent2".to_string(),
        to_agent: "agent1".to_string(),
        message_type: A2aMessageType::Event,
        payload: serde_json::json!({}),
        session_id: None,
        origin: None,
        correlation_id: None,
    };

    let response = transport.handle_message(message).await;

    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, "HANDLER_NOT_FOUND");
}
