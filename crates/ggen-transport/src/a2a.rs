use crate::error::{Result, TransportError};
use crate::origin::{Origin, OriginValidator};
use crate::session::{SessionId, SessionManager};
use crate::streaming::{MessageStream, StreamBuilder, StreamSender};
use async_trait::async_trait;
use bytes::Bytes;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aMessage {
    pub id: String,
    pub from_agent: String,
    pub to_agent: String,
    pub message_type: A2aMessageType,
    pub payload: serde_json::Value,
    pub session_id: Option<SessionId>,
    pub origin: Option<String>,
    pub correlation_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum A2aMessageType {
    Request,
    Response,
    Event,
    Command,
    Query,
    Notification,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aResponse {
    pub id: String,
    pub correlation_id: String,
    pub from_agent: String,
    pub to_agent: String,
    pub payload: serde_json::Value,
    pub error: Option<A2aError>,
    pub session_id: Option<SessionId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aError {
    pub code: String,
    pub message: String,
    pub details: Option<serde_json::Value>,
}

impl A2aError {
    pub fn new(code: String, message: String) -> Self {
        Self {
            code,
            message,
            details: None,
        }
    }

    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = Some(details);
        self
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aStreamMessage {
    pub id: String,
    pub from_agent: String,
    pub to_agent: String,
    pub session_id: SessionId,
    pub stream_type: A2aStreamType,
    pub metadata: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum A2aStreamType {
    Bidirectional,
    ServerStream,
    ClientStream,
}

#[async_trait]
pub trait A2aMessageHandler: Send + Sync {
    async fn handle_message(&self, message: A2aMessage) -> Result<A2aResponse>;
    async fn handle_stream(
        &self,
        message: A2aStreamMessage,
    ) -> Result<(StreamSender, MessageStream)>;
}

pub struct A2aTransport {
    agent_id: String,
    handlers: Arc<RwLock<HashMap<String, Arc<dyn A2aMessageHandler>>>>,
    session_manager: SessionManager,
    origin_validator: OriginValidator,
    message_router: Arc<RwLock<HashMap<String, String>>>,
}

impl A2aTransport {
    pub fn new(
        agent_id: String,
        session_manager: SessionManager,
        origin_validator: OriginValidator,
    ) -> Self {
        Self {
            agent_id,
            handlers: Arc::new(RwLock::new(HashMap::new())),
            session_manager,
            origin_validator,
            message_router: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn register_handler(
        &self,
        message_type: String,
        handler: Arc<dyn A2aMessageHandler>,
    ) {
        let mut handlers = self.handlers.write().await;
        handlers.insert(message_type, handler);
    }

    pub async fn register_agent_route(&self, agent_id: String, endpoint: String) {
        let mut router = self.message_router.write().await;
        router.insert(agent_id, endpoint);
    }

    pub async fn handle_message(&self, message: A2aMessage) -> A2aResponse {
        if message.to_agent != self.agent_id {
            return A2aResponse {
                id: message.id.clone(),
                correlation_id: message.id.clone(),
                from_agent: self.agent_id.clone(),
                to_agent: message.from_agent.clone(),
                payload: serde_json::json!(null),
                error: Some(A2aError::new(
                    "ROUTING_ERROR".to_string(),
                    format!("Message not addressed to this agent: {}", self.agent_id),
                )),
                session_id: message.session_id.clone(),
            };
        }

        if let Some(origin_str) = &message.origin {
            if let Ok(origin) = Origin::from_url(origin_str) {
                if let Err(e) = self.origin_validator.validate(&origin) {
                    return A2aResponse {
                        id: message.id.clone(),
                        correlation_id: message.id.clone(),
                        from_agent: self.agent_id.clone(),
                        to_agent: message.from_agent.clone(),
                        payload: serde_json::json!(null),
                        error: Some(A2aError::new("ORIGIN_ERROR".to_string(), e.to_string())),
                        session_id: message.session_id.clone(),
                    };
                }
            }
        }

        if let Some(session_id) = &message.session_id {
            if let Err(e) = self.session_manager.touch_session(session_id).await {
                return A2aResponse {
                    id: message.id.clone(),
                    correlation_id: message.id.clone(),
                    from_agent: self.agent_id.clone(),
                    to_agent: message.from_agent.clone(),
                    payload: serde_json::json!(null),
                    error: Some(A2aError::new("SESSION_ERROR".to_string(), e.to_string())),
                    session_id: Some(session_id.clone()),
                };
            }
        }

        let message_type_key = format!("{:?}", message.message_type);
        let handlers = self.handlers.read().await;
        let handler = match handlers.get(&message_type_key) {
            Some(h) => h.clone(),
            None => {
                return A2aResponse {
                    id: message.id.clone(),
                    correlation_id: message.id.clone(),
                    from_agent: self.agent_id.clone(),
                    to_agent: message.from_agent.clone(),
                    payload: serde_json::json!(null),
                    error: Some(A2aError::new(
                        "HANDLER_NOT_FOUND".to_string(),
                        format!("No handler for message type: {:?}", message.message_type),
                    )),
                    session_id: message.session_id.clone(),
                };
            }
        };
        drop(handlers);

        match handler.handle_message(message.clone()).await {
            Ok(response) => response,
            Err(e) => A2aResponse {
                id: message.id.clone(),
                correlation_id: message.id,
                from_agent: self.agent_id.clone(),
                to_agent: message.from_agent,
                payload: serde_json::json!(null),
                error: Some(A2aError::new("HANDLER_ERROR".to_string(), e.to_string())),
                session_id: message.session_id,
            },
        }
    }

    pub async fn handle_stream(
        &self,
        message: A2aStreamMessage,
    ) -> Result<(StreamSender, MessageStream)> {
        if message.to_agent != self.agent_id {
            return Err(TransportError::ProtocolError(format!(
                "Message not addressed to this agent: {}",
                self.agent_id
            )));
        }

        self.session_manager
            .touch_session(&message.session_id)
            .await?;

        let stream_type_key = format!("{:?}", message.stream_type);
        let handlers = self.handlers.read().await;
        let handler = handlers
            .get(&stream_type_key)
            .ok_or_else(|| {
                TransportError::ProtocolError(format!(
                    "No handler for stream type: {:?}",
                    message.stream_type
                ))
            })?
            .clone();
        drop(handlers);

        handler.handle_stream(message).await
    }

    pub async fn send_message(&self, message: A2aMessage) -> Result<A2aResponse> {
        let router = self.message_router.read().await;
        let _endpoint = router.get(&message.to_agent).ok_or_else(|| {
            TransportError::ProtocolError(format!("No route to agent: {}", message.to_agent))
        })?;

        Ok(A2aResponse {
            id: uuid::Uuid::new_v4().to_string(),
            correlation_id: message.id,
            from_agent: message.to_agent,
            to_agent: self.agent_id.clone(),
            payload: serde_json::json!({"status": "sent"}),
            error: None,
            session_id: message.session_id,
        })
    }

    pub fn get_agent_id(&self) -> &str {
        &self.agent_id
    }
}

pub struct EchoA2aHandler;

#[async_trait]
impl A2aMessageHandler for EchoA2aHandler {
    async fn handle_message(&self, message: A2aMessage) -> Result<A2aResponse> {
        Ok(A2aResponse {
            id: uuid::Uuid::new_v4().to_string(),
            correlation_id: message.id,
            from_agent: message.to_agent,
            to_agent: message.from_agent,
            payload: message.payload,
            error: None,
            session_id: message.session_id,
        })
    }

    async fn handle_stream(
        &self,
        message: A2aStreamMessage,
    ) -> Result<(StreamSender, MessageStream)> {
        let builder = StreamBuilder::new(message.session_id.clone());
        Ok(builder.build())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_a2a_message_handling() {
        let session_manager = SessionManager::new(3600);
        let origin_validator = OriginValidator::allow_all();
        let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

        transport
            .register_handler("Request".to_string(), Arc::new(EchoA2aHandler))
            .await;

        let message = A2aMessage {
            id: "1".to_string(),
            from_agent: "agent2".to_string(),
            to_agent: "agent1".to_string(),
            message_type: A2aMessageType::Request,
            payload: serde_json::json!({"test": "value"}),
            session_id: None,
            origin: None,
            correlation_id: None,
        };

        let response = transport.handle_message(message).await;
        assert!(response.error.is_none());
        assert_eq!(response.payload, serde_json::json!({"test": "value"}));
    }

    #[tokio::test]
    async fn test_a2a_routing_error() {
        let session_manager = SessionManager::new(3600);
        let origin_validator = OriginValidator::allow_all();
        let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

        let message = A2aMessage {
            id: "1".to_string(),
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
        assert_eq!(response.error.unwrap().code, "ROUTING_ERROR");
    }

    #[tokio::test]
    async fn test_a2a_agent_routing() {
        let session_manager = SessionManager::new(3600);
        let origin_validator = OriginValidator::allow_all();
        let transport = A2aTransport::new("agent1".to_string(), session_manager, origin_validator);

        transport
            .register_agent_route("agent2".to_string(), "http://agent2:8080".to_string())
            .await;

        let message = A2aMessage {
            id: "1".to_string(),
            from_agent: "agent1".to_string(),
            to_agent: "agent2".to_string(),
            message_type: A2aMessageType::Request,
            payload: serde_json::json!({}),
            session_id: None,
            origin: None,
            correlation_id: None,
        };

        let result = transport.send_message(message).await;
        assert!(result.is_ok());
    }
}
