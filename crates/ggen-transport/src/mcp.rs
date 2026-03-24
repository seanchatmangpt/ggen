use crate::error::{Result, TransportError};
use crate::origin::{Origin, OriginValidator};
use crate::session::{ResumeCursor, SessionId, SessionManager};
use crate::streaming::{MessageStream, StreamBuilder, StreamSender};
use async_trait::async_trait;
use bytes::Bytes;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpRequest {
    pub id: String,
    pub method: String,
    pub params: serde_json::Value,
    pub session_id: Option<SessionId>,
    pub origin: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpResponse {
    pub id: String,
    pub result: Option<serde_json::Value>,
    pub error: Option<McpError>,
    pub session_id: Option<SessionId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpError {
    pub code: i32,
    pub message: String,
    pub data: Option<serde_json::Value>,
}

impl McpError {
    pub fn new(code: i32, message: String) -> Self {
        Self {
            code,
            message,
            data: None,
        }
    }

    pub fn with_data(mut self, data: serde_json::Value) -> Self {
        self.data = Some(data);
        self
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpStreamRequest {
    pub id: String,
    pub method: String,
    pub params: serde_json::Value,
    pub session_id: SessionId,
    pub resume_cursor: Option<ResumeCursor>,
}

#[async_trait]
pub trait McpHandler: Send + Sync {
    async fn handle_request(&self, request: McpRequest) -> Result<McpResponse>;
    async fn handle_stream_request(
        &self,
        request: McpStreamRequest,
    ) -> Result<(StreamSender, MessageStream)>;
}

pub struct McpTransport {
    handlers: Arc<RwLock<HashMap<String, Arc<dyn McpHandler>>>>,
    session_manager: SessionManager,
    origin_validator: OriginValidator,
}

impl McpTransport {
    pub fn new(session_manager: SessionManager, origin_validator: OriginValidator) -> Self {
        Self {
            handlers: Arc::new(RwLock::new(HashMap::new())),
            session_manager,
            origin_validator,
        }
    }

    pub async fn register_handler(&self, method: String, handler: Arc<dyn McpHandler>) {
        let mut handlers = self.handlers.write().await;
        handlers.insert(method, handler);
    }

    pub async fn handle_request(&self, request: McpRequest) -> McpResponse {
        if let Some(origin_str) = &request.origin {
            if let Ok(origin) = Origin::from_url(origin_str) {
                if let Err(e) = self.origin_validator.validate(&origin) {
                    return McpResponse {
                        id: request.id.clone(),
                        result: None,
                        error: Some(McpError::new(-32001, e.to_string())),
                        session_id: request.session_id.clone(),
                    };
                }
            }
        }

        if let Some(session_id) = &request.session_id {
            if let Err(e) = self.session_manager.touch_session(session_id).await {
                return McpResponse {
                    id: request.id.clone(),
                    result: None,
                    error: Some(McpError::new(-32002, e.to_string())),
                    session_id: Some(session_id.clone()),
                };
            }
        }

        let handlers = self.handlers.read().await;
        let handler = match handlers.get(&request.method) {
            Some(h) => h.clone(),
            None => {
                return McpResponse {
                    id: request.id.clone(),
                    result: None,
                    error: Some(McpError::new(
                        -32601,
                        format!("Method not found: {}", request.method),
                    )),
                    session_id: request.session_id.clone(),
                };
            }
        };

        drop(handlers);

        match handler.handle_request(request.clone()).await {
            Ok(response) => response,
            Err(e) => McpResponse {
                id: request.id,
                result: None,
                error: Some(McpError::new(-32603, e.to_string())),
                session_id: request.session_id,
            },
        }
    }

    pub async fn handle_stream_request(
        &self,
        request: McpStreamRequest,
    ) -> Result<(StreamSender, MessageStream)> {
        self.session_manager
            .touch_session(&request.session_id)
            .await?;

        let handlers = self.handlers.read().await;
        let handler = handlers
            .get(&request.method)
            .ok_or_else(|| {
                TransportError::ProtocolError(format!("Method not found: {}", request.method))
            })?
            .clone();
        drop(handlers);

        handler.handle_stream_request(request).await
    }

    pub async fn create_session(&self) -> SessionId {
        let session = self.session_manager.create_session().await;
        session.id
    }

    pub async fn resume_stream(
        &self,
        session_id: &SessionId,
        cursor: &ResumeCursor,
    ) -> Result<(StreamSender, MessageStream)> {
        let session = self.session_manager.get_session(session_id).await?;

        let builder = StreamBuilder::new(session.id.clone()).resume_from(cursor.position);

        Ok(builder.build())
    }
}

pub struct EchoHandler;

#[async_trait]
impl McpHandler for EchoHandler {
    async fn handle_request(&self, request: McpRequest) -> Result<McpResponse> {
        Ok(McpResponse {
            id: request.id,
            result: Some(request.params),
            error: None,
            session_id: request.session_id,
        })
    }

    async fn handle_stream_request(
        &self,
        request: McpStreamRequest,
    ) -> Result<(StreamSender, MessageStream)> {
        let builder = StreamBuilder::new(request.session_id.clone());
        Ok(builder.build())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mcp_request_response() {
        let session_manager = SessionManager::new(3600);
        let origin_validator = OriginValidator::allow_all();
        let transport = McpTransport::new(session_manager, origin_validator);

        transport
            .register_handler("echo".to_string(), Arc::new(EchoHandler))
            .await;

        let request = McpRequest {
            id: "1".to_string(),
            method: "echo".to_string(),
            params: serde_json::json!({"test": "value"}),
            session_id: None,
            origin: None,
        };

        let response = transport.handle_request(request).await;
        assert!(response.error.is_none());
        assert_eq!(
            response.result.unwrap(),
            serde_json::json!({"test": "value"})
        );
    }

    #[tokio::test]
    async fn test_mcp_method_not_found() {
        let session_manager = SessionManager::new(3600);
        let origin_validator = OriginValidator::allow_all();
        let transport = McpTransport::new(session_manager, origin_validator);

        let request = McpRequest {
            id: "1".to_string(),
            method: "nonexistent".to_string(),
            params: serde_json::json!({}),
            session_id: None,
            origin: None,
        };

        let response = transport.handle_request(request).await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32601);
    }

    #[tokio::test]
    async fn test_mcp_origin_validation() {
        let session_manager = SessionManager::new(3600);
        let origin_validator = OriginValidator::new(vec!["https://allowed.com".to_string()]);
        let transport = McpTransport::new(session_manager, origin_validator);

        let request = McpRequest {
            id: "1".to_string(),
            method: "test".to_string(),
            params: serde_json::json!({}),
            session_id: None,
            origin: Some("https://blocked.com".to_string()),
        };

        let response = transport.handle_request(request).await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32001);
    }
}
