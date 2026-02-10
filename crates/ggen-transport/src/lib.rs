pub mod a2a;
pub mod error;
pub mod mcp;
pub mod origin;
pub mod session;
pub mod streaming;

use async_trait::async_trait;
use bytes::Bytes;
use serde::{Deserialize, Serialize};

pub use error::{Result, TransportError};
pub use origin::{Origin, OriginValidator};
pub use session::{ResumeCursor, Session, SessionId, SessionManager};
pub use streaming::{MessageStream, StreamBuilder, StreamControl, StreamMessage, StreamSender};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportConfig {
    pub session_ttl_seconds: i64,
    pub stream_buffer_size: usize,
    pub max_message_size: usize,
    pub allowed_origins: Vec<String>,
    pub enable_compression: bool,
}

impl Default for TransportConfig {
    fn default() -> Self {
        Self {
            session_ttl_seconds: 3600,
            stream_buffer_size: 100,
            max_message_size: 1024 * 1024,
            allowed_origins: vec![],
            enable_compression: false,
        }
    }
}

#[async_trait]
pub trait Transport: Send + Sync {
    async fn connect(&mut self, endpoint: &str) -> Result<()>;
    async fn disconnect(&mut self) -> Result<()>;
    async fn send(&mut self, data: Bytes) -> Result<()>;
    async fn receive(&mut self) -> Result<Bytes>;
    async fn is_connected(&self) -> bool;
}

#[async_trait]
pub trait StreamingTransport: Transport {
    async fn create_stream(&mut self, session_id: SessionId) -> Result<(StreamSender, MessageStream)>;
    async fn resume_stream(&mut self, cursor: ResumeCursor) -> Result<(StreamSender, MessageStream)>;
    async fn close_stream(&mut self, session_id: &SessionId) -> Result<()>;
}

pub struct TransportBuilder {
    config: TransportConfig,
}

impl TransportBuilder {
    pub fn new() -> Self {
        Self {
            config: TransportConfig::default(),
        }
    }

    pub fn with_config(mut self, config: TransportConfig) -> Self {
        self.config = config;
        self
    }

    pub fn with_session_ttl(mut self, seconds: i64) -> Self {
        self.config.session_ttl_seconds = seconds;
        self
    }

    pub fn with_buffer_size(mut self, size: usize) -> Self {
        self.config.stream_buffer_size = size;
        self
    }

    pub fn with_allowed_origins(mut self, origins: Vec<String>) -> Self {
        self.config.allowed_origins = origins;
        self
    }

    pub fn enable_compression(mut self) -> Self {
        self.config.enable_compression = true;
        self
    }

    pub fn build_mcp(self) -> mcp::McpTransport {
        let session_manager = SessionManager::new(self.config.session_ttl_seconds);
        let origin_validator = if self.config.allowed_origins.is_empty() {
            OriginValidator::allow_all()
        } else {
            OriginValidator::new(self.config.allowed_origins)
        };
        mcp::McpTransport::new(session_manager, origin_validator)
    }

    pub fn build_a2a(self, agent_id: String) -> a2a::A2aTransport {
        let session_manager = SessionManager::new(self.config.session_ttl_seconds);
        let origin_validator = if self.config.allowed_origins.is_empty() {
            OriginValidator::allow_all()
        } else {
            OriginValidator::new(self.config.allowed_origins)
        };
        a2a::A2aTransport::new(agent_id, session_manager, origin_validator)
    }
}

impl Default for TransportBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transport_config_default() {
        let config = TransportConfig::default();
        assert_eq!(config.session_ttl_seconds, 3600);
        assert_eq!(config.stream_buffer_size, 100);
        assert!(!config.enable_compression);
    }

    #[test]
    fn test_transport_builder() {
        let builder = TransportBuilder::new()
            .with_session_ttl(7200)
            .with_buffer_size(200)
            .enable_compression();

        assert_eq!(builder.config.session_ttl_seconds, 7200);
        assert_eq!(builder.config.stream_buffer_size, 200);
        assert!(builder.config.enable_compression);
    }

    #[test]
    fn test_mcp_transport_creation() {
        let builder = TransportBuilder::new();
        let _transport = builder.build_mcp();
    }

    #[test]
    fn test_a2a_transport_creation() {
        let builder = TransportBuilder::new();
        let _transport = builder.build_a2a("test-agent".to_string());
    }
}
