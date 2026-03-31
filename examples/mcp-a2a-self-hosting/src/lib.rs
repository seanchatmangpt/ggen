// Library module for MCP/A2A Self-Hosting Example

pub mod generated;
pub use generated::agent;

use a2a_rs::port::AsyncMessageHandler;
use a2a_rs::domain::{A2AError, Message, Task};
use async_trait::async_trait;

/// Main agent handler implementing the A2A AsyncMessageHandler trait.
///
/// Storage, auth, and streaming are managed by AgentBuilder's AutoStorage.
/// Only message processing logic lives here.
#[derive(Clone)]
pub struct GgenAgentHandler;

impl GgenAgentHandler {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl AsyncMessageHandler for GgenAgentHandler {
    async fn process_message(
        &self,
        task_id: &str,
        message: &Message,
        _session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // TODO: wire into ggen-core code generation pipeline
        tracing::info!(task_id, "processing A2A message");

        Err(A2AError::UnsupportedOperation(
            "Code generation pipeline not yet wired".to_string(),
        ))
    }
}

/// MCP handler for Model Context Protocol integration
pub struct McpHandler {
    ontology_path: String,
}

impl McpHandler {
    pub fn new(ontology_path: String) -> anyhow::Result<Self> {
        Ok(Self { ontology_path })
    }
}
