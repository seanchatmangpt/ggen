// Library module for MCP/A2A Self-Hosting Example

use a2a_rs::domain::{A2AError, Message, Task};
use a2a_rs::port::AsyncMessageHandler;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};

/// Agent metadata extracted from ontology.
/// NOTE: With AgentBuilder, most metadata lives in a2a-server.toml.
/// This struct is kept for programmatic access (e.g., agent card generation).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentInfo {
    pub name: String,
    pub version: String,
    pub description: String,
    pub url: String,
    pub provider: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_prompt: Option<String>,
}

impl AgentInfo {
    pub fn new(
        name: String,
        version: String,
        description: String,
        url: String,
        provider: String,
    ) -> Self {
        Self {
            name,
            version,
            description,
            url,
            provider,
            system_prompt: None,
        }
    }

    pub fn with_system_prompt(mut self, prompt: String) -> Self {
        self.system_prompt = Some(prompt);
        self
    }
}

/// Default agent info from ontology.
pub fn agent_info() -> AgentInfo {
    AgentInfo::new(
        "CodeGeneratorAgent".to_string(),
        "1.0.0".to_string(),
        "AI-powered code generation agent for RDF ontologies".to_string(),
        "https://github.com/seanchatmangpt/ggen".to_string(),
        "ggen".to_string(),
    )
    .with_system_prompt(
        "You are an expert code generation agent specializing in RDF ontologies and the ggen framework.\
         Your role is to help developers transform semantic specifications into production-ready code.\
         You understand SPARQL, Tera templating, and multiple programming languages.".to_string(),
    )
}

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
        _message: &Message,
        _session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        tracing::info!(task_id, "processing A2A message");

        Err(A2AError::UnsupportedOperation(
            "Code generation pipeline not yet wired".to_string(),
        ))
    }
}

/// MCP handler for Model Context Protocol integration
pub struct McpHandler {
    #[allow(dead_code)]
    ontology_path: String,
}

impl McpHandler {
    pub fn new(ontology_path: String) -> anyhow::Result<Self> {
        Ok(Self { ontology_path })
    }
}
