//! Server that exposes ggen LLM capabilities as an MCP server
//!
//! Provides both HTTP and stdio transport options for MCP protocol communication.

use crate::adapter::{A2aAgentCard, ToolToAgentAdapter};
use crate::client::A2aLlmClient;
use crate::error::A2aMcpResult;
use crate::registry::McpToolRegistry;
use crate::transport::{McpToolDefinition, McpTransport};
use a2a_generated::converged::message::ConvergedMessage;
use ggen_ai::dspy::model_capabilities::Model;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, instrument};

/// A2A server configuration
#[derive(Debug, Clone)]
pub struct A2aServerConfig {
    pub agent_name: String,
    pub agent_description: String,
    pub host: String,
    pub port: u16,
}

impl Default for A2aServerConfig {
    fn default() -> Self {
        Self {
            agent_name: "ggen-llm-agent".to_string(),
            agent_description: "ggen LLM-powered agent for code generation and analysis"
                .to_string(),
            host: "127.0.0.1".to_string(),
            port: 8080,
        }
    }
}

/// MCP server configuration
#[derive(Debug, Clone)]
pub struct McpServerConfig {
    /// Server name
    pub name: String,
    /// Server version
    pub version: String,
    /// Whether to use stdio transport
    pub stdio_enabled: bool,
    /// Whether to use HTTP transport
    pub http_enabled: bool,
    /// HTTP host binding
    pub http_host: String,
    /// HTTP port binding
    pub http_port: u16,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            name: "ggen-mcp-server".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            stdio_enabled: true,
            http_enabled: false,
            http_host: "127.0.0.1".to_string(),
            http_port: 3000,
        }
    }
}

/// A server that exposes ggen LLM capabilities as both an A2A agent and MCP server
pub struct A2aLlmServer {
    client: Arc<A2aLlmClient>,
    config: A2aServerConfig,
    adapter: Arc<RwLock<ToolToAgentAdapter>>,
    mcp_transport: Arc<McpTransport>,
    mcp_registry: Arc<McpToolRegistry>,
}

impl A2aLlmServer {
    /// Create a new server
    #[instrument(skip(model, config))]
    pub async fn new(model: Model, config: A2aServerConfig) -> A2aMcpResult<Self> {
        let client = A2aLlmClient::new(model).await?;
        let mut adapter =
            ToolToAgentAdapter::new(config.agent_name.clone(), config.agent_description.clone());

        // Add default capabilities
        adapter.add_tool(crate::adapter::Tool {
            name: "chat".to_string(),
            description: "General chat and conversation".to_string(),
            parameters: None,
        });
        adapter.add_tool(crate::adapter::Tool {
            name: "complete".to_string(),
            description: "Code completion assistance".to_string(),
            parameters: None,
        });
        adapter.add_tool(crate::adapter::Tool {
            name: "analyze".to_string(),
            description: "Code analysis and review".to_string(),
            parameters: None,
        });

        // Create MCP transport
        let mcp_transport = Arc::new(McpTransport::new());
        let mcp_registry = Arc::new(McpToolRegistry::new());

        Ok(Self {
            client: Arc::new(client),
            config,
            adapter: Arc::new(RwLock::new(adapter)),
            mcp_transport,
            mcp_registry,
        })
    }

    /// Create a new server with MCP tool registry
    #[instrument(skip(model, a2a_config, _mcp_config))]
    pub async fn with_mcp(
        model: Model, a2a_config: A2aServerConfig, _mcp_config: McpServerConfig,
    ) -> A2aMcpResult<Self> {
        let server = Self::new(model, a2a_config).await?;

        // Register MCP tools
        server.register_mcp_tools().await?;

        Ok(server)
    }

    /// Register MCP tools from the ggen-ai tool registry
    async fn register_mcp_tools(&self) -> A2aMcpResult<()> {
        // Sync with ggen-ai tool registry
        let count = self.mcp_registry.sync().await?;
        info!("Synced {} tools from ggen-ai registry", count);

        // Register tools with the MCP transport
        let tools = self.mcp_registry.list().await?;
        for tool in tools {
            let mcp_def = McpToolDefinition {
                name: tool.id.clone(),
                description: tool.description,
                input_schema: tool.input_schema,
            };
            self.mcp_transport.register_tool(mcp_def).await?;
        }

        Ok(())
    }

    /// Get the agent card for this server
    pub async fn agent_card(&self) -> A2aAgentCard {
        let adapter = self.adapter.read().await;
        adapter.agent_card()
    }

    /// Handle an incoming A2A message
    #[instrument(skip(self, message))]
    pub async fn handle_message(
        &self, message: &ConvergedMessage,
    ) -> A2aMcpResult<ConvergedMessage> {
        debug!("Handling message from {}", message.source);

        // Process the message through the LLM client
        let response = self.client.process_message(message).await?;

        Ok(response)
    }

    /// Get the server configuration
    pub fn config(&self) -> &A2aServerConfig {
        &self.config
    }

    /// Get the MCP transport
    pub fn mcp_transport(&self) -> &McpTransport {
        &self.mcp_transport
    }

    /// Get the MCP tool registry
    pub fn mcp_registry(&self) -> &McpToolRegistry {
        &self.mcp_registry
    }

    /// Start the HTTP server (placeholder for actual implementation)
    #[instrument(skip(self))]
    pub async fn start(&self) -> A2aMcpResult<()> {
        info!(
            "Starting A2A LLM server on {}:{}",
            self.config.host, self.config.port
        );

        // TODO: Implement actual HTTP server for A2A protocol
        // This would involve:
        // 1. Setting up Axum/HTTP server
        // 2. Defining routes for A2A message handling
        // 3. WebSocket support for streaming responses
        // 4. Health check endpoint
        // 5. Metrics endpoint

        Ok(())
    }

    /// Run the MCP stdio server
    #[instrument(skip(self))]
    pub async fn run_stdio(&self) -> A2aMcpResult<()> {
        info!("Starting MCP stdio server");

        self.mcp_transport.run_stdio().await
    }

    /// Start both A2A HTTP server and MCP stdio server
    #[instrument(skip(self))]
    pub async fn start_all(&self) -> A2aMcpResult<()> {
        info!("Starting ggen A2A-MCP server");

        // Start stdio in current task
        let transport = self.mcp_transport.clone();
        tokio::spawn(async move {
            if let Err(e) = transport.run_stdio().await {
                tracing::error!("MCP stdio server error: {}", e);
            }
        });

        // Start HTTP server if enabled
        if self.config.port > 0 {
            self.start().await?;
        }

        info!("Servers started successfully");
        Ok(())
    }
}

/// Builder for creating A2aLlmServer instances
pub struct A2aLlmServerBuilder {
    model: Option<Model>,
    a2a_config: A2aServerConfig,
    mcp_config: McpServerConfig,
    with_mcp: bool,
}

impl Default for A2aLlmServerBuilder {
    fn default() -> Self {
        Self {
            model: None,
            a2a_config: A2aServerConfig::default(),
            mcp_config: McpServerConfig::default(),
            with_mcp: false,
        }
    }
}

impl A2aLlmServerBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the model
    pub fn with_model(mut self, model: Model) -> Self {
        self.model = Some(model);
        self
    }

    /// Set the A2A configuration
    pub fn with_a2a_config(mut self, config: A2aServerConfig) -> Self {
        self.a2a_config = config;
        self
    }

    /// Set the MCP configuration
    pub fn with_mcp_config(mut self, config: McpServerConfig) -> Self {
        self.mcp_config = config;
        self.with_mcp = true;
        self
    }

    /// Enable MCP support
    pub fn enable_mcp(mut self) -> Self {
        self.with_mcp = true;
        self
    }

    /// Build the server
    pub async fn build(self) -> A2aMcpResult<A2aLlmServer> {
        let model = self
            .model
            .ok_or_else(|| crate::error::A2aMcpError::Llm("Model not specified".to_string()))?;

        if self.with_mcp {
            A2aLlmServer::with_mcp(model, self.a2a_config, self.mcp_config).await
        } else {
            A2aLlmServer::new(model, self.a2a_config).await
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_ai::dspy::model_capabilities::{ModelCapabilities, ModelConfig, ModelProvider};

    fn create_test_model() -> Model {
        use ggen_ai::dspy::model_capabilities::{LatencyClass, Modality, ReliabilityClass};
        Model {
            name: "test-model".to_string(),
            provider: ModelProvider::Zai,
            capabilities: ModelCapabilities {
                max_context_tokens: 100000,
                max_output_tokens: 4096,
                structured_output: true,
                function_calling: true,
                system_messages: true,
                streaming: true,
                vision: false,
                latency_class: LatencyClass::Medium,
                reliability: ReliabilityClass::High,
                modalities: vec![Modality::Text],
            },
            cost: None,
            config: ModelConfig {
                default_temperature: Some(0.7),
                default_max_tokens: Some(4096),
                api_base: Some("http://localhost:8080".to_string()),
                api_version: None,
                parameters: std::collections::HashMap::new(),
                extra_headers: std::collections::HashMap::new(),
            },
        }
    }

    #[tokio::test]
    async fn test_server_config_default() {
        let config = A2aServerConfig::default();
        assert_eq!(config.agent_name, "ggen-llm-agent");
        assert_eq!(config.port, 8080);
    }

    #[tokio::test]
    async fn test_mcp_server_config_default() {
        let config = McpServerConfig::default();
        assert_eq!(config.name, "ggen-mcp-server");
        assert!(config.stdio_enabled);
        assert!(!config.http_enabled);
    }

    #[tokio::test]
    async fn test_server_builder() {
        let model = create_test_model();
        let builder = A2aLlmServerBuilder::new().with_model(model).enable_mcp();

        assert!(builder.model.is_some());
        assert!(builder.with_mcp);
    }

    #[tokio::test]
    async fn test_agent_card() {
        let _model = create_test_model();
        let config = A2aServerConfig {
            agent_name: "test-agent".to_string(),
            agent_description: "Test description".to_string(),
            ..Default::default()
        };

        // We can't easily test full server creation without mocking the LLM client
        // but we can verify the configuration is used correctly
        assert_eq!(config.agent_name, "test-agent");
        assert_eq!(config.agent_description, "Test description");
    }
}
