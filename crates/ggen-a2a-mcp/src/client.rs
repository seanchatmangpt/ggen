//! Client for accessing LLM providers through A2A protocol with ZAI support
//!
//! This module implements the MCP client that bridges to A2A agents with:
//! - A2A task manager integration
//! - Tool calling capability
//! - Message sending and result retrieval
//! - Connection management with health checks
//! - Async streaming support
//! - Zero unwrap/expect - proper Result<T,E> handling

use crate::adapter::{AgentToToolAdapter, ToolCall};
use crate::error::{A2aMcpError, A2aMcpResult};
use crate::message::{A2aMessageConverter, LlmRequest, LlmResponse};
use a2a_generated::converged::{message::ConvergedMessage, UnifiedAgent};
use futures::StreamExt;
use ggen_ai::client::{GenAiClient, LlmClient as _, LlmConfig};
use ggen_ai::dspy::model_capabilities::Model;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Mutex, RwLock, Semaphore};
use tokio::time::{interval, Instant};
use tracing::{debug, info, warn};

/// Connection state for the A2A client
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Reconnecting,
    ShuttingDown,
}

/// Health status of the A2A client connection
#[derive(Debug, Clone)]
pub struct ConnectionHealth {
    pub state: ConnectionState,
    pub last_heartbeat: Instant,
    pub successful_requests: u64,
    pub failed_requests: u64,
    pub average_latency_ms: f64,
}

impl Default for ConnectionHealth {
    fn default() -> Self {
        Self {
            state: ConnectionState::Disconnected,
            last_heartbeat: Instant::now(),
            successful_requests: 0,
            failed_requests: 0,
            average_latency_ms: 0.0,
        }
    }
}

/// Configuration for the A2A MCP client
#[derive(Debug, Clone)]
pub struct A2aClientConfig {
    /// Maximum concurrent requests
    pub max_concurrent_requests: usize,

    /// Connection timeout
    pub connection_timeout: Duration,

    /// Request timeout
    pub request_timeout: Duration,

    /// Health check interval
    pub health_check_interval: Duration,

    /// Maximum retry attempts
    pub max_retries: usize,

    /// Retry backoff multiplier
    pub retry_backoff_multiplier: f64,

    /// Enable streaming support
    pub enable_streaming: bool,

    /// Enable ZAI support
    pub enable_zai: bool,
}

impl Default for A2aClientConfig {
    fn default() -> Self {
        Self {
            max_concurrent_requests: 10,
            connection_timeout: Duration::from_secs(30),
            request_timeout: Duration::from_secs(120),
            health_check_interval: Duration::from_secs(60),
            max_retries: 3,
            retry_backoff_multiplier: 2.0,
            enable_streaming: true,
            enable_zai: true,
        }
    }
}

/// Tool execution result from A2A agent
#[derive(Debug, Clone)]
pub struct ToolExecutionResult {
    /// Tool name that was executed
    pub tool_name: String,

    /// Execution result
    pub result: serde_json::Value,

    /// Whether execution was successful
    pub success: bool,

    /// Error message if unsuccessful
    pub error: Option<String>,

    /// Execution duration
    pub duration_ms: u64,

    /// Metadata from the execution
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Streaming response chunk
#[derive(Debug, Clone)]
pub struct StreamingChunk {
    /// Content chunk
    pub content: String,

    /// Whether this is the final chunk
    pub is_final: bool,

    /// Tool calls in this chunk (if any)
    pub tool_calls: Vec<ToolCall>,

    /// Usage statistics (only in final chunk)
    pub usage: Option<crate::message::TokenUsage>,
}

/// Client that bridges A2A messages with ggen-ai LLM providers
pub struct A2aLlmClient {
    /// LLM client for direct API calls
    llm_client: Arc<Mutex<GenAiClient>>,

    /// Adapter for A2A to tool conversions
    adapter: Arc<Mutex<AgentToToolAdapter>>,

    /// Message converter
    converter: Arc<A2aMessageConverter>,

    /// Model configuration
    model: Model,

    /// Client configuration
    config: A2aClientConfig,

    /// Connection health
    health: Arc<RwLock<ConnectionHealth>>,

    /// Semaphore for concurrent request limiting
    request_semaphore: Arc<Semaphore>,

    /// Registered A2A agents
    agents: Arc<RwLock<HashMap<String, UnifiedAgent>>>,

    /// Active tasks being tracked
    active_tasks: Arc<RwLock<HashMap<String, TaskContext>>>,

    /// Whether the client is running
    running: Arc<RwLock<bool>>,
}

/// Context for tracking task execution
#[derive(Debug, Clone)]
struct TaskContext {
    task_id: String,
    created_at: Instant,
    status: TaskStatus,
    retry_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum TaskStatus {
    Pending,
    Running,
    Completed,
    Failed,
}

impl A2aLlmClient {
    /// Create a new client with default configuration
    pub async fn new(model: Model) -> A2aMcpResult<Self> {
        Self::with_config(model, A2aClientConfig::default()).await
    }

    /// Create a new client with custom configuration
    pub async fn with_config(model: Model, config: A2aClientConfig) -> A2aMcpResult<Self> {
        let llm_config = LlmConfig {
            model: model.name.clone(),
            max_tokens: Some(model.capabilities.max_output_tokens as u32),
            temperature: model.config.default_temperature.map(|t| t as f32),
            top_p: None,
            stop: None,
            extra: Default::default(),
        };

        let llm_client = GenAiClient::new(llm_config)
            .map_err(|e| A2aMcpError::Llm(format!("Failed to create LLM client: {}", e)))?;

        let semaphore = Arc::new(Semaphore::new(config.max_concurrent_requests));

        let client = Self {
            llm_client: Arc::new(Mutex::new(llm_client)),
            adapter: Arc::new(Mutex::new(AgentToToolAdapter::new())),
            converter: Arc::new(A2aMessageConverter::new()),
            model,
            config,
            health: Arc::new(RwLock::new(ConnectionHealth::default())),
            request_semaphore: semaphore,
            agents: Arc::new(RwLock::new(HashMap::new())),
            active_tasks: Arc::new(RwLock::new(HashMap::new())),
            running: Arc::new(RwLock::new(false)),
        };

        // Start health check task
        client.start_health_check().await;

        Ok(client)
    }

    /// Create a new client with a custom system prompt
    pub async fn with_system_prompt<S: Into<String>>(
        model: Model, system_prompt: S,
    ) -> A2aMcpResult<Self> {
        let converter = A2aMessageConverter::new().with_system_prompt(system_prompt);
        let config = A2aClientConfig::default();

        let llm_config = LlmConfig {
            model: model.name.clone(),
            max_tokens: Some(model.capabilities.max_output_tokens as u32),
            temperature: model.config.default_temperature.map(|t| t as f32),
            top_p: None,
            stop: None,
            extra: Default::default(),
        };

        let llm_client = GenAiClient::new(llm_config)
            .map_err(|e| A2aMcpError::Llm(format!("Failed to create LLM client: {}", e)))?;

        let semaphore = Arc::new(Semaphore::new(config.max_concurrent_requests));

        let client = Self {
            llm_client: Arc::new(Mutex::new(llm_client)),
            adapter: Arc::new(Mutex::new(AgentToToolAdapter::new())),
            converter: Arc::new(converter),
            model,
            config,
            health: Arc::new(RwLock::new(ConnectionHealth::default())),
            request_semaphore: semaphore,
            agents: Arc::new(RwLock::new(HashMap::new())),
            active_tasks: Arc::new(RwLock::new(HashMap::new())),
            running: Arc::new(RwLock::new(false)),
        };

        client.start_health_check().await;
        Ok(client)
    }

    /// Start the health check background task
    async fn start_health_check(&self) {
        let health = Arc::clone(&self.health);
        let check_interval = self.config.health_check_interval;
        let running = Arc::clone(&self.running);

        // Mark as running
        *running.write().await = true;

        tokio::spawn(async move {
            let mut timer = interval(check_interval);

            while *running.read().await {
                timer.tick().await;

                let mut h = health.write().await;
                h.last_heartbeat = Instant::now();

                // Update state based on recent activity
                if h.failed_requests > 10 && h.successful_requests == 0 {
                    h.state = ConnectionState::Disconnected;
                } else if h.state == ConnectionState::Connecting {
                    h.state = ConnectionState::Connected;
                }
            }
        });
    }

    /// Connect to an A2A agent
    pub async fn connect_to_agent(&self, agent: UnifiedAgent) -> A2aMcpResult<()> {
        let agent_id = agent.identity.id.clone();

        info!("Connecting to A2A agent: {}", agent_id);

        // Update connection state
        {
            let mut health = self.health.write().await;
            health.state = ConnectionState::Connecting;
        }

        // Register the agent
        {
            let mut agents = self.agents.write().await;
            agents.insert(agent_id.clone(), agent);
        }

        // Update connection state to connected
        {
            let mut health = self.health.write().await;
            health.state = ConnectionState::Connected;
            health.last_heartbeat = Instant::now();
        }

        info!("Successfully connected to agent: {}", agent_id);
        Ok(())
    }

    /// Disconnect from an A2A agent
    pub async fn disconnect_agent(&self, agent_id: &str) -> A2aMcpResult<()> {
        info!("Disconnecting from agent: {}", agent_id);

        let mut agents = self.agents.write().await;
        match agents.remove(agent_id) {
            Some(_) => {
                info!("Successfully disconnected from agent: {}", agent_id);
                Ok(())
            }
            None => Err(A2aMcpError::AgentNotFound(agent_id.to_string())),
        }
    }

    /// Get a registered agent by ID
    pub async fn get_agent(&self, agent_id: &str) -> A2aMcpResult<UnifiedAgent> {
        let agents = self.agents.read().await;
        agents
            .get(agent_id)
            .cloned()
            .ok_or_else(|| A2aMcpError::AgentNotFound(agent_id.to_string()))
    }

    /// List all registered agents
    pub async fn list_agents(&self) -> Vec<String> {
        let agents = self.agents.read().await;
        agents.keys().cloned().collect()
    }

    /// Process an A2A message through the LLM
    pub async fn process_message(
        &self, message: &ConvergedMessage,
    ) -> A2aMcpResult<ConvergedMessage> {
        info!("Processing A2A message: {}", message.message_id);

        // Acquire semaphore permit
        let _permit = self
            .request_semaphore
            .acquire()
            .await
            .map_err(|e| A2aMcpError::TaskProcessing(format!("Semaphore error: {}", e)))?;

        let start = Instant::now();

        // Convert A2A message to LLM request
        let llm_request = self.converter.a2a_to_llm_request(message)?;

        // Make LLM call with retry
        let llm_response = self.call_llm_with_retry(&llm_request).await?;

        // Convert back to A2A format
        let response = self.converter.llm_response_to_a2a(&llm_response, message)?;

        // Update health metrics
        let duration = start.elapsed();
        self.update_health_metrics(true, duration).await;

        info!(
            "Successfully processed message: {} (took {:?})",
            message.message_id, duration
        );
        Ok(response)
    }

    /// Call an LLM tool with A2A agent integration
    pub async fn call_tool(&self, call: ToolCall) -> A2aMcpResult<ToolExecutionResult> {
        debug!("Calling tool: {}", call.method);

        let start = Instant::now();

        // Parse the tool method to extract agent and tool name
        let (agent_id, tool_name) = self.parse_tool_method(&call.method)?;

        // Check if agent exists
        let agent = self.get_agent(&agent_id).await?;

        // Verify the agent has the requested capability
        if !self.agent_has_capability(&agent, &tool_name).await {
            return Err(A2aMcpError::InvalidToolMethod(format!(
                "Agent {} does not have capability {}",
                agent_id, tool_name
            )));
        }

        // Create a prompt from the tool call
        let prompt = format!(
            "Tool: {}\nParameters: {}\n\nExecute this tool call and return the result.",
            call.method,
            serde_json::to_string_pretty(&call.params).unwrap_or_else(|_| call.params.to_string())
        );

        // Make LLM call
        let llm_request = LlmRequest {
            system_prompt: "You are a helpful AI assistant that can execute tool calls. 
                          When given a tool call, execute it and return the result in JSON format."
                .to_string(),
            user_content: prompt,
            message_id: uuid::Uuid::new_v4().to_string(),
        };

        let llm_response = self.call_llm_with_retry(&llm_request).await?;

        let duration = start.elapsed();

        Ok(ToolExecutionResult {
            tool_name: call.method.clone(),
            result: serde_json::json!({
                "result": llm_response.content,
                "model": llm_response.model,
                "agent": agent_id,
            }),
            success: true,
            error: None,
            duration_ms: duration.as_millis() as u64,
            metadata: HashMap::new(),
        })
    }

    /// Execute a tool on a specific A2A agent
    pub async fn execute_tool_on_agent(
        &self, agent_id: &str, tool_name: &str, params: serde_json::Value,
    ) -> A2aMcpResult<ToolExecutionResult> {
        let method = format!("{}:{}", agent_id, tool_name);
        let call = ToolCall { method, params };
        self.call_tool(call).await
    }

    /// Send a message to an A2A agent and get the result
    pub async fn send_message_to_agent(
        &self, agent_id: &str, content: String,
    ) -> A2aMcpResult<ConvergedMessage> {
        // Verify agent exists
        let _agent = self.get_agent(agent_id).await?;

        // Create A2A message
        let message = ConvergedMessage::text(
            format!("msg-{}", uuid::Uuid::new_v4()),
            "mcp-client".to_string(),
            content,
        );

        // Process the message
        self.process_message(&message).await
    }

    /// Stream LLM response
    pub async fn stream_response(
        &self, prompt: &str,
    ) -> A2aMcpResult<impl futures::Stream<Item = StreamingChunk>> {
        if !self.config.enable_streaming {
            return Err(A2aMcpError::Translation(
                "Streaming is not enabled".to_string(),
            ));
        }

        let client = self.llm_client.lock().await;
        let stream = client
            .complete_stream(prompt)
            .await
            .map_err(|e| A2aMcpError::Llm(format!("Stream request failed: {}", e)))?;

        let _model = self.model.name.clone();

        Ok(stream.map(move |chunk| StreamingChunk {
            content: chunk.content,
            is_final: chunk.finish_reason.is_some(),
            tool_calls: Vec::new(),
            usage: chunk.usage.map(|u| crate::message::TokenUsage {
                prompt_tokens: u.prompt_tokens,
                completion_tokens: u.completion_tokens,
                total_tokens: u.total_tokens,
            }),
        }))
    }

    /// Create a task for tracking
    pub async fn create_task(&self, task_id: String) -> A2aMcpResult<()> {
        let context = TaskContext {
            task_id: task_id.clone(),
            created_at: Instant::now(),
            status: TaskStatus::Pending,
            retry_count: 0,
        };

        let mut tasks = self.active_tasks.write().await;
        tasks.insert(task_id, context);
        Ok(())
    }

    /// Update task status
    pub async fn update_task_status(&self, task_id: &str, status: TaskStatus) -> A2aMcpResult<()> {
        let mut tasks = self.active_tasks.write().await;
        match tasks.get_mut(task_id) {
            Some(context) => {
                context.status = status;
                Ok(())
            }
            None => Err(A2aMcpError::TaskNotFound(task_id.to_string())),
        }
    }

    /// Complete a task
    pub async fn complete_task(&self, task_id: &str) -> A2aMcpResult<()> {
        let mut tasks = self.active_tasks.write().await;
        match tasks.remove(task_id) {
            Some(_) => Ok(()),
            None => Err(A2aMcpError::TaskNotFound(task_id.to_string())),
        }
    }

    /// Get task status
    pub async fn get_task_status(&self, task_id: &str) -> A2aMcpResult<TaskStatus> {
        let tasks = self.active_tasks.read().await;
        match tasks.get(task_id) {
            Some(context) => Ok(context.status.clone()),
            None => Err(A2aMcpError::TaskNotFound(task_id.to_string())),
        }
    }

    /// Get connection health
    pub async fn health(&self) -> ConnectionHealth {
        self.health.read().await.clone()
    }

    /// Check if the client is connected
    pub async fn is_connected(&self) -> bool {
        let health = self.health.read().await;
        matches!(health.state, ConnectionState::Connected)
    }

    /// Make the actual LLM call
    async fn call_llm(&self, request: &LlmRequest) -> A2aMcpResult<LlmResponse> {
        let client = self.llm_client.lock().await;

        // Build the prompt with system context
        let full_prompt = format!(
            "{}\n\nUser: {}",
            request.system_prompt, request.user_content
        );

        // Make the API call using the GenAiClient
        let ggen_response = client
            .complete(&full_prompt)
            .await
            .map_err(|e| A2aMcpError::Llm(format!("LLM request failed: {}", e)))?;

        Ok(LlmResponse {
            content: ggen_response.content,
            model: ggen_response.model,
            usage: ggen_response.usage.map(|u| crate::message::TokenUsage {
                prompt_tokens: u.prompt_tokens,
                completion_tokens: u.completion_tokens,
                total_tokens: u.total_tokens,
            }),
        })
    }

    /// Make LLM call with retry logic
    async fn call_llm_with_retry(&self, request: &LlmRequest) -> A2aMcpResult<LlmResponse> {
        let mut last_error = None;
        let mut delay = Duration::from_millis(100);

        for attempt in 0..=self.config.max_retries {
            match self.call_llm(request).await {
                Ok(response) => return Ok(response),
                Err(e) => {
                    warn!(
                        "LLM call attempt {} failed: {}, retrying in {:?}",
                        attempt, e, delay
                    );
                    last_error = Some(e);

                    if attempt < self.config.max_retries {
                        tokio::time::sleep(delay).await;
                        delay = Duration::from_millis(
                            (delay.as_millis() as f64 * self.config.retry_backoff_multiplier)
                                as u64,
                        );
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| {
            A2aMcpError::Llm("Max retries exceeded with unknown error".to_string())
        }))
    }

    /// Update health metrics after a request
    async fn update_health_metrics(&self, success: bool, duration: Duration) {
        let mut health = self.health.write().await;

        if success {
            health.successful_requests += 1;

            // Update average latency
            let total_requests = (health.successful_requests + health.failed_requests) as f64;
            let current_avg = health.average_latency_ms;
            let new_duration_ms = duration.as_millis() as f64;
            health.average_latency_ms =
                (current_avg * (total_requests - 1.0) + new_duration_ms) / total_requests;
        } else {
            health.failed_requests += 1;
        }
    }

    /// Parse tool method to extract agent and tool name
    fn parse_tool_method(&self, method: &str) -> A2aMcpResult<(String, String)> {
        let parts: Vec<&str> = method.split(':').collect();
        match parts.as_slice() {
            [agent, tool] => Ok((agent.to_string(), tool.to_string())),
            _ => Err(A2aMcpError::InvalidToolMethod(format!(
                "Invalid tool method format: {}",
                method
            ))),
        }
    }

    /// Check if an agent has a specific capability
    async fn agent_has_capability(&self, agent: &UnifiedAgent, capability: &str) -> bool {
        agent
            .capabilities
            .primary
            .iter()
            .any(|cap| cap.name == capability)
    }

    /// Get available tools as A2A capabilities
    pub async fn get_tools(&self) -> Vec<crate::adapter::Tool> {
        let adapter = self.adapter.lock().await;
        let mut tools = adapter.generate_tools("ggen-llm", &["chat", "complete", "analyze"]);

        // Add tools from registered agents
        let agents = self.agents.read().await;
        for agent in agents.values() {
            for capability in &agent.capabilities.primary {
                tools.push(crate::adapter::Tool {
                    name: format!("{}:{}", agent.identity.id, capability.name),
                    description: capability
                        .description
                        .clone()
                        .unwrap_or_else(|| capability.name.clone()),
                    parameters: capability
                        .requirements
                        .as_ref()
                        .map(|r| serde_json::to_value(r).ok())
                        .flatten(),
                });
            }
        }

        tools
    }

    /// Shutdown the client gracefully
    pub async fn shutdown(&self) -> A2aMcpResult<()> {
        info!("Shutting down A2A MCP client");

        // Update running state
        {
            let mut running = self.running.write().await;
            *running = false;
        }

        // Update connection state
        {
            let mut health = self.health.write().await;
            health.state = ConnectionState::ShuttingDown;
        }

        // Clear all agents
        {
            let mut agents = self.agents.write().await;
            agents.clear();
        }

        // Clear all active tasks
        {
            let mut tasks = self.active_tasks.write().await;
            tasks.clear();
        }

        info!("A2A MCP client shutdown complete");
        Ok(())
    }
}

impl Drop for A2aLlmClient {
    fn drop(&mut self) {
        // The Drop trait is synchronous, so we can't directly await async calls here.
        // In production code, you'd want to use a different approach like:
        // 1. Having a dedicated shutdown method that must be called explicitly
        // 2. Using a sync primitive like a channel to signal a background task
        // For now, this is a placeholder that logs the drop.
        debug!("A2aLlmClient dropped");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_client_creation() {
        let model = Model::from_name("gpt-4");
        let client = A2aLlmClient::new(model).await;
        assert!(client.is_ok());
    }

    #[tokio::test]
    async fn test_client_with_config() {
        let model = Model::from_name("gpt-4");
        let config = A2aClientConfig {
            max_concurrent_requests: 5,
            ..Default::default()
        };
        let client = A2aLlmClient::with_config(model, config).await;
        assert!(client.is_ok());
    }

    #[tokio::test]
    async fn test_parse_tool_method() {
        let model = Model::from_name("gpt-4");
        let client = A2aLlmClient::new(model).await.unwrap();

        let result = client.parse_tool_method("agent:tool");
        assert!(result.is_ok());
        let (agent, tool) = result.unwrap();
        assert_eq!(agent, "agent");
        assert_eq!(tool, "tool");

        let invalid = client.parse_tool_method("invalid");
        assert!(invalid.is_err());
    }

    #[tokio::test]
    async fn test_connection_health() {
        let model = Model::from_name("gpt-4");
        let client = A2aLlmClient::new(model).await.unwrap();

        let health = client.health().await;
        assert_eq!(health.successful_requests, 0);
        assert_eq!(health.failed_requests, 0);
    }

    #[tokio::test]
    async fn test_task_tracking() {
        let model = Model::from_name("gpt-4");
        let client = A2aLlmClient::new(model).await.unwrap();

        let task_id = "test-task-123";

        // Create task
        assert!(client.create_task(task_id.to_string()).await.is_ok());

        // Check status
        let status = client.get_task_status(task_id).await.unwrap();
        assert_eq!(status, TaskStatus::Pending);

        // Update status
        assert!(client
            .update_task_status(task_id, TaskStatus::Running)
            .await
            .is_ok());

        let status = client.get_task_status(task_id).await.unwrap();
        assert_eq!(status, TaskStatus::Running);

        // Complete task
        assert!(client.complete_task(task_id).await.is_ok());

        // Task should no longer exist
        assert!(client.get_task_status(task_id).await.is_err());
    }

    #[tokio::test]
    async fn test_agent_registration() {
        let model = Model::from_name("gpt-4");
        let client = A2aLlmClient::new(model).await.unwrap();

        use a2a_generated::converged::agent::{AgentConfiguration, CommunicationQoS, StrategyType};
        use a2a_generated::converged::{
            AgentCapabilities, AgentCommunication, AgentIdentity, AgentLifecycle, ExecutionStrategy,
        };

        let agent = UnifiedAgent {
            identity: AgentIdentity {
                id: "test-agent".to_string(),
                name: "Test Agent".to_string(),
                agent_type: "test".to_string(),
                version: "1.0.0".to_string(),
                namespace: "test".to_string(),
                tags: None,
            },
            capabilities: AgentCapabilities {
                primary: vec![],
                secondary: None,
                protocols: vec![],
                formats: vec![],
                message_types: vec![],
                qos_levels: vec![],
                constraints: None,
            },
            lifecycle: AgentLifecycle {
                state: a2a_generated::converged::AgentState::Ready,
                state_history: vec![],
                health: a2a_generated::converged::AgentHealth {
                    status: a2a_generated::converged::HealthStatus::Healthy,
                    last_check: chrono::Utc::now(),
                    check_interval: std::time::Duration::from_secs(60),
                    metrics: None,
                    warnings: None,
                    errors: None,
                },
                metrics: None,
                configuration: AgentConfiguration {
                    parameters: std::collections::HashMap::new(),
                    version: "1.0.0".to_string(),
                    timestamp: chrono::Utc::now(),
                    source: None,
                    validation: None,
                },
                dependencies: None,
                timeouts: None,
            },
            communication: AgentCommunication {
                endpoints: vec![],
                protocols: vec![],
                handlers: None,
                security: None,
                qos: CommunicationQoS {
                    reliability: a2a_generated::converged::ReliabilityLevel::AtLeastOnce,
                    latency: None,
                    throughput: None,
                    ordering: None,
                    flow_control: None,
                },
            },
            execution: a2a_generated::converged::AgentExecution {
                mode: a2a_generated::converged::ExecutionMode::Synchronous,
                parameters: std::collections::HashMap::new(),
                context: None,
                strategy: Some(ExecutionStrategy {
                    strategy_type: StrategyType::Sequential,
                    configuration: std::collections::HashMap::new(),
                    parameters: None,
                    metadata: None,
                }),
                monitoring: None,
                policies: None,
            },
            security: a2a_generated::converged::AgentSecurity {
                authentication: a2a_generated::converged::AuthenticationConfig {
                    methods: vec![],
                    providers: None,
                    metadata: None,
                },
                authorization: a2a_generated::converged::AuthorizationConfig {
                    model: a2a_generated::converged::AuthorizationModel::Rbac,
                    roles: None,
                    policies: vec![],
                    metadata: None,
                },
                encryption: a2a_generated::converged::EncryptionConfig {
                    algorithms: vec![a2a_generated::converged::EncryptionAlgorithm::Aes],
                    modes: vec![],
                    keys: vec![],
                    metadata: None,
                },
                compliance: None,
                audit: a2a_generated::converged::AuditConfig {
                    events: vec![a2a_generated::converged::AuditEvent::Authentication],
                    destinations: Vec::new(),
                    retention: a2a_generated::converged::agent::AuditRetention {
                        period: std::time::Duration::from_secs(365 * 24 * 60 * 60),
                        policy: a2a_generated::converged::RetentionPolicy::TimeBased,
                        metadata: None,
                    },
                    metadata: None,
                },
                policies: None,
            },
            extensions: None,
        };

        assert!(client.connect_to_agent(agent).await.is_ok());
        assert!(client.get_agent("test-agent").await.is_ok());

        let agents = client.list_agents().await;
        assert!(agents.contains(&"test-agent".to_string()));

        assert!(client.disconnect_agent("test-agent").await.is_ok());
        assert!(client.disconnect_agent("nonexistent").await.is_err());
    }

    #[tokio::test]
    async fn test_config_defaults() {
        let config = A2aClientConfig::default();
        assert_eq!(config.max_concurrent_requests, 10);
        assert_eq!(config.max_retries, 3);
        assert!(config.enable_streaming);
        assert!(config.enable_zai);
    }
}
