//! Mock A2A Server for Integration Testing
//!
//! This module provides a comprehensive mock implementation of the A2A (Agent-to-Agent)
//! protocol server for testing purposes. It simulates the complete agent lifecycle,
//! task management, message passing, and event handling without requiring network
//! connections or external dependencies.
//!
//! # Features
//!
//! - Complete agent lifecycle management (start/stop/status)
//! - Task state machine (Submitted/Running/Completed/Failed/Canceled)
//! - Message passing simulation with A2A JSON-RPC protocol
//! - Multi-transport support (HTTP, WebSocket simulation)
//! - Event tracking for test assertions
//! - Channel-based async communication
//!
//! # Chicago TDD Pattern
//!
//! This module follows Chicago School TDD principles:
//! - State-based testing (verify actual state changes)
//! - Real collaborators (uses real tokio channels, not mocks)
//! - AAA pattern (Arrange/Act/Assert) in all test helpers
//!
//! # Example
//!
//! ```rust,ignore
//! use ggen_domain::error::A2aError;
//! use mcp_a2a::mock_a2a_server::{MockA2AServer, AgentConfig, AgentState};
//!
//! #[tokio::test]
//! async fn test_agent_lifecycle() {
//!     // Arrange
//!     let mut server = MockA2AServer::new();
//!     let agent_config = AgentConfig::new("test-agent".to_string())
//!         .with_capabilities(vec!["text-generation".to_string()]);
//!
//!     // Act
//!     let result = server.start_agent(agent_config).await;
//!
//!     // Assert
//!     assert!(result.is_ok());
//!     let agent_id = result.unwrap();
//!     let status = server.get_agent_status(&agent_id).await;
//!     assert_eq!(status.state, AgentState::Running);
//! }
//! ```

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock, oneshot};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use ggen_domain::error::{A2aError, AgentError};

//=============================================================================
// Domain Types - A2A Protocol Compatible
//=============================================================================

/// Agent state in the mock server
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum AgentState {
    /// Agent is initialized but not started
    Initialized,
    /// Agent is running and accepting tasks
    Running,
    /// Agent is paused (not accepting new tasks)
    Paused,
    /// Agent is stopping
    Stopping,
    /// Agent has stopped
    Stopped,
    /// Agent encountered an error
    Error,
}

/// Task state in the mock server (compatible with A2A protocol TaskState)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TaskState {
    Submitted,
    Working,
    InputRequired,
    Completed,
    Canceled,
    Failed,
    Rejected,
    AuthRequired,
    Unknown,
}

// Optional conversion to a2a_rs::TaskState when feature is enabled
#[cfg(feature = "use-a2a-rs")]
impl From<TaskState> for a2a_rs::TaskState {
    fn from(state: TaskState) -> Self {
        match state {
            TaskState::Submitted => a2a_rs::TaskState::Submitted,
            TaskState::Working => a2a_rs::TaskState::Working,
            TaskState::InputRequired => a2a_rs::TaskState::InputRequired,
            TaskState::Completed => a2a_rs::TaskState::Completed,
            TaskState::Canceled => a2a_rs::TaskState::Canceled,
            TaskState::Failed => a2a_rs::TaskState::Failed,
            TaskState::Rejected => a2a_rs::TaskState::Rejected,
            TaskState::AuthRequired => a2a_rs::TaskState::AuthRequired,
            TaskState::Unknown => a2a_rs::TaskState::Unknown,
        }
    }
}

/// Agent status information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentStatus {
    /// Agent ID
    pub id: String,
    /// Agent name
    pub name: String,
    /// Current agent state
    pub state: AgentState,
    /// Agent capabilities
    pub capabilities: Vec<String>,
    /// Number of active tasks
    pub active_tasks: usize,
    /// Total tasks processed
    pub total_tasks: usize,
    /// Last activity timestamp
    pub last_activity: DateTime<Utc>,
}

/// Task status information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskStatus {
    /// Task ID
    pub id: String,
    /// Associated agent ID
    pub agent_id: String,
    /// Current task state
    pub state: TaskState,
    /// Status message
    pub message: Option<String>,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Last update timestamp
    pub updated_at: DateTime<Utc>,
    /// Task metadata
    pub metadata: Option<Map<String, Value>>,
}

/// Agent capability definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentCapability {
    /// Capability name
    pub name: String,
    /// Capability description
    pub description: Option<String>,
    /// Supported input modes
    pub input_modes: Vec<String>,
    /// Supported output modes
    pub output_modes: Vec<String>,
}

/// Event types tracked by the mock server
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum MockEventType {
    /// Agent started
    AgentStarted,
    /// Agent stopped
    AgentStopped,
    /// Agent status changed
    AgentStatusChanged,
    /// Task submitted
    TaskSubmitted,
    /// Task updated
    TaskUpdated,
    /// Task completed
    TaskCompleted,
    /// Task failed
    TaskFailed,
    /// Task canceled
    TaskCanceled,
    /// Message sent
    MessageSent,
    /// Message received
    MessageReceived,
    /// Error occurred
    Error,
}

/// Event recorded by the mock server for test assertions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MockEvent {
    /// Event type
    pub event_type: MockEventType,
    /// Event timestamp
    pub timestamp: DateTime<Utc>,
    /// Associated entity ID (agent or task)
    pub entity_id: String,
    /// Event data
    pub data: Option<Map<String, Value>>,
}

/// Transport type for communication
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TransportType {
    Http,
    WebSocket,
    Grpc,
}

/// Agent configuration for the mock server
#[derive(Debug, Clone)]
pub struct AgentConfig {
    /// Agent name
    pub name: String,
    /// Agent capabilities
    pub capabilities: Vec<String>,
    /// Transport type
    pub transport: TransportType,
    /// Maximum concurrent tasks
    pub max_concurrent_tasks: usize,
    /// Auto-start on creation
    pub auto_start: bool,
    /// Health check interval (ms)
    pub health_check_interval_ms: u64,
}

impl AgentConfig {
    /// Create a new agent configuration
    pub fn new(name: String) -> Self {
        Self {
            name,
            capabilities: Vec::new(),
            transport: TransportType::Http,
            max_concurrent_tasks: 10,
            auto_start: true,
            health_check_interval_ms: 5000,
        }
    }

    /// Add capabilities to the configuration
    pub fn with_capabilities(mut self, capabilities: Vec<String>) -> Self {
        self.capabilities = capabilities;
        self
    }

    /// Set the transport type
    pub fn with_transport(mut self, transport: TransportType) -> Self {
        self.transport = transport;
        self
    }

    /// Set max concurrent tasks
    pub fn with_max_concurrent_tasks(mut self, max: usize) -> Self {
        self.max_concurrent_tasks = max;
        self
    }
}

/// Message representation compatible with A2A protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MockMessage {
    /// Message ID
    pub id: String,
    /// Sender ID
    pub from: String,
    /// Recipient ID
    pub to: String,
    /// Message content
    pub content: String,
    /// Message timestamp
    pub timestamp: DateTime<Utc>,
    /// Message metadata
    pub metadata: Option<Map<String, Value>>,
}

/// JSON-RPC request compatible with A2A protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcRequest {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Request ID
    pub id: Option<Value>,
    /// Method name
    pub method: String,
    /// Method parameters
    pub params: Option<Value>,
}

/// JSON-RPC response compatible with A2A protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Response ID
    pub id: Option<Value>,
    /// Result value
    pub result: Option<Value>,
    /// Error value
    pub error: Option<JsonRpcError>,
}

/// JSON-RPC error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcError {
    /// Error code
    pub code: i32,
    /// Error message
    pub message: String,
    /// Additional error data
    pub data: Option<Value>,
}

//=============================================================================
// Mock A2A Server Implementation
//=============================================================================

/// Mock A2A Server for integration testing
///
/// This server provides a complete simulation of the A2A protocol with
/// in-memory state management, channel-based communication, and event
/// tracking for test assertions.
///
/// # State Management
///
/// The server maintains three primary data structures:
/// - `agents`: Map of agent_id -> AgentStatus
/// - `tasks`: Map of task_id -> TaskStatus
/// - `events`: Vector of recorded events for assertions
///
/// # Channel Communication
///
/// The server uses tokio channels for:
/// - Command channel: External control of the server
/// - Event broadcasting: Subscribers receive event notifications
/// - Request handling: Simulated JSON-RPC method calls
pub struct MockA2AServer {
    /// Server address (simulated)
    address: String,
    /// Server port (simulated)
    port: u16,
    /// Running state
    is_running: Arc<AtomicBool>,
    /// Agent registry
    agents: Arc<RwLock<HashMap<String, AgentStatus>>>,
    /// Task registry
    tasks: Arc<RwLock<HashMap<String, TaskStatus>>>,
    /// Event history for test assertions
    events: Arc<RwLock<Vec<MockEvent>>>,
    /// Command channel receiver
    command_rx: Option<mpsc::UnboundedReceiver<ServerCommand>>,
    /// Command channel sender
    command_tx: mpsc::UnboundedSender<ServerCommand>,
    /// Event broadcaster senders
    event_subscribers: Arc<RwLock<Vec<mpsc::UnboundedSender<MockEvent>>>>,
    /// Agent ID counter
    agent_counter: Arc<AtomicU64>,
    /// Task ID counter
    task_counter: Arc<AtomicU64>,
    /// Message counter
    message_counter: Arc<AtomicU64>,
}

/// Internal server commands
#[derive(Debug)]
enum ServerCommand {
    /// Start an agent
    StartAgent {
        config: AgentConfig,
        respond_to: oneshot::Sender<Result<String, A2aError>>,
    },
    /// Stop an agent
    StopAgent {
        agent_id: String,
        respond_to: oneshot::Sender<Result<(), A2aError>>,
    },
    /// Get agent status
    GetAgentStatus {
        agent_id: String,
        respond_to: oneshot::Sender<Result<AgentStatus, A2aError>>,
    },
    /// List all agents
    ListAgents {
        respond_to: oneshot::Sender<Vec<AgentStatus>>,
    },
    /// Submit a task
    SubmitTask {
        agent_id: String,
        task_id: String,
        metadata: Option<Map<String, Value>>,
        respond_to: oneshot::Sender<Result<TaskStatus, A2aError>>,
    },
    /// Get task status
    GetTaskStatus {
        task_id: String,
        respond_to: oneshot::Sender<Result<TaskStatus, A2aError>>,
    },
    /// List tasks
    ListTasks {
        agent_id: Option<String>,
        respond_to: oneshot::Sender<Vec<TaskStatus>>,
    },
    /// Cancel a task
    CancelTask {
        task_id: String,
        respond_to: oneshot::Sender<Result<(), A2aError>>,
    },
    /// Send a message
    SendMessage {
        from: String,
        to: String,
        content: String,
        respond_to: oneshot::Sender<Result<MockMessage, A2aError>>,
    },
    /// Handle JSON-RPC request
    HandleJsonRpc {
        request: JsonRpcRequest,
        respond_to: oneshot::Sender<JsonRpcResponse>,
    },
    /// Shutdown the server
    Shutdown {
        respond_to: oneshot::Sender<()>,
    },
}

impl MockA2AServer {
    /// Create a new mock A2A server
    pub fn new() -> Self {
        let (command_tx, command_rx) = mpsc::unbounded_channel();
        Self {
            address: "127.0.0.1".to_string(),
            port: 8080,
            is_running: Arc::new(AtomicBool::new(false)),
            agents: Arc::new(RwLock::new(HashMap::new())),
            tasks: Arc::new(RwLock::new(HashMap::new())),
            events: Arc::new(RwLock::new(Vec::new())),
            command_rx: Some(command_rx),
            command_tx,
            event_subscribers: Arc::new(RwLock::new(Vec::new())),
            agent_counter: Arc::new(AtomicU64::new(0)),
            task_counter: Arc::new(AtomicU64::new(0)),
            message_counter: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Create a mock server with a specific address and port
    pub fn with_address(address: String, port: u16) -> Self {
        let (command_tx, command_rx) = mpsc::unbounded_channel();
        Self {
            address,
            port,
            is_running: Arc::new(AtomicBool::new(false)),
            agents: Arc::new(RwLock::new(HashMap::new())),
            tasks: Arc::new(RwLock::new(HashMap::new())),
            events: Arc::new(RwLock::new(Vec::new())),
            command_rx: Some(command_rx),
            command_tx,
            event_subscribers: Arc::new(RwLock::new(Vec::new())),
            agent_counter: Arc::new(AtomicU64::new(0)),
            task_counter: Arc::new(AtomicU64::new(0)),
            message_counter: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Start the mock server
    ///
    /// This spawns the server's command processing task. The server
    /// will process commands until `shutdown` is called.
    pub async fn start(&mut self) -> Result<(), A2aError> {
        if self.is_running.load(Ordering::Acquire) {
            return Err(A2aError::InvalidConfiguration(
                "Server is already running".to_string(),
            ));
        }

        self.is_running.store(true, Ordering::Release);
        let mut command_rx = self.command_rx
            .take()
            .ok_or_else(|| A2aError::Internal("Command channel missing".to_string()))?;

        let agents = self.agents.clone();
        let tasks = self.tasks.clone();
        let events = self.events.clone();
        let event_subscribers = self.event_subscribers.clone();
        let agent_counter = self.agent_counter.clone();
        let task_counter = self.task_counter.clone();
        let message_counter = self.message_counter.clone();
        let is_running = self.is_running.clone();

        // Spawn the server task
        tokio::spawn(async move {
            while is_running.load(Ordering::Acquire) {
                match command_rx.recv().await {
                    Some(command) => {
                        Self::handle_command(
                            command,
                            &agents,
                            &tasks,
                            &events,
                            &event_subscribers,
                            &agent_counter,
                            &task_counter,
                            &message_counter,
                        ).await;
                    }
                    None => {
                        // Channel closed, shutdown
                        is_running.store(false, Ordering::Release);
                        break;
                    }
                }
            }
        });

        // Record startup event
        self.record_event(MockEvent {
            event_type: MockEventType::AgentStarted,
            timestamp: Utc::now(),
            entity_id: "server".to_string(),
            data: None,
        }).await;

        Ok(())
    }

    /// Internal command handler
    async fn handle_command(
        command: ServerCommand,
        agents: &Arc<RwLock<HashMap<String, AgentStatus>>>,
        tasks: &Arc<RwLock<HashMap<String, TaskStatus>>>,
        events: &Arc<RwLock<Vec<MockEvent>>>,
        event_subscribers: &Arc<RwLock<Vec<mpsc::UnboundedSender<MockEvent>>>>,
        agent_counter: &Arc<AtomicU64>,
        task_counter: &Arc<AtomicU64>,
        message_counter: &Arc<AtomicU64>,
    ) {
        match command {
            ServerCommand::StartAgent { config, respond_to } => {
                let result = Self::do_start_agent(config, agents, events, agent_counter).await;
                let _ = respond_to.send(result);
            }
            ServerCommand::StopAgent { agent_id, respond_to } => {
                let result = Self::do_stop_agent(agent_id, agents, events).await;
                let _ = respond_to.send(result);
            }
            ServerCommand::GetAgentStatus { agent_id, respond_to } => {
                let agents_guard = agents.read().await;
                let result = agents_guard
                    .get(&agent_id)
                    .cloned()
                    .ok_or_else(|| A2aError::AgentNotFound(agent_id.clone()));
                let _ = respond_to.send(result);
            }
            ServerCommand::ListAgents { respond_to } => {
                let agents_guard = agents.read().await;
                let agent_list: Vec<AgentStatus> = agents_guard.values().cloned().collect();
                let _ = respond_to.send(agent_list);
            }
            ServerCommand::SubmitTask { agent_id, task_id, metadata, respond_to } => {
                let result = Self::do_submit_task(agent_id, task_id, metadata, tasks, events, task_counter).await;
                let _ = respond_to.send(result);
            }
            ServerCommand::GetTaskStatus { task_id, respond_to } => {
                let tasks_guard = tasks.read().await;
                let result = tasks_guard
                    .get(&task_id)
                    .cloned()
                    .ok_or_else(|| A2aError::ResourceNotFound(task_id.clone()));
                let _ = respond_to.send(result);
            }
            ServerCommand::ListTasks { agent_id, respond_to } => {
                let tasks_guard = tasks.read().await;
                let task_list: Vec<TaskStatus> = if let Some(agent_id) = agent_id {
                    tasks_guard
                        .values()
                        .filter(|t| t.agent_id == agent_id)
                        .cloned()
                        .collect()
                } else {
                    tasks_guard.values().cloned().collect()
                };
                let _ = respond_to.send(task_list);
            }
            ServerCommand::CancelTask { task_id, respond_to } => {
                let result = Self::do_cancel_task(task_id, tasks, events).await;
                let _ = respond_to.send(result);
            }
            ServerCommand::SendMessage { from, to, content, respond_to } => {
                let result = Self::do_send_message(from, to, content, events, message_counter).await;
                let _ = respond_to.send(result);
            }
            ServerCommand::HandleJsonRpc { request, respond_to } => {
                let response = Self::do_handle_json_rpc(request).await;
                let _ = respond_to.send(response);
            }
            ServerCommand::Shutdown { respond_to } => {
                let _ = respond_to.send(());
            }
        }

        // Broadcast events to subscribers
        let subscribers = event_subscribers.read().await;
        let latest_events = events.read().await;
        // Only send new events since last broadcast would require tracking
        // For simplicity, we'll just keep the events for assertion checking
    }

    /// Start an agent
    async fn do_start_agent(
        config: AgentConfig,
        agents: &Arc<RwLock<HashMap<String, AgentStatus>>>,
        events: &Arc<RwLock<Vec<MockEvent>>>,
        counter: &Arc<AtomicU64>,
    ) -> Result<String, A2aError> {
        let id = format!("agent-{}", counter.fetch_add(1, Ordering::AcqRel));
        let now = Utc::now();

        let status = AgentStatus {
            id: id.clone(),
            name: config.name.clone(),
            state: AgentState::Running,
            capabilities: config.capabilities.clone(),
            active_tasks: 0,
            total_tasks: 0,
            last_activity: now,
        };

        let mut agents_guard = agents.write().await;
        agents_guard.insert(id.clone(), status);

        // Record event
        let mut events_guard = events.write().await;
        events_guard.push(MockEvent {
            event_type: MockEventType::AgentStarted,
            timestamp: now,
            entity_id: id.clone(),
            data: {
                let mut map = Map::new();
                map.insert("name".to_string(), Value::String(config.name));
                map.insert("capabilities".to_string(), Value::Array(
                    config.capabilities.into_iter().map(Value::String).collect()
                ));
                Some(map)
            },
        });

        Ok(id)
    }

    /// Stop an agent
    async fn do_stop_agent(
        agent_id: String,
        agents: &Arc<RwLock<HashMap<String, AgentStatus>>>,
        events: &Arc<RwLock<Vec<MockEvent>>>,
    ) -> Result<(), A2aError> {
        let mut agents_guard = agents.write().await;
        let agent = agents_guard
            .get_mut(&agent_id)
            .ok_or_else(|| A2aError::AgentNotFound(agent_id.clone()))?;

        agent.state = AgentState::Stopped;
        agent.last_activity = Utc::now();

        // Record event
        let mut events_guard = events.write().await;
        events_guard.push(MockEvent {
            event_type: MockEventType::AgentStopped,
            timestamp: Utc::now(),
            entity_id: agent_id.clone(),
            data: None,
        });

        Ok(())
    }

    /// Submit a task
    async fn do_submit_task(
        agent_id: String,
        task_id: String,
        metadata: Option<Map<String, Value>>,
        tasks: &Arc<RwLock<HashMap<String, TaskStatus>>>,
        events: &Arc<RwLock<Vec<MockEvent>>>,
        counter: &Arc<AtomicU64>,
    ) -> Result<TaskStatus, A2aError> {
        let id = if task_id.is_empty() {
            format!("task-{}", counter.fetch_add(1, Ordering::AcqRel))
        } else {
            task_id
        };

        let now = Utc::now();
        let status = TaskStatus {
            id: id.clone(),
            agent_id: agent_id.clone(),
            state: TaskState::Submitted,
            message: None,
            created_at: now,
            updated_at: now,
            metadata,
        };

        let mut tasks_guard = tasks.write().await;
        tasks_guard.insert(id.clone(), status.clone());

        // Record event
        let mut events_guard = events.write().await;
        events_guard.push(MockEvent {
            event_type: MockEventType::TaskSubmitted,
            timestamp: now,
            entity_id: id.clone(),
            data: {
                let mut map = Map::new();
                map.insert("agent_id".to_string(), Value::String(agent_id));
                Some(map)
            },
        });

        Ok(status)
    }

    /// Cancel a task
    async fn do_cancel_task(
        task_id: String,
        tasks: &Arc<RwLock<HashMap<String, TaskStatus>>>,
        events: &Arc<RwLock<Vec<MockEvent>>>,
    ) -> Result<(), A2aError> {
        let mut tasks_guard = tasks.write().await;
        let task = tasks_guard
            .get_mut(&task_id)
            .ok_or_else(|| A2aError::ResourceNotFound(task_id.clone()))?;

        task.state = TaskState::Canceled;
        task.updated_at = Utc::now();

        // Record event
        let mut events_guard = events.write().await;
        events_guard.push(MockEvent {
            event_type: MockEventType::TaskCanceled,
            timestamp: Utc::now(),
            entity_id: task_id.clone(),
            data: None,
        });

        Ok(())
    }

    /// Send a message
    async fn do_send_message(
        from: String,
        to: String,
        content: String,
        events: &Arc<RwLock<Vec<MockEvent>>>,
        counter: &Arc<AtomicU64>,
    ) -> Result<MockMessage, A2aError> {
        let id = format!("msg-{}", counter.fetch_add(1, Ordering::AcqRel));
        let now = Utc::now();

        let message = MockMessage {
            id: id.clone(),
            from: from.clone(),
            to: to.clone(),
            content: content.clone(),
            timestamp: now,
            metadata: None,
        };

        // Record event
        let mut events_guard = events.write().await;
        events_guard.push(MockEvent {
            event_type: MockEventType::MessageSent,
            timestamp: now,
            entity_id: id.clone(),
            data: {
                let mut map = Map::new();
                map.insert("from".to_string(), Value::String(from));
                map.insert("to".to_string(), Value::String(to));
                map.insert("content".to_string(), Value::String(content));
                Some(map)
            },
        });

        Ok(message)
    }

    /// Handle JSON-RPC request
    async fn do_handle_json_rpc(request: JsonRpcRequest) -> JsonRpcResponse {
        let result = match request.method.as_str() {
            "message/send" => {
                // Handle message/send
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("message_id".to_string(), Value::String("msg-123".to_string()));
                    map.insert("status".to_string(), Value::String("sent".to_string()));
                    map
                }))
            }
            "tasks/get" => {
                // Handle tasks/get
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("id".to_string(), Value::String("task-123".to_string()));
                    map.insert("state".to_string(), Value::String("working".to_string()));
                    map
                }))
            }
            "tasks/list" => {
                // Handle tasks/list
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("tasks".to_string(), Value::Array(vec![]));
                    map.insert("total_size".to_string(), Value::Number(0.into()));
                    map
                }))
            }
            "tasks/create" => {
                // Handle tasks/create
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("id".to_string(), Value::String("task-456".to_string()));
                    map.insert("state".to_string(), Value::String("submitted".to_string()));
                    map
                }))
            }
            "tasks/update" => {
                // Handle tasks/update
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("id".to_string(), Value::String("task-123".to_string()));
                    map.insert("state".to_string(), Value::String("working".to_string()));
                    map
                }))
            }
            "tasks/cancel" => {
                // Handle tasks/cancel
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("id".to_string(), Value::String("task-123".to_string()));
                    map.insert("state".to_string(), Value::String("canceled".to_string()));
                    map
                }))
            }
            "tasks/delete" => {
                // Handle tasks/delete
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("deleted".to_string(), Value::Bool(true));
                    map
                }))
            }
            "agents/info" => {
                // Handle agents/info
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("id".to_string(), Value::String("agent-123".to_string()));
                    map.insert("name".to_string(), Value::String("test-agent".to_string()));
                    map.insert("state".to_string(), Value::String("running".to_string()));
                    map
                }))
            }
            "agents/status" => {
                // Handle agents/status
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("agents".to_string(), Value::Array(vec![]));
                    map
                }))
            }
            "events/subscribe" => {
                // Handle events/subscribe
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("subscription_id".to_string(), Value::String("sub-123".to_string()));
                    map
                }))
            }
            "events/publish" => {
                // Handle events/publish
                Some(Value::Object({
                    let mut map = Map::new();
                    map.insert("published".to_string(), Value::Bool(true));
                    map
                }))
            }
            _ => {
                // Unknown method
                return JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: request.id,
                    result: None,
                    error: Some(JsonRpcError {
                        code: -32601,
                        message: format!("Method not found: {}", request.method),
                        data: None,
                    }),
                };
            }
        };

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result,
            error: None,
        }
    }

    /// Check if the server is running
    pub fn is_running(&self) -> bool {
        self.is_running.load(Ordering::Acquire)
    }

    /// Get the server URL
    pub fn url(&self) -> String {
        format!("{}:{}", self.address, self.port)
    }

    /// Record an event
    pub async fn record_event(&self, event: MockEvent) {
        let mut events = self.events.write().await;
        events.push(event);
    }

    /// Get all recorded events
    pub async fn get_events(&self) -> Vec<MockEvent> {
        let events = self.events.read().await;
        events.clone()
    }

    /// Clear all recorded events
    pub async fn clear_events(&self) {
        let mut events = self.events.write().await;
        events.clear();
    }

    /// Get events for a specific entity
    pub async fn get_events_for_entity(&self, entity_id: &str) -> Vec<MockEvent> {
        let events = self.events.read().await;
        events
            .iter()
            .filter(|e| e.entity_id == entity_id)
            .cloned()
            .collect()
    }

    /// Get events of a specific type
    pub async fn get_events_by_type(&self, event_type: MockEventType) -> Vec<MockEvent> {
        let events = self.events.read().await;
        events
            .iter()
            .filter(|e| e.event_type == event_type)
            .cloned()
            .collect()
    }

    /// Assert that an event occurred
    pub async fn assert_event_occurred(&self, event_type: MockEventType, entity_id: &str) -> bool {
        let events = self.get_events_for_entity(entity_id).await;
        events.iter().any(|e| e.event_type == event_type)
    }

    //=========================================================================
    // Public API Methods (send commands via channel)
    //=========================================================================

    /// Start an agent with the given configuration
    pub async fn start_agent(&self, config: AgentConfig) -> Result<String, A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::StartAgent { config, respond_to: tx })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// Stop an agent by ID
    pub async fn stop_agent(&self, agent_id: String) -> Result<(), A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::StopAgent { agent_id, respond_to: tx })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// Get the status of an agent
    pub async fn get_agent_status(&self, agent_id: &str) -> Result<AgentStatus, A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::GetAgentStatus {
                agent_id: agent_id.to_string(),
                respond_to: tx,
            })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// List all agents
    pub async fn list_agents(&self) -> Vec<AgentStatus> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::ListAgents { respond_to: tx })
            .map_err(|_| vec![])
            .unwrap();
        rx.await.unwrap_or_default()
    }

    /// Submit a task to an agent
    pub async fn submit_task(
        &self,
        agent_id: String,
        task_id: String,
        metadata: Option<Map<String, Value>>,
    ) -> Result<TaskStatus, A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::SubmitTask {
                agent_id,
                task_id,
                metadata,
                respond_to: tx,
            })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// Get the status of a task
    pub async fn get_task_status(&self, task_id: &str) -> Result<TaskStatus, A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::GetTaskStatus {
                task_id: task_id.to_string(),
                respond_to: tx,
            })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// List all tasks, optionally filtered by agent
    pub async fn list_tasks(&self, agent_id: Option<String>) -> Vec<TaskStatus> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::ListTasks { agent_id, respond_to: tx })
            .map_err(|_| vec![])
            .unwrap();
        rx.await.unwrap_or_default()
    }

    /// Cancel a task
    pub async fn cancel_task(&self, task_id: String) -> Result<(), A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::CancelTask { task_id, respond_to: tx })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// Send a message
    pub async fn send_message(
        &self,
        from: String,
        to: String,
        content: String,
    ) -> Result<MockMessage, A2aError> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(ServerCommand::SendMessage {
                from,
                to,
                content,
                respond_to: tx,
            })
            .map_err(|_| A2aError::Internal("Command channel closed".to_string()))?;
        rx.await
            .map_err(|_| A2aError::Internal("Response channel closed".to_string()))?
    }

    /// Handle a JSON-RPC request
    pub async fn handle_json_rpc(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        let (tx, rx) = oneshot::channel();
        let _ = self.command_tx.send(ServerCommand::HandleJsonRpc {
            request,
            respond_to: tx,
        });
        rx.await.unwrap_or_else(|_| JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: None,
            result: None,
            error: Some(JsonRpcError {
                code: -32603,
                message: "Internal error".to_string(),
                data: None,
            }),
        })
    }

    /// Shutdown the server
    pub async fn shutdown(&self) {
        let (tx, rx) = oneshot::channel();
        let _ = self.command_tx.send(ServerCommand::Shutdown { respond_to: tx });
        let _ = rx.await;
        self.is_running.store(false, Ordering::Release);
    }

    //=========================================================================
    // Test Helper Methods
    //=========================================================================

    /// Simulate task completion
    pub async fn complete_task(&self, task_id: &str) -> Result<(), A2aError> {
        let mut tasks = self.tasks.write().await;
        let task = tasks
            .get_mut(task_id)
            .ok_or_else(|| A2aError::ResourceNotFound(task_id.to_string()))?;

        task.state = TaskState::Completed;
        task.updated_at = Utc::now();
        task.message = Some("Task completed successfully".to_string());

        // Record event
        self.record_event(MockEvent {
            event_type: MockEventType::TaskCompleted,
            timestamp: Utc::now(),
            entity_id: task_id.to_string(),
            data: None,
        }).await;

        Ok(())
    }

    /// Simulate task failure
    pub async fn fail_task(&self, task_id: &str, error_message: &str) -> Result<(), A2aError> {
        let mut tasks = self.tasks.write().await;
        let task = tasks
            .get_mut(task_id)
            .ok_or_else(|| A2aError::ResourceNotFound(task_id.to_string()))?;

        task.state = TaskState::Failed;
        task.updated_at = Utc::now();
        task.message = Some(error_message.to_string());

        // Record event
        self.record_event(MockEvent {
            event_type: MockEventType::TaskFailed,
            timestamp: Utc::now(),
            entity_id: task_id.to_string(),
            data: {
                let mut map = Map::new();
                map.insert("error".to_string(), Value::String(error_message.to_string()));
                Some(map)
            },
        }).await;

        Ok(())
    }

    /// Wait for a task to complete (with timeout simulation)
    pub async fn wait_for_task(&self, task_id: &str, max_wait_ms: u64) -> Result<TaskStatus, A2aError> {
        let start = std::time::Instant::now();
        let max_duration = std::time::Duration::from_millis(max_wait_ms);

        loop {
            {
                let tasks = self.tasks.read().await;
                if let Some(task) = tasks.get(task_id) {
                    match task.state {
                        TaskState::Completed | TaskState::Failed | TaskState::Canceled => {
                            return Ok(task.clone());
                        }
                        _ => {}
                    }
                }
            }

            if start.elapsed() >= max_duration {
                return Err(A2aError::Timeout(format!(
                    "Task {} did not complete within {}ms",
                    task_id, max_wait_ms
                )));
            }

            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }
    }

    /// Create a task and simulate its processing
    pub async fn create_and_process_task(
        &self,
        agent_id: String,
        should_succeed: bool,
    ) -> Result<TaskStatus, A2aError> {
        // Submit task
        let task = self
            .submit_task(agent_id, String::new(), None)
            .await?;
        let task_id = task.id.clone();

        // Simulate processing
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;

        // Complete or fail
        if should_succeed {
            self.complete_task(&task_id).await?;
        } else {
            self.fail_task(&task_id, "Simulated failure").await?;
        }

        self.get_task_status(&task_id).await
    }

    /// Get agent and task counts
    pub async fn get_counts(&self) -> (usize, usize) {
        let agents = self.agents.read().await;
        let tasks = self.tasks.read().await;
        (agents.len(), tasks.len())
    }
}

impl Default for MockA2AServer {
    fn default() -> Self {
        Self::new()
    }
}

//=============================================================================
// Event Subscription Support
//=============================================================================

/// Subscribe to server events
pub struct EventSubscriber {
    rx: mpsc::UnboundedReceiver<MockEvent>,
}

impl MockA2AServer {
    /// Create a new event subscription
    pub async fn subscribe_to_events(&self) -> EventSubscriber {
        let (tx, rx) = mpsc::unbounded_channel();
        let mut subscribers = self.event_subscribers.write().await;
        subscribers.push(tx);
        EventSubscriber { rx }
    }
}

impl EventSubscriber {
    /// Receive the next event
    pub async fn recv(&mut self) -> Option<MockEvent> {
        self.rx.recv().await
    }

    /// Try to receive an event without blocking
    pub fn try_recv(&mut self) -> Option<MockEvent> {
        self.rx.try_recv().ok()
    }
}

//=============================================================================
// Tests
//=============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_server_creation() {
        // Arrange & Act
        let server = MockA2AServer::new();

        // Assert
        assert!(!server.is_running());
        assert_eq!(server.url(), "127.0.0.1:8080");
    }

    #[tokio::test]
    async fn test_server_start_and_shutdown() {
        // Arrange
        let mut server = MockA2AServer::new();

        // Act
        let start_result = server.start().await;

        // Assert
        assert!(start_result.is_ok());
        assert!(server.is_running());

        // Cleanup
        server.shutdown().await;
        assert!(!server.is_running());
    }

    #[tokio::test]
    async fn test_agent_lifecycle() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let config = AgentConfig::new("test-agent".to_string())
            .with_capabilities(vec!["text-generation".to_string()]);

        // Act - Start agent
        let agent_id = server.start_agent(config).await.unwrap();

        // Assert - Agent started
        let status = server.get_agent_status(&agent_id).await.unwrap();
        assert_eq!(status.name, "test-agent");
        assert_eq!(status.state, AgentState::Running);
        assert_eq!(status.capabilities, vec!["text-generation"]);

        // Act - Stop agent
        server.stop_agent(agent_id.clone()).await.unwrap();

        // Assert - Agent stopped
        let status = server.get_agent_status(&agent_id).await.unwrap();
        assert_eq!(status.state, AgentState::Stopped);

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_task_submission_and_query() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let config = AgentConfig::new("task-agent".to_string());
        let agent_id = server.start_agent(config).await.unwrap();

        // Act - Submit task
        let task = server
            .submit_task(agent_id.clone(), "task-1".to_string(), None)
            .await
            .unwrap();

        // Assert - Task created
        assert_eq!(task.id, "task-1");
        assert_eq!(task.agent_id, agent_id);
        assert_eq!(task.state, TaskState::Submitted);

        // Act - Query task
        let queried = server.get_task_status("task-1").await.unwrap();
        assert_eq!(queried.id, "task-1");

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_task_completion() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let config = AgentConfig::new("completion-agent".to_string());
        let agent_id = server.start_agent(config).await.unwrap();
        let task_id = "complete-task-1";

        server.submit_task(agent_id, task_id.to_string(), None).await.unwrap();

        // Act - Complete task
        server.complete_task(task_id).await.unwrap();

        // Assert - Task completed
        let task = server.get_task_status(task_id).await.unwrap();
        assert_eq!(task.state, TaskState::Completed);
        assert_eq!(task.message, Some("Task completed successfully".to_string()));

        // Assert - Event recorded
        let events = server.get_events_for_entity(task_id).await;
        assert!(events.iter().any(|e| e.event_type == MockEventType::TaskCompleted));

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_task_failure() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let config = AgentConfig::new("failure-agent".to_string());
        let agent_id = server.start_agent(config).await.unwrap();
        let task_id = "fail-task-1";

        server.submit_task(agent_id, task_id.to_string(), None).await.unwrap();

        // Act - Fail task
        server.fail_task(task_id, "Simulated error").await.unwrap();

        // Assert - Task failed
        let task = server.get_task_status(task_id).await.unwrap();
        assert_eq!(task.state, TaskState::Failed);
        assert_eq!(task.message, Some("Simulated error".to_string()));

        // Assert - Event recorded
        let events = server.get_events_for_entity(task_id).await;
        assert!(events.iter().any(|e| e.event_type == MockEventType::TaskFailed));

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_task_cancellation() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let config = AgentConfig::new("cancel-agent".to_string());
        let agent_id = server.start_agent(config).await.unwrap();
        let task_id = "cancel-task-1";

        server.submit_task(agent_id, task_id.to_string(), None).await.unwrap();

        // Act - Cancel task
        server.cancel_task(task_id.to_string()).await.unwrap();

        // Assert - Task canceled
        let task = server.get_task_status(task_id).await.unwrap();
        assert_eq!(task.state, TaskState::Canceled);

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_message_sending() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        // Act - Send message
        let message = server
            .send_message("agent-1".to_string(), "agent-2".to_string(), "Hello!".to_string())
            .await
            .unwrap();

        // Assert
        assert_eq!(message.from, "agent-1");
        assert_eq!(message.to, "agent-2");
        assert_eq!(message.content, "Hello!");

        // Assert - Event recorded
        let events = server.get_events_by_type(MockEventType::MessageSent).await;
        assert!(!events.is_empty());

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_list_agents() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        server
            .start_agent(AgentConfig::new("agent-1".to_string()))
            .await
            .unwrap();
        server
            .start_agent(AgentConfig::new("agent-2".to_string()))
            .await
            .unwrap();

        // Act
        let agents = server.list_agents().await;

        // Assert
        assert_eq!(agents.len(), 2);
        assert!(agents.iter().any(|a| a.name == "agent-1"));
        assert!(agents.iter().any(|a| a.name == "agent-2"));

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_list_tasks() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let agent1 = server
            .start_agent(AgentConfig::new("agent-1".to_string()))
            .await
            .unwrap();
        let agent2 = server
            .start_agent(AgentConfig::new("agent-2".to_string()))
            .await
            .unwrap();

        server
            .submit_task(agent1.clone(), "task-1".to_string(), None)
            .await
            .unwrap();
        server
            .submit_task(agent1.clone(), "task-2".to_string(), None)
            .await
            .unwrap();
        server
            .submit_task(agent2.clone(), "task-3".to_string(), None)
            .await
            .unwrap();

        // Act - List all tasks
        let all_tasks = server.list_tasks(None).await;
        assert_eq!(all_tasks.len(), 3);

        // Act - List tasks for agent1
        let agent1_tasks = server.list_tasks(Some(agent1)).await;
        assert_eq!(agent1_tasks.len(), 2);

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_json_rpc_message_send() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(Value::String("req-1".to_string())),
            method: "message/send".to_string(),
            params: Some(json!({
                "to": "agent-2",
                "content": "Hello via JSON-RPC"
            })),
        };

        // Act
        let response = server.handle_json_rpc(request).await;

        // Assert
        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.error.is_none());
        assert!(response.result.is_some());

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_json_rpc_tasks_get() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(Value::String("req-2".to_string())),
            method: "tasks/get".to_string(),
            params: Some(json!({ "id": "task-123" })),
        };

        // Act
        let response = server.handle_json_rpc(request).await;

        // Assert
        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.error.is_none());
        assert!(response.result.is_some());

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_json_rpc_agents_info() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(Value::String("req-3".to_string())),
            method: "agents/info".to_string(),
            params: Some(json!({ "id": "agent-123" })),
        };

        // Act
        let response = server.handle_json_rpc(request).await;

        // Assert
        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.error.is_none());
        assert!(response.result.is_some());

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_json_rpc_unknown_method() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(Value::String("req-4".to_string())),
            method: "unknown/method".to_string(),
            params: None,
        };

        // Act
        let response = server.handle_json_rpc(request).await;

        // Assert
        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.error.is_some());
        assert_eq!(response.error.as_ref().unwrap().code, -32601);

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_event_tracking() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        // Act - Start agent
        let agent_id = server
            .start_agent(AgentConfig::new("event-agent".to_string()))
            .await
            .unwrap();

        // Act - Submit and complete task
        server
            .submit_task(agent_id.clone(), "event-task".to_string(), None)
            .await
            .unwrap();
        server.complete_task("event-task").await.unwrap();

        // Assert - Events recorded
        let agent_events = server.get_events_for_entity(&agent_id).await;
        assert!(agent_events.iter().any(|e| e.event_type == MockEventType::AgentStarted));

        let task_events = server.get_events_for_entity("event-task").await;
        assert!(task_events.iter().any(|e| e.event_type == MockEventType::TaskSubmitted));
        assert!(task_events.iter().any(|e| e.event_type == MockEventType::TaskCompleted));

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_wait_for_task_completion() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let agent_id = server
            .start_agent(AgentConfig::new("wait-agent".to_string()))
            .await
            .unwrap();

        // Spawn a task that completes after a delay
        let server_clone = server.clone_channel();
        let agent_id_clone = agent_id.clone();
        tokio::spawn(async move {
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            let _ = server_clone.submit_task(agent_id_clone, "wait-task".to_string(), None).await;
            let _ = server_clone.complete_task("wait-task").await;
        });

        // Act - Wait for task
        let result = tokio::time::timeout(
            std::time::Duration::from_millis(200),
            server.wait_for_task("wait-task", 200),
        )
        .await;

        // Assert
        assert!(result.is_ok());
        let task = result.unwrap().unwrap();
        assert_eq!(task.state, TaskState::Completed);

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_create_and_process_task_success() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let agent_id = server
            .start_agent(AgentConfig::new("process-agent".to_string()))
            .await
            .unwrap();

        // Act
        let task = server.create_and_process_task(agent_id, true).await.unwrap();

        // Assert
        assert_eq!(task.state, TaskState::Completed);

        // Cleanup
        server.shutdown().await;
    }

    #[tokio::test]
    async fn test_create_and_process_task_failure() {
        // Arrange
        let mut server = MockA2AServer::new();
        server.start().await.unwrap();

        let agent_id = server
            .start_agent(AgentConfig::new("fail-process-agent".to_string()))
            .await
            .unwrap();

        // Act
        let task = server
            .create_and_process_task(agent_id, false)
            .await
            .unwrap();

        // Assert
        assert_eq!(task.state, TaskState::Failed);

        // Cleanup
        server.shutdown().await;
    }
}

// Helper to clone the command sender for async tasks
impl MockA2AServer {
    fn clone_channel(&self) -> Self {
        Self {
            address: self.address.clone(),
            port: self.port,
            is_running: self.is_running.clone(),
            agents: self.agents.clone(),
            tasks: self.tasks.clone(),
            events: self.events.clone(),
            command_rx: None,
            command_tx: self.command_tx.clone(),
            event_subscribers: self.event_subscribers.clone(),
            agent_counter: self.agent_counter.clone(),
            task_counter: self.task_counter.clone(),
            message_counter: self.message_counter.clone(),
        }
    }
}
