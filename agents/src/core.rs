//! # Core Agent Framework
//!
//! Defines the fundamental interfaces and types for the ultrathink multi-agent system.
//! All agents implement these core traits to participate in the collaborative workflow.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::{mpsc, RwLock};
use uuid::Uuid;

use crate::protocols::Message;
use crate::coordination::Task;

/// Unique identifier for agents (serialized as string)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AgentId(pub Uuid);

/// Unique identifier for execution plans (serialized as string)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlanId(pub Uuid);

impl AgentId {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    pub fn as_str(&self) -> &str {
        // Uuid doesn't implement Display, so we'll need to format it
        // For now, return a placeholder
        "agent-id"
    }
}

impl From<Uuid> for AgentId {
    fn from(uuid: Uuid) -> Self {
        Self(uuid)
    }
}


impl std::fmt::Display for AgentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl serde::Serialize for AgentId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.0.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for AgentId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let uuid = Uuid::parse_str(&s).map_err(serde::de::Error::custom)?;
        Ok(AgentId(uuid))
    }
}

impl PlanId {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    pub fn as_str(&self) -> &str {
        "plan-id"
    }
}

impl From<Uuid> for PlanId {
    fn from(uuid: Uuid) -> Self {
        Self(uuid)
    }
}

impl std::fmt::Display for PlanId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl serde::Serialize for PlanId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.0.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for PlanId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let uuid = Uuid::parse_str(&s).map_err(serde::de::Error::custom)?;
        Ok(PlanId(uuid))
    }
}

/// Result type for agent operations
pub type AgentResult<T> = Result<T, AgentError>;

/// Errors that can occur during agent operations
#[derive(Debug, Error)]
pub enum AgentError {
    #[error("Agent initialization failed: {0}")]
    InitializationFailed(String),

    #[error("Agent execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Communication error: {0}")]
    CommunicationError(String),

    #[error("Configuration error: {0}")]
    ConfigurationError(String),

    #[error("Resource error: {0}")]
    ResourceError(String),

    #[error("Validation error: {0}")]
    ValidationError(String),
}

/// Core agent trait that all agents must implement
#[async_trait]
pub trait Agent: Send + Sync {
    /// Get the agent's unique identifier
    fn id(&self) -> AgentId;

    /// Get the agent's name for display purposes
    fn name(&self) -> &'static str;

    /// Get the agent's description of its capabilities
    fn description(&self) -> &'static str;

    /// Initialize the agent with its configuration
    async fn initialize(&mut self, context: &AgentContext) -> AgentResult<()>;

    /// Execute the agent's primary function
    async fn execute(&self, context: &ExecutionContext) -> AgentResult<ExecutionResult>;

    /// Handle cleanup when the agent shuts down
    async fn shutdown(&self) -> AgentResult<()>;

    /// Get the agent's capabilities for coordination
    fn capabilities(&self) -> Vec<AgentCapability>;
}

/// Context provided to agents during initialization
#[derive(Debug, Clone)]
pub struct AgentContext {
    /// Agent's unique identifier
    pub agent_id: AgentId,

    /// Configuration parameters for the agent
    pub config: HashMap<String, String>,

    /// Shared state accessible to all agents
    pub shared_state: Arc<RwLock<HashMap<String, serde_json::Value>>>,

    /// Communication channels for inter-agent messaging
    pub message_tx: mpsc::UnboundedSender<Message>,
}

/// Execution context passed to agents during execution
#[derive(Debug, Clone)]
pub struct ExecutionContext {
    /// The task being executed
    pub task: SerializableTask,

    /// Current execution state
    pub state: ExecutionState,

    /// Input data for the task
    pub input: serde_json::Value,

    /// Shared state for coordination
    pub shared_state: Arc<RwLock<HashMap<String, serde_json::Value>>>,

    /// Communication channel for results
    pub result_tx: mpsc::UnboundedSender<ExecutionResult>,
}

/// Serializable execution context (for JSON serialization)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SerializableExecutionContext {
    /// The task being executed
    pub task: SerializableTask,

    /// Current execution state
    pub state: ExecutionState,

    /// Input data for the task
    pub input: serde_json::Value,

    /// Shared state for coordination (simplified for serialization)
    pub shared_state_keys: Vec<String>,
}

/// Serializable task (for JSON serialization)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SerializableTask {
    /// Unique identifier for this task
    pub task_id: String,

    /// The agent responsible for this task (as string)
    pub agent_id: String,

    /// Task description
    pub description: String,

    /// Input data for the task
    pub input: serde_json::Value,

    /// Expected output schema
    pub output_schema: serde_json::Value,

    /// Task priority (higher numbers = higher priority)
    pub priority: i32,

    /// Maximum execution time in seconds
    pub timeout_seconds: Option<u64>,
}

/// Result of agent execution
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExecutionResult {
    /// Unique identifier for this execution (as string)
    pub execution_id: String,

    /// The agent that produced this result (as AgentId)
    pub agent_id: AgentId,

    /// Success or failure status
    pub status: ExecutionStatus,

    /// Primary output data
    pub output: serde_json::Value,

    /// Metadata about the execution
    pub metadata: HashMap<String, String>,

    /// Execution duration in milliseconds
    pub duration_ms: u64,

    /// Any errors or warnings encountered
    pub messages: Vec<String>,
}

/// Status of execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExecutionStatus {
    Success,
    PartialSuccess,
    Failure,
    Skipped,
}

/// Current state of execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExecutionState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Capabilities that agents can declare
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentCapability {
    /// Capability name
    pub name: String,

    /// Capability description
    pub description: String,

    /// Input schema for this capability
    pub input_schema: serde_json::Value,

    /// Output schema for this capability
    pub output_schema: serde_json::Value,
}

