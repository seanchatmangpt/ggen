// Core types for the unified execution framework
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ============================================================================
// CORE IDENTIFIERS
// ============================================================================

/// Unique identifier for agents
pub type AgentId = String;

/// Unique identifier for tasks
pub type TaskId = String;

/// Unique identifier for messages
pub type MessageId = String;

/// Unique identifier for workflows
pub type WorkflowId = String;

/// Unique identifier for pipelines
pub type PipelineId = String;

// ============================================================================
// AGENT TYPES
// ============================================================================

/// Unified agent interface that works across different agent types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedAgent {
    pub id: AgentId,
    pub name: String,
    pub agent_type: String,
    pub capabilities: Vec<String>,
    pub status: AgentStatus,
    pub configuration: AgentConfiguration,
    pub health: AgentHealth,
    pub metrics: AgentMetrics,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Agent status with semantic convergence support
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentStatus {
    Idle,
    Busy,
    Running,
    Stopped,
    Error(String),
    Starting,
    Stopping,
    Paused,
    Resuming,
    Unknown,
}

impl Default for AgentStatus {
    fn default() -> Self {
        AgentStatus::Idle
    }
}

/// Agent configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfiguration {
    pub max_concurrent_tasks: usize,
    pub timeout_seconds: u64,
    pub retry_attempts: u32,
    pub memory_limit_mb: Option<u64>,
    pub cpu_limit_percent: Option<u32>,
    pub custom_settings: HashMap<String, serde_json::Value>,
}

impl Default for AgentConfiguration {
    fn default() -> Self {
        Self {
            max_concurrent_tasks: 10,
            timeout_seconds: 300,
            retry_attempts: 3,
            memory_limit_mb: None,
            cpu_limit_percent: None,
            custom_settings: HashMap::new(),
        }
    }
}

/// Agent health metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentHealth {
    pub status: HealthStatus,
    pub uptime_seconds: u64,
    pub last_heartbeat: DateTime<Utc>,
    pub error_count: u32,
    pub success_rate: f64,
    pub resource_usage: ResourceUsage,
}

/// Health status levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum HealthStatus {
    Healthy,
    Warning,
    Critical,
    Unknown,
}

/// Resource usage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsage {
    pub cpu_percent: f64,
    pub memory_mb: u64,
    pub network_in_mb: u64,
    pub network_out_mb: u64,
}

impl Default for ResourceUsage {
    fn default() -> Self {
        Self {
            cpu_percent: 0.0,
            memory_mb: 0,
            network_in_mb: 0,
            network_out_mb: 0,
        }
    }
}

/// Agent performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMetrics {
    pub tasks_completed: u64,
    pub tasks_failed: u64,
    pub average_task_duration_ms: u64,
    pub throughput_tasks_per_second: f64,
    pub error_rate: f64,
}

// ============================================================================
// TASK TYPES
// ============================================================================

/// Unified task structure for semantic convergence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    pub id: TaskId,
    pub name: String,
    pub task_type: String,
    pub priority: TaskPriority,
    pub status: TaskStatus,
    pub payload: serde_json::Value,
    pub agent_id: Option<AgentId>,
    pub workflow_id: Option<WorkflowId>,
    pub dependencies: Vec<TaskId>,
    pub created_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub retry_count: u32,
    pub max_retries: u32,
    pub result: Option<TaskResult>,
}

/// Task priority levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum TaskPriority {
    Critical = 4,
    High = 3,
    Normal = 2,
    Low = 1,
}

impl Default for TaskPriority {
    fn default() -> Self {
        TaskPriority::Normal
    }
}

/// Task execution status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TaskStatus {
    Pending,
    Queued,
    Running,
    Completed,
    Failed(String),
    Cancelled,
    Retrying,
    Timeout,
    Unknown,
}

/// Task execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskResult {
    pub success: bool,
    pub output: Option<serde_json::Value>,
    pub error: Option<String>,
    pub execution_time_ms: u64,
    pub resources_used: ResourceUsage,
}

// ============================================================================
// MESSAGE TYPES
// ============================================================================

/// Unified message structure for semantic convergence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedMessage {
    pub id: MessageId,
    pub message_type: MessageType,
    pub source: AgentId,
    pub target: Option<AgentId>,
    pub payload: UnifiedPayload,
    pub created_at: DateTime<Utc>,
    pub priority: MessagePriority,
    pub ttl_seconds: Option<u64>,
    pub correlation_id: Option<String>,
}

/// Message types for different protocols
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum MessageType {
    TaskRequest,
    TaskResponse,
    TaskUpdate,
    AgentHeartbeat,
    AgentStatus,
    SystemEvent,
    Error,
    Custom(String),
}

/// Message priority levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum MessagePriority {
    Low = 1,
    Normal = 2,
    High = 3,
    Critical = 4,
}

impl Default for MessagePriority {
    fn default() -> Self {
        MessagePriority::Normal
    }
}

/// Unified payload structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedPayload {
    pub content: serde_json::Value,
    pub metadata: HashMap<String, serde_json::Value>,
    pub attachments: Vec<MessageAttachment>,
}

/// Message attachment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageAttachment {
    pub id: String,
    pub name: String,
    pub content_type: String,
    pub size_bytes: u64,
    pub content: String, // Base64 encoded
}

// ============================================================================
// EXECUTION CONTEXT
// ============================================================================

/// Execution context for tracking and state
#[derive(Debug, Clone)]
pub struct ExecutionContext {
    pub workflow_id: WorkflowId,
    pub pipeline_id: PipelineId,
    pub agent_id: AgentId,
    pub started_at: DateTime<Utc>,
    pub current_step: usize,
    pub total_steps: usize,
    pub status: ExecutionStatus,
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Execution status
#[derive(Debug, Clone, PartialEq)]
pub enum ExecutionStatus {
    Running,
    Completed,
    Failed(String),
    Cancelled,
    Paused,
    Unknown,
}

impl Default for ExecutionContext {
    fn default() -> Self {
        Self {
            workflow_id: Uuid::new_v4().to_string(),
            pipeline_id: Uuid::new_v4().to_string(),
            agent_id: "default".to_string(),
            started_at: Utc::now(),
            current_step: 0,
            total_steps: 0,
            status: ExecutionStatus::Running,
            metadata: HashMap::new(),
        }
    }
}

// ============================================================================
// ERROR TYPES
// ============================================================================

/// Result type for operations - re-exported from error module
pub use crate::error::ExecutionError;

/// Result type alias for operations
pub type ExecutionResult<T> = Result<T, ExecutionError>;

// ============================================================================
// RE-EXPORTS FROM OTHER MODULES
// ============================================================================

// Re-export PerformanceMetrics from metrics module to avoid duplication
pub use crate::metrics::PerformanceMetrics;

// Re-export ConvergenceMetrics from convergence module
pub use crate::convergence::ConvergenceMetrics;

// ============================================================================
// TEST HELPERS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_creation() {
        let agent = UnifiedAgent::new("test-agent", "Test Agent");
        assert_eq!(agent.id, "test-agent");
        assert_eq!(agent.name, "Test Agent");
        assert_eq!(agent.status, AgentStatus::Idle);
    }

    #[test]
    fn test_task_creation() {
        let task = Task::new(
            "task-123",
            "Test Task",
            "test",
            TaskPriority::Normal,
            serde_json::json!({"input": "test"}),
        );
        assert_eq!(task.id, "task-123");
        assert_eq!(task.name, "Test Task");
        assert_eq!(task.task_type, "test");
        assert_eq!(task.priority, TaskPriority::Normal);
        assert_eq!(task.status, TaskStatus::Pending);
    }

    #[test]
    fn test_message_creation() {
        let message = UnifiedMessage::new(
            "msg-123",
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2"),
            UnifiedPayload::new(serde_json::json!({"task": "test"})),
        );
        assert_eq!(message.id, "msg-123");
        assert_eq!(message.source, "agent-1");
        assert_eq!(message.target, Some("agent-2".to_string()));
        assert_eq!(message.message_type, MessageType::TaskRequest);
    }
}

// ============================================================================
// CONSTRUCTORS
// ============================================================================

impl UnifiedAgent {
    pub fn new(id: &str, name: &str) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            agent_type: "default".to_string(),
            capabilities: Vec::new(),
            status: AgentStatus::default(),
            configuration: AgentConfiguration::default(),
            health: AgentHealth::new(),
            metrics: AgentMetrics::new(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }
}

impl AgentHealth {
    pub fn new() -> Self {
        Self {
            status: HealthStatus::Healthy,
            uptime_seconds: 0,
            last_heartbeat: Utc::now(),
            error_count: 0,
            success_rate: 1.0,
            resource_usage: ResourceUsage {
                cpu_percent: 0.0,
                memory_mb: 0,
                network_in_mb: 0,
                network_out_mb: 0,
            },
        }
    }
}

impl AgentMetrics {
    pub fn new() -> Self {
        Self {
            tasks_completed: 0,
            tasks_failed: 0,
            average_task_duration_ms: 0,
            throughput_tasks_per_second: 0.0,
            error_rate: 0.0,
        }
    }
}

impl Task {
    pub fn new(
        id: &str,
        name: &str,
        task_type: &str,
        priority: TaskPriority,
        payload: serde_json::Value,
    ) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            task_type: task_type.to_string(),
            priority,
            status: TaskStatus::Pending,
            payload,
            agent_id: None,
            workflow_id: None,
            dependencies: Vec::new(),
            created_at: Utc::now(),
            started_at: None,
            completed_at: None,
            retry_count: 0,
            max_retries: 3,
            result: None,
        }
    }
}

impl UnifiedPayload {
    pub fn new(content: serde_json::Value) -> Self {
        Self {
            content,
            metadata: HashMap::new(),
            attachments: Vec::new(),
        }
    }
}

impl UnifiedMessage {
    pub fn new(
        id: &str,
        message_type: MessageType,
        source: &str,
        target: Option<&str>,
        payload: UnifiedPayload,
    ) -> Self {
        Self {
            id: id.to_string(),
            message_type,
            source: source.to_string(),
            target: target.map(|s| s.to_string()),
            payload,
            created_at: Utc::now(),
            priority: MessagePriority::default(),
            ttl_seconds: None,
            correlation_id: None,
        }
    }
}