//! Unified Agent Architecture - Core Team Best Practice
//!
//! Single source of truth for all ggen agents following 80/20 principle:
//! - Simple trait interface
//! - Clear separation of concerns
//! - Composable agent types
//!
//! ## Architecture
//!
//! ```
//! Agent Trait (unified interface)
//!   ↓
//! ├── Core Agents (graph_evolution, regeneration, feedback)
//! ├── Swarm Coordination (orchestration, events)
//! └── Registry (discovery, health)
//! ```

pub mod core;
pub mod registry;

use crate::error::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

// Re-export for convenience
pub use registry::AgentRegistry;

/// Unified agent trait - combines best practices from both architectures
///
/// Supports both simplified (execute) and full lifecycle (initialize/start/stop) patterns
#[async_trait]
pub trait Agent: Send + Sync + std::fmt::Debug {
    /// Initialize agent (lifecycle method)
    async fn initialize(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Start agent execution (lifecycle method)
    async fn start(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Stop agent execution (lifecycle method)
    async fn stop(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Get agent status
    async fn status(&self) -> AgentStatus;

    /// Get agent configuration
    fn config(&self) -> &AgentConfig;

    /// Handle incoming messages
    async fn handle_message(&mut self, message: AgentMessage) -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>>;
}

/// Agent configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    pub id: Uuid,
    pub name: String,
    pub role: AgentRole,
    pub capabilities: Vec<String>,
    pub max_concurrent_tasks: usize,
}

/// Agent role in the system
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentRole {
    GraphEvolution,
    Regeneration,
    Feedback,
    Validator,
    Coordinator,
}

/// Agent status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentStatus {
    Healthy,
    Degraded,
    Unhealthy,
    Offline,
}

/// Agent messages for communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AgentMessage {
    TaskAssignment {
        task_id: Uuid,
        task: TaskDefinition,
    },
    TaskCompletion {
        task_id: Uuid,
        result: TaskResult,
    },
    HealthCheck {
        requester: Uuid,
    },
    HealthResponse {
        status: AgentStatus,
        metrics: Option<serde_json::Value>,
    },
    Shutdown,
}

/// Task definition for agent execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskDefinition {
    pub id: Uuid,
    pub task_type: TaskType,
    pub parameters: HashMap<String, serde_json::Value>,
    pub priority: TaskPriority,
}

/// Task types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TaskType {
    TemplateGeneration,
    GraphEvolution,
    Regeneration,
    Validation,
    Analysis,
}

/// Task priority levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TaskPriority {
    Critical,
    High,
    Normal,
    Low,
}

/// Task execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskResult {
    pub task_id: Uuid,
    pub success: bool,
    pub result: Option<serde_json::Value>,
    pub error: Option<String>,
    pub duration_ms: u64,
    pub metrics: Option<serde_json::Value>,
}

/// Agent input for execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentInput {
    /// Input data (flexible JSON structure)
    pub data: serde_json::Value,

    /// Input type (for routing/validation)
    pub input_type: String,

    /// Source agent (if part of workflow)
    pub source_agent: Option<String>,

    /// Execution context (environment, config, etc.)
    pub context: HashMap<String, String>,
}

/// Agent output from execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentOutput {
    /// Output data
    pub data: serde_json::Value,

    /// Output type
    pub output_type: String,

    /// Target agents for next step (workflow chaining)
    pub target_agents: Vec<String>,

    /// Execution metadata (timing, resources, etc.)
    pub metadata: HashMap<String, String>,
}

/// Agent health status (simplified version for compatibility)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentHealth {
    /// Health status enum
    pub status: HealthStatus,

    /// Health score (0.0 - 1.0)
    pub score: f64,

    /// Last health check timestamp
    pub last_check: String,

    /// Issues detected
    pub issues: Vec<String>,
}

/// Health status enum (alias for AgentStatus)
pub type HealthStatus = AgentStatus;

/// Agent execution result (for workflows)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentResult {
    /// Agent that produced this result
    pub agent_name: String,

    /// Success status
    pub success: bool,

    /// Result data
    pub result: Option<serde_json::Value>,

    /// Error if failed
    pub error: Option<String>,

    /// Execution duration (milliseconds)
    pub duration_ms: u64,

    /// Additional metrics
    pub metrics: HashMap<String, f64>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct MockAgent {
        config: AgentConfig,
        status: AgentStatus,
    }

    #[async_trait]
    impl Agent for MockAgent {
        async fn initialize(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
            Ok(())
        }

        async fn start(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
            Ok(())
        }

        async fn stop(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
            Ok(())
        }

        async fn status(&self) -> AgentStatus {
            self.status.clone()
        }

        fn config(&self) -> &AgentConfig {
            &self.config
        }

        async fn handle_message(&mut self, message: AgentMessage) -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
            match message {
                AgentMessage::HealthCheck { requester } => {
                    Ok(AgentMessage::HealthResponse {
                        status: self.status.clone(),
                        metrics: None,
                    })
                }
                _ => Ok(message),
            }
        }
    }

    #[tokio::test]
    async fn test_agent_trait() {
        let mut agent = MockAgent {
            config: AgentConfig {
                id: Uuid::new_v4(),
                name: "test-agent".to_string(),
                role: AgentRole::Validator,
                capabilities: vec!["test".to_string()],
                max_concurrent_tasks: 10,
            },
            status: AgentStatus::Healthy,
        };

        agent.initialize().await.unwrap();
        agent.start().await.unwrap();

        assert_eq!(agent.config().name, "test-agent");
        assert_eq!(agent.status().await, AgentStatus::Healthy);

        agent.stop().await.unwrap();
    }
}
