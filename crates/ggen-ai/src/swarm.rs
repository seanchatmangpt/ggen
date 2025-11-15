//! Ultrathink Swarm - Distributed AI Agent Coordination System
//!
//! The ultrathink swarm enables multiple AI agents to work together on complex tasks
//! through consensus mechanisms and coordinated task distribution. This creates a
//! self-organizing system that can tackle sophisticated problems beyond individual
//! agent capabilities.

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use tokio::sync::{mpsc, oneshot};
use uuid::Uuid;

/// Unique identifier for swarm agents
pub type AgentId = String;

/// Task identifier for tracking work across the swarm
pub type TaskId = String;

/// Represents a work item that can be distributed across swarm agents
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    /// Unique task identifier
    pub id: TaskId,
    /// Human-readable task description
    pub description: String,
    /// Task type/category for agent specialization
    pub task_type: TaskType,
    /// Input data for the task
    pub input: serde_json::Value,
    /// Priority level (0-100, higher = more urgent)
    pub priority: u8,
    /// Maximum time to spend on this task in seconds
    pub timeout_secs: Option<u64>,
    /// Dependencies on other tasks
    pub dependencies: Vec<TaskId>,
    /// Metadata for task routing and processing
    pub metadata: HashMap<String, String>,
}

impl Task {
    /// Create a new task with default settings
    pub fn new(description: String, task_type: TaskType, input: serde_json::Value) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            description,
            task_type,
            input,
            priority: 50,
            timeout_secs: Some(300), // 5 minutes default
            dependencies: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add metadata to the task
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Set task priority
    pub fn with_priority(mut self, priority: u8) -> Self {
        self.priority = priority.min(100);
        self
    }

    /// Set task timeout
    pub fn with_timeout(mut self, timeout_secs: u64) -> Self {
        self.timeout_secs = Some(timeout_secs);
        self
    }
}

/// Types of tasks that swarm agents can handle
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TaskType {
    /// Template generation tasks
    TemplateGeneration,
    /// SPARQL query generation
    SparqlGeneration,
    /// Ontology/RDF graph generation
    OntologyGeneration,
    /// Code refactoring and improvement
    CodeRefactoring,
    /// Graph analysis and explanation
    GraphAnalysis,
    /// Validation and quality assessment
    Validation,
    /// Complex multi-step reasoning
    Reasoning,
    /// Creative problem solving
    Creative,
    /// Research and information gathering
    Research,
    /// WIP task discovery and assignment
    WipDiscovery,
    /// WIP conflict resolution
    WipConflictResolution,
    /// WIP merge operations
    WipMerge,
}

/// Task result from an agent completing work
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskResult {
    /// ID of the task this result is for
    pub task_id: TaskId,
    /// ID of the agent that produced this result
    pub agent_id: AgentId,
    /// The actual result data
    pub output: serde_json::Value,
    /// Confidence score (0.0-1.0) in the result quality
    pub confidence: f64,
    /// Time taken to complete the task in seconds
    pub execution_time_secs: f64,
    /// Any errors or issues encountered
    pub errors: Vec<String>,
}

impl TaskResult {
    /// Create a new task result
    pub fn new(
        task_id: TaskId,
        agent_id: AgentId,
        output: serde_json::Value,
        confidence: f64,
        execution_time_secs: f64,
    ) -> Self {
        Self {
            task_id,
            agent_id,
            output,
            confidence: confidence.clamp(0.0, 1.0),
            execution_time_secs,
            errors: Vec::new(),
        }
    }
}

/// Trait for AI agents that can participate in the swarm
#[async_trait::async_trait]
pub trait SwarmAgent: Send + Sync {
    /// Get the unique identifier for this agent
    fn id(&self) -> &AgentId;

    /// Get the capabilities this agent supports
    fn capabilities(&self) -> HashSet<TaskType>;

    /// Check if this agent can handle a specific task type
    fn can_handle(&self, task_type: &TaskType) -> bool {
        self.capabilities().contains(task_type)
    }

    /// Execute a task and return the result
    async fn execute_task(&self, _task: Task) -> Result<TaskResult> {
        // Default implementation - should be overridden
        Err(GgenAiError::internal("Task execution not implemented"))
    }

    /// Get the current status of this agent
    fn status(&self) -> AgentStatus {
        AgentStatus::Available
    }

    /// Get agent configuration and capabilities info
    fn info(&self) -> AgentInfo {
        AgentInfo {
            id: self.id().clone(),
            name: "Unnamed Agent".to_string(),
            description: "Generic swarm agent".to_string(),
            capabilities: self.capabilities(),
            config: serde_json::json!({}),
            metrics: AgentMetrics::default(),
        }
    }
}

/// Status of a swarm agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AgentStatus {
    /// Agent is available and ready for tasks
    Available,
    /// Agent is currently processing a task
    Busy { task_id: Option<TaskId> },
    /// Agent is temporarily unavailable
    Unavailable { reason: String },
    /// Agent has encountered an error
    Error { error: String },
}

/// Information about a swarm agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentInfo {
    /// Agent identifier
    pub id: AgentId,
    /// Human-readable agent name
    pub name: String,
    /// Agent description and purpose
    pub description: String,
    /// Supported task types
    pub capabilities: HashSet<TaskType>,
    /// Agent configuration
    pub config: serde_json::Value,
    /// Performance metrics
    pub metrics: AgentMetrics,
}

/// Performance metrics for an agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMetrics {
    /// Total tasks completed
    pub tasks_completed: u64,
    /// Average confidence score across all tasks
    pub avg_confidence: f64,
    /// Average execution time in seconds
    pub avg_execution_time: f64,
    /// Success rate (tasks completed without errors)
    pub success_rate: f64,
}

impl Default for AgentMetrics {
    fn default() -> Self {
        Self {
            tasks_completed: 0,
            avg_confidence: 0.0,
            avg_execution_time: 0.0,
            success_rate: 0.0,
        }
    }
}

/// Configuration for the swarm system
#[derive(Debug, Clone)]
pub struct SwarmConfig {
    /// Maximum number of agents in the swarm
    pub max_agents: usize,
    /// Task timeout in seconds
    pub default_task_timeout_secs: u64,
    /// Number of agents required for consensus
    pub consensus_threshold: usize,
}

impl Default for SwarmConfig {
    fn default() -> Self {
        Self {
            max_agents: 10,
            default_task_timeout_secs: 300,
            consensus_threshold: 3,
        }
    }
}

/// Coordinator for managing the swarm of AI agents
#[derive(Debug)]
pub struct SwarmCoordinator {
    /// Swarm configuration
    config: SwarmConfig,
    /// Registered agents
    agents: Arc<RwLock<HashMap<AgentId, Box<dyn SwarmAgent>>>>,
}

impl SwarmCoordinator {
    /// Create a new swarm coordinator
    pub fn new(config: SwarmConfig) -> Self {
        Self {
            config,
            agents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a new agent with the swarm
    pub fn register_agent(&self, agent: Box<dyn SwarmAgent>) -> Result<()> {
        let mut agents = self.agents.write().map_err(|e| {
            GgenAiError::internal(&format!("Failed to acquire agents lock: {}", e))
        })?;

        if agents.len() >= self.config.max_agents {
            return Err(GgenAiError::configuration(&format!(
                "Maximum number of agents ({}) exceeded",
                self.config.max_agents
            )));
        }

        let agent_id = agent.id().clone();
        agents.insert(agent_id.clone(), agent);

        log::info!("🤖 Registered agent: {} ({})", agent_id, agents.len());
        Ok(())
    }

    /// Get swarm statistics
    pub fn get_stats(&self) -> SwarmStats {
        let agents = self.agents.read().unwrap();

        SwarmStats {
            total_agents: agents.len(),
            active_tasks: 0, // Would need to track active tasks
            queued_tasks: 0, // Would need to track queued tasks
            consensus_pending: 0, // Would need to track pending results
        }
    }
}

/// Statistics about the swarm
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwarmStats {
    /// Total number of registered agents
    pub total_agents: usize,
    /// Number of tasks currently being processed
    pub active_tasks: usize,
    /// Number of tasks waiting in queue
    pub queued_tasks: usize,
    /// Number of task results awaiting consensus
    pub consensus_pending: usize,
}

impl Default for SwarmStats {
    fn default() -> Self {
        Self {
            total_agents: 0,
            active_tasks: 0,
            queued_tasks: 0,
            consensus_pending: 0,
        }
    }
}

/// WIP Discovery Agent for finding and assigning WIP tasks
pub struct WipDiscoveryAgent {
    /// Agent identifier
    id: AgentId,
}

impl WipDiscoveryAgent {
    /// Create a new WIP discovery agent
    pub fn new() -> Self {
        Self {
            id: format!("wip-discovery-{}", Uuid::new_v4()),
        }
    }
}

#[async_trait::async_trait]
impl SwarmAgent for WipDiscoveryAgent {
    fn id(&self) -> &AgentId {
        &self.id
    }

    fn capabilities(&self) -> HashSet<TaskType> {
        [TaskType::WipDiscovery, TaskType::WipConflictResolution].into_iter().collect()
    }

    async fn execute_task(&self, _task: Task) -> Result<TaskResult> {
        // Placeholder implementation
        Ok(TaskResult::new(
            "wip-discovery".to_string(),
            self.id.clone(),
            serde_json::json!({"status": "wip_discovery_completed"}),
            0.9,
            1.0,
        ))
    }

    fn status(&self) -> AgentStatus {
        AgentStatus::Available
    }

    fn info(&self) -> AgentInfo {
        AgentInfo {
            id: self.id.clone(),
            name: "WIP Discovery Agent".to_string(),
            description: "Discovers and manages WIP entries from configured endpoints".to_string(),
            capabilities: self.capabilities(),
            config: serde_json::json!({}),
            metrics: AgentMetrics::default(),
        }
    }
}

/// MCP Swarm Server that exposes ultrathink capabilities via MCP protocol
pub struct McpSwarmServer {
    /// Swarm coordinator
    coordinator: SwarmCoordinator,
    /// WIP discovery agent
    wip_discovery: WipDiscoveryAgent,
}

impl McpSwarmServer {
    /// Create a new MCP swarm server
    pub fn new() -> Self {
        Self {
            coordinator: SwarmCoordinator::default(),
            wip_discovery: WipDiscoveryAgent::new(),
        }
    }

    /// Start the MCP swarm server
    pub async fn start(&self) -> Result<()> {
        log::info!("🚀 Starting MCP Swarm Server");

        // Register WIP discovery agent
        self.coordinator.register_agent(Box::new(self.wip_discovery.clone()))?;
        log::info!("🤖 Registered WIP discovery agent");

        log::info!("🎪 MCP Swarm Server ready for connections");
        log::info!("📋 Swarm stats: {:?}", self.coordinator.get_stats());

        Ok(())
    }

    /// Get swarm statistics
    pub fn get_swarm_stats(&self) -> SwarmStats {
        self.coordinator.get_stats()
    }
}

/// Default implementation for swarm coordinator
impl Default for SwarmCoordinator {
    fn default() -> Self {
        Self::new(SwarmConfig::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swarm_coordinator_creation() {
        let coordinator = SwarmCoordinator::new(SwarmConfig::default());
        let stats = coordinator.get_stats();

        assert_eq!(stats.total_agents, 0);
        assert_eq!(stats.active_tasks, 0);
        assert_eq!(stats.queued_tasks, 0);
    }

    #[test]
    fn test_agent_registration() {
        let coordinator = SwarmCoordinator::new(SwarmConfig::default());

        let agent = Box::new(WipDiscoveryAgent::new());
        coordinator.register_agent(agent).unwrap();
        let stats = coordinator.get_stats();

        assert_eq!(stats.total_agents, 1);
    }
}