//! 12-Agent Architecture for GGen MCP Server
//! 
//! Implements London-BDD patterns with Byzantine fault tolerance
//! Following core team best practices for distributed systems

pub mod coordinator;
pub mod validator;
pub mod executor;
pub mod monitor;
pub mod cache;
pub mod security;
pub mod metrics;
pub mod health;
pub mod recovery;
pub mod consensus;
pub mod discovery;
pub mod scheduler;
pub mod graph_evolution;
pub mod regeneration;
pub mod feedback;

use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Agent ID type for unique identification
pub type AgentId = Uuid;

/// Consensus result type - true if consensus reached, false otherwise
pub type ConsensusResult = bool;

/// Agent status for health monitoring
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentStatus {
    Healthy,
    Degraded,
    Unhealthy,
    Recovering,
    Active,
    Idle,
}

/// Agent type for different agent specializations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentType {
    LondonBdd,
    Byzantine,
    Template,
    Graph,
    Cache,
    Security,
    Metrics,
    Health,
    Recovery,
    Consensus,
    Discovery,
    Scheduler,
    GraphEvolution,
    Regeneration,
    Feedback,
}

/// Agent capabilities for skill-based coordination
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentCapability {
    TemplateGeneration,
    GraphQuery,
    Validation,
    Caching,
    SecurityCheck,
    MetricsCollection,
    HealthCheck,
    Recovery,
    Consensus,
    Discovery,
    Scheduling,
    Evolution,
    Regeneration,
    Feedback,
}

/// Basic Agent info struct for swarm coordination
/// Note: This is different from the `Agent` trait which defines the async interface
#[derive(Debug, Clone)]
pub struct AgentInfo {
    pub id: AgentId,
    pub agent_type: AgentType,
    pub status: AgentStatus,
    pub capabilities: Vec<AgentCapability>,
}

impl AgentInfo {
    pub fn new(id: AgentId, agent_type: AgentType, status: AgentStatus, capabilities: Vec<AgentCapability>) -> Self {
        Self {
            id,
            agent_type,
            status,
            capabilities,
        }
    }
}

/// Agent configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    pub id: AgentId,
    pub name: String,
    pub role: AgentRole,
    pub timeout_ms: u64,
    pub retry_count: u32,
    pub health_check_interval_ms: u64,
}

/// Agent roles in the 12-agent architecture
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentRole {
    /// London-BDD Coordinator - Orchestrates BDD test execution
    LondonBddCoordinator,
    /// Byzantine Validator - Validates operations with fault tolerance
    ByzantineValidator,
    /// Template Executor - Handles template generation
    TemplateExecutor,
    /// Graph Monitor - Monitors RDF graph operations
    GraphMonitor,
    /// Cache Manager - Manages template and result caching
    CacheManager,
    /// Security Agent - Validates inputs and enforces policies
    SecurityAgent,
    /// Metrics Collector - Collects and aggregates metrics
    MetricsCollector,
    /// Health Monitor - Monitors system health
    HealthMonitor,
    /// Recovery Agent - Handles failure recovery
    RecoveryAgent,
    /// Consensus Manager - Manages distributed consensus
    ConsensusManager,
    /// Service Discovery - Discovers and registers services
    ServiceDiscovery,
    /// Task Scheduler - Schedules and prioritizes tasks
    TaskScheduler,
}

/// Agent trait for all agents in the system
#[async_trait::async_trait]
pub trait Agent: Send + Sync {
    /// Initialize the agent
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    
    /// Start the agent
    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    
    /// Stop the agent
    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    
    /// Get agent status
    async fn status(&self) -> AgentStatus;
    
    /// Get agent configuration
    fn config(&self) -> &AgentConfig;
    
    /// Handle incoming message
    async fn handle_message(&mut self, message: AgentMessage) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>>;
}

/// Message types for inter-agent communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AgentMessage {
    /// Health check request
    HealthCheck { from: AgentId },
    /// Health check response
    HealthResponse { status: AgentStatus, metrics: Option<serde_json::Value> },
    /// Task assignment
    TaskAssignment { task_id: Uuid, task: TaskDefinition },
    /// Task completion
    TaskCompletion { task_id: Uuid, result: TaskResult },
    /// Consensus request
    ConsensusRequest { proposal: ConsensusProposal },
    /// Consensus response
    ConsensusResponse { accepted: bool, reason: Option<String> },
    /// Error notification
    ErrorNotification { error: String, severity: ErrorSeverity },
    /// Recovery request
    RecoveryRequest { failed_agent: AgentId, context: serde_json::Value },
}

/// Task definition for distributed execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskDefinition {
    pub id: Uuid,
    pub task_type: TaskType,
    pub parameters: serde_json::Value,
    pub priority: TaskPriority,
    pub timeout_ms: u64,
    pub dependencies: Vec<Uuid>,
}

/// Task types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TaskType {
    TemplateGeneration,
    GraphQuery,
    Validation,
    CacheOperation,
    SecurityCheck,
    MetricsCollection,
    HealthCheck,
    Recovery,
    Consensus,
    Discovery,
    Scheduling,
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

/// Consensus proposal for Byzantine fault tolerance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsensusProposal {
    pub id: Uuid,
    pub proposal_type: ConsensusType,
    pub data: serde_json::Value,
    pub proposer: AgentId,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Consensus types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConsensusType {
    TemplateValidation,
    GraphOperation,
    SecurityPolicy,
    ConfigurationChange,
    RecoveryAction,
}

/// Error severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ErrorSeverity {
    Critical,
    High,
    Medium,
    Low,
    Info,
}

/// Agent registry for managing all agents
pub struct AgentRegistry {
    agents: Arc<RwLock<Vec<Arc<RwLock<dyn Agent>>>>>,
    coordinator: Option<Arc<RwLock<coordinator::LondonBddCoordinator>>>,
}

impl AgentRegistry {
    pub fn new() -> Self {
        Self {
            agents: Arc::new(RwLock::new(Vec::new())),
            coordinator: None,
        }
    }
    
    /// Register an agent
    pub async fn register_agent(&mut self, agent: Arc<RwLock<dyn Agent>>) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut agents = self.agents.write().await;
        agents.push(agent);
        Ok(())
    }
    
    /// Get all agents
    pub async fn get_agents(&self) -> Vec<Arc<RwLock<dyn Agent>>> {
        self.agents.read().await.clone()
    }
    
    /// Find agent by role
    pub async fn find_agent_by_role(&self, role: AgentRole) -> Option<Arc<RwLock<dyn Agent>>> {
        let agents = self.agents.read().await;
        for agent in agents.iter() {
            if agent.read().await.config().role == role {
                return Some(agent.clone());
            }
        }
        None
    }
    
    /// Broadcast message to all agents
    pub async fn broadcast(&self, message: AgentMessage) -> Result<Vec<AgentMessage>, Box<dyn std::error::Error + Send + Sync>> {
        let agents = self.agents.read().await;
        let mut responses = Vec::new();
        
        for agent in agents.iter() {
            let mut agent_guard = agent.write().await;
            match agent_guard.handle_message(message.clone()).await {
                Ok(response) => responses.push(response),
                Err(e) => {
                    tracing::error!("Agent {} failed to handle message: {}", 
                        agent_guard.config().name, e);
                }
            }
        }
        
        Ok(responses)
    }
}

/// London-BDD test execution context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BddContext {
    pub scenario: String,
    pub steps: Vec<BddStep>,
    pub variables: std::collections::HashMap<String, serde_json::Value>,
    pub assertions: Vec<BddAssertion>,
}

/// BDD step definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BddStep {
    pub step_type: BddStepType,
    pub description: String,
    pub action: String,
    pub expected: Option<String>,
}

/// BDD step types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum BddStepType {
    Given,
    When,
    Then,
    And,
    But,
}

/// BDD assertion for validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BddAssertion {
    pub assertion_type: BddAssertionType,
    pub condition: String,
    pub expected_value: serde_json::Value,
    pub tolerance: Option<f64>,
}

/// BDD assertion types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum BddAssertionType {
    Equals,
    Contains,
    Matches,
    GreaterThan,
    LessThan,
    IsEmpty,
    IsNotEmpty,
    IsValid,
    IsInvalid,
}

/// Byzantine fault tolerance configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ByzantineConfig {
    pub total_nodes: usize,
    pub faulty_nodes: usize,
    pub consensus_threshold: f64,
    pub timeout_ms: u64,
    pub retry_count: u32,
}

impl ByzantineConfig {
    /// Calculate minimum nodes needed for consensus
    pub fn min_nodes_for_consensus(&self) -> usize {
        (2 * self.faulty_nodes + 1).max(3)
    }
    
    /// Check if consensus is possible with current configuration
    pub fn is_consensus_possible(&self) -> bool {
        self.total_nodes >= self.min_nodes_for_consensus()
    }
}

/// Re-export all agent modules
pub use coordinator::LondonBddCoordinator;
pub use validator::ByzantineValidator;
pub use executor::TemplateExecutor;
pub use monitor::GraphMonitor;
pub use cache::CacheManager;
pub use security::SecurityAgent;
pub use metrics::MetricsCollector;
pub use health::HealthMonitor;
pub use recovery::RecoveryAgent;
pub use consensus::ConsensusManager;
pub use discovery::ServiceDiscovery;
pub use scheduler::TaskScheduler;
pub use graph_evolution::GraphEvolutionAgent;
pub use regeneration::RegenerationAgent;
pub use feedback::FeedbackAgent;