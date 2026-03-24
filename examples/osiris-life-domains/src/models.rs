//! Core data models for life domains system

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use std::collections::HashMap;

/// Domain ID type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DomainId(pub String);

/// Agent ID type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AgentId(pub String);

/// Goal ID type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GoalId(pub String);

/// Domain health score (0.0 - 1.0)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct DomainHealth(pub f64);

impl DomainHealth {
    /// Create new domain health score
    pub fn new(score: f64) -> Self {
        Self(score.clamp(0.0, 1.0))
    }

    /// Check if domain is healthy (score >= 0.7)
    pub fn is_healthy(&self) -> bool {
        self.0 >= 0.7
    }

    /// Check if domain is imbalanced (score < 0.6)
    pub fn is_imbalanced(&self) -> bool {
        self.0 < 0.6
    }

    /// Check if domain is critical (score < 0.4)
    pub fn is_critical(&self) -> bool {
        self.0 < 0.4
    }
}

/// Goal status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GoalStatus {
    /// Planned but not started
    Planned,
    /// Currently in progress
    InProgress,
    /// Completed
    Completed,
    /// On hold
    OnHold,
    /// Cancelled
    Cancelled,
}

impl std::fmt::Display for GoalStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Planned => write!(f, "Planned"),
            Self::InProgress => write!(f, "InProgress"),
            Self::Completed => write!(f, "Completed"),
            Self::OnHold => write!(f, "OnHold"),
            Self::Cancelled => write!(f, "Cancelled"),
        }
    }
}

/// A goal in a domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Goal {
    /// Goal identifier
    pub id: GoalId,
    /// Goal title
    pub title: String,
    /// Goal description
    pub description: String,
    /// Current status
    pub status: GoalStatus,
    /// Progress (0.0 - 1.0)
    pub progress: f64,
    /// Priority (0.0 - 1.0)
    pub priority: f64,
    /// Target date
    pub target_date: Option<DateTime<Utc>>,
    /// Created at
    pub created_at: DateTime<Utc>,
    /// Completed at
    pub completed_at: Option<DateTime<Utc>>,
    /// Metrics for goal
    pub metrics: HashMap<String, f64>,
}

impl Goal {
    /// Create a new goal
    pub fn new(id: GoalId, title: String, description: String, priority: f64) -> Self {
        Self {
            id,
            title,
            description,
            status: GoalStatus::Planned,
            progress: 0.0,
            priority: priority.clamp(0.0, 1.0),
            target_date: None,
            created_at: Utc::now(),
            completed_at: None,
            metrics: HashMap::new(),
        }
    }

    /// Update goal progress
    pub fn update_progress(&mut self, progress: f64) {
        self.progress = progress.clamp(0.0, 1.0);
        if self.progress >= 1.0 {
            self.status = GoalStatus::Completed;
            self.completed_at = Some(Utc::now());
        }
    }

    /// Mark goal as in progress
    pub fn start(&mut self) {
        self.status = GoalStatus::InProgress;
    }
}

/// Action planned or executed by an agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Action {
    /// Action ID
    pub id: String,
    /// Action name
    pub name: String,
    /// Action description
    pub description: String,
    /// MCP tools to call
    pub tools: Vec<String>,
    /// Expected health improvement
    pub expected_health_impact: f64,
    /// Actual health impact (after execution)
    pub actual_health_impact: Option<f64>,
}

impl Action {
    /// Create new action
    pub fn new(name: String, description: String, expected_impact: f64) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name,
            description,
            tools: Vec::new(),
            expected_health_impact: expected_impact,
            actual_health_impact: None,
        }
    }

    /// Add a tool to this action
    pub fn with_tool(mut self, tool: String) -> Self {
        self.tools.push(tool);
        self
    }
}

/// Autonomous agent state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentState {
    /// Agent ID
    pub id: AgentId,
    /// Managed domain
    pub domain_id: DomainId,
    /// Current health score
    pub health: DomainHealth,
    /// Active goals
    pub goals: Vec<Goal>,
    /// Recent actions taken
    pub recent_actions: Vec<Action>,
    /// Learned patterns
    pub learned_patterns: HashMap<String, f64>,
    /// Last update
    pub last_updated: DateTime<Utc>,
    /// Consensus votes
    pub consensus_votes: HashMap<String, u32>,
}

impl AgentState {
    /// Create new agent state
    pub fn new(id: AgentId, domain_id: DomainId) -> Self {
        Self {
            id,
            domain_id,
            health: DomainHealth::new(0.5),
            goals: Vec::new(),
            recent_actions: Vec::new(),
            learned_patterns: HashMap::new(),
            last_updated: Utc::now(),
            consensus_votes: HashMap::new(),
        }
    }

    /// Update health score
    pub fn update_health(&mut self, new_health: f64) {
        self.health = DomainHealth::new(new_health);
        self.last_updated = Utc::now();
    }

    /// Add goal
    pub fn add_goal(&mut self, goal: Goal) {
        self.goals.push(goal);
    }

    /// Record action taken
    pub fn record_action(&mut self, action: Action) {
        // Keep last 10 actions
        self.recent_actions.push(action);
        if self.recent_actions.len() > 10 {
            self.recent_actions.remove(0);
        }
    }

    /// Learn pattern from action
    pub fn learn_pattern(&mut self, pattern: String, effectiveness: f64) {
        self.learned_patterns.insert(pattern, effectiveness);
    }
}

/// Core agent interface
#[async_trait::async_trait]
pub trait Agent: Send + Sync {
    /// Get agent ID
    fn id(&self) -> &AgentId;

    /// Get managed domain
    fn domain_id(&self) -> &DomainId;

    /// Get current health score
    async fn get_health(&self) -> DomainHealth;

    /// Analyze domain state and discover goals
    async fn analyze_domain(&mut self) -> crate::Result<()>;

    /// Plan actions to improve domain
    async fn plan_actions(&mut self) -> crate::Result<Vec<Action>>;

    /// Execute a planned action
    async fn execute_action(&mut self, action: Action) -> crate::Result<f64>;

    /// Get current state
    fn get_state(&self) -> AgentState;

    /// Report status
    async fn report_status(&self) -> HashMap<String, serde_json::Value>;

    /// Learn from results
    async fn learn_from_outcome(&mut self, action: &Action, impact: f64) -> crate::Result<()>;

    /// Vote on consensus proposal
    async fn vote_on_proposal(&mut self, proposal: &str) -> crate::Result<bool>;
}
