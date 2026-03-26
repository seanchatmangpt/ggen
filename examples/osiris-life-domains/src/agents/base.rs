//! Base agent trait and common structures

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Agent status report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentStatus {
    pub domain_id: String,
    pub health_score: f64,
    pub goals: Vec<String>,
    pub current_actions: Vec<String>,
    pub recent_outcomes: Vec<String>,
}

/// Base trait for all domain agents
#[async_trait]
pub trait AgentBase: Send + Sync {
    /// Get current agent status
    async fn get_status(&self) -> anyhow::Result<AgentStatus>;

    /// Set goals for the domain based on current state
    async fn set_goals(&self, goals: Vec<String>) -> anyhow::Result<()>;

    /// Allocate resources to this domain
    async fn allocate_resources(&self, allocation: f64) -> anyhow::Result<()>;

    /// Report recommendations for improvement
    async fn recommend_improvements(&self) -> anyhow::Result<Vec<String>>;

    /// Execute an action via MCP tool
    async fn execute_action(&self, action: &str) -> anyhow::Result<String>;
}
