//! OSIRIS Life Domains Example - Wave 4: Autonomous Agent Management
//!
//! Demonstrates:
//! - Autonomous agent reasoning (agents discover goals and optimize)
//! - Multi-agent coordination (consensus for domain balancing)
//! - Self-improvement (agents learn from outcomes)
//! - MCP tool integration (agents discover and use tools)

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod agents;
pub mod balancing;
pub mod improvement;
pub mod metrics;
pub mod mcp_tools;
pub mod persistence;
pub mod reasoning;

pub use agents::{AgentBase, AgentStatus, HealthAgent, CareerAgent, RelationshipAgent, FinanceAgent, LearningAgent, SpiritualityAgent};
pub use balancing::{DomainBalance, ConsensusVoting};
pub use improvement::{ImprovementTracker, ActionOutcome};
pub use metrics::HealthScore;
pub use mcp_tools::MCPToolRegistry;
pub use reasoning::AutonomousReasoning;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::info;

/// OSIRIS Life Domains System
#[derive(Clone)]
pub struct LifeDomainsSystem {
    agents: Arc<RwLock<HashMap<String, Arc<dyn AgentBase>>>>,
    balance: Arc<RwLock<DomainBalance>>,
    improvement_tracker: Arc<ImprovementTracker>,
    mcp_tools: Arc<MCPToolRegistry>,
}

impl LifeDomainsSystem {
    /// Create a new life domains system
    pub async fn new() -> Self {
        info!("Initializing OSIRIS Life Domains System");
        Self {
            agents: Arc::new(RwLock::new(HashMap::new())),
            balance: Arc::new(RwLock::new(DomainBalance::new())),
            improvement_tracker: Arc::new(ImprovementTracker::new()),
            mcp_tools: Arc::new(MCPToolRegistry::new()),
        }
    }

    /// Initialize the system with all domain agents
    pub async fn initialize(&self) -> anyhow::Result<()> {
        info!("Initializing OSIRIS Life Domains with 6 autonomous agents");
        
        let mut agents = self.agents.write().await;
        
        // Register all 6 domain agents
        agents.insert(
            "health".to_string(),
            Arc::new(HealthAgent::new("health")) as Arc<dyn AgentBase>,
        );
        agents.insert(
            "career".to_string(),
            Arc::new(CareerAgent::new("career")) as Arc<dyn AgentBase>,
        );
        agents.insert(
            "relationships".to_string(),
            Arc::new(RelationshipAgent::new("relationships")) as Arc<dyn AgentBase>,
        );
        agents.insert(
            "finance".to_string(),
            Arc::new(FinanceAgent::new("finance")) as Arc<dyn AgentBase>,
        );
        agents.insert(
            "learning".to_string(),
            Arc::new(LearningAgent::new("learning")) as Arc<dyn AgentBase>,
        );
        agents.insert(
            "spirituality".to_string(),
            Arc::new(SpiritualityAgent::new("spirituality")) as Arc<dyn AgentBase>,
        );

        info!("Registered 6 domain agents");
        Ok(())
    }

    /// Run autonomous reasoning cycle for all agents
    pub async fn reasoning_cycle(&self) -> anyhow::Result<()> {
        let agents = self.agents.read().await;
        
        for (domain_id, agent) in agents.iter() {
            // Each agent analyzes its domain state
            let status = agent.get_status().await?;
            let reasoning = AutonomousReasoning::analyze(&status)?;
            
            // Agent sets goals based on current state
            agent.set_goals(reasoning.recommended_goals).await?;
            
            // Agent plans actions
            let plan = reasoning.action_plan;
            
            // Agent calls MCP tools to execute plan
            for action in plan {
                let result = self.mcp_tools.execute_action(&action).await?;
                
                // Track outcome for self-improvement
                self.improvement_tracker.record_action(domain_id, action, result).await?;
            }
        }
        
        Ok(())
    }

    /// Run domain consensus and balancing
    pub async fn balance_domains(&self) -> anyhow::Result<()> {
        let agents = self.agents.read().await;
        
        // Collect domain health scores
        let mut scores = HashMap::new();
        for (id, agent) in agents.iter() {
            let status = agent.get_status().await?;
            scores.insert(id.clone(), status.health_score);
        }
        
        // Run consensus voting for resource allocation
        let mut balance = self.balance.write().await;
        balance.run_consensus_voting(&scores)?;
        
        // Detect imbalances
        let imbalanced = balance.detect_imbalances(&scores)?;
        
        // Propose rebalancing if needed
        if !imbalanced.is_empty() {
            let strategy = balance.propose_rebalancing(&imbalanced)?;
            // Agents negotiate and enforce agreed balance
            for (domain_id, allocation) in strategy {
                if let Some(agent) = agents.get(&domain_id) {
                    agent.allocate_resources(allocation).await?;
                }
            }
        }
        
        Ok(())
    }

    /// Get system status across all domains
    pub async fn get_system_status(&self) -> anyhow::Result<serde_json::Value> {
        let agents = self.agents.read().await;
        let balance = self.balance.read().await;
        
        let mut statuses = HashMap::new();
        for (id, agent) in agents.iter() {
            let status = agent.get_status().await?;
            statuses.insert(id.clone(), serde_json::to_value(status)?);
        }
        
        Ok(serde_json::json!({
            "domain_statuses": statuses,
            "balance": serde_json::to_value(&*balance)?,
            "timestamp": chrono::Utc::now().to_rfc3339(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_life_domains_creation() {
        let system = LifeDomainsSystem::new().await;
        assert!(system.agents.read().await.is_empty());
    }

    #[tokio::test]
    async fn test_system_initialization() {
        let system = LifeDomainsSystem::new().await;
        let result = system.initialize().await;
        assert!(result.is_ok());
        
        let agents = system.agents.read().await;
        assert_eq!(agents.len(), 6);
    }
}
