//! Main OSIRIS Life Domains system orchestrator

use crate::agents::*;
use crate::balancing::{BalancingEngine, BalancingStatus};
use crate::improvement::{ImprovementEngine, LearningOutcome};
use crate::metrics::MetricsCollector;
use crate::mcp_tools::MCPToolRegistry;
use crate::models::*;
use crate::persistence::{AuditEntry, AuditTrail, PersistenceManager};
use crate::reasoning::ReasoningEngine;
use crate::Result;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Main OSIRIS Life Domains System
///
/// Coordinates 6 autonomous agents managing life domains with consensus-based balancing,
/// self-improvement loops, and MCP tool integration.
pub struct LifeDomainsSystem {
    /// All agents
    agents: Arc<RwLock<HashMap<String, Box<dyn Agent>>>>,
    /// MCP tool registry
    tools: Arc<MCPToolRegistry>,
    /// Metrics collectors per domain
    metrics: Arc<RwLock<HashMap<String, MetricsCollector>>>,
    /// Balancing status
    balance: Arc<RwLock<BalancingStatus>>,
    /// Audit trail
    audit_trail: Arc<RwLock<AuditTrail>>,
    /// Learning outcomes history
    outcomes: Arc<RwLock<Vec<LearningOutcome>>>,
}

impl LifeDomainsSystem {
    /// Create and initialize new system
    pub async fn new() -> Result<Self> {
        info!("Initializing OSIRIS Life Domains System");

        let mut agents_map = HashMap::new();

        // Create and initialize all 6 agents
        let mut health_agent = HealthAgent::new();
        health_agent.initialize_goals();
        agents_map.insert("health".to_string(), Box::new(health_agent) as Box<dyn Agent>);

        let mut career_agent = CareerAgent::new();
        career_agent.initialize_goals();
        agents_map.insert("career".to_string(), Box::new(career_agent) as Box<dyn Agent>);

        let mut relationship_agent = RelationshipAgent::new();
        relationship_agent.initialize_goals();
        agents_map.insert("relationships".to_string(), Box::new(relationship_agent) as Box<dyn Agent>);

        let mut finance_agent = FinanceAgent::new();
        finance_agent.initialize_goals();
        agents_map.insert("finance".to_string(), Box::new(finance_agent) as Box<dyn Agent>);

        let mut learning_agent = LearningAgent::new();
        learning_agent.initialize_goals();
        agents_map.insert("learning".to_string(), Box::new(learning_agent) as Box<dyn Agent>);

        let mut spirituality_agent = SpiritualityAgent::new();
        spirituality_agent.initialize_goals();
        agents_map.insert("spirituality".to_string(), Box::new(spirituality_agent) as Box<dyn Agent>);

        // Initialize metrics collectors
        let mut metrics_map = HashMap::new();
        for domain in &["health", "career", "relationships", "finance", "learning", "spirituality"] {
            metrics_map.insert(domain.to_string(), MetricsCollector::new());
        }

        // Get initial balance status
        let mut states = Vec::new();
        for agent in agents_map.values() {
            states.push(agent.get_state());
        }

        let balance = BalancingEngine::analyze_balance(&states);

        info!("System initialized with 6 agents. Balance score: {:.2}", balance.overall_score);

        Ok(Self {
            agents: Arc::new(RwLock::new(agents_map)),
            tools: Arc::new(MCPToolRegistry::new()),
            metrics: Arc::new(RwLock::new(metrics_map)),
            balance: Arc::new(RwLock::new(balance)),
            audit_trail: Arc::new(RwLock::new(AuditTrail::new())),
            outcomes: Arc::new(RwLock::new(Vec::new())),
        })
    }

    /// Run a reasoning cycle for all agents
    pub async fn reasoning_cycle(&mut self) -> Result<HashMap<String, Vec<Action>>> {
        debug!("Starting reasoning cycle");

        let agents = self.agents.read().await;
        let mut all_actions = HashMap::new();

        for (domain_id, agent) in agents.iter() {
            let state = agent.get_state();
            let analysis = ReasoningEngine::analyze_state(&state);
            let actions = ReasoningEngine::plan_actions(&analysis);

            all_actions.insert(domain_id.clone(), actions);
        }

        Ok(all_actions)
    }

    /// Execute all planned actions for agents
    pub async fn execution_cycle(&mut self, all_actions: HashMap<String, Vec<Action>>) -> Result<()> {
        debug!("Starting execution cycle");

        let mut agents = self.agents.write().await;
        let mut outcomes_list = self.outcomes.write().await;

        for (domain_id, actions) in all_actions {
            if let Some(agent) = agents.get_mut(domain_id.as_str()) {
                for action in actions {
                    match agent.execute_action(action.clone()).await {
                        Ok(impact) => {
                            // Record learning outcome
                            let outcome = ImprovementEngine::learn_outcome(&action, impact);
                            outcomes_list.push(outcome);

                            // Learn from outcome
                            let _ = agent.learn_from_outcome(&action, impact).await;

                            info!(
                                "Action '{}' in domain {} completed with impact {:.2}",
                                action.name, domain_id, impact
                            );
                        }
                        Err(e) => {
                            warn!("Action failed in {}: {}", domain_id, e);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Run domain balancing and consensus
    pub async fn balance_domains(&mut self) -> Result<()> {
        debug!("Starting domain balancing");

        let agents = self.agents.read().await;
        let mut states = Vec::new();

        for agent in agents.values() {
            states.push(agent.get_state());
        }

        let balance = BalancingEngine::analyze_balance(&states);

        if BalancingEngine::needs_rebalancing(&balance) {
            info!("Rebalancing needed. Critical domains: {:?}", balance.critical_domains);

            let proposal = BalancingEngine::create_rebalancing_proposal(&balance);

            // Collect votes from all agents
            let mut vote_count = 0;
            for agent in agents.values() {
                if agent.vote_on_proposal(&proposal.description).await.unwrap_or(true) {
                    vote_count += 1;
                }
            }

            if vote_count as f64 / agents.len() as f64 > 0.66 {
                info!("Rebalancing proposal approved. Applying changes.");

                // Apply rebalancing
                let mut agents_mut = drop(agents);
                let mut agents_mut = self.agents.write().await;
                let mut states_mut = Vec::new();

                for agent in agents_mut.values() {
                    states_mut.push(agent.get_state());
                }

                BalancingEngine::apply_rebalancing(&mut states_mut, &proposal)?;

                // Record in audit trail
                let mut trail = self.audit_trail.write().await;
                trail.record(AuditEntry::new(
                    "rebalancing_applied".to_string(),
                    "system".to_string(),
                    "all".to_string(),
                    serde_json::json!({
                        "critical_domains": balance.critical_domains,
                        "proposal": proposal.description
                    }),
                ));
            }
        }

        let mut balance_write = self.balance.write().await;
        *balance_write = balance;

        Ok(())
    }

    /// Run full improvement cycle
    pub async fn improvement_cycle(&mut self) -> Result<()> {
        debug!("Starting improvement cycle");

        let outcomes = self.outcomes.read().await;

        if outcomes.is_empty() {
            return Ok(());
        }

        // Analyze patterns
        let patterns = ImprovementEngine::analyze_patterns(&outcomes);
        let recommendations = ImprovementEngine::recommend_actions(&patterns, 3);

        info!("Improvement cycle: {} patterns learned, {} actions recommended", patterns.len(), recommendations.len());

        Ok(())
    }

    /// Get system status
    pub async fn get_status(&self) -> HashMap<String, serde_json::Value> {
        let agents = self.agents.read().await;
        let balance = self.balance.read().await;
        let outcomes = self.outcomes.read().await;

        let mut status = HashMap::new();

        // Agent statuses
        let mut agent_statuses = HashMap::new();
        for (domain_id, agent) in agents.iter() {
            agent_statuses.insert(domain_id.clone(), agent.report_status().await);
        }

        status.insert("agents".to_string(), serde_json::to_value(agent_statuses).unwrap());

        // Balance status
        status.insert("balance".to_string(), serde_json::json!({
            "overall_score": balance.overall_score,
            "variance": balance.variance,
            "imbalanced_domains": balance.imbalanced_domains,
            "critical_domains": balance.critical_domains,
        }));

        // Learning status
        status.insert("learning".to_string(), serde_json::json!({
            "outcomes_recorded": outcomes.len(),
            "effective_outcomes": outcomes.iter().filter(|o| o.is_effective()).count(),
        }));

        // Tool status
        status.insert("mcp_tools".to_string(), serde_json::json!({
            "total": self.tools.count(),
            "by_domain": {
                "health": self.tools.get_tools_for_domain("health").len(),
                "career": self.tools.get_tools_for_domain("career").len(),
                "relationships": self.tools.get_tools_for_domain("relationships").len(),
                "finance": self.tools.get_tools_for_domain("finance").len(),
                "learning": self.tools.get_tools_for_domain("learning").len(),
                "spirituality": self.tools.get_tools_for_domain("spirituality").len(),
            }
        }));

        status
    }

    /// Save all agent states to disk
    pub async fn save_state(&self, path: &str) -> Result<()> {
        let agents = self.agents.read().await;
        let mut states = Vec::new();

        for agent in agents.values() {
            states.push(agent.get_state());
        }

        PersistenceManager::save_all_states(&states, path).await?;
        info!("Agent states saved to {}", path);

        Ok(())
    }

    /// Load agent states from disk
    pub async fn load_state(&mut self, path: &str) -> Result<()> {
        let states = PersistenceManager::load_all_states(path).await?;
        info!("Loaded {} agent states from {}", states.len(), path);

        Ok(())
    }

    /// Run a complete daily cycle
    pub async fn run_daily_cycle(&mut self) -> Result<()> {
        info!("=== STARTING DAILY CYCLE ===");

        // Phase 1: Reasoning
        let actions = self.reasoning_cycle().await?;
        info!("Reasoning phase complete. {} actions planned", actions.values().map(|a| a.len()).sum::<usize>());

        // Phase 2: Execution
        self.execution_cycle(actions).await?;
        info!("Execution phase complete");

        // Phase 3: Balancing
        self.balance_domains().await?;
        info!("Balancing phase complete");

        // Phase 4: Improvement
        self.improvement_cycle().await?;
        info!("Improvement phase complete");

        // Get final status
        let status = self.get_status().await;
        info!("=== CYCLE COMPLETE ===");

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_system_creation() {
        let system = LifeDomainsSystem::new().await;
        assert!(system.is_ok());
    }

    #[tokio::test]
    async fn test_system_initialization() {
        let system = LifeDomainsSystem::new().await.unwrap();
        let status = system.get_status().await;

        assert!(status.contains_key("agents"));
        assert!(status.contains_key("balance"));
        assert!(status.contains_key("mcp_tools"));
    }

    #[tokio::test]
    async fn test_reasoning_cycle() {
        let mut system = LifeDomainsSystem::new().await.unwrap();
        let result = system.reasoning_cycle().await;

        assert!(result.is_ok());
        let actions = result.unwrap();
        assert!(!actions.is_empty());
    }
}
