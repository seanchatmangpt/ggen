//! Finance Domain Agent

use super::base::{AgentBase, AgentStatus};
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FinanceState {
    monthly_income: f64,
    monthly_savings: f64,
    net_worth: f64,
    investment_return: f64,
}

/// Finance Domain Agent - manages personal financial resources
pub struct FinanceAgent {
    domain_id: String,
    state: Arc<RwLock<FinanceState>>,
    goals: Arc<RwLock<Vec<String>>>,
}

impl FinanceAgent {
    /// Create new finance agent
    pub fn new(domain_id: &str) -> Self {
        Self {
            domain_id: domain_id.to_string(),
            state: Arc::new(RwLock::new(FinanceState {
                monthly_income: 5000.0,
                monthly_savings: 500.0,
                net_worth: 50000.0,
                investment_return: 0.07,
            })),
            goals: Arc::new(RwLock::new(vec![])),
        }
    }

    fn calculate_health_score(state: &FinanceState) -> f64 {
        let savings_rate = state.monthly_savings / state.monthly_income;
        let net_worth_score = (state.net_worth / 200000.0).min(1.0);
        let investment_score = (state.investment_return * 10.0).min(1.0);
        
        ((savings_rate.min(1.0)) + net_worth_score + investment_score) / 3.0
    }
}

#[async_trait]
impl AgentBase for FinanceAgent {
    async fn get_status(&self) -> anyhow::Result<AgentStatus> {
        let state = self.state.read().await;
        let goals = self.goals.read().await;
        
        Ok(AgentStatus {
            domain_id: self.domain_id.clone(),
            health_score: Self::calculate_health_score(&state),
            goals: goals.clone(),
            current_actions: vec!["budgeting".to_string(), "investing".to_string()],
            recent_outcomes: vec!["savings_increased".to_string()],
        })
    }

    async fn set_goals(&self, goals: Vec<String>) -> anyhow::Result<()> {
        info!("Finance Agent: setting goals: {:?}", goals);
        let mut agent_goals = self.goals.write().await;
        *agent_goals = goals;
        Ok(())
    }

    async fn allocate_resources(&self, allocation: f64) -> anyhow::Result<()> {
        info!("Finance Agent: allocating resources: {}", allocation);
        Ok(())
    }

    async fn recommend_improvements(&self) -> anyhow::Result<Vec<String>> {
        let state = self.state.read().await;
        let mut recommendations = vec![];
        let savings_rate = state.monthly_savings / state.monthly_income;
        
        if savings_rate < 0.2 {
            recommendations.push("Increase savings rate to 20%".to_string());
        }
        if state.net_worth < 100000.0 {
            recommendations.push("Build emergency fund".to_string());
        }
        if state.investment_return < 0.08 {
            recommendations.push("Optimize investment portfolio".to_string());
        }
        
        Ok(recommendations)
    }

    async fn execute_action(&self, action: &str) -> anyhow::Result<String> {
        info!("Finance Agent: executing action: {}", action);
        
        match action {
            "increase_savings" => {
                let mut state = self.state.write().await;
                state.monthly_savings += 100.0;
                state.net_worth += 100.0;
                Ok("Increased monthly savings".to_string())
            }
            "invest" => {
                let mut state = self.state.write().await;
                let growth = state.net_worth * state.investment_return;
                state.net_worth += growth;
                Ok(format!("Investment returned ${:.2}", growth))
            }
            "budget_plan" => {
                let mut state = self.state.write().await;
                state.monthly_savings = (state.monthly_income * 0.25).min(state.monthly_savings + 200.0);
                Ok("Created budget plan".to_string())
            }
            _ => Ok("Action executed".to_string()),
        }
    }
}
