//! Career Domain Agent

use super::base::{AgentBase, AgentStatus};
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CareerState {
    skills_count: u32,
    salary: f64,
    network_size: u32,
    projects_completed: u32,
}

/// Career Domain Agent - manages professional growth
pub struct CareerAgent {
    domain_id: String,
    state: Arc<RwLock<CareerState>>,
    goals: Arc<RwLock<Vec<String>>>,
}

impl CareerAgent {
    /// Create new career agent
    pub fn new(domain_id: &str) -> Self {
        Self {
            domain_id: domain_id.to_string(),
            state: Arc::new(RwLock::new(CareerState {
                skills_count: 5,
                salary: 80000.0,
                network_size: 20,
                projects_completed: 3,
            })),
            goals: Arc::new(RwLock::new(vec![])),
        }
    }

    fn calculate_health_score(state: &CareerState) -> f64 {
        // Career score: based on skills, salary growth, network, projects
        let skill_score = (state.skills_count as f64 / 20.0).min(1.0);
        let salary_score = (state.salary / 150000.0).min(1.0);
        let network_score = (state.network_size as f64 / 100.0).min(1.0);
        let project_score = (state.projects_completed as f64 / 10.0).min(1.0);
        
        (skill_score + salary_score + network_score + project_score) / 4.0
    }
}

#[async_trait]
impl AgentBase for CareerAgent {
    async fn get_status(&self) -> anyhow::Result<AgentStatus> {
        let state = self.state.read().await;
        let goals = self.goals.read().await;
        
        Ok(AgentStatus {
            domain_id: self.domain_id.clone(),
            health_score: Self::calculate_health_score(&state),
            goals: goals.clone(),
            current_actions: vec!["skill_development".to_string()],
            recent_outcomes: vec!["completed_project".to_string()],
        })
    }

    async fn set_goals(&self, goals: Vec<String>) -> anyhow::Result<()> {
        info!("Career Agent: setting goals: {:?}", goals);
        let mut agent_goals = self.goals.write().await;
        *agent_goals = goals;
        Ok(())
    }

    async fn allocate_resources(&self, allocation: f64) -> anyhow::Result<()> {
        info!("Career Agent: allocating resources: {}", allocation);
        Ok(())
    }

    async fn recommend_improvements(&self) -> anyhow::Result<Vec<String>> {
        let state = self.state.read().await;
        let mut recommendations = vec![];
        
        if state.skills_count < 10 {
            recommendations.push("Learn new technical skills".to_string());
        }
        if state.network_size < 50 {
            recommendations.push("Expand professional network".to_string());
        }
        if state.projects_completed < 5 {
            recommendations.push("Complete more projects".to_string());
        }
        
        Ok(recommendations)
    }

    async fn execute_action(&self, action: &str) -> anyhow::Result<String> {
        info!("Career Agent: executing action: {}", action);
        
        match action {
            "learn_skill" => {
                let mut state = self.state.write().await;
                state.skills_count += 1;
                Ok("Learned new skill".to_string())
            }
            "complete_project" => {
                let mut state = self.state.write().await;
                state.projects_completed += 1;
                state.salary += 5000.0;
                Ok("Completed project and received raise".to_string())
            }
            "network_event" => {
                let mut state = self.state.write().await;
                state.network_size += 5;
                Ok("Expanded network by 5 connections".to_string())
            }
            _ => Ok("Action executed".to_string()),
        }
    }
}
