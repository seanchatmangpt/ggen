//! Health Domain Agent

use super::base::{AgentBase, AgentStatus};
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct HealthState {
    exercise_minutes: f64,
    sleep_hours: f64,
    stress_level: f64,
    energy_level: f64,
}

/// Health Domain Agent - manages wellness
pub struct HealthAgent {
    domain_id: String,
    state: Arc<RwLock<HealthState>>,
    goals: Arc<RwLock<Vec<String>>>,
}

impl HealthAgent {
    /// Create new health agent
    pub fn new(domain_id: &str) -> Self {
        Self {
            domain_id: domain_id.to_string(),
            state: Arc::new(RwLock::new(HealthState {
                exercise_minutes: 0.0,
                sleep_hours: 6.0,
                stress_level: 0.5,
                energy_level: 0.6,
            })),
            goals: Arc::new(RwLock::new(vec![])),
        }
    }

    fn calculate_health_score(state: &HealthState) -> f64 {
        // Health score: 0-1, based on exercise, sleep, stress, energy
        let exercise_score = (state.exercise_minutes / 150.0).min(1.0);
        let sleep_score = (state.sleep_hours / 8.0).min(1.0);
        let stress_score = 1.0 - state.stress_level;
        let energy_score = state.energy_level;
        
        (exercise_score + sleep_score + stress_score + energy_score) / 4.0
    }
}

#[async_trait]
impl AgentBase for HealthAgent {
    async fn get_status(&self) -> anyhow::Result<AgentStatus> {
        let state = self.state.read().await;
        let goals = self.goals.read().await;
        
        Ok(AgentStatus {
            domain_id: self.domain_id.clone(),
            health_score: Self::calculate_health_score(&state),
            goals: goals.clone(),
            current_actions: vec!["monitoring_vitals".to_string()],
            recent_outcomes: vec!["improved_sleep_quality".to_string()],
        })
    }

    async fn set_goals(&self, goals: Vec<String>) -> anyhow::Result<()> {
        info!("Health Agent: setting goals: {:?}", goals);
        let mut agent_goals = self.goals.write().await;
        *agent_goals = goals;
        Ok(())
    }

    async fn allocate_resources(&self, allocation: f64) -> anyhow::Result<()> {
        info!("Health Agent: allocating resources: {}", allocation);
        // In real system, allocation would affect goal setting
        Ok(())
    }

    async fn recommend_improvements(&self) -> anyhow::Result<Vec<String>> {
        let state = self.state.read().await;
        let mut recommendations = vec![];
        
        if state.exercise_minutes < 150.0 {
            recommendations.push("Increase exercise to 150+ min/week".to_string());
        }
        if state.sleep_hours < 7.0 {
            recommendations.push("Aim for 7-8 hours of sleep".to_string());
        }
        if state.stress_level > 0.6 {
            recommendations.push("Practice stress management techniques".to_string());
        }
        
        Ok(recommendations)
    }

    async fn execute_action(&self, action: &str) -> anyhow::Result<String> {
        info!("Health Agent: executing action: {}", action);
        
        match action {
            "workout" => {
                let mut state = self.state.write().await;
                state.exercise_minutes += 30.0;
                Ok("Completed 30-minute workout".to_string())
            }
            "sleep_8hrs" => {
                let mut state = self.state.write().await;
                state.sleep_hours = 8.0;
                Ok("Achieved 8 hours of sleep".to_string())
            }
            "meditation" => {
                let mut state = self.state.write().await;
                state.stress_level = (state.stress_level - 0.1).max(0.0);
                Ok("Completed 20-minute meditation".to_string())
            }
            _ => Ok("Action executed".to_string()),
        }
    }
}
