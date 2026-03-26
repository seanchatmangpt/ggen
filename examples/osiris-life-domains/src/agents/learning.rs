//! Learning Domain Agent

use super::base::{AgentBase, AgentStatus};
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LearningState {
    courses_completed: u32,
    books_read: u32,
    hours_practiced: f64,
    certifications: u32,
}

/// Learning Domain Agent - manages continuous learning and self-improvement
pub struct LearningAgent {
    domain_id: String,
    state: Arc<RwLock<LearningState>>,
    goals: Arc<RwLock<Vec<String>>>,
}

impl LearningAgent {
    /// Create new learning agent
    pub fn new(domain_id: &str) -> Self {
        Self {
            domain_id: domain_id.to_string(),
            state: Arc::new(RwLock::new(LearningState {
                courses_completed: 2,
                books_read: 5,
                hours_practiced: 50.0,
                certifications: 1,
            })),
            goals: Arc::new(RwLock::new(vec![])),
        }
    }

    fn calculate_health_score(state: &LearningState) -> f64 {
        let courses_score = (state.courses_completed as f64 / 10.0).min(1.0);
        let books_score = (state.books_read as f64 / 20.0).min(1.0);
        let practice_score = (state.hours_practiced / 200.0).min(1.0);
        let cert_score = (state.certifications as f64 / 5.0).min(1.0);
        
        (courses_score + books_score + practice_score + cert_score) / 4.0
    }
}

#[async_trait]
impl AgentBase for LearningAgent {
    async fn get_status(&self) -> anyhow::Result<AgentStatus> {
        let state = self.state.read().await;
        let goals = self.goals.read().await;
        
        Ok(AgentStatus {
            domain_id: self.domain_id.clone(),
            health_score: Self::calculate_health_score(&state),
            goals: goals.clone(),
            current_actions: vec!["studying".to_string(), "practicing".to_string()],
            recent_outcomes: vec!["course_completed".to_string()],
        })
    }

    async fn set_goals(&self, goals: Vec<String>) -> anyhow::Result<()> {
        info!("Learning Agent: setting goals: {:?}", goals);
        let mut agent_goals = self.goals.write().await;
        *agent_goals = goals;
        Ok(())
    }

    async fn allocate_resources(&self, allocation: f64) -> anyhow::Result<()> {
        info!("Learning Agent: allocating resources: {}", allocation);
        Ok(())
    }

    async fn recommend_improvements(&self) -> anyhow::Result<Vec<String>> {
        let state = self.state.read().await;
        let mut recommendations = vec![];
        
        if state.courses_completed < 5 {
            recommendations.push("Take more online courses".to_string());
        }
        if state.books_read < 10 {
            recommendations.push("Read more books in your field".to_string());
        }
        if state.hours_practiced < 100.0 {
            recommendations.push("Increase deliberate practice hours".to_string());
        }
        
        Ok(recommendations)
    }

    async fn execute_action(&self, action: &str) -> anyhow::Result<String> {
        info!("Learning Agent: executing action: {}", action);
        
        match action {
            "complete_course" => {
                let mut state = self.state.write().await;
                state.courses_completed += 1;
                Ok("Completed course".to_string())
            }
            "read_book" => {
                let mut state = self.state.write().await;
                state.books_read += 1;
                Ok("Finished reading book".to_string())
            }
            "practice_skill" => {
                let mut state = self.state.write().await;
                state.hours_practiced += 5.0;
                Ok("Practiced skill for 5 hours".to_string())
            }
            "earn_certification" => {
                let mut state = self.state.write().await;
                state.certifications += 1;
                Ok("Earned new certification".to_string())
            }
            _ => Ok("Action executed".to_string()),
        }
    }
}
