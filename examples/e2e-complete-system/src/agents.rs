/// Domain Agent Implementations
/// Each agent independently analyzes its life domain
use crate::orchestrator::{DomainAssessment, LifeDomain};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::Mutex;
use uuid::Uuid;

/// State for a domain agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentState {
    pub agent_id: String,
    pub domain: LifeDomain,
    pub health_score: f64,
    pub last_update: chrono::DateTime<chrono::Utc>,
}

/// A domain agent that independently analyzes its domain
#[derive(Clone)]
pub struct DomainAgent {
    pub state: Arc<Mutex<AgentState>>,
}

impl DomainAgent {
    /// Create a new domain agent
    pub async fn new(domain: LifeDomain) -> Result<Self> {
        let agent_id = Uuid::new_v4().to_string();
        let state = AgentState {
            agent_id,
            domain,
            health_score: 0.5,
            last_update: chrono::Utc::now(),
        };

        Ok(Self {
            state: Arc::new(Mutex::new(state)),
        })
    }

    /// Analyze this domain and return assessment
    pub async fn analyze(&self) -> Result<DomainAssessment> {
        let state = self.state.lock().await;
        let health_score = generate_realistic_score(&state.domain);

        let (issues, opportunities) = generate_issues_and_opportunities(&state.domain);

        Ok(DomainAssessment {
            domain: state.domain.clone(),
            health_score,
            key_issues: issues,
            opportunities,
            agent_id: state.agent_id.clone(),
        })
    }

    /// Get current health score
    pub async fn get_health_score(&self) -> f64 {
        self.state.lock().await.health_score
    }

    /// Update state (simulating recovery after crash)
    pub async fn restore_state(&self, health_score: f64) -> Result<()> {
        let mut state = self.state.lock().await;
        state.health_score = health_score;
        state.last_update = chrono::Utc::now();
        Ok(())
    }
}

/// Generate realistic health scores for different domains
fn generate_realistic_score(domain: &LifeDomain) -> f64 {
    match domain {
        LifeDomain::Health => 0.45,
        LifeDomain::Career => 0.72,
        LifeDomain::Relationships => 0.65,
        LifeDomain::Finance => 0.58,
        LifeDomain::Learning => 0.40,
        LifeDomain::Leisure => 0.35,
    }
}

/// Generate realistic issues and opportunities
fn generate_issues_and_opportunities(domain: &LifeDomain) -> (Vec<String>, Vec<String>) {
    match domain {
        LifeDomain::Health => (
            vec![
                "sleep irregular".to_string(),
                "exercise 2x/week".to_string(),
            ],
            vec!["improve fitness routine".to_string()],
        ),
        LifeDomain::Career => (
            vec!["salary review pending".to_string()],
            vec!["potential promotion".to_string()],
        ),
        LifeDomain::Relationships => (
            vec!["close family".to_string()],
            vec!["dating opportunities".to_string()],
        ),
        LifeDomain::Finance => (
            vec!["adequate savings".to_string()],
            vec!["investment optimization".to_string()],
        ),
        LifeDomain::Learning => (
            vec!["skills stale".to_string()],
            vec!["online courses available".to_string()],
        ),
        LifeDomain::Leisure => (
            vec!["overworked".to_string()],
            vec!["vacation opportunities".to_string()],
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_agent_creation() {
        let agent = DomainAgent::new(LifeDomain::Health)
            .await
            .expect("Failed to create agent");
        assert_eq!(agent.get_health_score().await, 0.45);
    }

    #[tokio::test]
    async fn test_agent_analysis() {
        let agent = DomainAgent::new(LifeDomain::Career)
            .await
            .expect("Failed to create agent");
        let assessment = agent.analyze().await.expect("Failed to analyze");
        assert_eq!(assessment.domain, LifeDomain::Career);
        assert_eq!(assessment.health_score, 0.72);
    }

    #[tokio::test]
    async fn test_agent_state_restore() {
        let agent = DomainAgent::new(LifeDomain::Health)
            .await
            .expect("Failed to create agent");
        agent.restore_state(0.75).await.expect("Failed to restore");
        assert_eq!(agent.get_health_score().await, 0.75);
    }
}
