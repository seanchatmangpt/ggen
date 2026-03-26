//! Autonomous Reasoning Engine
//! 
//! Agents analyze their domain state and autonomously:
//! - Discover own goals
//! - Plan actions
//! - Call MCP tools
//! - Learn from results

use crate::agents::AgentStatus;
use serde::{Deserialize, Serialize};
use tracing::info;

/// Reasoning result from autonomous analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutonomousReasoning {
    pub domain_id: String,
    pub health_score: f64,
    pub recommended_goals: Vec<String>,
    pub action_plan: Vec<String>,
    pub priority_level: String,
}

impl AutonomousReasoning {
    /// Analyze agent status and generate reasoning
    pub fn analyze(status: &AgentStatus) -> anyhow::Result<Self> {
        info!("Autonomous reasoning for domain: {}", status.domain_id);
        
        let (goals, actions, priority) = match status.domain_id.as_str() {
            "health" => {
                if status.health_score < 0.7 {
                    (
                        vec![
                            "increase_exercise".to_string(),
                            "improve_sleep".to_string(),
                        ],
                        vec![
                            "workout".to_string(),
                            "sleep_8hrs".to_string(),
                        ],
                        "high".to_string(),
                    )
                } else {
                    (
                        vec!["maintain_wellness".to_string()],
                        vec!["meditation".to_string()],
                        "medium".to_string(),
                    )
                }
            }
            "career" => {
                if status.health_score < 0.65 {
                    (
                        vec![
                            "develop_skills".to_string(),
                            "expand_network".to_string(),
                        ],
                        vec![
                            "learn_skill".to_string(),
                            "network_event".to_string(),
                        ],
                        "high".to_string(),
                    )
                } else {
                    (
                        vec!["advance_career".to_string()],
                        vec!["complete_project".to_string()],
                        "medium".to_string(),
                    )
                }
            }
            "relationships" => {
                if status.health_score < 0.65 {
                    (
                        vec!["strengthen_bonds".to_string()],
                        vec!["quality_time".to_string(), "deep_conversation".to_string()],
                        "high".to_string(),
                    )
                } else {
                    (
                        vec!["maintain_connections".to_string()],
                        vec!["community_event".to_string()],
                        "medium".to_string(),
                    )
                }
            }
            "finance" => {
                if status.health_score < 0.7 {
                    (
                        vec!["increase_savings".to_string()],
                        vec!["increase_savings".to_string(), "budget_plan".to_string()],
                        "high".to_string(),
                    )
                } else {
                    (
                        vec!["grow_wealth".to_string()],
                        vec!["invest".to_string()],
                        "medium".to_string(),
                    )
                }
            }
            "learning" => {
                if status.health_score < 0.65 {
                    (
                        vec![
                            "acquire_skills".to_string(),
                            "complete_courses".to_string(),
                        ],
                        vec![
                            "complete_course".to_string(),
                            "practice_skill".to_string(),
                        ],
                        "high".to_string(),
                    )
                } else {
                    (
                        vec!["continuous_improvement".to_string()],
                        vec!["read_book".to_string(), "earn_certification".to_string()],
                        "medium".to_string(),
                    )
                }
            }
            "spirituality" => {
                if status.health_score < 0.65 {
                    (
                        vec!["increase_mindfulness".to_string()],
                        vec!["meditate_20min".to_string()],
                        "high".to_string(),
                    )
                } else {
                    (
                        vec!["deepen_practice".to_string()],
                        vec!["reflection".to_string(), "mindfulness_practice".to_string()],
                        "medium".to_string(),
                    )
                }
            }
            _ => {
                (
                    vec!["maintain_balance".to_string()],
                    vec![],
                    "low".to_string(),
                )
            }
        };
        
        Ok(Self {
            domain_id: status.domain_id.clone(),
            health_score: status.health_score,
            recommended_goals: goals,
            action_plan: actions,
            priority_level: priority,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reasoning_high_health_score() {
        let status = AgentStatus {
            domain_id: "health".to_string(),
            health_score: 0.8,
            goals: vec![],
            current_actions: vec![],
            recent_outcomes: vec![],
        };
        
        let reasoning = AutonomousReasoning::analyze(&status).unwrap();
        assert_eq!(reasoning.priority_level, "medium");
    }

    #[test]
    fn test_reasoning_low_health_score() {
        let status = AgentStatus {
            domain_id: "career".to_string(),
            health_score: 0.5,
            goals: vec![],
            current_actions: vec![],
            recent_outcomes: vec![],
        };
        
        let reasoning = AutonomousReasoning::analyze(&status).unwrap();
        assert_eq!(reasoning.priority_level, "high");
        assert!(!reasoning.action_plan.is_empty());
    }
}
