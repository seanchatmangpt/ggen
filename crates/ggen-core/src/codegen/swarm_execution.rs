use crate::manifest::{GenerationRule, QuerySource, TemplateSource};
use ggen_utils::error::{Error, Result};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct Agent {
    pub id: String,
    pub capability_level: u8,
    pub assigned_rules: Vec<String>,
    pub completed_rules: Vec<String>,
    pub failed_rules: Vec<String>,
}

pub struct SwarmCoordinator {
    agents: Arc<RwLock<HashMap<String, Agent>>>,
    task_queue: Arc<RwLock<VecDeque<GenerationRule>>>,
    rule_assignments: Arc<RwLock<HashMap<String, String>>>, // rule_name -> agent_id
}

impl SwarmCoordinator {
    pub fn new(agent_count: usize) -> Self {
        let mut agents = HashMap::new();

        for i in 0..agent_count {
            let agent_id = format!("agent-{}", i);
            agents.insert(
                agent_id.clone(),
                Agent {
                    id: agent_id,
                    capability_level: ((i % 3) + 1) as u8,
                    assigned_rules: vec![],
                    completed_rules: vec![],
                    failed_rules: vec![],
                },
            );
        }

        Self {
            agents: Arc::new(RwLock::new(agents)),
            task_queue: Arc::new(RwLock::new(VecDeque::new())),
            rule_assignments: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn enqueue_rules(&self, rules: Vec<GenerationRule>) -> Result<()> {
        let mut queue = self.task_queue.write().await;
        for rule in rules {
            queue.push_back(rule);
        }
        Ok(())
    }

    pub async fn distribute_work(&self) -> Result<()> {
        let mut queue = self.task_queue.write().await;
        let mut agents = self.agents.write().await;
        let mut assignments = self.rule_assignments.write().await;

        // Priority: assign to agents with lowest workload first
        while let Some(rule) = queue.pop_front() {
            // Find agent with least assigned work
            let best_agent = agents
                .values_mut()
                .min_by_key(|a| a.assigned_rules.len())
                .ok_or_else(|| Error::new("No agents available"))?;

            best_agent.assigned_rules.push(rule.name.clone());
            assignments.insert(rule.name, best_agent.id.clone());
        }

        Ok(())
    }

    pub async fn record_completion(&self, rule_name: &str) -> Result<()> {
        let assignments = self.rule_assignments.read().await;
        let agent_id = assignments
            .get(rule_name)
            .ok_or_else(|| Error::new("Rule not assigned"))?
            .clone();

        let mut agents = self.agents.write().await;
        if let Some(agent) = agents.get_mut(&agent_id) {
            agent.assigned_rules.retain(|r| r != rule_name);
            agent.completed_rules.push(rule_name.to_string());
        }

        Ok(())
    }

    pub async fn record_failure(&self, rule_name: &str) -> Result<()> {
        let assignments = self.rule_assignments.read().await;
        let agent_id = assignments
            .get(rule_name)
            .ok_or_else(|| Error::new("Rule not assigned"))?
            .clone();

        let mut agents = self.agents.write().await;
        if let Some(agent) = agents.get_mut(&agent_id) {
            agent.assigned_rules.retain(|r| r != rule_name);
            agent.failed_rules.push(rule_name.to_string());
        }

        Ok(())
    }

    pub async fn get_agent_status(&self, agent_id: &str) -> Result<AgentStatus> {
        let agents = self.agents.read().await;
        let agent = agents
            .get(agent_id)
            .ok_or_else(|| Error::new("Agent not found"))?;

        Ok(AgentStatus {
            id: agent.id.clone(),
            capability_level: agent.capability_level,
            assigned_count: agent.assigned_rules.len(),
            completed_count: agent.completed_rules.len(),
            failed_count: agent.failed_rules.len(),
            efficiency: Self::calculate_efficiency(
                agent.completed_rules.len(),
                agent.failed_rules.len(),
            ),
        })
    }

    pub async fn get_swarm_summary(&self) -> Result<SwarmSummary> {
        let agents = self.agents.read().await;

        let total_assigned = agents.values().map(|a| a.assigned_rules.len()).sum();
        let total_completed = agents.values().map(|a| a.completed_rules.len()).sum();
        let total_failed = agents.values().map(|a| a.failed_rules.len()).sum();

        let idle_agents = agents.values().filter(|a| a.assigned_rules.is_empty()).count();

        Ok(SwarmSummary {
            total_agents: agents.len(),
            active_agents: agents.len() - idle_agents,
            idle_agents,
            total_assigned,
            total_completed,
            total_failed,
            utilization: if agents.is_empty() {
                0.0
            } else {
                (total_assigned as f64 / agents.len() as f64) * 100.0
            },
        })
    }

    fn calculate_efficiency(completed: usize, failed: usize) -> f64 {
        let total = completed + failed;
        if total == 0 {
            100.0
        } else {
            (completed as f64 / total as f64) * 100.0
        }
    }

    pub async fn rebalance_work(&self) -> Result<()> {
        let mut agents = self.agents.write().await;

        // Find overloaded agents (assigned > 2x average)
        let avg_load = agents
            .values()
            .map(|a| a.assigned_rules.len())
            .sum::<usize>() as f64
            / agents.len() as f64;

        for agent in agents.values_mut() {
            if agent.assigned_rules.len() as f64 > avg_load * 2.0 {
                // This agent is overloaded, but actual redistribution
                // happens in distribute_work() after queue updates
                agent.assigned_rules.clear();
            }
        }

        Ok(())
    }
}

pub struct AgentStatus {
    pub id: String,
    pub capability_level: u8,
    pub assigned_count: usize,
    pub completed_count: usize,
    pub failed_count: usize,
    pub efficiency: f64,
}

pub struct SwarmSummary {
    pub total_agents: usize,
    pub active_agents: usize,
    pub idle_agents: usize,
    pub total_assigned: usize,
    pub total_completed: usize,
    pub total_failed: usize,
    pub utilization: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_swarm_coordinator_creation() {
        let swarm = SwarmCoordinator::new(4);
        let summary = swarm.get_swarm_summary().await.unwrap();

        assert_eq!(summary.total_agents, 4);
        assert_eq!(summary.active_agents, 0);
        assert_eq!(summary.idle_agents, 4);
    }

    #[tokio::test]
    async fn test_agent_status_retrieval() {
        let swarm = SwarmCoordinator::new(2);
        let status = swarm.get_agent_status("agent-0").await.unwrap();

        assert_eq!(status.id, "agent-0");
        assert_eq!(status.capability_level, 1);
        assert_eq!(status.efficiency, 100.0);
    }

    #[tokio::test]
    async fn test_work_distribution() {
        let swarm = SwarmCoordinator::new(3);

        let rules = vec![
            GenerationRule {
                name: "rule-1".to_string(),
                query: QuerySource::Inline { inline: "SELECT * WHERE {}".to_string() },
                template: TemplateSource::Inline { inline: "t.tera".to_string() },
                output_file: "out.rs".to_string(),
                skip_empty: false,
                mode: Default::default(),
            },
            GenerationRule {
                name: "rule-2".to_string(),
                query: QuerySource::Inline { inline: "SELECT * WHERE {}".to_string() },
                template: TemplateSource::Inline { inline: "t.tera".to_string() },
                output_file: "out.rs".to_string(),
                skip_empty: false,
                mode: Default::default(),
            },
        ];

        swarm.enqueue_rules(rules).await.unwrap();
        swarm.distribute_work().await.unwrap();

        let summary = swarm.get_swarm_summary().await.unwrap();
        assert_eq!(summary.total_assigned, 2);
    }

    #[tokio::test]
    async fn test_completion_tracking() {
        let swarm = SwarmCoordinator::new(2);

        let rules = vec![GenerationRule {
            name: "rule-1".to_string(),
            query: QuerySource::Inline { inline: "SELECT * WHERE {}".to_string() },
            template: TemplateSource::Inline { inline: "t.tera".to_string() },
            output_file: "out.rs".to_string(),
            skip_empty: false,
            mode: Default::default(),
        }];

        swarm.enqueue_rules(rules).await.unwrap();
        swarm.distribute_work().await.unwrap();
        swarm.record_completion("rule-1").await.unwrap();

        let summary = swarm.get_swarm_summary().await.unwrap();
        assert_eq!(summary.total_assigned, 0);
        assert_eq!(summary.total_completed, 1);
    }

    #[test]
    fn test_efficiency_calculation() {
        assert_eq!(SwarmCoordinator::calculate_efficiency(10, 0), 100.0);
        assert_eq!(SwarmCoordinator::calculate_efficiency(5, 5), 50.0);
        assert_eq!(SwarmCoordinator::calculate_efficiency(0, 0), 100.0);
    }
}
