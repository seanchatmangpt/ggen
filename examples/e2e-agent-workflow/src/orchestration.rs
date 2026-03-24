//! Orchestration layer coordinating all components
//!
//! Brings together agents, domains, tools, and consensus into a cohesive system.

use crate::error::Result;
use crate::{
    agent::{Agent, AgentManager, AgentState},
    consensus::{ConsensusManager, Proposal, ProposalStatus},
    domain::{DomainManager, LifeDomain},
    tools::{ToolDiscovery, ToolRegistry},
    workflow::{WorkflowEngine, WorkflowPhase},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Re-export workflow state
pub use crate::workflow::WorkflowPhase as WorkflowState;

/// Orchestration engine managing all subsystems
pub struct OrchestrationEngine {
    workflow: WorkflowEngine,
    agents: AgentManager,
    domains: DomainManager,
    tools: ToolRegistry,
    consensus: ConsensusManager,
    execution_log: Vec<ExecutionEvent>,
}

/// Event in the execution log
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionEvent {
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub event_type: String,
    pub description: String,
    pub agent_id: Option<Uuid>,
    pub status: String,
}

impl OrchestrationEngine {
    /// Create a new orchestration engine
    pub fn new() -> Self {
        Self {
            workflow: WorkflowEngine::new(),
            agents: AgentManager::new(),
            domains: DomainManager::new(),
            tools: ToolRegistry::new(),
            consensus: ConsensusManager::new(3),
            execution_log: Vec::new(),
        }
    }

    /// Initialize the orchestration engine
    pub fn initialize(&mut self, num_agents: usize, num_tools: usize) -> Result<()> {
        // Initialize workflow
        self.workflow.initialize(num_agents, num_tools, 8)?;

        // Create agents
        for i in 0..num_agents {
            self.agents.create_agent(
                format!("Agent-{}", i),
                vec!["planning".to_string(), "execution".to_string()],
            );
        }

        // Create tools
        for i in 0..num_tools {
            self.tools.create_tool(
                format!("Tool-{}", i),
                "general".to_string(),
                vec![],
            );
        }

        // Log initialization
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "initialization".to_string(),
            description: format!("Initialized with {} agents and {} tools", num_agents, num_tools),
            agent_id: None,
            status: "success".to_string(),
        });

        Ok(())
    }

    /// Execute the complete workflow
    pub fn execute_workflow(&mut self) -> Result<OrchestrationResult> {
        // Phase 1: Initialization (already done)
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "phase".to_string(),
            description: format!("Entering phase: {}", self.workflow.phase()),
            agent_id: None,
            status: "started".to_string(),
        });

        // Phase 2: Planning
        self.workflow.next_phase()?;
        self.execute_planning_phase()?;

        // Phase 3: Execution
        self.workflow.next_phase()?;
        self.execute_execution_phase()?;

        // Phase 4: Analysis
        self.workflow.next_phase()?;
        self.execute_analysis_phase()?;

        // Phase 5: Learning
        self.workflow.next_phase()?;
        self.execute_learning_phase()?;

        // Complete
        self.workflow.next_phase()?;

        Ok(self.get_result())
    }

    /// Execute planning phase: agents discover tools and create plans
    fn execute_planning_phase(&mut self) -> Result<()> {
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "phase_execution".to_string(),
            description: "Planning phase: agents discovering tools".to_string(),
            agent_id: None,
            status: "started".to_string(),
        });

        let available_tools = self.tools.get_available_tools();
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "tool_discovery".to_string(),
            description: format!("Discovered {} available tools", available_tools.len()),
            agent_id: None,
            status: "success".to_string(),
        });

        Ok(())
    }

    /// Execute execution phase: agents execute plans
    fn execute_execution_phase(&mut self) -> Result<()> {
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "phase_execution".to_string(),
            description: "Execution phase: agents executing plans".to_string(),
            agent_id: None,
            status: "started".to_string(),
        });

        let agents = self.agents.list_agents();
        for agent in agents {
            // Simulate agent execution
            self.agents.record_success(agent.id)?;
            self.workflow.complete_task();

            self.log_event(ExecutionEvent {
                timestamp: chrono::Utc::now(),
                event_type: "task_completed".to_string(),
                description: format!("Agent {} completed task", agent.name),
                agent_id: Some(agent.id),
                status: "success".to_string(),
            });
        }

        Ok(())
    }

    /// Execute analysis phase: analyze results
    fn execute_analysis_phase(&mut self) -> Result<()> {
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "phase_execution".to_string(),
            description: "Analysis phase: analyzing results".to_string(),
            agent_id: None,
            status: "started".to_string(),
        });

        let balance = self.domains.calculate_balance();
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "domain_analysis".to_string(),
            description: format!(
                "Domain balance: {:.2}",
                balance.overall_balance
            ),
            agent_id: None,
            status: "success".to_string(),
        });

        Ok(())
    }

    /// Execute learning phase: improve future decisions
    fn execute_learning_phase(&mut self) -> Result<()> {
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "phase_execution".to_string(),
            description: "Learning phase: improving future decisions".to_string(),
            agent_id: None,
            status: "started".to_string(),
        });

        // Create a consensus proposal for workflow improvements
        let proposal = Proposal {
            id: Uuid::new_v4(),
            description: "Approve workflow optimization".to_string(),
            created_at: chrono::Utc::now(),
            votes: HashMap::new(),
            status: crate::consensus::ProposalStatus::Pending,
        };

        self.consensus.submit_proposal(proposal.clone())?;

        // Agents vote on proposal
        let agents = self.agents.list_agents();
        for (i, agent) in agents.iter().enumerate() {
            // Majority approval
            let approved = i < (agents.len() / 2 + 1);
            self.consensus.vote(agent.id, proposal.id, approved)?;
        }

        let status = self.consensus.finalize_proposal(proposal.id)?;
        self.log_event(ExecutionEvent {
            timestamp: chrono::Utc::now(),
            event_type: "consensus".to_string(),
            description: format!("Consensus reached: {:?}", status),
            agent_id: None,
            status: if status == ProposalStatus::Approved {
                "approved".to_string()
            } else {
                "rejected".to_string()
            },
        });

        Ok(())
    }

    /// Log an execution event
    fn log_event(&mut self, event: ExecutionEvent) {
        self.execution_log.push(event);
    }

    /// Get the orchestration result
    fn get_result(&self) -> OrchestrationResult {
        let workflow_summary = self.workflow.get_summary();
        let agents = self.agents.list_agents();
        let balance = self.domains.get_balance().clone();

        OrchestrationResult {
            success: self.workflow.phase() == WorkflowPhase::Complete,
            workflow_summary,
            agents_created: agents.len(),
            agents_ready: agents.iter().filter(|a| a.state == AgentState::Ready).count(),
            tasks_completed: self.workflow.get_summary().tasks_completed,
            tools_available: self.tools.get_available_tools().len(),
            domain_balance: balance,
            events_logged: self.execution_log.len(),
        }
    }

    /// Get execution log
    pub fn get_log(&self) -> &[ExecutionEvent] {
        &self.execution_log
    }
}

impl Default for OrchestrationEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of orchestration execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestrationResult {
    pub success: bool,
    pub workflow_summary: crate::workflow::WorkflowSummary,
    pub agents_created: usize,
    pub agents_ready: usize,
    pub tasks_completed: usize,
    pub tools_available: usize,
    pub domain_balance: crate::domain::DomainBalance,
    pub events_logged: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_orchestration_initialization() {
        let mut engine = OrchestrationEngine::new();
        assert!(engine.initialize(3, 5).is_ok());

        let agents = engine.agents.list_agents();
        assert_eq!(agents.len(), 3);
    }

    #[tokio::test]
    async fn test_orchestration_workflow() {
        let mut engine = OrchestrationEngine::new();
        engine.initialize(2, 3).unwrap();

        let result = engine.execute_workflow().unwrap();
        assert!(result.success);
        assert_eq!(result.agents_created, 2);
        assert!(result.events_logged > 0);
    }
}
