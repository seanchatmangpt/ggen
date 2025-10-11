//! # Agent Coordination System
//!
//! Coordinates the execution of multiple agents following the 80/20 principle.
//! Focuses on the 20% of coordination patterns that deliver 80% of the value.

use crate::{
    core::{
        Agent, AgentContext, AgentError, AgentId, AgentResult, ExecutionContext, ExecutionResult,
        ExecutionState, ExecutionStatus, PlanId, SerializableTask,
    },
    protocols::{Message, MessageType},
};
use async_trait::async_trait;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use uuid::Uuid;

/// Coordinates the execution of multiple agents
pub struct AgentCoordinator {
    /// Registered agents
    agents: Arc<DashMap<String, Box<dyn Agent>>>,

    /// Execution plans being processed
    execution_plans: Arc<RwLock<HashMap<String, ExecutionPlan>>>,

    /// Message channels for inter-agent communication
    message_channels: Arc<RwLock<HashMap<String, mpsc::UnboundedSender<Message>>>>,

    /// Shared state for all agents
    shared_state: Arc<RwLock<HashMap<String, serde_json::Value>>>,
}

/// An execution plan that coordinates multiple agents
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExecutionPlan {
    /// Unique identifier for this plan
    pub plan_id: PlanId,

    /// Tasks to be executed
    pub tasks: Vec<SerializableTask>,

    /// Current state of the plan
    pub state: ExecutionState,

    /// Results from completed tasks
    pub results: HashMap<String, ExecutionResult>,

    /// Dependencies between tasks
    pub dependencies: HashMap<String, Vec<String>>,
}

/// A single task within an execution plan
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Task {
    /// Unique identifier for this task
    pub task_id: String,

    /// The agent responsible for this task (as string)
    pub agent_id: String,

    /// Task description
    pub description: String,

    /// Input data for the task
    pub input: serde_json::Value,

    /// Expected output schema
    pub output_schema: serde_json::Value,

    /// Task priority (higher numbers = higher priority)
    pub priority: i32,

    /// Maximum execution time in seconds
    pub timeout_seconds: Option<u64>,
}

impl AgentCoordinator {
    /// Create a new agent coordinator
    pub fn new() -> Self {
        Self {
            agents: Arc::new(DashMap::new()),
            execution_plans: Arc::new(RwLock::new(HashMap::new())),
            message_channels: Arc::new(RwLock::new(HashMap::new())),
            shared_state: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register an agent with the coordinator
    pub async fn register_agent(
        &self, mut agent: Box<dyn Agent>, context: AgentContext,
    ) -> AgentResult<()> {
        agent.initialize(&context).await?;

        let agent_id = agent.id().to_string();
        self.agents.insert(agent_id.clone(), agent);

        tracing::info!("Registered agent: {} ({})", context.agent_id, agent_id);

        Ok(())
    }

    /// Execute an execution plan
    pub async fn execute_plan(
        &self, mut plan: ExecutionPlan,
    ) -> AgentResult<HashMap<String, ExecutionResult>> {
        tracing::info!("Executing plan: {}", plan.plan_id);

        // Initialize plan state
        plan.state = ExecutionState::Running;
        self.execution_plans
            .write()
            .await
            .insert(plan.plan_id.to_string(), plan.clone());

        // Execute tasks in dependency order
        let results = self.execute_tasks_in_order(&plan).await?;

        // Update plan state
        let mut plans = self.execution_plans.write().await;
        if let Some(plan_ref) = plans.get_mut(&plan.plan_id.to_string()) {
            plan_ref.state = ExecutionState::Completed;
            plan_ref.results = results.clone();
        }

        tracing::info!("Plan {} completed successfully", plan.plan_id);
        Ok(results)
    }

    /// Execute tasks respecting dependencies
    async fn execute_tasks_in_order(
        &self, plan: &ExecutionPlan,
    ) -> AgentResult<HashMap<String, ExecutionResult>> {
        let mut results = HashMap::new();
        let mut completed_tasks = std::collections::HashSet::new();

        // Simple topological sort - execute tasks when dependencies are met
        loop {
            let mut progress = false;

            for task in &plan.tasks {
                if completed_tasks.contains(&task.task_id) {
                    continue;
                }

                // Check if all dependencies are completed
                let dependencies_met = plan
                    .dependencies
                    .get(&task.task_id)
                    .map(|deps| deps.iter().all(|dep| completed_tasks.contains(dep)))
                    .unwrap_or(true);

                if dependencies_met {
                    // Execute the task
                    let result = self.execute_task(task).await?;
                    results.insert(task.task_id.clone(), result.clone());
                    completed_tasks.insert(task.task_id.clone());
                    progress = true;

                    // Update shared state with task results
                    let mut shared_state = self.shared_state.write().await;
                    shared_state.insert(
                        format!("task_result_{}", task.task_id),
                        result.output.clone(),
                    );
                }
            }

            if !progress && completed_tasks.len() < plan.tasks.len() {
                return Err(AgentError::ExecutionFailed(
                    "Circular dependency detected in execution plan".to_string(),
                ));
            }

            if completed_tasks.len() == plan.tasks.len() {
                break;
            }
        }

        Ok(results)
    }

    /// Execute a single task
    async fn execute_task(&self, task: &SerializableTask) -> AgentResult<ExecutionResult> {
        let agent = self.agents.get(&task.agent_id).ok_or_else(|| {
            AgentError::ExecutionFailed(format!("Agent {} not found", task.agent_id))
        })?;

        let start_time = std::time::Instant::now();

        let execution_context = ExecutionContext {
            task: task.clone(),
            state: ExecutionState::Running,
            input: task.input.clone(),
            shared_state: self.shared_state.clone(),
            result_tx: mpsc::unbounded_channel().0, // We'll handle results differently
        };

        let result = agent.execute(&execution_context).await?;

        let duration_ms = start_time.elapsed().as_millis() as u64;
        let mut result_with_timing = result;
        result_with_timing.duration_ms = duration_ms;

        Ok(result_with_timing)
    }
}

impl Default for AgentCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

/// Create a standard execution plan for ggen development workflow
pub fn create_ggen_development_plan() -> ExecutionPlan {
    let plan_id = PlanId::new();

    // Define tasks following 80/20 principle - focus on high-value activities
    let tasks = vec![
        // Phase 1: Foundation (20% effort, 80% value)
        SerializableTask {
            task_id: "london_bdd_init".to_string(),
            agent_id: Uuid::new_v4().to_string(), // Will be set when agent is registered
            description: "Initialize London School BDD patterns".to_string(),
            input: serde_json::json!({
                "patterns": ["given_when_then", "outside_in", "mockist_vs_classicist"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "test_structure": {"type": "object"},
                    "bdd_patterns": {"type": "array"}
                }
            }),
            priority: 100,
            timeout_seconds: Some(30),
        },
        SerializableTask {
            task_id: "byzantene_security".to_string(),
            agent_id: Uuid::new_v4().to_string(),
            description: "Implement Byzantine fault tolerance patterns".to_string(),
            input: serde_json::json!({
                "fault_types": ["crash", "omission", "timing", "byzantine"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "fault_tolerance": {"type": "object"},
                    "redundancy_patterns": {"type": "array"}
                }
            }),
            priority: 90,
            timeout_seconds: Some(45),
        },
        // Phase 2: Core testing and quality
        SerializableTask {
            task_id: "test_oracle_generation".to_string(),
            agent_id: Uuid::new_v4().to_string(),
            description: "Generate comprehensive test oracles".to_string(),
            input: serde_json::json!({
                "test_types": ["unit", "integration", "property", "bdd"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "test_oracles": {"type": "array"},
                    "coverage_metrics": {"type": "object"}
                }
            }),
            priority: 80,
            timeout_seconds: Some(60),
        },
    ];

    ExecutionPlan {
        plan_id,
        tasks,
        state: ExecutionState::Pending,
        results: HashMap::new(),
        dependencies: HashMap::new(), // No dependencies for initial implementation
    }
}
