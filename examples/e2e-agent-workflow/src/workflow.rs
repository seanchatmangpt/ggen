//! Workflow execution and orchestration

use serde::{Deserialize, Serialize};
use uuid::Uuid;
use std::collections::VecDeque;

/// Status of a workflow step
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkflowStatus {
    /// Waiting to execute
    Pending,
    /// Currently executing
    Running,
    /// Completed successfully
    Completed,
    /// Failed
    Failed,
    /// Skipped
    Skipped,
}

impl WorkflowStatus {
    pub fn code(&self) -> &'static str {
        match self {
            WorkflowStatus::Pending => "PENDING",
            WorkflowStatus::Running => "RUNNING",
            WorkflowStatus::Completed => "COMPLETED",
            WorkflowStatus::Failed => "FAILED",
            WorkflowStatus::Skipped => "SKIPPED",
        }
    }
}

/// A single step in a workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowStep {
    /// Step ID
    pub id: Uuid,
    /// Step name
    pub name: String,
    /// Step description
    pub description: String,
    /// Agent responsible for this step
    pub agent_id: Uuid,
    /// Tool to invoke
    pub tool_name: String,
    /// Tool arguments
    pub tool_args: String,
    /// Dependencies on other steps
    pub dependencies: Vec<Uuid>,
    /// Current status
    pub status: WorkflowStatus,
    /// Result output
    pub result: Option<String>,
    /// Error message if failed
    pub error: Option<String>,
}

impl WorkflowStep {
    /// Create new workflow step
    pub fn new(
        name: String,
        agent_id: Uuid,
        tool_name: String,
        tool_args: String,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            description: String::new(),
            agent_id,
            tool_name,
            tool_args,
            dependencies: Vec::new(),
            status: WorkflowStatus::Pending,
            result: None,
            error: None,
        }
    }

    /// Add step description
    pub fn with_description(mut self, desc: String) -> Self {
        self.description = desc;
        self
    }

    /// Add dependency on another step
    pub fn add_dependency(mut self, dep_id: Uuid) -> Self {
        self.dependencies.push(dep_id);
        self
    }

    /// Mark step as completed
    pub fn mark_completed(&mut self, result: String) {
        self.status = WorkflowStatus::Completed;
        self.result = Some(result);
    }

    /// Mark step as failed
    pub fn mark_failed(&mut self, error: String) {
        self.status = WorkflowStatus::Failed;
        self.error = Some(error);
    }
}

/// Complete workflow
pub struct Workflow {
    /// Workflow ID
    pub id: Uuid,
    /// Workflow name
    pub name: String,
    /// All steps in workflow
    steps: Vec<WorkflowStep>,
    /// Execution queue
    queue: VecDeque<Uuid>,
}

impl Workflow {
    /// Create new workflow
    pub fn new(name: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            steps: Vec::new(),
            queue: VecDeque::new(),
        }
    }

    /// Add step to workflow
    pub fn add_step(&mut self, step: WorkflowStep) -> Uuid {
        let id = step.id;
        self.steps.push(step);
        self.queue.push_back(id);
        id
    }

    /// Get step by ID
    pub fn get_step(&self, id: Uuid) -> Option<&WorkflowStep> {
        self.steps.iter().find(|s| s.id == id)
    }

    /// Get mutable step by ID
    pub fn get_step_mut(&mut self, id: Uuid) -> Option<&mut WorkflowStep> {
        self.steps.iter_mut().find(|s| s.id == id)
    }

    /// Get all steps
    pub fn all_steps(&self) -> &[WorkflowStep] {
        &self.steps
    }

    /// Check if all dependencies are met for a step
    pub fn dependencies_met(&self, step_id: Uuid) -> bool {
        if let Some(step) = self.get_step(step_id) {
            step.dependencies.iter().all(|&dep_id| {
                if let Some(dep_step) = self.get_step(dep_id) {
                    dep_step.status == WorkflowStatus::Completed
                } else {
                    false
                }
            })
        } else {
            false
        }
    }

    /// Get next executable step
    pub fn next_executable_step(&self) -> Option<Uuid> {
        for &step_id in self.queue.iter() {
            if let Some(step) = self.get_step(step_id) {
                if step.status == WorkflowStatus::Pending && self.dependencies_met(step_id) {
                    return Some(step_id);
                }
            }
        }
        None
    }

    /// Get workflow progress percentage
    pub fn progress_percentage(&self) -> f64 {
        if self.steps.is_empty() {
            return 0.0;
        }
        let completed = self
            .steps
            .iter()
            .filter(|s| s.status == WorkflowStatus::Completed)
            .count();
        (completed as f64 / self.steps.len() as f64) * 100.0
    }

    /// Check if workflow is complete
    pub fn is_complete(&self) -> bool {
        self.steps
            .iter()
            .all(|s| s.status == WorkflowStatus::Completed || s.status == WorkflowStatus::Skipped)
    }

    /// Check if workflow has failures
    pub fn has_failures(&self) -> bool {
        self.steps.iter().any(|s| s.status == WorkflowStatus::Failed)
    }
}

/// Executes workflows
pub struct WorkflowExecutor;

impl WorkflowExecutor {
    /// Execute a workflow step
    pub async fn execute_step(step: &mut WorkflowStep) {
        step.status = WorkflowStatus::Running;
        
        // Simulate tool execution
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        
        step.mark_completed(format!("Executed tool: {}", step.tool_name));
    }

    /// Execute entire workflow
    pub async fn execute_workflow(workflow: &mut Workflow) -> bool {
        while let Some(step_id) = workflow.next_executable_step() {
            if let Some(step) = workflow.get_step_mut(step_id) {
                Self::execute_step(step).await;
            }
        }

        !workflow.has_failures()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_creation() {
        let workflow = Workflow::new("test_flow".to_string());
        assert_eq!(workflow.name, "test_flow");
        assert!(workflow.steps.is_empty());
    }

    #[test]
    fn test_workflow_step_creation() {
        let agent_id = Uuid::new_v4();
        let step = WorkflowStep::new(
            "step1".to_string(),
            agent_id,
            "search".to_string(),
            "{}".to_string(),
        );
        assert_eq!(step.status, WorkflowStatus::Pending);
    }

    #[test]
    fn test_add_step_to_workflow() {
        let mut workflow = Workflow::new("test".to_string());
        let agent_id = Uuid::new_v4();
        let step = WorkflowStep::new(
            "step1".to_string(),
            agent_id,
            "search".to_string(),
            "{}".to_string(),
        );
        let step_id = step.id;
        workflow.add_step(step);
        assert!(workflow.get_step(step_id).is_some());
    }

    #[test]
    fn test_dependencies() {
        let mut workflow = Workflow::new("test".to_string());
        let agent_id = Uuid::new_v4();
        let step1 = WorkflowStep::new(
            "step1".to_string(),
            agent_id,
            "search".to_string(),
            "{}".to_string(),
        );
        let step1_id = step1.id;
        workflow.add_step(step1);

        let mut step2 = WorkflowStep::new(
            "step2".to_string(),
            agent_id,
            "analyze".to_string(),
            "{}".to_string(),
        );
        step2 = step2.add_dependency(step1_id);
        workflow.add_step(step2);

        assert!(!workflow.dependencies_met(step1_id));
    }

    #[test]
    fn test_progress_percentage() {
        let mut workflow = Workflow::new("test".to_string());
        let agent_id = Uuid::new_v4();
        let step = WorkflowStep::new(
            "step1".to_string(),
            agent_id,
            "search".to_string(),
            "{}".to_string(),
        );
        workflow.add_step(step);
        assert_eq!(workflow.progress_percentage(), 0.0);
    }

    #[tokio::test]
    async fn test_workflow_execution() {
        let mut workflow = Workflow::new("test".to_string());
        let agent_id = Uuid::new_v4();
        let step = WorkflowStep::new(
            "step1".to_string(),
            agent_id,
            "search".to_string(),
            "{}".to_string(),
        );
        workflow.add_step(step);

        let success = WorkflowExecutor::execute_workflow(&mut workflow).await;
        assert!(success);
        assert!(workflow.is_complete());
    }
}
