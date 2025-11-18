pub mod auth_flows;

pub use auth_flows::*;

/// Workflow execution module - integrates BPMN workflows with identity flows
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Workflow execution result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum WorkflowResult {
    /// Workflow completed successfully
    Success(serde_json::Value),
    /// Workflow requires additional input
    Pending(String),
    /// Workflow failed
    Failed(String),
}

/// Workflow execution context
#[derive(Clone, Debug)]
pub struct WorkflowExecutionContext {
    pub execution_id: Uuid,
    pub workflow_name: String,
    pub user_id: Option<Uuid>,
    pub org_id: Uuid,
    pub variables: std::collections::HashMap<String, serde_json::Value>,
    pub started_at: chrono::DateTime<chrono::Utc>,
}

impl WorkflowExecutionContext {
    pub fn new(workflow_name: &str, org_id: Uuid) -> Self {
        Self {
            execution_id: Uuid::new_v4(),
            workflow_name: workflow_name.to_string(),
            user_id: None,
            org_id,
            variables: std::collections::HashMap::new(),
            started_at: chrono::Utc::now(),
        }
    }

    pub fn set_variable(&mut self, key: &str, value: serde_json::Value) {
        self.variables.insert(key.to_string(), value);
    }

    pub fn get_variable(&self, key: &str) -> Option<serde_json::Value> {
        self.variables.get(key).cloned()
    }
}

/// Workflow executor - runs BPMN workflows
pub struct WorkflowExecutor {
    workflows: std::collections::HashMap<String, AuthFlow>,
}

impl WorkflowExecutor {
    pub fn new() -> Self {
        Self {
            workflows: std::collections::HashMap::new(),
        }
    }

    /// Register a workflow
    pub fn register_workflow(&mut self, workflow: AuthFlow) {
        self.workflows.insert(workflow.id.clone(), workflow);
    }

    /// Execute a workflow
    pub async fn execute(
        &self,
        workflow_name: &str,
        context: WorkflowExecutionContext,
    ) -> Result<WorkflowResult, String> {
        let workflow = self
            .workflows
            .get(workflow_name)
            .ok_or("Workflow not found".to_string())?;

        let executor = FlowExecutor::new(workflow.clone());

        // Convert WorkflowExecutionContext to FlowContext
        let flow_context = FlowContext {
            execution_id: context.execution_id,
            flow_id: workflow.id.clone(),
            user_id: context.user_id,
            organization_id: context.org_id,
            current_step: workflow
                .steps
                .first()
                .map(|s| s.id.clone())
                .ok_or("Workflow has no steps".to_string())?,
            variables: context.variables,
            started_at: context.started_at,
        };

        let result = executor.execute(flow_context).await;

        match result {
            Ok(ctx) => Ok(WorkflowResult::Success(serde_json::json!({
                "execution_id": ctx.execution_id,
                "variables": ctx.variables
            }))),
            Err(e) => Ok(WorkflowResult::Failed(e)),
        }
    }
}

impl Default for WorkflowExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_execution_context() {
        let mut ctx = WorkflowExecutionContext::new("login", Uuid::new_v4());
        ctx.set_variable("username", serde_json::json!("user@example.com"));
        assert_eq!(
            ctx.get_variable("username"),
            Some(serde_json::json!("user@example.com"))
        );
    }
}
