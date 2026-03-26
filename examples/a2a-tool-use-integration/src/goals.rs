//! Goal definitions and goal decomposition
//!
//! Goals represent high-level objectives that agents want to accomplish.
//! Goals are decomposed into task sequences that can be executed via tools.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GoalType {
    GenerateCode,
    AnalyzeData,
    ResearchInformation,
    ValidateArtifact,
    TransformData,
}

impl std::fmt::Display for GoalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GenerateCode => write!(f, "GenerateCode"),
            Self::AnalyzeData => write!(f, "AnalyzeData"),
            Self::ResearchInformation => write!(f, "ResearchInformation"),
            Self::ValidateArtifact => write!(f, "ValidateArtifact"),
            Self::TransformData => write!(f, "TransformData"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GoalState {
    Pending,
    Executing,
    Completed,
    Failed,
}

impl std::fmt::Display for GoalState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pending => write!(f, "Pending"),
            Self::Executing => write!(f, "Executing"),
            Self::Completed => write!(f, "Completed"),
            Self::Failed => write!(f, "Failed"),
        }
    }
}

/// High-level objective for an agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Goal {
    pub id: String,
    pub goal_type: GoalType,
    pub description: String,
    pub context: GoalContext,
    pub success_criteria: Vec<String>,
    pub state: GoalState,
    pub created_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

/// Context information for goal execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoalContext {
    pub input: String,
    pub parameters: BTreeMap<String, serde_json::Value>,
}

impl Goal {
    /// Create a new goal with the given type and description
    pub fn new(goal_type: GoalType, description: String) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            goal_type,
            description,
            context: GoalContext {
                input: String::new(),
                parameters: BTreeMap::new(),
            },
            success_criteria: Vec::new(),
            state: GoalState::Pending,
            created_at: Utc::now(),
            started_at: None,
            completed_at: None,
        }
    }

    /// Set the context for this goal
    pub fn with_context(mut self, input: String, parameters: BTreeMap<String, serde_json::Value>) -> Self {
        self.context = GoalContext { input, parameters };
        self
    }

    /// Add success criteria
    pub fn with_success_criteria(mut self, criteria: Vec<String>) -> Self {
        self.success_criteria = criteria;
        self
    }

    /// Mark goal as started
    pub fn mark_started(&mut self) {
        self.state = GoalState::Executing;
        self.started_at = Some(Utc::now());
    }

    /// Mark goal as completed
    pub fn mark_completed(&mut self) {
        self.state = GoalState::Completed;
        self.completed_at = Some(Utc::now());
    }

    /// Mark goal as failed
    pub fn mark_failed(&mut self) {
        self.state = GoalState::Failed;
        self.completed_at = Some(Utc::now());
    }

    /// Get execution duration if started and completed
    pub fn execution_duration_ms(&self) -> Option<u128> {
        match (self.started_at, self.completed_at) {
            (Some(start), Some(end)) => {
                let duration = end.signed_duration_since(start);
                Some(duration.num_milliseconds() as u128)
            }
            _ => None,
        }
    }
}

/// Decomposition of a goal into executable tasks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoalDecomposition {
    pub goal_id: String,
    pub tasks: Vec<TaskDescription>,
}

/// Description of a task in a decomposed goal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskDescription {
    pub task_id: String,
    pub sequence: usize,
    pub description: String,
    pub required_tool_types: Vec<String>,
    pub input_mapping: BTreeMap<String, String>,
    pub output_binding: String,
}

impl GoalDecomposition {
    /// Create a decomposition for a goal with tasks
    pub fn new(goal_id: String, tasks: Vec<TaskDescription>) -> Self {
        Self { goal_id, tasks }
    }

    /// Get tasks in execution order
    pub fn ordered_tasks(&self) -> Vec<&TaskDescription> {
        let mut tasks = self.tasks.iter().collect::<Vec<_>>();
        tasks.sort_by_key(|t| t.sequence);
        tasks
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_goal_creation() {
        let goal = Goal::new(GoalType::GenerateCode, "Generate Rust module".to_string());
        assert_eq!(goal.goal_type, GoalType::GenerateCode);
        assert_eq!(goal.state, GoalState::Pending);
    }

    #[test]
    fn test_goal_state_transitions() {
        let mut goal = Goal::new(GoalType::GenerateCode, "Generate Rust module".to_string());
        goal.mark_started();
        assert_eq!(goal.state, GoalState::Executing);
        goal.mark_completed();
        assert_eq!(goal.state, GoalState::Completed);
    }

    #[test]
    fn test_goal_context() {
        let mut params = BTreeMap::new();
        params.insert("language".to_string(), serde_json::Value::String("rust".to_string()));

        let goal = Goal::new(GoalType::GenerateCode, "Generate module".to_string())
            .with_context("Generate a client library".to_string(), params);

        assert_eq!(goal.context.input, "Generate a client library");
        assert_eq!(goal.context.parameters.len(), 1);
    }
}
