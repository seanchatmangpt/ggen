//! State synchronization between YAWL workflows and A2A tasks.
//!
//! This module provides bidirectional state mapping and synchronization
//! between YAWL workflow states and A2A task states.

use a2a_generated::task::{Task, TaskPriority, TaskStatus};
use crate::a2a::error::{A2AIntegrationError, IntegrationResult};
use crate::template::TemplateContext;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::time::Duration;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Overall workflow execution status.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkflowStatus {
    /// Workflow is initialized but not started
    Initialized,
    /// Workflow is currently executing
    Running,
    /// Workflow is suspended (can be resumed)
    Suspended,
    /// Workflow completed successfully
    Completed,
    /// Workflow failed with errors
    Failed,
    /// Workflow was cancelled
    Cancelled,
}

impl WorkflowStatus {
    /// Check if workflow is in a terminal state.
    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Completed | Self::Failed | Self::Cancelled)
    }

    /// Check if workflow is active.
    pub fn is_active(&self) -> bool {
        matches!(self, Self::Running | Self::Suspended)
    }
}

/// State of a single task within workflow execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskStateSync {
    /// YAWL task ID
    pub yawl_task_id: String,
    /// A2A task ID
    pub a2a_task_id: String,
    /// Current YAWL state
    pub yawl_state: YawlTaskState,
    /// Current A2A state
    pub a2a_state: TaskStatus,
    /// Time of last state update
    pub last_updated: chrono::DateTime<chrono::Utc>,
    /// Number of retry attempts
    pub retry_count: u32,
    /// Error message if task failed
    pub error_message: Option<String>,
}

/// YAWL task states as defined in the YAWL specification.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum YawlTaskState {
    /// Task is waiting to start (unreached)
    Unstarted,
    /// Task is currently executing
    Running,
    /// Task completed successfully
    Completed,
    /// Task execution failed
    Failed,
    /// Task was cancelled before completion
    Cancelled,
    /// Task is suspended (can be resumed)
    Suspended,
    /// Task is waiting at an input condition
    InputWaiting,
    /// Task is waiting at an output condition
    OutputWaiting,
}

impl YawlTaskState {
    /// Check if this is a terminal state.
    pub fn is_terminal(&self) -> bool {
        matches!(
            self,
            Self::Completed | Self::Failed | Self::Cancelled
        )
    }

    /// Check if task is actively executing.
    pub fn is_active(&self) -> bool {
        matches!(self, Self::Running | Self::InputWaiting | Self::OutputWaiting)
    }
}

/// Mapping between YAWL and A2A states.
pub struct StateMapping;

impl StateMapping {
    /// Convert YAWL state to A2A TaskStatus.
    pub fn yawl_to_a2a(yawl_state: YawlTaskState) -> TaskStatus {
        match yawl_state {
            YawlTaskState::Unstarted => TaskStatus::Pending,
            YawlTaskState::Running => TaskStatus::Running,
            YawlTaskState::Completed => TaskStatus::Completed,
            YawlTaskState::Failed => TaskStatus::Failed,
            YawlTaskState::Cancelled => TaskStatus::Cancelled,
            YawlTaskState::Suspended => TaskStatus::Pending,
            YawlTaskState::InputWaiting => TaskStatus::Ready,
            YawlTaskState::OutputWaiting => TaskStatus::Ready,
        }
    }

    /// Convert A2A TaskStatus to YAWL state.
    pub fn a2a_to_yawl(a2a_status: TaskStatus) -> YawlTaskState {
        match a2a_status {
            TaskStatus::Pending => YawlTaskState::Unstarted,
            TaskStatus::Ready => YawlTaskState::InputWaiting,
            TaskStatus::Running => YawlTaskState::Running,
            TaskStatus::Completed => YawlTaskState::Completed,
            TaskStatus::Failed => YawlTaskState::Failed,
            TaskStatus::Cancelled => YawlTaskState::Cancelled,
        }
    }

    /// Check if state transition is valid.
    pub fn is_valid_transition(from: YawlTaskState, to: YawlTaskState) -> bool {
        use YawlTaskState::*;

        // Define valid transitions
        let valid_transitions: HashMap<YawlTaskState, HashSet<YawlTaskState>> = [
            (Unstarted, [Running, Cancelled].iter().cloned().collect()),
            (
                Running,
                [Completed, Failed, Suspended, Cancelled]
                    .iter()
                    .cloned()
                    .collect(),
            ),
            (
                Suspended,
                [Running, Cancelled].iter().cloned().collect(),
            ),
            (InputWaiting, [Running, Cancelled].iter().cloned().collect()),
            (OutputWaiting, [Completed, Failed, Cancelled].iter().cloned().collect()),
            (Completed, HashSet::new()), // Terminal state
            (Failed, HashSet::new()),    // Terminal state
            (Cancelled, HashSet::new()), // Terminal state
        ]
        .into();

        valid_transitions
            .get(&from)
            .map(|valid| valid.contains(&to))
            .unwrap_or(false)
    }
}

/// Synchronizer for maintaining state consistency between YAWL and A2A.
pub struct StateSynchronizer {
    /// Current workflow status
    workflow_status: WorkflowStatus,
    /// Task state mappings
    task_states: HashMap<String, TaskStateSync>,
    /// Completed tasks
    completed_tasks: HashSet<String>,
    /// Failed tasks
    failed_tasks: HashSet<String>,
    /// Running tasks
    running_tasks: HashSet<String>,
    /// Pending tasks (ready to execute)
    pending_tasks: HashSet<String>,
}

impl StateSynchronizer {
    /// Create a new state synchronizer.
    pub fn new() -> Self {
        Self {
            workflow_status: WorkflowStatus::Initialized,
            task_states: HashMap::new(),
            completed_tasks: HashSet::new(),
            failed_tasks: HashSet::new(),
            running_tasks: HashSet::new(),
            pending_tasks: HashSet::new(),
        }
    }

    /// Initialize the synchronizer with a template context.
    pub fn initialize(&mut self, ctx: &TemplateContext) -> IntegrationResult<()> {
        // Create state entries for all tasks
        for task in &ctx.tasks {
            let sync = TaskStateSync {
                yawl_task_id: task.id.clone(),
                a2a_task_id: Uuid::new_v4().to_string(),
                yawl_state: YawlTaskState::Unstarted,
                a2a_state: TaskStatus::Pending,
                last_updated: chrono::Utc::now(),
                retry_count: 0,
                error_message: None,
            };

            self.task_states.insert(task.id.clone(), sync);
            self.pending_tasks.insert(task.id.clone());
        }

        self.workflow_status = WorkflowStatus::Initialized;
        Ok(())
    }

    /// Update task state based on A2A task status.
    pub fn update_task_from_a2a(
        &mut self,
        yawl_task_id: &str,
        a2a_status: TaskStatus,
    ) -> IntegrationResult<()> {
        let sync = self
            .task_states
            .get_mut(yawl_task_id)
            .ok_or_else(|| A2AIntegrationError::state_sync(format!("Task '{}' not found", yawl_task_id)))?;

        let new_yawl_state = StateMapping::a2a_to_yawl(a2a_status.clone());

        // Validate state transition
        if !StateMapping::is_valid_transition(sync.yawl_state.clone(), new_yawl_state.clone()) {
            return Err(A2AIntegrationError::state_sync(format!(
                "Invalid state transition for task '{}': {:?} -> {:?}",
                yawl_task_id, sync.yawl_state, new_yawl_state
            )));
        }

        // Update state tracking sets
        self.remove_from_tracking(yawl_task_id);
        self.add_to_tracking(yawl_task_id, &new_yawl_state);

        // Update sync record
        sync.yawl_state = new_yawl_state;
        sync.a2a_state = a2a_status;
        sync.last_updated = chrono::Utc::now();

        // Update workflow status if needed
        self.update_workflow_status()?;

        Ok(())
    }

    /// Get task state sync by YAWL task ID.
    pub fn get_task_state(&self, yawl_task_id: &str) -> Option<&TaskStateSync> {
        self.task_states.get(yawl_task_id)
    }

    /// Get all tasks in a specific state.
    pub fn get_tasks_by_state(&self, state: YawlTaskState) -> Vec<&TaskStateSync> {
        self.task_states
            .values()
            .filter(|sync| sync.yawl_state == state)
            .collect()
    }

    /// Get pending tasks ready for execution.
    pub fn get_ready_tasks(&self) -> Vec<String> {
        self.task_states
            .values()
            .filter(|sync| {
                sync.yawl_state == YawlTaskState::Unstarted
                    || sync.yawl_state == YawlTaskState::InputWaiting
            })
            .map(|sync| sync.yawl_task_id.clone())
            .collect()
    }

    /// Get completed tasks.
    pub fn get_completed_tasks(&self) -> Vec<String> {
        self.completed_tasks.iter().cloned().collect()
    }

    /// Get failed tasks.
    pub fn get_failed_tasks(&self) -> Vec<String> {
        self.failed_tasks.iter().cloned().collect()
    }

    /// Get running tasks.
    pub fn get_running_tasks(&self) -> Vec<String> {
        self.running_tasks.iter().cloned().collect()
    }

    /// Check if workflow can transition to running.
    pub fn can_start(&self) -> bool {
        self.workflow_status == WorkflowStatus::Initialized
            || self.workflow_status == WorkflowStatus::Suspended
    }

    /// Check if workflow is complete.
    pub fn is_complete(&self) -> bool {
        self.workflow_status == WorkflowStatus::Completed
    }

    /// Check if workflow has failed.
    pub fn has_failed(&self) -> bool {
        self.workflow_status == WorkflowStatus::Failed
    }

    /// Get current workflow status.
    pub fn workflow_status(&self) -> &WorkflowStatus {
        &self.workflow_status
    }

    /// Mark workflow as started.
    pub fn start(&mut self) -> IntegrationResult<()> {
        if !self.can_start() {
            return Err(A2AIntegrationError::execution(format!(
                "Cannot start workflow in {:?} state",
                self.workflow_status
            )));
        }

        self.workflow_status = WorkflowStatus::Running;
        Ok(())
    }

    /// Suspend workflow execution.
    pub fn suspend(&mut self) -> IntegrationResult<()> {
        if self.workflow_status != WorkflowStatus::Running {
            return Err(A2AIntegrationError::execution(format!(
                "Cannot suspend workflow in {:?} state",
                self.workflow_status
            )));
        }

        // Suspend all running tasks
        let running_ids: Vec<_> = self.running_tasks.iter().cloned().collect();
        for task_id in running_ids {
            if let Some(sync) = self.task_states.get_mut(&task_id) {
                sync.yawl_state = YawlTaskState::Suspended;
                sync.a2a_state = TaskStatus::Pending;
                sync.last_updated = chrono::Utc::now();
            }
        }

        self.running_tasks.clear();
        self.workflow_status = WorkflowStatus::Suspended;
        Ok(())
    }

    /// Resume suspended workflow.
    pub fn resume(&mut self) -> IntegrationResult<()> {
        if self.workflow_status != WorkflowStatus::Suspended {
            return Err(A2AIntegrationError::execution(format!(
                "Cannot resume workflow in {:?} state",
                self.workflow_status
            )));
        }

        // Move suspended tasks back to pending
        let suspended = self.get_tasks_by_state(YawlTaskState::Suspended);
        for sync in suspended {
            self.pending_tasks.insert(sync.yawl_task_id.clone());
            if let Some(s) = self.task_states.get_mut(&sync.yawl_task_id) {
                s.yawl_state = YawlTaskState::Unstarted;
                s.a2a_state = TaskStatus::Pending;
                s.last_updated = chrono::Utc::now();
            }
        }

        self.workflow_status = WorkflowStatus::Running;
        Ok(())
    }

    /// Cancel workflow execution.
    pub fn cancel(&mut self) -> IntegrationResult<()> {
        if self.workflow_status.is_terminal() {
            return Err(A2AIntegrationError::execution(format!(
                "Cannot cancel workflow in terminal state {:?}",
                self.workflow_status
            )));
        }

        // Mark all active tasks as cancelled
        for sync in self.task_states.values_mut() {
            if sync.yawl_state.is_active() {
                sync.yawl_state = YawlTaskState::Cancelled;
                sync.a2a_state = TaskStatus::Cancelled;
                sync.last_updated = chrono::Utc::now();
            }
        }

        self.running_tasks.clear();
        self.pending_tasks.clear();
        self.workflow_status = WorkflowStatus::Cancelled;
        Ok(())
    }

    /// Remove task from tracking sets.
    fn remove_from_tracking(&mut self, task_id: &str) {
        self.completed_tasks.remove(task_id);
        self.failed_tasks.remove(task_id);
        self.running_tasks.remove(task_id);
        self.pending_tasks.remove(task_id);
    }

    /// Add task to appropriate tracking set.
    fn add_to_tracking(&mut self, task_id: &str, state: &YawlTaskState) {
        match state {
            YawlTaskState::Completed => {
                self.completed_tasks.insert(task_id.to_string());
            }
            YawlTaskState::Failed => {
                self.failed_tasks.insert(task_id.to_string());
            }
            YawlTaskState::Running => {
                self.running_tasks.insert(task_id.to_string());
            }
            YawlTaskState::Unstarted | YawlTaskState::InputWaiting => {
                self.pending_tasks.insert(task_id.to_string());
            }
            _ => {}
        }
    }

    /// Update workflow status based on task states.
    fn update_workflow_status(&mut self) -> IntegrationResult<()> {
        if self.workflow_status == WorkflowStatus::Cancelled {
            return Ok(());
        }

        let total_tasks = self.task_states.len();
        let completed = self.completed_tasks.len();
        let failed = self.failed_tasks.len();
        let running = self.running_tasks.len();

        if failed > 0 {
            self.workflow_status = WorkflowStatus::Failed;
        } else if completed == total_tasks {
            self.workflow_status = WorkflowStatus::Completed;
        } else if running > 0 {
            self.workflow_status = WorkflowStatus::Running;
        }

        Ok(())
    }

    /// Get a summary of workflow state.
    pub fn summary(&self) -> WorkflowStateSummary {
        WorkflowStateSummary {
            status: self.workflow_status.clone(),
            total_tasks: self.task_states.len(),
            completed_tasks: self.completed_tasks.len(),
            failed_tasks: self.failed_tasks.len(),
            running_tasks: self.running_tasks.len(),
            pending_tasks: self.pending_tasks.len(),
        }
    }
}

impl Default for StateSynchronizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Summary of workflow state for reporting.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowStateSummary {
    /// Overall workflow status
    pub status: WorkflowStatus,
    /// Total number of tasks
    pub total_tasks: usize,
    /// Number of completed tasks
    pub completed_tasks: usize,
    /// Number of failed tasks
    pub failed_tasks: usize,
    /// Number of running tasks
    pub running_tasks: usize,
    /// Number of pending tasks
    pub pending_tasks: usize,
}

impl WorkflowStateSummary {
    /// Calculate completion percentage.
    pub fn completion_percent(&self) -> f64 {
        if self.total_tasks == 0 {
            return 100.0;
        }
        (self.completed_tasks as f64 / self.total_tasks as f64) * 100.0
    }

    /// Check if workflow is healthy.
    pub fn is_healthy(&self) -> bool {
        self.failed_tasks == 0 && self.status != WorkflowStatus::Cancelled
    }
}

/// Thread-safe state synchronizer wrapper.
pub struct ThreadSafeStateSynchronizer {
    inner: RwLock<StateSynchronizer>,
}

impl ThreadSafeStateSynchronizer {
    /// Create a new thread-safe state synchronizer.
    pub fn new() -> Self {
        Self {
            inner: RwLock::new(StateSynchronizer::new()),
        }
    }

    /// Initialize with template context.
    pub async fn initialize(&self, ctx: &TemplateContext) -> IntegrationResult<()> {
        let mut guard = self.inner.write().await;
        guard.initialize(ctx)
    }

    /// Update task state.
    pub async fn update_task_from_a2a(
        &self,
        yawl_task_id: &str,
        a2a_status: TaskStatus,
    ) -> IntegrationResult<()> {
        let mut guard = self.inner.write().await;
        guard.update_task_from_a2a(yawl_task_id, a2a_status)
    }

    /// Get ready tasks.
    pub async fn get_ready_tasks(&self) -> Vec<String> {
        let guard = self.inner.read().await;
        guard.get_ready_tasks()
    }

    /// Get workflow summary.
    pub async fn summary(&self) -> WorkflowStateSummary {
        let guard = self.inner.read().await;
        guard.summary()
    }

    /// Start workflow.
    pub async fn start(&self) -> IntegrationResult<()> {
        let mut guard = self.inner.write().await;
        guard.start()
    }

    /// Suspend workflow.
    pub async fn suspend(&self) -> IntegrationResult<()> {
        let mut guard = self.inner.write().await;
        guard.suspend()
    }

    /// Resume workflow.
    pub async fn resume(&self) -> IntegrationResult<()> {
        let mut guard = self.inner.write().await;
        guard.resume()
    }

    /// Cancel workflow.
    pub async fn cancel(&self) -> IntegrationResult<()> {
        let mut guard = self.inner.write().await;
        guard.cancel()
    }
}

impl Default for ThreadSafeStateSynchronizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::template::{TaskContext, TemplateContext};

    #[test]
    fn test_state_mapping_yawl_to_a2a() {
        assert_eq!(
            StateMapping::yawl_to_a2a(YawlTaskState::Unstarted),
            TaskStatus::Pending
        );
        assert_eq!(
            StateMapping::yawl_to_a2a(YawlTaskState::Running),
            TaskStatus::Running
        );
        assert_eq!(
            StateMapping::yawl_to_a2a(YawlTaskState::Completed),
            TaskStatus::Completed
        );
        assert_eq!(
            StateMapping::yawl_to_a2a(YawlTaskState::Failed),
            TaskStatus::Failed
        );
    }

    #[test]
    fn test_state_mapping_a2a_to_yawl() {
        assert_eq!(
            StateMapping::a2a_to_yawl(TaskStatus::Pending),
            YawlTaskState::Unstarted
        );
        assert_eq!(
            StateMapping::a2a_to_yawl(TaskStatus::Running),
            YawlTaskState::Running
        );
        assert_eq!(
            StateMapping::a2a_to_yawl(TaskStatus::Completed),
            YawlTaskState::Completed
        );
    }

    #[test]
    fn test_valid_transitions() {
        assert!(StateMapping::is_valid_transition(
            YawlTaskState::Unstarted,
            YawlTaskState::Running
        ));
        assert!(StateMapping::is_valid_transition(
            YawlTaskState::Running,
            YawlTaskState::Completed
        ));
        assert!(!StateMapping::is_valid_transition(
            YawlTaskState::Completed,
            YawlTaskState::Running
        ));
    }

    #[test]
    fn test_state_synchronizer_initialization() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test".to_string(),
            version: "1.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "task1".to_string(),
                    name: "Task 1".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        let mut sync = StateSynchronizer::new();
        sync.initialize(&ctx).unwrap();

        assert_eq!(sync.get_ready_tasks().len(), 1);
        assert_eq!(sync.get_ready_tasks()[0], "task1");
    }

    #[test]
    fn test_state_synchronizer_workflow_completion() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test".to_string(),
            version: "1.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "task1".to_string(),
                    name: "Task 1".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        let mut sync = StateSynchronizer::new();
        sync.initialize(&ctx).unwrap();
        sync.start().unwrap();

        sync.update_task_from_a2a("task1", TaskStatus::Completed)
            .unwrap();

        assert!(sync.is_complete());
        assert!(sync.summary().is_healthy());
    }

    #[test]
    fn test_workflow_status_is_terminal() {
        assert!(WorkflowStatus::Completed.is_terminal());
        assert!(WorkflowStatus::Failed.is_terminal());
        assert!(WorkflowStatus::Cancelled.is_terminal());
        assert!(!WorkflowStatus::Running.is_terminal());
        assert!(!WorkflowStatus::Initialized.is_terminal());
    }

    #[test]
    fn test_yawl_task_state_is_active() {
        assert!(YawlTaskState::Running.is_active());
        assert!(YawlTaskState::InputWaiting.is_active());
        assert!(YawlTaskState::OutputWaiting.is_active());
        assert!(!YawlTaskState::Completed.is_active());
        assert!(!YawlTaskState::Unstarted.is_active());
    }

    #[test]
    fn test_workflow_state_summary_completion_percent() {
        let summary = WorkflowStateSummary {
            status: WorkflowStatus::Running,
            total_tasks: 10,
            completed_tasks: 5,
            failed_tasks: 0,
            running_tasks: 3,
            pending_tasks: 2,
        };

        assert_eq!(summary.completion_percent(), 50.0);
    }

    #[test]
    fn test_workflow_state_summary_is_healthy() {
        let healthy = WorkflowStateSummary {
            status: WorkflowStatus::Running,
            total_tasks: 10,
            completed_tasks: 5,
            failed_tasks: 0,
            running_tasks: 3,
            pending_tasks: 2,
        };

        assert!(healthy.is_healthy());

        let unhealthy = WorkflowStateSummary {
            status: WorkflowStatus::Failed,
            total_tasks: 10,
            completed_tasks: 5,
            failed_tasks: 2,
            running_tasks: 3,
            pending_tasks: 0,
        };

        assert!(!unhealthy.is_healthy());
    }
}
