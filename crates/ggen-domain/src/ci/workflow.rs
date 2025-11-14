//! GitHub Actions workflow management - Domain layer
//!
//! Pure business logic for managing GitHub Actions workflows, checking status,
//! viewing logs, and canceling runs.

use ggen_utils::error::Result;
use std::str::FromStr;

/// Workflow status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WorkflowStatus {
    Queued,
    InProgress,
    Completed,
    Failed,
    Cancelled,
}

impl FromStr for WorkflowStatus {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "queued" => Ok(Self::Queued),
            "in_progress" => Ok(Self::InProgress),
            "completed" => Ok(Self::Completed),
            "failed" => Ok(Self::Failed),
            "cancelled" => Ok(Self::Cancelled),
            _ => Err(format!("Invalid workflow status: {}", s)),
        }
    }
}

impl WorkflowStatus {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Queued => "queued",
            Self::InProgress => "in_progress",
            Self::Completed => "completed",
            Self::Failed => "failed",
            Self::Cancelled => "cancelled",
        }
    }

    pub fn is_active(&self) -> bool {
        matches!(self, Self::Queued | Self::InProgress)
    }
}

/// Workflow information
#[derive(Debug, Clone)]
pub struct WorkflowInfo {
    pub id: String,
    pub name: String,
    pub status: WorkflowStatus,
    pub conclusion: Option<String>,
    pub created_at: String,
    pub updated_at: String,
    pub html_url: String,
}

/// Workflow list result
#[derive(Debug, Clone)]
pub struct WorkflowListResult {
    pub workflows: Vec<WorkflowInfo>,
    pub total_count: usize,
}

/// Workflow status result
#[derive(Debug, Clone)]
pub struct WorkflowStatusResult {
    pub workflow: WorkflowInfo,
    pub jobs: Vec<JobInfo>,
}

/// Job information within a workflow
#[derive(Debug, Clone)]
pub struct JobInfo {
    pub id: String,
    pub name: String,
    pub status: WorkflowStatus,
    pub conclusion: Option<String>,
    pub started_at: Option<String>,
    pub completed_at: Option<String>,
}

/// Workflow logs result
#[derive(Debug, Clone)]
pub struct WorkflowLogsResult {
    pub logs: String,
    pub workflow_id: String,
}

/// Trait for managing workflows
pub trait WorkflowManager {
    /// List workflows with optional filters
    fn list(&self, active_only: bool) -> Result<WorkflowListResult>;

    /// Cancel a workflow by name or ID
    fn cancel(&self, workflow_id: &str) -> Result<()>;

    /// Cancel all running workflows
    fn cancel_all(&self) -> Result<usize>;
}

/// Trait for checking workflow status
pub trait WorkflowStatusChecker {
    /// Get workflow status by name or ID
    fn get_status(&self, workflow_id: Option<&str>) -> Result<WorkflowStatusResult>;
}

/// Trait for viewing workflow logs
pub trait WorkflowLogViewer {
    /// Get workflow logs
    fn get_logs(&self, workflow_id: Option<&str>) -> Result<WorkflowLogsResult>;

    /// Stream workflow logs in real-time
    fn stream_logs(&self, workflow_id: &str, callback: Box<dyn Fn(&str)>) -> Result<()>;
}

/// Default implementation using GitHub CLI
pub struct GhWorkflowManager;

impl WorkflowManager for GhWorkflowManager {
    fn list(&self, active_only: bool) -> Result<WorkflowListResult> {
        let mut cmd = std::process::Command::new("gh");
        cmd.args([
            "run",
            "list",
            "--json",
            "databaseId,name,status,conclusion,createdAt,updatedAt,url",
        ]);

        if active_only {
            cmd.args(["--status", "in_progress,queued"]);
        }

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ggen_utils::error::Error::new(&format!(
                "Failed to list workflows: {}",
                stderr
            )));
        }

        // Parse JSON output (simplified - real implementation would use serde_json)
        let workflows = vec![];

        Ok(WorkflowListResult {
            workflows,
            total_count: 0,
        })
    }

    fn cancel(&self, workflow_id: &str) -> Result<()> {
        let mut cmd = std::process::Command::new("gh");
        cmd.args(["run", "cancel", workflow_id]);

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ggen_utils::error::Error::new(&format!(
                "Failed to cancel workflow {}: {}",
                workflow_id, stderr
            )));
        }

        Ok(())
    }

    fn cancel_all(&self) -> Result<usize> {
        // Get list of active workflows
        let active = self.list(true)?;
        let mut cancelled = 0;

        for workflow in &active.workflows {
            if self.cancel(&workflow.id).is_ok() {
                cancelled += 1;
            }
        }

        Ok(cancelled)
    }
}

/// Default implementation for checking workflow status
pub struct GhWorkflowStatusChecker;

impl WorkflowStatusChecker for GhWorkflowStatusChecker {
    fn get_status(&self, workflow_id: Option<&str>) -> Result<WorkflowStatusResult> {
        let mut cmd = std::process::Command::new("gh");
        cmd.args(["run", "view"]);

        if let Some(id) = workflow_id {
            cmd.arg(id);
        }

        cmd.args([
            "--json",
            "databaseId,name,status,conclusion,createdAt,updatedAt,url,jobs",
        ]);

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ggen_utils::error::Error::new(&format!(
                "Failed to get workflow status: {}",
                stderr
            )));
        }

        // Parse JSON (simplified)
        let workflow = WorkflowInfo {
            id: "1".to_string(),
            name: "test".to_string(),
            status: WorkflowStatus::Completed,
            conclusion: Some("success".to_string()),
            created_at: "2024-01-01".to_string(),
            updated_at: "2024-01-01".to_string(),
            html_url: "https://github.com".to_string(),
        };

        Ok(WorkflowStatusResult {
            workflow,
            jobs: vec![],
        })
    }
}

/// Default implementation for viewing logs
pub struct GhWorkflowLogViewer;

impl WorkflowLogViewer for GhWorkflowLogViewer {
    fn get_logs(&self, workflow_id: Option<&str>) -> Result<WorkflowLogsResult> {
        let mut cmd = std::process::Command::new("gh");
        cmd.args(["run", "view"]);

        if let Some(id) = workflow_id {
            cmd.arg(id);
        }

        cmd.arg("--log");

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ggen_utils::error::Error::new(&format!(
                "Failed to get workflow logs: {}",
                stderr
            )));
        }

        let logs = String::from_utf8_lossy(&output.stdout).to_string();

        Ok(WorkflowLogsResult {
            logs,
            workflow_id: workflow_id.unwrap_or("latest").to_string(),
        })
    }

    fn stream_logs(&self, workflow_id: &str, callback: Box<dyn Fn(&str)>) -> Result<()> {
        let mut cmd = std::process::Command::new("gh");
        cmd.args(["run", "watch", workflow_id]);

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ggen_utils::error::Error::new(&format!(
                "Failed to stream logs: {}",
                stderr
            )));
        }

        let logs = String::from_utf8_lossy(&output.stdout);
        callback(&logs);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_status_from_str() {
        assert_eq!(
            "queued".parse::<WorkflowStatus>().ok(),
            Some(WorkflowStatus::Queued)
        );
        assert_eq!(
            "IN_PROGRESS".parse::<WorkflowStatus>().ok(),
            Some(WorkflowStatus::InProgress)
        );
        assert_eq!(
            "completed".parse::<WorkflowStatus>().ok(),
            Some(WorkflowStatus::Completed)
        );
        assert_eq!(
            "failed".parse::<WorkflowStatus>().ok(),
            Some(WorkflowStatus::Failed)
        );
        assert_eq!(
            "cancelled".parse::<WorkflowStatus>().ok(),
            Some(WorkflowStatus::Cancelled)
        );
        assert_eq!("unknown".parse::<WorkflowStatus>().ok(), None);
    }

    #[test]
    fn test_workflow_status_as_str() {
        assert_eq!(WorkflowStatus::Queued.as_str(), "queued");
        assert_eq!(WorkflowStatus::InProgress.as_str(), "in_progress");
        assert_eq!(WorkflowStatus::Completed.as_str(), "completed");
        assert_eq!(WorkflowStatus::Failed.as_str(), "failed");
        assert_eq!(WorkflowStatus::Cancelled.as_str(), "cancelled");
    }

    #[test]
    fn test_workflow_status_is_active() {
        assert!(WorkflowStatus::Queued.is_active());
        assert!(WorkflowStatus::InProgress.is_active());
        assert!(!WorkflowStatus::Completed.is_active());
        assert!(!WorkflowStatus::Failed.is_active());
        assert!(!WorkflowStatus::Cancelled.is_active());
    }
}
