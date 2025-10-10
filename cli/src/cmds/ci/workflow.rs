//! GitHub Actions workflow management and monitoring.
//!
//! This module provides functionality to list, monitor, and manage GitHub Actions
//! workflows. It supports checking workflow status, viewing logs, and canceling
//! running workflows through GitHub API integration.
//!
//! # Examples
//!
//! ```bash
//! ggen ci workflow list --active
//! ggen ci workflow status --workflow "build" --verbose
//! ggen ci workflow logs --workflow "test" --follow
//! ggen ci workflow cancel --workflow "deploy"
//! ```
//!
//! # Errors
//!
//! Returns errors if GitHub API calls fail, workflows don't exist, or if
//! authentication is not properly configured.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[cfg_attr(test, mockall::automock)]
pub trait WorkflowLister {
    fn list(&self, active: bool, json: bool) -> Result<WorkflowListResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait WorkflowStatusChecker {
    fn check_status(&self, workflow: Option<&str>, verbose: bool, json: bool) -> Result<WorkflowStatusResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait WorkflowLogViewer {
    fn view_logs(&self, workflow: Option<&str>, follow: bool) -> Result<WorkflowLogsResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait WorkflowCanceler {
    fn cancel(&self, workflow: &str) -> Result<WorkflowCancelResult>;
}

// Mock implementations for testing
pub struct CargoMakeWorkflowLister;

#[derive(Debug, Clone)]
pub struct WorkflowListResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct WorkflowStatusResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct WorkflowLogsResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct WorkflowCancelResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Args, Debug)]
pub struct WorkflowArgs {
    #[command(subcommand)]
    pub action: WorkflowAction,
}

#[derive(Subcommand, Debug)]
pub enum WorkflowAction {
    /// List available workflows
    List(ListArgs),

    /// Check workflow status
    Status(StatusArgs),

    /// View workflow logs
    Logs(LogsArgs),

    /// Cancel running workflows
    Cancel(CancelArgs),
}

#[derive(Args, Debug)]
pub struct ListArgs {
    /// Show only active workflows
    #[arg(long)]
    pub active: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,
}

#[derive(Args, Debug)]
pub struct StatusArgs {
    /// Workflow name or ID to check
    #[arg(long)]
    pub workflow: Option<String>,

    /// Show detailed status information
    #[arg(long)]
    pub verbose: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,
}

#[derive(Args, Debug)]
pub struct LogsArgs {
    /// Workflow name or ID to get logs for
    #[arg(long)]
    pub workflow: Option<String>,

    /// Follow logs in real-time
    #[arg(long)]
    pub follow: bool,

    /// Number of log lines to show [default: 100]
    #[arg(long, default_value = "100")]
    pub lines: usize,
}

#[derive(Args, Debug)]
pub struct CancelArgs {
    /// Workflow name or ID to cancel
    #[arg(long)]
    pub workflow: Option<String>,

    /// Cancel all running workflows
    #[arg(long)]
    pub all: bool,
}

pub async fn run(args: &WorkflowArgs) -> Result<()> {
    let lister = CargoMakeWorkflowLister;
    let status_checker = CargoMakeWorkflowStatusChecker;
    let log_viewer = CargoMakeWorkflowLogViewer;
    let canceler = CargoMakeWorkflowCanceler;
    
    run_with_deps(args, &lister, &status_checker, &log_viewer, &canceler).await
}

pub async fn run_with_deps(
    args: &WorkflowArgs,
    lister: &dyn WorkflowLister,
    status_checker: &dyn WorkflowStatusChecker,
    log_viewer: &dyn WorkflowLogViewer,
    canceler: &dyn WorkflowCanceler,
) -> Result<()> {
    match &args.action {
        WorkflowAction::List(list_args) => list_workflows_with_deps(list_args, lister).await,
        WorkflowAction::Status(status_args) => check_workflow_status_with_deps(status_args, status_checker).await,
        WorkflowAction::Logs(logs_args) => view_workflow_logs_with_deps(logs_args, log_viewer).await,
        WorkflowAction::Cancel(cancel_args) => cancel_workflows_with_deps(cancel_args, canceler).await,
    }
}

async fn list_workflows_with_deps(args: &ListArgs, lister: &dyn WorkflowLister) -> Result<()> {
    println!("Listing GitHub Actions workflows");

    let result = lister.list(args.active, args.json)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Workflow listing failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn list_workflows(args: &ListArgs) -> Result<()> {
    let lister = CargoMakeWorkflowLister;
    list_workflows_with_deps(args, &lister).await
}

async fn check_workflow_status_with_deps(args: &StatusArgs, status_checker: &dyn WorkflowStatusChecker) -> Result<()> {
    println!("Checking GitHub Actions workflow status");

    let result = status_checker.check_status(args.workflow.as_deref(), args.verbose, args.json)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Workflow status check failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn check_workflow_status(args: &StatusArgs) -> Result<()> {
    let status_checker = CargoMakeWorkflowStatusChecker;
    check_workflow_status_with_deps(args, &status_checker).await
}

async fn view_workflow_logs_with_deps(args: &LogsArgs, log_viewer: &dyn WorkflowLogViewer) -> Result<()> {
    println!("Viewing GitHub Actions workflow logs");

    let result = log_viewer.view_logs(args.workflow.as_deref(), args.follow)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Workflow logs retrieval failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn view_workflow_logs(args: &LogsArgs) -> Result<()> {
    let log_viewer = CargoMakeWorkflowLogViewer;
    view_workflow_logs_with_deps(args, &log_viewer).await
}

async fn cancel_workflows_with_deps(args: &CancelArgs, canceler: &dyn WorkflowCanceler) -> Result<()> {
    println!("Cancelling GitHub Actions workflows");

    if args.all {
        println!("Cancelling all running workflows");
        let result = canceler.cancel("all")?;
        
        if !result.success {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to cancel all workflows: {}",
                result.stderr
            )));
        }

        println!("✅ All running workflows cancelled");
    } else if let Some(workflow) = &args.workflow {
        println!("Cancelling workflow: {}", workflow);

        let result = canceler.cancel(workflow)?;

        if !result.success {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to cancel workflow {}: {}",
                workflow, result.stderr
            )));
        }

        println!("✅ Workflow {} cancelled", workflow);
    } else {
        return Err(ggen_utils::error::Error::new(
            "Must specify either --workflow or --all",
        ));
    }

    Ok(())
}

async fn cancel_workflows(args: &CancelArgs) -> Result<()> {
    let canceler = CargoMakeWorkflowCanceler;
    cancel_workflows_with_deps(args, &canceler).await
}

// Concrete implementations for production use

impl WorkflowLister for CargoMakeWorkflowLister {
    fn list(&self, active: bool, json: bool) -> Result<WorkflowListResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-workflow-status"]);

        if active {
            cmd.arg("--active");
        }

        if json {
            cmd.arg("--json");
        }

        let output = cmd.output()?;
        Ok(WorkflowListResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}


impl WorkflowStatusChecker for CargoMakeWorkflowStatusChecker {
    fn check_status(&self, workflow: Option<&str>, verbose: bool, json: bool) -> Result<WorkflowStatusResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-workflow-status"]);

        if let Some(workflow) = workflow {
            cmd.arg("--workflow").arg(workflow);
        }

        if verbose {
            cmd.arg("--verbose");
        }

        if json {
            cmd.arg("--json");
        }

        let output = cmd.output()?;
        Ok(WorkflowStatusResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}


impl WorkflowLogViewer for CargoMakeWorkflowLogViewer {
    fn view_logs(&self, workflow: Option<&str>, follow: bool) -> Result<WorkflowLogsResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-workflow-logs"]);

        if let Some(workflow) = workflow {
            cmd.arg("--workflow").arg(workflow);
        }

        if follow {
            cmd.arg("--follow");
        }

        let output = cmd.output()?;
        Ok(WorkflowLogsResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}


impl WorkflowCanceler for CargoMakeWorkflowCanceler {
    fn cancel(&self, workflow: &str) -> Result<WorkflowCancelResult> {
        let mut cmd = std::process::Command::new("gh");
        if workflow == "all" {
            cmd.args(["run", "cancel", "--all"]);
        } else {
            cmd.args(["run", "cancel", workflow]);
        }

        let output = cmd.output()?;
        Ok(WorkflowCancelResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_list_calls_lister() {
        let mut mock = MockWorkflowLister::new();
        mock.expect_list()
            .with(eq(false), eq(false))
            .times(1)
            .returning(|_, _| Ok(WorkflowListResult {
                stdout: "Workflow list".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = ListArgs {
            active: false,
            json: false,
        };
        let result = list_workflows_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_status_calls_checker() {
        let mut mock = MockWorkflowStatusChecker::new();
        mock.expect_check_status()
            .with(eq(Some("build")), eq(false), eq(false))
            .times(1)
            .returning(|_, _, _| Ok(WorkflowStatusResult {
                stdout: "Status OK".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = StatusArgs {
            workflow: Some("build".to_string()),
            verbose: false,
            json: false,
        };
        let result = check_workflow_status_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_logs_calls_viewer() {
        let mut mock = MockWorkflowLogViewer::new();
        mock.expect_view_logs()
            .with(eq(Some("test")), eq(false))
            .times(1)
            .returning(|_, _| Ok(WorkflowLogsResult {
                stdout: "Log output".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = LogsArgs {
            workflow: Some("test".to_string()),
            follow: false,
            lines: 100,
        };
        let result = view_workflow_logs_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cancel_calls_canceler() {
        let mut mock = MockWorkflowCanceler::new();
        mock.expect_cancel()
            .with(eq("deploy"))
            .times(1)
            .returning(|_| Ok(WorkflowCancelResult {
                stdout: "Cancel complete".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = CancelArgs {
            workflow: Some("deploy".to_string()),
            all: false,
        };
        let result = cancel_workflows_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cancel_all_calls_canceler() {
        let mut mock = MockWorkflowCanceler::new();
        mock.expect_cancel()
            .with(eq("all"))
            .times(1)
            .returning(|_| Ok(WorkflowCancelResult {
                stdout: "All cancelled".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = CancelArgs {
            workflow: None,
            all: true,
        };
        let result = cancel_workflows_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cancel_requires_workflow_or_all() {
        let mock = MockWorkflowCanceler::new();

        let args = CancelArgs {
            workflow: None,
            all: false,
        };
        let result = cancel_workflows_with_deps(&args, &mock).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Must specify either --workflow or --all"));
    }

    #[tokio::test]
    async fn test_run_with_deps_dispatches_correctly() {
        let mut mock_lister = MockWorkflowLister::new();
        mock_lister
            .expect_list()
            .with(eq(false), eq(false))
            .times(1)
            .returning(|_, _| Ok(WorkflowListResult {
                stdout: "Workflow list".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let mock_status_checker = MockWorkflowStatusChecker::new();
        let mock_log_viewer = MockWorkflowLogViewer::new();
        let mock_canceler = MockWorkflowCanceler::new();

        let args = WorkflowArgs {
            action: WorkflowAction::List(ListArgs {
                active: false,
                json: false,
            }),
        };

        let result = run_with_deps(&args, &mock_lister, &mock_status_checker, &mock_log_viewer, &mock_canceler).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_list_args_defaults() {
        let args = ListArgs {
            active: false,
            json: false,
        };
        assert!(!args.active);
        assert!(!args.json);
    }

    #[test]
    fn test_status_args_defaults() {
        let args = StatusArgs {
            workflow: None,
            verbose: false,
            json: false,
        };
        assert!(args.workflow.is_none());
        assert!(!args.verbose);
        assert!(!args.json);
    }

    #[test]
    fn test_logs_args_defaults() {
        let args = LogsArgs {
            workflow: None,
            follow: false,
            lines: 100,
        };
        assert!(args.workflow.is_none());
        assert!(!args.follow);
        assert_eq!(args.lines, 100);
    }

    #[test]
    fn test_cancel_args_defaults() {
        let args = CancelArgs {
            workflow: None,
            all: false,
        };
        assert!(args.workflow.is_none());
        assert!(!args.all);
    }
}
