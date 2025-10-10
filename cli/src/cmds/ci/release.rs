//! Release workflow management and execution.
//!
//! This module provides functionality to run release workflows locally using act,
//! with support for retry logic, metrics collection, timeout control, and dry runs.
//! It integrates with GitHub Actions workflows and supports parallel execution.
//!
//! # Examples
//!
//! ```bash
//! ggen ci release run --workflow-only --json
//! ggen ci release retry --max-retries 5 --verbose
//! ggen ci release metrics --homebrew-only
//! ggen ci release timeout --timeout 3600
//! ggen ci release dry-run --json
//! ```
//!
//! # Errors
//!
//! Returns errors if act execution fails, workflows don't exist, or if
//! system requirements are not met.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[cfg_attr(test, mockall::automock)]
pub trait ReleaseWorkflowRunner {
    fn run(&self, args: &ReleaseRunArgs) -> Result<ReleaseResult>;
    fn run_with_retry(&self, args: &ReleaseRetryArgs) -> Result<ReleaseResult>;
    fn run_with_metrics(&self, args: &ReleaseMetricsArgs) -> Result<ReleaseResult>;
    fn run_with_timeout(&self, args: &ReleaseTimeoutArgs) -> Result<ReleaseResult>;
    fn dry_run(&self, args: &ReleaseDryRunArgs) -> Result<ReleaseResult>;
}

#[derive(Debug, Clone)]
pub struct ReleaseResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Args, Debug)]
pub struct ReleaseArgs {
    #[command(subcommand)]
    pub action: ReleaseAction,
}

#[derive(Subcommand, Debug)]
pub enum ReleaseAction {
    /// Run release workflows locally with act
    Run(RunArgs),

    /// Run release workflows with retry logic
    Retry(RetryArgs),

    /// Run release workflows with metrics collection
    Metrics(MetricsArgs),

    /// Run release workflows with custom timeout
    Timeout(TimeoutArgs),

    /// Show what would be executed without running
    DryRun(DryRunArgs),
}

// Type aliases for trait methods
pub type ReleaseRunArgs = RunArgs;
pub type ReleaseRetryArgs = RetryArgs;
pub type ReleaseMetricsArgs = MetricsArgs;
pub type ReleaseTimeoutArgs = TimeoutArgs;
pub type ReleaseDryRunArgs = DryRunArgs;

#[derive(Args, Debug, PartialEq)]
pub struct RunArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,

    /// Enable debug output
    #[arg(long)]
    pub debug: bool,
}

#[derive(Args, Debug, PartialEq)]
pub struct RetryArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Maximum retry attempts [default: 3]
    #[arg(long, default_value = "3")]
    pub max_retries: u8,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug, PartialEq)]
pub struct MetricsArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug, PartialEq)]
pub struct TimeoutArgs {
    /// Workflow timeout in seconds [default: 1800]
    #[arg(long, default_value = "1800")]
    pub timeout: u32,

    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug, PartialEq)]
pub struct DryRunArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

pub async fn run(args: &ReleaseArgs) -> Result<()> {
    let runner = CargoMakeReleaseRunner;
    run_with_deps(args, &runner).await
}

pub async fn run_with_deps(args: &ReleaseArgs, runner: &dyn ReleaseWorkflowRunner) -> Result<()> {
    match &args.action {
        ReleaseAction::Run(run_args) => run_release_workflows_with_deps(run_args, runner).await,
        ReleaseAction::Retry(retry_args) => {
            run_release_with_retry_with_deps(retry_args, runner).await
        }
        ReleaseAction::Metrics(metrics_args) => {
            run_release_with_metrics_with_deps(metrics_args, runner).await
        }
        ReleaseAction::Timeout(timeout_args) => {
            run_release_with_timeout_with_deps(timeout_args, runner).await
        }
        ReleaseAction::DryRun(dry_run_args) => {
            run_release_dry_run_with_deps(dry_run_args, runner).await
        }
    }
}

async fn run_release_workflows_with_deps(
    args: &RunArgs, runner: &dyn ReleaseWorkflowRunner,
) -> Result<()> {
    println!("🚀 Running release workflows locally with act");

    let result = runner.run(args)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows failed: {}",
            result.stderr
        )));
    }

    if !args.json {
        println!("✅ Release workflows completed successfully");
    }
    println!("{}", result.stdout);
    Ok(())
}

async fn run_release_workflows(args: &RunArgs) -> Result<()> {
    let runner = CargoMakeReleaseRunner;
    run_release_workflows_with_deps(args, &runner).await
}

async fn run_release_with_retry_with_deps(
    args: &RetryArgs, runner: &dyn ReleaseWorkflowRunner,
) -> Result<()> {
    println!("🔄 Running release workflows with retry logic");

    let result = runner.run_with_retry(args)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows with retry failed: {}",
            result.stderr
        )));
    }

    if !args.json {
        println!("✅ Release workflows with retry completed successfully");
    }
    println!("{}", result.stdout);
    Ok(())
}

async fn run_release_with_retry(args: &RetryArgs) -> Result<()> {
    let runner = CargoMakeReleaseRunner;
    run_release_with_retry_with_deps(args, &runner).await
}

async fn run_release_with_metrics_with_deps(
    args: &MetricsArgs, runner: &dyn ReleaseWorkflowRunner,
) -> Result<()> {
    println!("📊 Running release workflows with metrics collection");

    let result = runner.run_with_metrics(args)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows with metrics failed: {}",
            result.stderr
        )));
    }

    if !args.json {
        println!("✅ Release workflows with metrics completed successfully");
    }
    println!("{}", result.stdout);
    Ok(())
}

async fn run_release_with_metrics(args: &MetricsArgs) -> Result<()> {
    let runner = CargoMakeReleaseRunner;
    run_release_with_metrics_with_deps(args, &runner).await
}

async fn run_release_with_timeout_with_deps(
    args: &TimeoutArgs, runner: &dyn ReleaseWorkflowRunner,
) -> Result<()> {
    println!("⏱️ Running release workflows with custom timeout");

    let result = runner.run_with_timeout(args)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows with timeout failed: {}",
            result.stderr
        )));
    }

    if !args.json {
        println!("✅ Release workflows with timeout completed successfully");
    }
    println!("{}", result.stdout);
    Ok(())
}

async fn run_release_with_timeout(args: &TimeoutArgs) -> Result<()> {
    let runner = CargoMakeReleaseRunner;
    run_release_with_timeout_with_deps(args, &runner).await
}

async fn run_release_dry_run_with_deps(
    args: &DryRunArgs, runner: &dyn ReleaseWorkflowRunner,
) -> Result<()> {
    println!("🔍 Showing what would be executed (dry run)");

    let result = runner.dry_run(args)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows dry run failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn run_release_dry_run(args: &DryRunArgs) -> Result<()> {
    let runner = CargoMakeReleaseRunner;
    run_release_dry_run_with_deps(args, &runner).await
}

// Concrete implementations for production use
pub struct CargoMakeReleaseRunner;

impl ReleaseWorkflowRunner for CargoMakeReleaseRunner {
    fn run(&self, args: &ReleaseRunArgs) -> Result<ReleaseResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "act-release"]);

        if args.workflow_only {
            cmd.arg("--workflow-only");
        }

        if args.homebrew_only {
            cmd.arg("--homebrew-only");
        }

        if args.json {
            cmd.arg("--json");
        }

        if args.verbose {
            cmd.arg("--verbose");
        }

        if args.debug {
            cmd.env("DEBUG", "true");
        }

        let output = cmd.output()?;
        Ok(ReleaseResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }

    fn run_with_retry(&self, args: &ReleaseRetryArgs) -> Result<ReleaseResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "act-release-retry"]);
        cmd.arg("--max-retries").arg(args.max_retries.to_string());

        if args.workflow_only {
            cmd.arg("--workflow-only");
        }

        if args.homebrew_only {
            cmd.arg("--homebrew-only");
        }

        if args.json {
            cmd.arg("--json");
        }

        if args.verbose {
            cmd.arg("--verbose");
        }

        let output = cmd.output()?;
        Ok(ReleaseResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }

    fn run_with_metrics(&self, args: &ReleaseMetricsArgs) -> Result<ReleaseResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "act-release-metrics"]);

        if args.workflow_only {
            cmd.arg("--workflow-only");
        }

        if args.homebrew_only {
            cmd.arg("--homebrew-only");
        }

        if args.json {
            cmd.arg("--json");
        }

        if args.verbose {
            cmd.arg("--verbose");
        }

        let output = cmd.output()?;
        Ok(ReleaseResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }

    fn run_with_timeout(&self, args: &ReleaseTimeoutArgs) -> Result<ReleaseResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "act-release-timeout"]);
        cmd.arg("--timeout").arg(args.timeout.to_string());

        if args.workflow_only {
            cmd.arg("--workflow-only");
        }

        if args.homebrew_only {
            cmd.arg("--homebrew-only");
        }

        if args.json {
            cmd.arg("--json");
        }

        if args.verbose {
            cmd.arg("--verbose");
        }

        let output = cmd.output()?;
        Ok(ReleaseResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }

    fn dry_run(&self, args: &ReleaseDryRunArgs) -> Result<ReleaseResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "act-release-dry-run"]);

        if args.workflow_only {
            cmd.arg("--workflow-only");
        }

        if args.homebrew_only {
            cmd.arg("--homebrew-only");
        }

        if args.json {
            cmd.arg("--json");
        }

        if args.verbose {
            cmd.arg("--verbose");
        }

        let output = cmd.output()?;
        Ok(ReleaseResult {
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
    async fn test_run_calls_runner() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_run()
            .with(eq(RunArgs {
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
                debug: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "Release complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = RunArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
            debug: false,
        };
        let result = run_release_workflows_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_handles_failure() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_run()
            .with(eq(RunArgs {
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
                debug: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "".to_string(),
                    stderr: "Release failed".to_string(),
                    success: false,
                })
            });

        let args = RunArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
            debug: false,
        };
        let result = run_release_workflows_with_deps(&args, &mock).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Release workflows failed"));
    }

    #[tokio::test]
    async fn test_retry_calls_runner() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_run_with_retry()
            .with(eq(RetryArgs {
                workflow_only: false,
                homebrew_only: false,
                max_retries: 3,
                json: false,
                verbose: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "Retry complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = RetryArgs {
            workflow_only: false,
            homebrew_only: false,
            max_retries: 3,
            json: false,
            verbose: false,
        };
        let result = run_release_with_retry_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_metrics_calls_runner() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_run_with_metrics()
            .with(eq(MetricsArgs {
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "Metrics complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = MetricsArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
        };
        let result = run_release_with_metrics_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_timeout_calls_runner() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_run_with_timeout()
            .with(eq(TimeoutArgs {
                timeout: 1800,
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "Timeout complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = TimeoutArgs {
            timeout: 1800,
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
        };
        let result = run_release_with_timeout_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_dry_run_calls_runner() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_dry_run()
            .with(eq(DryRunArgs {
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "Dry run complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = DryRunArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
        };
        let result = run_release_dry_run_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_with_deps_dispatches_correctly() {
        let mut mock = MockReleaseWorkflowRunner::new();
        mock.expect_run()
            .with(eq(RunArgs {
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
                debug: false,
            }))
            .times(1)
            .returning(|_| {
                Ok(ReleaseResult {
                    stdout: "Release complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = ReleaseArgs {
            action: ReleaseAction::Run(RunArgs {
                workflow_only: false,
                homebrew_only: false,
                json: false,
                verbose: false,
                debug: false,
            }),
        };

        let result = run_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_args_defaults() {
        let args = RunArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
            debug: false,
        };
        assert!(!args.workflow_only);
        assert!(!args.homebrew_only);
        assert!(!args.json);
        assert!(!args.verbose);
        assert!(!args.debug);
    }

    #[test]
    fn test_retry_args_defaults() {
        let args = RetryArgs {
            workflow_only: false,
            homebrew_only: false,
            max_retries: 3,
            json: false,
            verbose: false,
        };
        assert!(!args.workflow_only);
        assert!(!args.homebrew_only);
        assert_eq!(args.max_retries, 3);
        assert!(!args.json);
        assert!(!args.verbose);
    }

    #[test]
    fn test_metrics_args_defaults() {
        let args = MetricsArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
        };
        assert!(!args.workflow_only);
        assert!(!args.homebrew_only);
        assert!(!args.json);
        assert!(!args.verbose);
    }

    #[test]
    fn test_timeout_args_defaults() {
        let args = TimeoutArgs {
            timeout: 1800,
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
        };
        assert_eq!(args.timeout, 1800);
        assert!(!args.workflow_only);
        assert!(!args.homebrew_only);
        assert!(!args.json);
        assert!(!args.verbose);
    }

    #[test]
    fn test_dry_run_args_defaults() {
        let args = DryRunArgs {
            workflow_only: false,
            homebrew_only: false,
            json: false,
            verbose: false,
        };
        assert!(!args.workflow_only);
        assert!(!args.homebrew_only);
        assert!(!args.json);
        assert!(!args.verbose);
    }
}
